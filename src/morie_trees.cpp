// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Compiled kernel for the native tree ensembles in R/trees_native.R.
//
// The pure-R recursion is correct but spends nearly all of its time in
// interpreter overhead: a fully grown forest visits O(n) nodes per tree and
// every visit allocates R objects. Moving the recursion here keeps the exact
// same split rule and leaf weight while removing that overhead.
//
// Split rule and leaf weight are XGBoost's regularised objective, per the
// project's documentation
// (https://xgboost.readthedocs.io/en/stable/tutorials/model.html):
//
//     w*   = -G / (H + lambda)
//     Gain = 0.5 [ G_L^2/(H_L+lambda) + G_R^2/(H_R+lambda)
//                  - (G_L+G_R)^2/(H_L+H_R+lambda) ] - gamma
//
// With g = -y, h = 1, lambda = 0 this is exactly the variance reduction CART
// maximises, which is what the random forest path uses (Breiman 2001; ESL
// Algorithm 15.1). Gini splitting for classification forests is handled by
// the same machinery applied to the 0/1 indicator of each class, since for a
// binary indicator the Gini impurity 2p(1-p) is twice its variance.
//
// Each column is sorted once up front; a node then recovers its own sorted
// order by scanning that global order and keeping the rows it owns. That is
// O(n) per node per feature instead of an O(n log n) sort, and is the single
// change that closes most of the gap to randomForest.

#include <Rcpp.h>
#include <algorithm>
#include <vector>

using namespace Rcpp;

namespace {

struct Node {
  bool leaf = true;
  int feature = -1;
  double threshold = 0.0;
  double weight = 0.0;
  int left = -1;
  int right = -1;
};

inline double soft_threshold(double g, double alpha) {
  if (alpha <= 0.0) return g;
  if (g > alpha) return g - alpha;
  if (g < -alpha) return g + alpha;
  return 0.0;
}

inline double leaf_weight(double G, double H, double lambda, double alpha) {
  return -soft_threshold(G, alpha) / (H + lambda);
}

struct Builder {
  const double* X;
  const double* g;
  const double* h;
  int n;
  int p;
  int max_depth;
  int min_node;
  int mtry;
  double lambda;
  double alpha;
  double gamma_pen;
  const std::vector<std::vector<int> >* order;   // global sort order per column
  std::vector<Node> nodes;
  std::vector<double> importance;
  std::vector<char> in_node;                     // membership mask, reused

  int build(std::vector<int>& idx, int depth) {
    double G = 0.0, H = 0.0;
    for (size_t i = 0; i < idx.size(); ++i) { G += g[idx[i]]; H += h[idx[i]]; }

    Node nd;
    nd.leaf = true;
    nd.weight = leaf_weight(G, H, lambda, alpha);
    int self = static_cast<int>(nodes.size());
    nodes.push_back(nd);

    if (depth >= max_depth ||
        static_cast<int>(idx.size()) < 2 * min_node) return self;

    // Candidate features for this node -- partial Fisher-Yates so the draw
    // is without replacement and costs O(mtry), not O(p log p).
    std::vector<int> feats(p);
    for (int j = 0; j < p; ++j) feats[j] = j;
    int m = std::min(mtry, p);
    for (int k = 0; k < m; ++k) {
      int r = k + static_cast<int>(R::unif_rand() * (p - k));
      if (r >= p) r = p - 1;
      std::swap(feats[k], feats[r]);
    }

    for (size_t i = 0; i < idx.size(); ++i) in_node[idx[i]] = 1;

    const double parent = G * G / (H + lambda);
    double best_gain = 0.0, best_thr = 0.0;
    int best_feat = -1;
    std::vector<int> sorted;
    sorted.reserve(idx.size());

    for (int k = 0; k < m; ++k) {
      const int j = feats[k];
      const std::vector<int>& ord = (*order)[j];
      sorted.clear();
      for (size_t t = 0; t < ord.size(); ++t)
        if (in_node[ord[t]]) sorted.push_back(ord[t]);

      const int nn = static_cast<int>(sorted.size());
      if (nn < 2) continue;
      double GL = 0.0, HL = 0.0;
      for (int t = 0; t < nn - 1; ++t) {
        GL += g[sorted[t]];
        HL += h[sorted[t]];
        const double v = X[sorted[t] + static_cast<size_t>(j) * n];
        const double vn = X[sorted[t + 1] + static_cast<size_t>(j) * n];
        if (!(v < vn)) continue;                 // tied values: not a split
        if (t + 1 < min_node || nn - t - 1 < min_node) continue;
        const double GR = G - GL, HR = H - HL;
        const double gain = 0.5 * (GL * GL / (HL + lambda) +
                                   GR * GR / (HR + lambda) - parent) - gamma_pen;
        if (gain > best_gain) {
          best_gain = gain;
          best_feat = j;
          best_thr = 0.5 * (v + vn);
        }
      }
    }

    for (size_t i = 0; i < idx.size(); ++i) in_node[idx[i]] = 0;

    if (best_feat < 0) return self;

    std::vector<int> l, r;
    l.reserve(idx.size());
    r.reserve(idx.size());
    for (size_t i = 0; i < idx.size(); ++i) {
      if (X[idx[i] + static_cast<size_t>(best_feat) * n] <= best_thr)
        l.push_back(idx[i]);
      else
        r.push_back(idx[i]);
    }
    if (l.empty() || r.empty()) return self;

    importance[best_feat] += best_gain;
    const int li = build(l, depth + 1);
    const int ri = build(r, depth + 1);
    nodes[self].leaf = false;
    nodes[self].feature = best_feat;
    nodes[self].threshold = best_thr;
    nodes[self].left = li;
    nodes[self].right = ri;
    return self;
  }
};

// Flatten a built tree into the parallel vectors handed back to R.
List tree_to_list(const std::vector<Node>& nodes) {
  const int k = static_cast<int>(nodes.size());
  IntegerVector feat(k), left(k), right(k);
  NumericVector thr(k), w(k);
  LogicalVector leaf(k);
  for (int i = 0; i < k; ++i) {
    leaf[i] = nodes[i].leaf;
    feat[i] = nodes[i].feature + 1;              // 1-based for R
    thr[i] = nodes[i].threshold;
    w[i] = nodes[i].weight;
    left[i] = nodes[i].left + 1;
    right[i] = nodes[i].right + 1;
  }
  return List::create(_["leaf"] = leaf, _["feature"] = feat,
                      _["threshold"] = thr, _["weight"] = w,
                      _["left"] = left, _["right"] = right);
}

std::vector<std::vector<int> > column_orders(const NumericMatrix& X) {
  const int n = X.nrow(), p = X.ncol();
  std::vector<std::vector<int> > ord(p);
  for (int j = 0; j < p; ++j) {
    ord[j].resize(n);
    for (int i = 0; i < n; ++i) ord[j][i] = i;
    const double* col = &X(0, j);
    std::stable_sort(ord[j].begin(), ord[j].end(),
                     [col](int a, int b) { return col[a] < col[b]; });
  }
  return ord;
}

}  // namespace

//' Grow one regression / second-order tree (compiled)
//' @noRd
// [[Rcpp::export]]
List morie_tree_fit_cpp(NumericMatrix X, NumericVector g, NumericVector h,
                        int max_depth, int min_node, int mtry,
                        double lambda, double alpha, double gamma_pen) {
  const int n = X.nrow(), p = X.ncol();
  std::vector<std::vector<int> > ord = column_orders(X);
  Builder b;
  b.X = &X[0]; b.g = &g[0]; b.h = &h[0];
  b.n = n; b.p = p;
  b.max_depth = max_depth; b.min_node = min_node; b.mtry = mtry;
  b.lambda = lambda; b.alpha = alpha; b.gamma_pen = gamma_pen;
  b.order = &ord;
  b.importance.assign(p, 0.0);
  b.in_node.assign(n, 0);
  std::vector<int> idx(n);
  for (int i = 0; i < n; ++i) idx[i] = i;
  b.build(idx, 0);
  return List::create(_["tree"] = tree_to_list(b.nodes),
                      _["importance"] = NumericVector(b.importance.begin(),
                                                      b.importance.end()));
}

//' Predict from a flattened tree (compiled)
//' @noRd
// [[Rcpp::export]]
NumericVector morie_tree_predict_cpp(List tree, NumericMatrix X) {
  LogicalVector leaf = tree["leaf"];
  IntegerVector feat = tree["feature"], left = tree["left"], right = tree["right"];
  NumericVector thr = tree["threshold"], w = tree["weight"];
  const int n = X.nrow();
  NumericVector out(n);
  for (int i = 0; i < n; ++i) {
    int node = 0;
    while (!leaf[node]) {
      node = (X(i, feat[node] - 1) <= thr[node]) ? left[node] - 1 : right[node] - 1;
    }
    out[i] = w[node];
  }
  return out;
}
