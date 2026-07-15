// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Weighted subsampled regression forest (feat/native-specializations,
// module 11). Powers the native causal forest: the R-learner objective
// min_tau sum_i v_i^2 (u_i/v_i - tau(x_i))^2 is a weighted regression
// of the pseudo-outcome u/v on X with weights v^2 (Nie & Wager 2021),
// so an honest weighted regression forest on (X, u/v, w = v^2) yields
// tau(x). Trees: greedy CART, mtry = ceil(sqrt(p)) features per node,
// subsample-without-replacement per tree, weighted-mean leaves.

#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <random>
#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif

namespace {

struct Node {
  int feature = -1;
  double threshold = 0.0;
  int left = -1, right = -1;
  double value = 0.0;
};

struct Tree {
  std::vector<Node> nodes;
};

void build_node(const Rcpp::NumericMatrix& X,
                const std::vector<double>& y,
                const std::vector<double>& w,
                std::vector<int>& idx, int lo, int hi,
                int depth, int max_depth, int min_node, int mtry,
                std::mt19937& rng, Tree& tree, int node_id) {
  double sw = 0.0, swy = 0.0;
  for (int i = lo; i < hi; ++i) {
    sw += w[idx[i]];
    swy += w[idx[i]] * y[idx[i]];
  }
  tree.nodes[node_id].value = sw > 0 ? swy / sw : 0.0;
  const int n = hi - lo;
  if (depth >= max_depth || n < 2 * min_node) return;

  const int p = X.ncol();
  std::vector<int> feats(p);
  for (int j = 0; j < p; ++j) feats[j] = j;
  std::shuffle(feats.begin(), feats.end(), rng);

  double best_gain = 1e-12;
  int best_f = -1;
  double best_thr = 0.0;
  const double parent_sse_term = sw > 0 ? swy * swy / sw : 0.0;

  for (int fi = 0; fi < mtry && fi < p; ++fi) {
    const int f = feats[fi];
    std::sort(idx.begin() + lo, idx.begin() + hi, [&](int a, int b) {
      return X(a, f) < X(b, f);
    });
    double lw = 0.0, lwy = 0.0;
    for (int i = lo; i < hi - 1; ++i) {
      const int ii = idx[i];
      lw += w[ii]; lwy += w[ii] * y[ii];
      if (i - lo + 1 < min_node || hi - i - 1 < min_node) continue;
      if (X(idx[i], f) == X(idx[i + 1], f)) continue;
      const double rw = sw - lw, rwy = swy - lwy;
      if (lw <= 0 || rw <= 0) continue;
      const double gain = lwy * lwy / lw + rwy * rwy / rw - parent_sse_term;
      if (gain > best_gain) {
        best_gain = gain;
        best_f = f;
        best_thr = 0.5 * (X(idx[i], f) + X(idx[i + 1], f));
      }
    }
  }
  if (best_f < 0) return;

  // partition idx[lo,hi) by the chosen split
  int mid = lo;
  std::vector<int> left_ids, right_ids;
  left_ids.reserve(n); right_ids.reserve(n);
  for (int i = lo; i < hi; ++i) {
    if (X(idx[i], best_f) <= best_thr) left_ids.push_back(idx[i]);
    else right_ids.push_back(idx[i]);
  }
  if (left_ids.empty() || right_ids.empty()) return;
  for (size_t k = 0; k < left_ids.size(); ++k) idx[lo + k] = left_ids[k];
  mid = lo + static_cast<int>(left_ids.size());
  for (size_t k = 0; k < right_ids.size(); ++k) idx[mid + k] = right_ids[k];

  const int li = static_cast<int>(tree.nodes.size());
  tree.nodes.push_back(Node());
  const int ri = static_cast<int>(tree.nodes.size());
  tree.nodes.push_back(Node());
  tree.nodes[node_id].feature = best_f;
  tree.nodes[node_id].threshold = best_thr;
  tree.nodes[node_id].left = li;
  tree.nodes[node_id].right = ri;
  build_node(X, y, w, idx, lo, mid, depth + 1, max_depth, min_node,
             mtry, rng, tree, li);
  build_node(X, y, w, idx, mid, hi, depth + 1, max_depth, min_node,
             mtry, rng, tree, ri);
}

double predict_tree(const Tree& tree, const Rcpp::NumericMatrix& X,
                    int row) {
  int cur = 0;
  while (tree.nodes[cur].feature >= 0) {
    cur = X(row, tree.nodes[cur].feature) <= tree.nodes[cur].threshold
      ? tree.nodes[cur].left : tree.nodes[cur].right;
  }
  return tree.nodes[cur].value;
}

}  // namespace

// [[Rcpp::export(".morie_rlearner_forest_cpp")]]
Rcpp::NumericMatrix morie_rlearner_forest_cpp(Rcpp::NumericMatrix X,
                                              Rcpp::NumericVector pseudo,
                                              Rcpp::NumericVector weight,
                                              Rcpp::NumericMatrix Xpred,
                                              int n_trees, int max_depth,
                                              int min_node, double subsample,
                                              int seed) {
  const int n = X.nrow();
  const int p = X.ncol();
  const int np = Xpred.nrow();
  const int mtry = std::max(1, static_cast<int>(std::ceil(std::sqrt(
      static_cast<double>(p)))));
  const int ss = std::max(2 * min_node,
                          static_cast<int>(subsample * n));
  std::vector<double> y(pseudo.begin(), pseudo.end());
  std::vector<double> w(weight.begin(), weight.end());

  // per-observation prediction sums; column 1 = mean over trees,
  // column 2 = between-half variance (little-bag style) for SEs
  std::vector<double> sum_all(np, 0.0), sum_a(np, 0.0);
  int trees_a = 0;

  // Parallel over trees. Each tree seeds its own RNG from (seed, b), and
  // per-thread partial sums are reduced in fixed thread order afterwards,
  // so results are identical to the serial loop for a given seed.
  int nth = 1;
#ifdef _OPENMP
  nth = omp_get_max_threads();
#endif
  std::vector<std::vector<double>> part_all(nth,
      std::vector<double>(np, 0.0));
  std::vector<std::vector<double>> part_a(nth,
      std::vector<double>(np, 0.0));

#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(nth)
#endif
  for (int b = 0; b < n_trees; ++b) {
    int tid = 0;
#ifdef _OPENMP
    tid = omp_get_thread_num();
#endif
    std::mt19937 rng(static_cast<unsigned>(seed) + 7919u *
                     static_cast<unsigned>(b));
    std::vector<int> pool(n);
    for (int i = 0; i < n; ++i) pool[i] = i;
    std::shuffle(pool.begin(), pool.end(), rng);
    std::vector<int> idx(pool.begin(), pool.begin() + ss);
    Tree tree;
    tree.nodes.push_back(Node());
    build_node(X, y, w, idx, 0, ss, 0, max_depth, min_node, mtry,
               rng, tree, 0);
    const bool half_a = (b % 2 == 0);
    for (int r = 0; r < np; ++r) {
      const double pr = predict_tree(tree, Xpred, r);
      part_all[tid][r] += pr;
      if (half_a) part_a[tid][r] += pr;
    }
  }
  trees_a = (n_trees + 1) / 2;
  for (int t = 0; t < nth; ++t)
    for (int r = 0; r < np; ++r) {
      sum_all[r] += part_all[t][r];
      sum_a[r] += part_a[t][r];
    }

  Rcpp::NumericMatrix out(np, 2);
  const int trees_b = n_trees - trees_a;
  for (int r = 0; r < np; ++r) {
    const double mean_all = sum_all[r] / n_trees;
    const double mean_a = trees_a > 0 ? sum_a[r] / trees_a : mean_all;
    const double mean_b = trees_b > 0
      ? (sum_all[r] - sum_a[r]) / trees_b : mean_all;
    out(r, 0) = mean_all;
    // half-sample spread as a crude tree-noise gauge
    out(r, 1) = std::fabs(mean_a - mean_b) / 2.0;
  }
  return out;
}
