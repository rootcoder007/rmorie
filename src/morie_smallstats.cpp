// SPDX-License-Identifier: AGPL-3.0-or-later
//
// morie_smallstats.cpp -- C++ kernels for the smallstats native layer.
//
// Hot loops promoted from R/smallstats_native.R + R/tsnrd.R after
// benchmarking showed the pure-R versions lagging the compiled
// packages they replaced (Sobol vs randtoolbox ~100x, kNN vs FNN
// ~30x, coordinate descent vs glmnet ~3.5x, exact t-SNE vs Rtsne
// ~8x). Each kernel is the same algorithm as its R twin; the R
// wrappers keep the API and the cross-validation tests unchanged.

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// ---------------------------------------------------------------------------
// Gray-code Sobol with the Bratley-Fox (TOMS 659) initialization used
// by randtoolbox -- bit-identical to randtoolbox::sobol(scrambling=0).
// [[Rcpp::export(.morie_sobol_cpp)]]
NumericMatrix morie_sobol_cpp(const int n, const int d) {
  if (d < 1 || d > 10) stop("native Sobol supports 1 <= d <= 10.");
  const int nbits = 31;
  // dims 2..10: degree s, coefficient a, initial m integers.
  const int SS[9] = {1, 2, 3, 3, 4, 4, 5, 5, 5};
  const int AA[9] = {0, 1, 1, 2, 1, 4, 2, 13, 7};
  const int MM[9][5] = {
    {1, 0, 0, 0, 0}, {1, 1, 0, 0, 0}, {1, 3, 7, 0, 0},
    {1, 1, 5, 0, 0}, {1, 3, 1, 1, 0}, {1, 1, 3, 7, 0},
    {1, 3, 3, 9, 9}, {1, 3, 7, 13, 3}, {1, 1, 5, 11, 27}};
  NumericMatrix out(n, d);
  const double scale = std::ldexp(1.0, -nbits); // 2^-31
  std::vector<uint32_t> v(nbits);
  for (int j = 0; j < d; ++j) {
    if (j == 0) {
      for (int i = 0; i < nbits; ++i)
        v[i] = 1u << (nbits - i - 1);
    } else {
      const int s = SS[j - 1];
      const int a = AA[j - 1];
      for (int i = 0; i < s; ++i)
        v[i] = static_cast<uint32_t>(MM[j - 1][i]) << (nbits - i - 1);
      for (int i = s; i < nbits; ++i) {
        v[i] = v[i - s] ^ (v[i - s] >> s);
        for (int k = 1; k < s; ++k)
          if ((a >> (s - 1 - k)) & 1) v[i] ^= v[i - k];
      }
    }
    uint32_t x = 0;
    for (int i = 0; i < n; ++i) {
      int c = 0;
      int ii = i;
      while (ii & 1) { ii >>= 1; ++c; }
      x ^= v[c];
      out(i, j) = x * scale;
    }
  }
  return out;
}

// ---------------------------------------------------------------------------
// Brute-force k-nearest-neighbour indices (Euclidean), 1-based, ties
// by first occurrence -- the FNN::get.knn()$nn.index contract.
// [[Rcpp::export(.morie_knn_index_cpp)]]
IntegerMatrix morie_knn_index_cpp(const arma::mat& coords, const int k) {
  const int n = coords.n_rows;
  if (k >= n) stop("k must be smaller than the number of rows.");
  IntegerMatrix res(n, k);
  const arma::vec sq = arma::sum(arma::square(coords), 1);
  // Full squared-distance matrix via one BLAS gemm, then a k-element
  // partial sort per column (arma is column-major).
  arma::mat D = -2.0 * (coords * coords.t());
  D.each_col() += sq;
  D.each_row() += sq.t();
  D.diag().fill(std::numeric_limits<double>::infinity());
  std::vector<std::pair<double, int>> cand(n);
  for (int i = 0; i < n; ++i) {
    const double* col = D.colptr(i); // symmetric: column i == row i
    for (int j = 0; j < n; ++j) cand[j] = {col[j], j};
    std::partial_sort(cand.begin(), cand.begin() + k, cand.end());
    for (int j = 0; j < k; ++j) res(i, j) = cand[j].second + 1;
  }
  return res;
}

// ---------------------------------------------------------------------------
// Elastic-net coordinate descent on pre-standardized columns. Xs must
// be centred/scaled (population sd), yc centred. Returns the
// standardized-scale coefficients; the R wrapper rescales.
// [[Rcpp::export(.morie_coord_descent_cpp)]]
List morie_coord_descent_cpp(const arma::mat& Xs, const arma::vec& yc,
                             const double alpha, const double lambda,
                             const int max_iter, const double tol,
                             const arma::vec& warm) {
  const int n = Xs.n_rows, p = Xs.n_cols;
  arma::vec beta = warm;
  arma::vec xtx = arma::sum(arma::square(Xs), 0).t() / n;
  arma::vec r = yc - Xs * beta;
  const double soft = lambda * alpha;
  const double ridge_t = lambda * (1.0 - alpha);
  int n_iter_done = max_iter;
  for (int it = 0; it < max_iter; ++it) {
    double max_change = 0.0;
    for (int j = 0; j < p; ++j) {
      const arma::vec xj = Xs.col(j);
      const double bj = beta(j);
      const double z = (arma::dot(xj, r) + bj * n * xtx(j)) / n;
      double nb = 0.0;
      if (z > soft) nb = (z - soft) / (xtx(j) + ridge_t);
      else if (z < -soft) nb = (z + soft) / (xtx(j) + ridge_t);
      const double ch = nb - bj;
      if (std::abs(ch) > max_change) max_change = std::abs(ch);
      if (ch != 0.0) { beta(j) = nb; r -= xj * ch; }
    }
    if (max_change < tol) { n_iter_done = it + 1; break; }
  }
  return List::create(_["beta_std"] = beta, _["n_iter"] = n_iter_done);
}

// ---------------------------------------------------------------------------
// Exact t-SNE gradient descent given the symmetrized P matrix. Runs
// the full early-exaggeration + momentum/gains schedule and returns
// the embedding plus final KL divergence.
// [[Rcpp::export(.morie_tsne_descent_cpp)]]
List morie_tsne_descent_cpp(const arma::mat& P, arma::mat Y,
                            const int n_iter, const double eta) {
  const int n = Y.n_rows, dims = Y.n_cols;
  arma::mat dY(n, dims, arma::fill::zeros);
  arma::mat gains(n, dims, arma::fill::ones);
  const int exag_iters = std::min(250, n_iter);
  arma::mat P_run = P * 12.0;
  double momentum = 0.5;
  double kl = NA_REAL;
  arma::mat num(n, n), Q(n, n), PQ(n, n), grad(n, dims);
  for (int iter = 1; iter <= n_iter; ++iter) {
    if (iter == exag_iters + 1) P_run = P;
    if (iter == 251) momentum = 0.8;
    const arma::vec sqy = arma::sum(arma::square(Y), 1);
    num = -2.0 * (Y * Y.t());
    num.each_col() += sqy;
    num.each_row() += sqy.t();
    num = 1.0 / (1.0 + num);
    num.diag().zeros();
    const double snum = arma::accu(num);
    Q = num / snum;
    Q.transform([](double q) { return q < 1e-12 ? 1e-12 : q; });
    PQ = (P_run - Q) % num;
    grad = 4.0 * (arma::diagmat(arma::sum(PQ, 1)) - PQ) * Y;
    for (int i = 0; i < n; ++i)
      for (int j = 0; j < dims; ++j) {
        const bool same = (grad(i, j) > 0) == (dY(i, j) > 0);
        gains(i, j) = same ? gains(i, j) * 0.8 : gains(i, j) + 0.2;
        if (gains(i, j) < 0.01) gains(i, j) = 0.01;
      }
    dY = momentum * dY - eta * (gains % grad);
    Y += dY;
    Y.each_row() -= arma::mean(Y, 0);
    if (iter == n_iter) kl = arma::accu(P % arma::log(P / Q));
  }
  return List::create(_["Y"] = Y, _["kl"] = kl);
}
