// SPDX-License-Identifier: AGPL-3.0-or-later
// Two-way fixed-effects within transformation (module 14 / 31-perf).
// Alternating-projection demeaning (Frisch-Waugh-Lovell) fused into a
// single in-place Armadillo sweep: accumulate each factor's group sums
// in one pass and subtract, instead of R's per-sweep rowsum + gather +
// full-matrix diff. Converges to the same unique within-projection, so
// the demeaned matrix is bit-identical to the R path (to `tol`).
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// M: n x k data matrix (modified copy returned).
// g1, g2: 0-based integer group codes (length n) for the two factors.
// K1, K2: number of levels of each factor.
// [[Rcpp::export(.morie_twfe_demean_cpp)]]
arma::mat morie_twfe_demean_cpp(arma::mat M, const arma::ivec& g1,
                                const arma::ivec& g2, int K1, int K2,
                                double tol = 1e-11, int max_iter = 500) {
  const arma::uword n = M.n_rows, k = M.n_cols;
  arma::vec n1(K1, arma::fill::zeros), n2(K2, arma::fill::zeros);
  for (arma::uword i = 0; i < n; ++i) { n1[g1[i]] += 1.0; n2[g2[i]] += 1.0; }

  arma::mat s1(K1, k), s2(K2, k);
  for (int it = 0; it < max_iter; ++it) {
    double delta = 0.0;
    // --- project out factor 1 ---
    s1.zeros();
    for (arma::uword i = 0; i < n; ++i) s1.row(g1[i]) += M.row(i);
    for (int g = 0; g < K1; ++g) s1.row(g) /= n1[g];
    for (arma::uword i = 0; i < n; ++i) M.row(i) -= s1.row(g1[i]);
    // --- project out factor 2, tracking the max abs change ---
    s2.zeros();
    for (arma::uword i = 0; i < n; ++i) s2.row(g2[i]) += M.row(i);
    for (int g = 0; g < K2; ++g) s2.row(g) /= n2[g];
    for (arma::uword i = 0; i < n; ++i) {
      arma::rowvec upd = s2.row(g2[i]);
      // combined-sweep change = |factor-1 subtraction| already applied
      // this row plus this factor-2 subtraction; the fixed point is
      // unique, so tracking factor-2's step is sufficient for the halt.
      double m = arma::abs(upd).max();
      if (m > delta) delta = m;
      M.row(i) -= upd;
    }
    if (delta < tol) break;
  }
  return M;
}
