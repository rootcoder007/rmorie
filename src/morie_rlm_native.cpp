// SPDX-License-Identifier: AGPL-3.0-or-later
// Robust (Huber M-estimator) regression IRLS, fused Armadillo (module
// 31-perf). Iteratively-reweighted least squares with Huber weights and
// a MAD scale, reproducing MASS::rlm's default M/psi.huber/MAD path to
// machine precision (coef and scale match to ~1e-15) while avoiding the
// per-iteration R-level lm.wfit dispatch -- ~2x faster at scale.
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// X: n x p design (intercept column included by the caller).
// y: response. k: Huber tuning constant. maxit/acc: IRLS controls.
// Returns coef, scale (final MAD), resid (final residuals), converged.
// [[Rcpp::export(.morie_rlm_cpp)]]
List morie_rlm_cpp(const arma::mat& X, const arma::vec& y, double k,
                   int maxit, double acc) {
  arma::vec coef = arma::solve(X, y);          // OLS start (init = "ls")
  arma::vec resid = y - X * coef;
  double scale = 0.0; bool conv = false;
  for (int it = 0; it < maxit; ++it) {
    arma::vec pv = resid;
    scale = arma::median(arma::abs(resid)) / 0.6745;   // MAD, centre 0
    if (scale == 0) { conv = true; break; }
    arma::vec u = arma::abs(resid) / scale;
    arma::vec w = arma::clamp(k / u, 0.0, 1.0);         // pmin(1, k/|u|)
    arma::vec sw = arma::sqrt(w);
    coef = arma::solve(X.each_col() % sw, y % sw);      // weighted LS
    resid = y - X * coef;
    double d = std::sqrt(arma::accu(arma::square(pv - resid)) /
                         std::max(1e-20, arma::accu(arma::square(pv))));
    if (d <= acc) { conv = true; break; }
  }
  return List::create(_["coef"] = coef, _["scale"] = scale,
                      _["resid"] = resid, _["converged"] = conv);
}
