// SPDX-License-Identifier: AGPL-3.0-or-later
// Frisch-Newton interior point solver for one regression quantile: the
// primal-dual predictor-corrector of Portnoy and Koenker (1997) in the
// formulation of quantreg's lpfnb, step for step as .rqn_fnb() in
// R/rq_native.R did it in R. The loop runs once per fit and three times for
// nid standard errors; in R its O(np) vector work dominated a 100,000-row fit.
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

namespace {

// largest step in (0, 1e20] keeping v + step * dv positive (v > 0)
double fnb_ratio(const arma::vec& v, const arma::vec& dv) {
  double m = R_NegInf;
  bool any = false;
  for (arma::uword i = 0; i < v.n_elem; ++i) {
    const double r = -dv[i] / v[i];
    if (std::isnan(r)) continue;
    if (!any || r > m) m = r;
    any = true;
  }
  return (any && m > 0) ? 1.0 / m : 1e20;
}

// R' R u = rhs for upper-triangular R
arma::vec chol_solve(const arma::mat& R, const arma::vec& rhs) {
  arma::vec t = arma::solve(arma::trimatl(R.t()), rhs);
  return arma::solve(arma::trimatu(R), t);
}

}  // namespace

// [[Rcpp::export(name = ".rqn_fnb_impl")]]
List rqn_fnb_impl(const arma::mat& X, const arma::vec& y, double tau,
                  double beta, double eps, int maxit) {
  const arma::uword n = X.n_rows;
  const double twon = 2.0 * static_cast<double>(n);
  arma::vec cc = -y;
  arma::vec b = (1.0 - tau) * arma::sum(X, 0).t();
  arma::vec x(n, arma::fill::value(1.0 - tau));
  arma::mat R;
  if (!arma::chol(R, X.t() * X)) stop("singular design matrix.");
  arma::vec yv = chol_solve(R, X.t() * cc);
  arma::vec s = cc - X * yv;
  arma::vec z = arma::clamp(s, 0.0, arma::datum::inf);
  arma::vec w = arma::clamp(-s, 0.0, arma::datum::inf);
  for (arma::uword i = 0; i < n; ++i) {
    if (std::fabs(s[i]) < eps) {
      z[i] += eps;
      w[i] += eps;
    }
  }
  s = 1.0 - x;
  double gap = arma::dot(z, x) + arma::dot(w, s);
  int it = 0;
  int ncor = 0;
  while (gap > eps && it < maxit) {
    ++it;
    arma::vec d = 1.0 / (z / x + w / s);
    arma::vec ds = z - w;
    arma::vec dz = d % ds;
    arma::vec rhs = b + X.t() * (dz - x);
    arma::mat Xd = X.each_col() % arma::sqrt(d);
    if (!arma::chol(R, Xd.t() * Xd)) stop("singular design matrix in the Newton step.");
    arma::vec dy = chol_solve(R, rhs);
    ds = X * dy - ds;
    arma::vec dx = d % ds;
    ds = -dx;
    dz = -z % (dx / x + 1.0);
    arma::vec dw = -w % (ds / s + 1.0);
    double deltap = std::min(beta * std::min(fnb_ratio(x, dx), fnb_ratio(s, ds)), 1.0);
    double deltad = std::min(beta * std::min(fnb_ratio(z, dz), fnb_ratio(w, dw)), 1.0);
    if (std::min(deltap, deltad) < 1.0) {
      ++ncor;
      double mu = arma::dot(x, z) + arma::dot(s, w);
      const double g = mu + deltap * arma::dot(dx, z) + deltad * arma::dot(dz, x) +
        deltap * deltad * arma::dot(dz, dx) + deltap * arma::dot(ds, w) +
        deltad * arma::dot(dw, s) + deltap * deltad * arma::dot(ds, dw);
      mu = mu * std::pow(g / mu, 3.0) / twon;
      arma::vec dr = d % (mu * (1.0 / s - 1.0 / x) + dx % dz / x - ds % dw / s);
      dy = chol_solve(R, rhs + X.t() * dr);
      arma::vec uu = X * dy;
      arma::vec dxdz = dx % dz;
      arma::vec dsdw = ds % dw;
      dx = d % (uu - z + w) - dr;
      ds = -dx;
      dz = -z + (mu - z % dx - dxdz) / x;
      dw = -w + (mu - w % ds - dsdw) / s;
      deltap = std::min(beta * std::min(fnb_ratio(x, dx), fnb_ratio(s, ds)), 1.0);
      deltad = std::min(beta * std::min(fnb_ratio(z, dz), fnb_ratio(w, dw)), 1.0);
    }
    x += deltap * dx;
    s += deltap * ds;
    yv += deltad * dy;
    z += deltad * dz;
    w += deltad * dw;
    gap = arma::dot(z, x) + arma::dot(w, s);
  }
  return List::create(_["yv"] = NumericVector(yv.begin(), yv.end()),
                      _["gap"] = gap, _["it"] = it, _["ncor"] = ncor);
}
