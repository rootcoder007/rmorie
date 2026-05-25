// SPDX-License-Identifier: AGPL-3.0-or-later
//
// morie_hawkes.cpp -- Rcpp hot-loop primitives for the non-stationary,
// non-Markovian Hawkes path in R/tps_hawkes_advanced.R.
//
// Conventions match src/morie/tps_hawkes_advanced.py exactly:
//   - Lomax density: (alpha-1) * c^(alpha-1) * (u+c)^(-alpha)
//   - Lomax CDF:     1 - (c / (u+c))^(alpha-1)
// These are the morie convention; NOT the scipy convention.

#include <Rcpp.h>
#include <cmath>
#include <cstddef>
#include <string>

using namespace Rcpp;

namespace {

constexpr double TWO_PI         = 6.283185307179586476925286766559;
constexpr double DAYS_PER_YEAR  = 365.25;
constexpr double TINY           = 1e-300;

enum class Kind { Exponential = 0, Gamma = 1, Weibull = 2, Lomax = 3 };

inline Kind kind_from_string(const std::string &s) {
    if (s == "exponential") return Kind::Exponential;
    if (s == "gamma")       return Kind::Gamma;
    if (s == "weibull")     return Kind::Weibull;
    if (s == "lomax")       return Kind::Lomax;
    Rcpp::stop("unknown kernel kind: %s", s);
}

inline double dens_scalar(double u, Kind kind, const double *psi) {
    switch (kind) {
    case Kind::Exponential: {
        const double beta = psi[0];
        return beta * std::exp(-beta * u);
    }
    case Kind::Gamma: {
        const double alpha = psi[0];
        const double beta  = psi[1];
        const double uu    = u > TINY ? u : TINY;
        const double log_d = alpha * std::log(beta)
                           + (alpha - 1.0) * std::log(uu)
                           - beta * u
                           - std::lgamma(alpha);
        return std::exp(log_d);
    }
    case Kind::Weibull: {
        const double alpha = psi[0];
        const double lam   = psi[1];
        const double x     = u / lam;
        const double xx    = x > TINY ? x : TINY;
        return (alpha / lam) * std::pow(xx, alpha - 1.0)
                             * std::exp(-std::pow(x, alpha));
    }
    case Kind::Lomax: {
        // v0.9.5.6+: scipy.stats.lomax convention.
        // alpha * c^alpha * (u+c)^-(alpha+1).
        const double alpha = psi[0];
        const double c     = psi[1];
        const double log_d = std::log(alpha)
                           + alpha * std::log(c)
                           - (alpha + 1.0) * std::log(u + c);
        return std::exp(log_d);
    }
    }
    return 0.0;
}

inline double cdf_scalar(double u, Kind kind, const double *psi) {
    switch (kind) {
    case Kind::Exponential:
        return 1.0 - std::exp(-psi[0] * u);
    case Kind::Gamma: {
        const double alpha = psi[0];
        const double beta  = psi[1];
        return R::pgamma(u, alpha, 1.0 / beta, /*lower=*/1, /*log=*/0);
    }
    case Kind::Weibull: {
        const double alpha = psi[0];
        const double lam   = psi[1];
        return 1.0 - std::exp(-std::pow(u / lam, alpha));
    }
    case Kind::Lomax: {
        // v0.9.5.6+: scipy convention 1 - (c/(u+c))^alpha (was: alpha-1).
        const double alpha = psi[0];
        const double c     = psi[1];
        return 1.0 - std::pow(c / (u + c), alpha);
    }
    }
    return 0.0;
}

}  // namespace

// [[Rcpp::export]]
NumericVector morie_hawkes_kernel_density_cpp(NumericVector u,
                                              std::string kind,
                                              NumericVector psi) {
    const Kind k = kind_from_string(kind);
    if (psi.size() < ((k == Kind::Exponential) ? 1 : 2)) {
        Rcpp::stop("psi too short for kernel kind '%s'", kind);
    }
    const std::size_t n = static_cast<std::size_t>(u.size());
    NumericVector out(n);
    const double *psi_ptr = REAL(psi);
    const double *u_ptr   = REAL(u);
    double *o_ptr         = REAL(out);
    for (std::size_t i = 0; i < n; ++i) {
        o_ptr[i] = dens_scalar(u_ptr[i], k, psi_ptr);
    }
    return out;
}

// [[Rcpp::export]]
NumericVector morie_hawkes_kernel_cdf_cpp(NumericVector u,
                                          std::string kind,
                                          NumericVector psi) {
    const Kind k = kind_from_string(kind);
    if (psi.size() < ((k == Kind::Exponential) ? 1 : 2)) {
        Rcpp::stop("psi too short for kernel kind '%s'", kind);
    }
    const std::size_t n = static_cast<std::size_t>(u.size());
    NumericVector out(n);
    const double *psi_ptr = REAL(psi);
    const double *u_ptr   = REAL(u);
    double *o_ptr         = REAL(out);
    for (std::size_t i = 0; i < n; ++i) {
        o_ptr[i] = cdf_scalar(u_ptr[i], k, psi_ptr);
    }
    return out;
}

// [[Rcpp::export]]
NumericVector morie_hawkes_pair_excitation_sum_cpp(NumericVector t,
                                                   double eta,
                                                   std::string kind,
                                                   NumericVector psi) {
    const Kind k = kind_from_string(kind);
    if (psi.size() < ((k == Kind::Exponential) ? 1 : 2)) {
        Rcpp::stop("psi too short for kernel kind '%s'", kind);
    }
    const std::size_t n   = static_cast<std::size_t>(t.size());
    NumericVector out(n);
    const double *t_ptr   = REAL(t);
    const double *psi_ptr = REAL(psi);
    double *o_ptr         = REAL(out);

    if (n == 0) return out;
    o_ptr[0] = 0.0;
    for (std::size_t i = 1; i < n; ++i) {
        const double ti = t_ptr[i];
        double s = 0.0;
        for (std::size_t j = 0; j < i; ++j) {
            s += dens_scalar(ti - t_ptr[j], k, psi_ptr);
        }
        o_ptr[i] = eta * s;
    }
    return out;
}

// [[Rcpp::export]]
double morie_hawkes_baseline_integral_cpp(double T_horizon,
                                          NumericVector alpha,
                                          int n_grid = 0) {
    if (alpha.size() < 4) {
        Rcpp::stop("sinusoidal baseline needs 4 alpha params");
    }
    if (T_horizon <= 0.0) return 0.0;
    if (n_grid < 2) {
        const int t_int = static_cast<int>(T_horizon) + 1;
        n_grid = (t_int > 64) ? t_int : 64;
    }
    const double a0 = alpha[0];
    const double a1 = alpha[1];
    const double a2 = alpha[2];
    const double a3 = alpha[3];
    const double T_safe = T_horizon > 1.0 ? T_horizon : 1.0;
    const double h  = T_horizon / static_cast<double>(n_grid - 1);
    const double w  = TWO_PI / DAYS_PER_YEAR;

    auto nu = [&](double tt) -> double {
        return std::exp(a0 + a1 * (tt / T_safe)
                            + a2 * std::sin(w * tt)
                            + a3 * std::cos(w * tt));
    };
    double acc = 0.5 * (nu(0.0) + nu(T_horizon));
    for (int g = 1; g < n_grid - 1; ++g) {
        acc += nu(static_cast<double>(g) * h);
    }
    return acc * h;
}
