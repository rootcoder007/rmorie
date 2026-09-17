// morie_fast.cpp -- Rcpp bindings for the R side of morie.
//
// Since v0.9.1 these functions are thin Rcpp adapters over the shared
// C++ numeric core in morie_core.h -- the SAME header the Python
// package binds via nanobind. The arithmetic is no longer duplicated
// per language: R and Python now call into one source of truth, which
// eliminates the Python<->R parity bug class by construction.
//
// Compile via R's standard mechanism (R CMD INSTALL). Without a C++
// toolchain at install time, R falls back to the pure-R kernels in
// R/_fast.R.
//
// morie_core.h is a vendored copy; the canonical file is
// libmorie/morie_core.hpp in the morie repository root. The R copy
// uses the .h extension so R CMD check accepts it as a src/ header.

#include <Rcpp.h>

#include <cstddef>

#include "morie_core.h"

// Shared compiled core for the rmorie ecosystem: the fast summary-stat
// kernels below resolve (via LinkingTo: rmoriebricklayer) to the single
// copy registered in rmoriebricklayer -- the same kernels rmoriedata
// links -- rather than recompiling a separate copy here. The domain
// kernels (Hawkes, below) stay on the vendored morie_core.h, which is
// also the Python-side source of truth.
#include <rmoriebricklayer.h>

using namespace Rcpp;

namespace {
inline std::size_t len(const NumericVector &v) {
    return static_cast<std::size_t>(v.size());
}
}  // namespace

// [[Rcpp::export]]
NumericVector morie_normal_pdf_cpp(NumericVector x, double mean, double sd) {
    if (sd <= 0.0) {
        Rcpp::stop("sd must be positive");
    }
    NumericVector out(x.size());
    for (R_xlen_t i = 0; i < x.size(); ++i) {
        out[i] = rmbl_normal_pdf(x[i], mean, sd);  // shared core
    }
    return out;
}

// [[Rcpp::export]]
double morie_mean_cpp(NumericVector x) {
    return rmbl_mean(x.begin(), len(x));  // shared core
}

// [[Rcpp::export]]
double morie_var_cpp(NumericVector x, int ddof = 1) {
    if (ddof == 1) {
        return rmbl_var(x.begin(), len(x));  // shared core (n-1 denominator)
    }
    return morie::core::variance(x.begin(), len(x), ddof);
}

// [[Rcpp::export]]
double morie_cor_pearson_cpp(NumericVector x, NumericVector y) {
    if (x.size() != y.size()) {
        return NA_REAL;
    }
    return rmbl_cor_pearson(x.begin(), y.begin(), len(x));  // shared core
}

// --- Hawkes negative log-likelihoods (constant baseline) -----------------
//
// Thin Rcpp adapters over the shared core -- the same functions the
// Python package binds via nanobind. The Weibull and gamma forms use
// the bit-identical sliding-window (sub-quadratic) variants. Each
// returns 1e12 for an infeasible parameter vector.

// [[Rcpp::export]]
double morie_hawkes_ll_exp_const_cpp(NumericVector t, double T_horizon, double a0,
                                     double eta, double beta) {
    return morie::core::hawkes_ll_exp_const(t.begin(), len(t), T_horizon, a0, eta,
                                            beta);
}

// [[Rcpp::export]]
double morie_hawkes_ll_weibull_const_cpp(NumericVector t, double T_horizon, double a0,
                                         double eta, double alpha,
                                         double lam) {
    return morie::core::hawkes_ll_weibull_const_trunc(t.begin(), len(t), T_horizon,
                                                      a0, eta, alpha, lam);
}

// [[Rcpp::export]]
double morie_hawkes_ll_lomax_const_cpp(NumericVector t, double T_horizon, double a0,
                                       double eta, double alpha, double c) {
    return morie::core::hawkes_ll_lomax_const(t.begin(), len(t), T_horizon, a0, eta,
                                              alpha, c);
}

// [[Rcpp::export]]
double morie_hawkes_ll_gamma_const_cpp(NumericVector t, double T_horizon, double a0,
                                       double eta, double alpha,
                                       double beta) {
    return morie::core::hawkes_ll_gamma_const_trunc(t.begin(), len(t), T_horizon, a0,
                                                    eta, alpha, beta);
}
