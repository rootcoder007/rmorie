// SPDX-License-Identifier: AGPL-3.0-or-later
//
// morie_dsp.cpp -- Rcpp ports of the hot inner loops in R/dsp_filters.R.
// Math notes (do NOT "fix"):
//   * LMS uses Widrow-Hoff:   w += 2 * mu * e[i] * seg
//   * NLMS (no factor 2):     w += (mu / (seg.seg + eps)) * e[i] * seg
//   * RLS uses lam, delta*I, P updated as (P - k * (seg' P)) / lam
//   * cross_correlation: 2*max_lag+1 outputs, lag 0 at the centre.

#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>

using namespace Rcpp;

namespace {

inline void fill_seg(const double *x, int i, int order, double *seg) {
    for (int j = 0; j < order; ++j) {
        seg[j] = x[(i - 1) - 1 - j];
    }
}

}  // namespace

// [[Rcpp::export]]
List morie_dsp_lms_cpp(NumericVector x, NumericVector d,
                       int order, double mu) {
    if (x.size() != d.size()) Rcpp::stop("x and d must be the same length");
    if (order < 1) Rcpp::stop("order must be >= 1");
    const int n = x.size();
    NumericVector y(n), e(n);
    std::vector<double> w(order, 0.0);
    std::vector<double> seg(order);
    const double *xp = REAL(x);
    const double *dp = REAL(d);
    double *yp = REAL(y);
    double *ep = REAL(e);
    for (int i = order + 1; i <= n; ++i) {
        fill_seg(xp, i, order, seg.data());
        double acc = 0.0;
        for (int k = 0; k < order; ++k) acc += w[k] * seg[k];
        const int idx = i - 1;
        yp[idx] = acc;
        const double err = dp[idx] - acc;
        ep[idx] = err;
        const double s = 2.0 * mu * err;
        for (int k = 0; k < order; ++k) w[k] += s * seg[k];
    }
    return List::create(_["y"] = y, _["e"] = e);
}

// [[Rcpp::export]]
List morie_dsp_nlms_cpp(NumericVector x, NumericVector d,
                        int order, double mu, double eps) {
    if (x.size() != d.size()) Rcpp::stop("x and d must be the same length");
    if (order < 1) Rcpp::stop("order must be >= 1");
    const int n = x.size();
    NumericVector y(n), e(n);
    std::vector<double> w(order, 0.0);
    std::vector<double> seg(order);
    const double *xp = REAL(x);
    const double *dp = REAL(d);
    double *yp = REAL(y);
    double *ep = REAL(e);
    for (int i = order + 1; i <= n; ++i) {
        fill_seg(xp, i, order, seg.data());
        double acc = 0.0, nrm = 0.0;
        for (int k = 0; k < order; ++k) {
            acc += w[k] * seg[k];
            nrm += seg[k] * seg[k];
        }
        nrm += eps;
        const int idx = i - 1;
        yp[idx] = acc;
        const double err = dp[idx] - acc;
        ep[idx] = err;
        const double s = (mu / nrm) * err;
        for (int k = 0; k < order; ++k) w[k] += s * seg[k];
    }
    return List::create(_["y"] = y, _["e"] = e);
}

// [[Rcpp::export]]
List morie_dsp_rls_cpp(NumericVector x, NumericVector d,
                       int order, double lam, double delta) {
    if (x.size() != d.size()) Rcpp::stop("x and d must be the same length");
    if (order < 1) Rcpp::stop("order must be >= 1");
    const int n = x.size();
    NumericVector y(n), e(n);
    std::vector<double> w(order, 0.0);
    std::vector<double> P(static_cast<size_t>(order) * order, 0.0);
    for (int k = 0; k < order; ++k) P[k * order + k] = delta;
    std::vector<double> seg(order), Pseg(order);
    const double *xp = REAL(x);
    const double *dp = REAL(d);
    double *yp = REAL(y);
    double *ep = REAL(e);
    for (int i = order + 1; i <= n; ++i) {
        fill_seg(xp, i, order, seg.data());
        double acc = 0.0;
        for (int k = 0; k < order; ++k) acc += w[k] * seg[k];
        const int idx = i - 1;
        yp[idx] = acc;
        const double err = dp[idx] - acc;
        ep[idx] = err;

        for (int r = 0; r < order; ++r) {
            double s = 0.0;
            const double *Pr = &P[r * order];
            for (int c = 0; c < order; ++c) s += Pr[c] * seg[c];
            Pseg[r] = s;
        }
        double denom = lam;
        for (int k = 0; k < order; ++k) denom += seg[k] * Pseg[k];
        std::vector<double> kg(order);
        for (int r = 0; r < order; ++r) kg[r] = Pseg[r] / denom;
        for (int r = 0; r < order; ++r) w[r] += kg[r] * err;
        const double inv_lam = 1.0 / lam;
        for (int r = 0; r < order; ++r) {
            double *Pr = &P[r * order];
            const double kgr = kg[r];
            for (int c = 0; c < order; ++c) {
                Pr[c] = (Pr[c] - kgr * Pseg[c]) * inv_lam;
            }
        }
    }
    return List::create(_["y"] = y, _["e"] = e);
}

// [[Rcpp::export]]
NumericVector morie_dsp_cross_correlation_cpp(NumericVector x,
                                              NumericVector y,
                                              int max_lag) {
    if (x.size() != y.size()) Rcpp::stop("x and y must be the same length");
    const int n = x.size();
    if (max_lag < 0) Rcpp::stop("max_lag must be >= 0");
    if (max_lag > n - 1) max_lag = n - 1;

    double mx = 0.0, my = 0.0;
    const double *xp = REAL(x);
    const double *yp = REAL(y);
    for (int i = 0; i < n; ++i) { mx += xp[i]; my += yp[i]; }
    mx /= n; my /= n;
    std::vector<double> xc(n), yc(n);
    double sx2 = 0.0, sy2 = 0.0;
    for (int i = 0; i < n; ++i) {
        xc[i] = xp[i] - mx;
        yc[i] = yp[i] - my;
        sx2 += xc[i] * xc[i];
        sy2 += yc[i] * yc[i];
    }
    const double nrm = std::sqrt(sx2 * sy2);

    const int out_len = 2 * max_lag + 1;
    NumericVector out(out_len);
    for (int t = 0; t < out_len; ++t) {
        const int tau = t - max_lag;
        double acc = 0.0;
        if (tau >= 0) {
            const int upper = n - tau;
            for (int i = 0; i < upper; ++i) acc += xc[i] * yc[i + tau];
        } else {
            const int start = -tau;
            for (int i = start; i < n; ++i) acc += xc[i] * yc[i + tau];
        }
        out[t] = acc;
    }
    if (nrm > 0.0) {
        for (int t = 0; t < out_len; ++t) out[t] /= nrm;
    }
    return out;
}

// [[Rcpp::export]]
NumericVector morie_dsp_median_filter_cpp(NumericVector x, int kernel_size) {
    if (kernel_size < 1) Rcpp::stop("kernel_size must be >= 1");
    int k = kernel_size;
    if ((k & 1) == 0) k += 1;
    const int half = k / 2;
    const int n = x.size();
    NumericVector out(n);
    if (n == 0) return out;

    std::vector<double> padded(static_cast<size_t>(n) + 2 * half, 0.0);
    const double *xp = REAL(x);
    for (int i = 0; i < n; ++i) padded[i + half] = xp[i];

    std::vector<double> window(k);
    double *op = REAL(out);
    const int mid = k / 2;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < k; ++j) window[j] = padded[i + j];
        std::nth_element(window.begin(), window.begin() + mid, window.end());
        op[i] = window[mid];
    }
    return out;
}
