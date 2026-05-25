// SPDX-License-Identifier: AGPL-3.0-or-later
//
// morie_matching.cpp -- hot inner loops for R/matching.R, ported from
// pure-R sweep()/order() chains to RcppArmadillo.

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <numeric>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
arma::mat morie_matching_mahalanobis_pairs_cpp(const arma::mat& X_t,
                                               const arma::mat& X_c,
                                               const arma::mat& S_inv) {
    const arma::uword n_t = X_t.n_rows;
    const arma::uword n_c = X_c.n_rows;
    const arma::uword d   = X_t.n_cols;
    if (X_c.n_cols != d) Rcpp::stop("X_t and X_c must have the same number of columns");
    if (S_inv.n_rows != d || S_inv.n_cols != d) Rcpp::stop("S_inv must be d x d");

    arma::mat S_sym = 0.5 * (S_inv + S_inv.t());
    arma::mat L;
    bool ok = arma::chol(L, S_sym, "lower");
    if (!ok) {
        arma::mat D(n_t, n_c, arma::fill::none);
        for (arma::uword i = 0; i < n_t; ++i) {
            arma::mat diffs = X_c.each_row() - X_t.row(i);
            arma::vec q = arma::sum((diffs * S_sym) % diffs, 1);
            q.transform([](double v) { return v < 0.0 ? 0.0 : std::sqrt(v); });
            D.row(i) = q.t();
        }
        return D;
    }
    arma::mat W_t = X_t * L;
    arma::mat W_c = X_c * L;
    arma::vec st = arma::sum(W_t % W_t, 1);
    arma::vec sc = arma::sum(W_c % W_c, 1);
    arma::mat cross = W_t * W_c.t();
    arma::mat D2 = -2.0 * cross;
    D2.each_col() += st;
    D2.each_row() += sc.t();
    D2.transform([](double v) { return v < 0.0 ? 0.0 : v; });
    return arma::sqrt(D2);
}

// [[Rcpp::export]]
arma::mat morie_matching_euclidean_pairs_cpp(const arma::mat& X_t,
                                             const arma::mat& X_c) {
    if (X_t.n_cols != X_c.n_cols) Rcpp::stop("X_t and X_c must have the same number of columns");
    arma::vec st = arma::sum(X_t % X_t, 1);
    arma::vec sc = arma::sum(X_c % X_c, 1);
    arma::mat cross = X_t * X_c.t();
    arma::mat D2 = -2.0 * cross;
    D2.each_col() += st;
    D2.each_row() += sc.t();
    D2.transform([](double v) { return v < 0.0 ? 0.0 : v; });
    return arma::sqrt(D2);
}

// [[Rcpp::export]]
List morie_matching_nn_select_cpp(const arma::mat& D,
                                  bool with_replacement,
                                  double caliper,
                                  int n_neighbors) {
    const arma::uword n_t = D.n_rows;
    const arma::uword n_c = D.n_cols;
    if (n_neighbors < 1) n_neighbors = 1;

    std::vector<int> out_t;
    std::vector<int> out_c;
    std::vector<double> out_d;
    out_t.reserve(static_cast<std::size_t>(n_t) * n_neighbors);
    out_c.reserve(static_cast<std::size_t>(n_t) * n_neighbors);
    out_d.reserve(static_cast<std::size_t>(n_t) * n_neighbors);

    std::unordered_set<arma::uword> used;
    const bool use_caliper = std::isfinite(caliper);

    std::vector<arma::uword> ord(n_c);
    for (arma::uword i = 0; i < n_t; ++i) {
        std::iota(ord.begin(), ord.end(), 0u);
        const arma::rowvec row_i = D.row(i);
        std::stable_sort(ord.begin(), ord.end(),
                         [&row_i](arma::uword a, arma::uword b) {
                             return row_i[a] < row_i[b];
                         });
        int matched = 0;
        for (arma::uword k = 0; k < n_c; ++k) {
            const arma::uword j = ord[k];
            const double dij = row_i[j];
            if (use_caliper && dij > caliper) break;
            if (!with_replacement && used.find(j) != used.end()) continue;
            out_t.push_back(static_cast<int>(i) + 1);
            out_c.push_back(static_cast<int>(j) + 1);
            out_d.push_back(dij);
            if (!with_replacement) used.insert(j);
            if (++matched >= n_neighbors) break;
        }
    }

    return List::create(
        _["treated_pos"] = IntegerVector(out_t.begin(), out_t.end()),
        _["control_pos"] = IntegerVector(out_c.begin(), out_c.end()),
        _["distance"]    = NumericVector(out_d.begin(), out_d.end())
    );
}

// [[Rcpp::export]]
IntegerVector morie_matching_cem_strata_cpp(const IntegerMatrix& X_binned) {
    const int n = X_binned.nrow();
    const int d = X_binned.ncol();
    std::vector<std::string> keys(n);
    for (int i = 0; i < n; ++i) {
        std::string& s = keys[i];
        s.reserve(static_cast<std::size_t>(d) * 6);
        for (int j = 0; j < d; ++j) {
            const int v = X_binned(i, j);
            if (j > 0) s.push_back('|');
            if (v == NA_INTEGER) s.append("NA");
            else                 s.append(std::to_string(v));
        }
    }
    IntegerVector out(n);
    std::unordered_map<std::string, int> dict;
    dict.reserve(static_cast<std::size_t>(n));
    int next_id = 1;
    for (int i = 0; i < n; ++i) {
        auto it = dict.find(keys[i]);
        if (it == dict.end()) {
            dict.emplace(keys[i], next_id);
            out[i] = next_id;
            ++next_id;
        } else {
            out[i] = it->second;
        }
    }
    return out;
}

// [[Rcpp::export]]
double morie_matching_abadie_imbens_kernel_cpp(NumericVector y,
                                               IntegerVector t,
                                               IntegerVector treated_pos,
                                               IntegerVector control_pos) {
    const int n = y.size();
    if (t.size() != n) Rcpp::stop("y and t must have equal length");
    const int m = treated_pos.size();
    if (control_pos.size() != m) Rcpp::stop("pair vectors must match length");

    std::vector<int> K(n, 0);
    std::vector<double> sigma2(n, 0.0);

    for (int k = 0; k < m; ++k) {
        const int tp = treated_pos[k] - 1;
        const int cp = control_pos[k] - 1;
        if (tp < 0 || tp >= n || cp < 0 || cp >= n) continue;
        K[cp] += 1;
        const double diff = y[tp] - y[cp];
        const double half_sq = 0.5 * diff * diff;
        sigma2[tp] = half_sq;
        sigma2[cp] = half_sq;
    }

    long long n_t_ll = 0;
    double s_t = 0.0;
    double s_c = 0.0;
    for (int i = 0; i < n; ++i) {
        if (t[i] == 1) {
            s_t += sigma2[i];
            ++n_t_ll;
        } else {
            const double kd = static_cast<double>(K[i]);
            s_c += kd * kd * sigma2[i];
        }
    }
    const double n_t = static_cast<double>(std::max<long long>(n_t_ll, 1));
    return (s_t + s_c) / (n_t * n_t);
}
