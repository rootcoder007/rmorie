// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Native optimal pair matching kernels (feat/native-specializations,
// module 5).
//
// Kernel 1 (.morie_match_optimal_1d_cpp): exact optimal 1:1 matching on
// a scalar score. By the non-crossing property of 1-D optimal matching
// (sorted treated match to a monotone subsequence of sorted controls),
// the problem is a dynamic program over the two sorted vectors:
//   D[i][j] = min total |t_i - c_j| matching first i treated within
//             first j controls
// O(nt*nc) time; the backtrack matrix is uint8 (1 byte per cell).
//
// Kernel 2 (.morie_match_optimal_assign_cpp): exact optimal 1:1
// assignment for multivariate (whitened) distances via the shortest
// augmenting path algorithm (Jonker-Volgenant style, dual-feasible),
// O(nt^2 * nc) worst case. Used for distance = "mahalanobis".

#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

// [[Rcpp::export(".morie_match_optimal_1d_cpp")]]
Rcpp::IntegerVector morie_match_optimal_1d_cpp(Rcpp::NumericVector treated_val,
                                               Rcpp::NumericVector control_val) {
  const int nt = treated_val.size();
  const int nc = control_val.size();
  if (nt == 0 || nc < nt)
    Rcpp::stop("optimal 1:1 matching needs n_control >= n_treated >= 1");
  const double big = std::numeric_limits<double>::infinity();

  std::vector<int> ord_t(nt), ord_c(nc);
  for (int i = 0; i < nt; ++i) ord_t[i] = i;
  for (int j = 0; j < nc; ++j) ord_c[j] = j;
  std::sort(ord_t.begin(), ord_t.end(), [&](int a, int b) {
    return treated_val[a] < treated_val[b];
  });
  std::sort(ord_c.begin(), ord_c.end(), [&](int a, int b) {
    return control_val[a] < control_val[b];
  });

  const double cells = static_cast<double>(nt + 1) *
                       static_cast<double>(nc + 1);
  if (cells > 4e9)
    Rcpp::stop("problem too large for exact optimal matching "
               "(%d treated x %d controls); use nearest-neighbour",
               nt, nc);

  // rolling DP rows + full uint8 backtrack (0 = skip control, 1 = match)
  std::vector<double> prev(nc + 1, 0.0), cur(nc + 1, 0.0);
  std::vector<uint8_t> bt(static_cast<size_t>(nt + 1) * (nc + 1), 0);

  for (int i = 1; i <= nt; ++i) {
    const double tv = treated_val[ord_t[i - 1]];
    for (int j = 0; j < i; ++j) cur[j] = big;
    for (int j = i; j <= nc; ++j) {
      const double take = prev[j - 1] +
        std::fabs(tv - control_val[ord_c[j - 1]]);
      const double skip = cur[j - 1];
      if (take <= skip) {
        cur[j] = take;
        bt[static_cast<size_t>(i) * (nc + 1) + j] = 1;
      } else {
        cur[j] = skip;
      }
    }
    std::swap(prev, cur);
  }

  Rcpp::IntegerVector match(nt, NA_INTEGER);
  int i = nt, j = nc;
  while (i > 0) {
    if (bt[static_cast<size_t>(i) * (nc + 1) + j]) {
      match[ord_t[i - 1]] = ord_c[j - 1] + 1;  // 1-based
      --i; --j;
    } else {
      --j;
    }
  }
  return match;
}

// [[Rcpp::export(".morie_match_optimal_assign_cpp")]]
Rcpp::IntegerVector morie_match_optimal_assign_cpp(Rcpp::NumericMatrix treated,
                                                   Rcpp::NumericMatrix control) {
  const int nt = treated.nrow();
  const int nc = control.nrow();
  const int k = treated.ncol();
  if (nt == 0 || nc < nt)
    Rcpp::stop("optimal 1:1 matching needs n_control >= n_treated >= 1");
  if (static_cast<double>(nt) * nc > 5e7)
    Rcpp::stop("distance matrix too large for multivariate optimal "
               "matching (%d x %d); use distance = \"propensity\"",
               nt, nc);
  const double inf = std::numeric_limits<double>::infinity();

  // cost matrix (float to halve memory)
  std::vector<float> cost(static_cast<size_t>(nt) * nc);
  for (int i = 0; i < nt; ++i)
    for (int j = 0; j < nc; ++j) {
      double d2 = 0.0;
      for (int c = 0; c < k; ++c) {
        const double d = treated(i, c) - control(j, c);
        d2 += d * d;
      }
      cost[static_cast<size_t>(i) * nc + j] =
        static_cast<float>(std::sqrt(d2));
    }

  // shortest augmenting path with duals (rows = treated, cols = controls)
  std::vector<double> u(nt + 1, 0.0), v(nc + 1, 0.0);
  std::vector<int> way(nc + 1, 0), p(nc + 1, 0);  // p[j] = row matched to col j
  for (int i = 1; i <= nt; ++i) {
    p[0] = i;
    int j0 = 0;
    std::vector<double> minv(nc + 1, inf);
    std::vector<int> used(nc + 1, 0);
    do {
      used[j0] = 1;
      const int i0 = p[j0];
      double delta = inf;
      int j1 = -1;
      for (int j = 1; j <= nc; ++j) {
        if (used[j]) continue;
        const double cur = cost[static_cast<size_t>(i0 - 1) * nc + (j - 1)]
          - u[i0] - v[j];
        if (cur < minv[j]) { minv[j] = cur; way[j] = j0; }
        if (minv[j] < delta) { delta = minv[j]; j1 = j; }
      }
      for (int j = 0; j <= nc; ++j) {
        if (used[j]) { u[p[j]] += delta; v[j] -= delta; }
        else minv[j] -= delta;
      }
      j0 = j1;
    } while (p[j0] != 0);
    do {
      const int j1 = way[j0];
      p[j0] = p[j1];
      j0 = j1;
    } while (j0);
  }

  Rcpp::IntegerVector match(nt, NA_INTEGER);
  for (int j = 1; j <= nc; ++j)
    if (p[j] > 0) match[p[j] - 1] = j;  // 1-based control index
  return match;
}
