// SPDX-License-Identifier: AGPL-3.0-or-later
// Greedy k-dimensional nearest-neighbour assignment on WHITENED
// coordinates (Mahalanobis matching, module 2). Streaming scan per
// treated unit with an availability mask: O(nt * nc * k) compute,
// O(nt + nc) memory — no distance matrix, unlike the O(n^2)-memory
// reference paths.
#include <Rcpp.h>
#include <vector>
#include <algorithm>
using namespace Rcpp;

// treated / control: whitened coordinate matrices (rows = units).
// Returns nt x ratio matrix of 1-based control row indices, NA where
// no admissible match. Treated processed in the given order of rows.
// [[Rcpp::export(.morie_match_greedy_kd_cpp)]]
IntegerMatrix morie_match_greedy_kd_cpp(NumericMatrix treated,
                                        NumericMatrix control,
                                        int ratio,
                                        double caliper_dist,
                                        bool replace) {
  const int nt = treated.nrow();
  const int nc = control.nrow();
  const int k = treated.ncol();
  IntegerMatrix out(nt, ratio);
  std::fill(out.begin(), out.end(), NA_INTEGER);
  if (nt == 0 || nc == 0) return out;
  const double cal2 = R_FINITE(caliper_dist)
      ? caliper_dist * caliper_dist : R_PosInf;
  std::vector<char> available(nc, 1);
  for (int ti = 0; ti < nt; ++ti) {
    for (int m = 0; m < ratio; ++m) {
      int best = -1;
      double best_d2 = R_PosInf;
      for (int ci = 0; ci < nc; ++ci) {
        if (!replace && !available[ci]) continue;
        double d2 = 0.0;
        for (int j = 0; j < k; ++j) {
          const double diff = treated(ti, j) - control(ci, j);
          d2 += diff * diff;
          if (d2 >= best_d2) break; // early exit
        }
        if (d2 < best_d2) { best_d2 = d2; best = ci; }
      }
      if (best < 0 || best_d2 > cal2) break;
      out(ti, m) = best + 1;
      if (!replace) available[best] = 0;
    }
  }
  return out;
}
