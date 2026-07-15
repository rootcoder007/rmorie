// SPDX-License-Identifier: AGPL-3.0-or-later
// Greedy 1-D nearest-neighbour assignment kernel for the native
// propensity matcher (feat/native-specializations module 1).
// Sorted control scores + binary search + outward expansion over an
// availability mask. O((nt + nc) log nc) typical.
#include <Rcpp.h>
#include <algorithm>
#include <vector>
using namespace Rcpp;

// [[Rcpp::export(.morie_match_greedy_1d_cpp)]]
IntegerMatrix morie_match_greedy_1d_cpp(NumericVector treated_val,
                                        NumericVector control_val,
                                        int ratio,
                                        double caliper_width,
                                        bool replace) {
  const int nt = treated_val.size();
  const int nc = control_val.size();
  IntegerMatrix out(nt, ratio);
  std::fill(out.begin(), out.end(), NA_INTEGER);
  if (nt == 0 || nc == 0) return out;

  std::vector<int> ord_c(nc);
  for (int i = 0; i < nc; ++i) ord_c[i] = i;
  std::sort(ord_c.begin(), ord_c.end(),
            [&](int a, int b) { return control_val[a] < control_val[b]; });
  std::vector<double> sorted_c(nc);
  for (int i = 0; i < nc; ++i) sorted_c[i] = control_val[ord_c[i]];
  std::vector<char> available(nc, 1);

  std::vector<int> process(nt);
  for (int i = 0; i < nt; ++i) process[i] = i;
  std::sort(process.begin(), process.end(), [&](int a, int b) {
    return treated_val[a] > treated_val[b];
  });

  for (int pi = 0; pi < nt; ++pi) {
    const int ti = process[pi];
    const double x = treated_val[ti];
    // insertion point: first element > x
    int pos = static_cast<int>(
      std::upper_bound(sorted_c.begin(), sorted_c.end(), x) -
      sorted_c.begin());
    for (int k = 0; k < ratio; ++k) {
      int lo = pos - 1, hi = pos;
      int best = -1;
      double best_d = R_PosInf;
      while (true) {
        const double lo_d = (lo >= 0) ? std::abs(x - sorted_c[lo]) : R_PosInf;
        const double hi_d = (hi < nc) ? std::abs(x - sorted_c[hi]) : R_PosInf;
        if (!R_FINITE(lo_d) && !R_FINITE(hi_d)) break;
        if (lo_d <= hi_d) {
          if (lo_d >= best_d) break;
          if (replace || available[lo]) { best = lo; best_d = lo_d; }
          --lo;
        } else {
          if (hi_d >= best_d) break;
          if (replace || available[hi]) { best = hi; best_d = hi_d; }
          ++hi;
        }
        if (best >= 0) {
          const double nxt_lo = (lo >= 0) ? std::abs(x - sorted_c[lo]) : R_PosInf;
          const double nxt_hi = (hi < nc) ? std::abs(x - sorted_c[hi]) : R_PosInf;
          if (std::min(nxt_lo, nxt_hi) >= best_d) break;
        }
      }
      if (best < 0 || best_d > caliper_width) break;
      out(ti, k) = ord_c[best] + 1; // 1-based for R
      if (!replace) available[best] = 0;
    }
  }
  return out;
}
