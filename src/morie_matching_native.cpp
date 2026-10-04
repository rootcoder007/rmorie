// SPDX-License-Identifier: AGPL-3.0-or-later
// Greedy 1-D nearest-neighbour assignment kernel for the native
// propensity matcher (feat/native-specializations module 1).
// Sorted control scores + binary search + outward expansion over an
// availability mask. O((nt + nc) log nc) typical.
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <numeric>
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
  // std::sort on a vector holding NaN is undefined behaviour (the
  // comparator is no longer a strict weak ordering) and corrupts the
  // heap on glibc; a caliper of NaN and a ratio below one are refused for
  // the same reason rather than producing a shape that cannot be filled.
  for (int i = 0; i < nt; ++i) {
    if (ISNAN(treated_val[i]))
      stop("treated_val contains NA or NaN at position %d", i + 1);
  }
  for (int i = 0; i < nc; ++i) {
    if (ISNAN(control_val[i]))
      stop("control_val contains NA or NaN at position %d", i + 1);
  }
  if (ratio == NA_INTEGER || ratio < 1) stop("ratio must be a positive integer");
  if (ISNAN(caliper_width)) stop("caliper_width must not be NA or NaN");
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

// ---------------------------------------------------------------------------
// MatchIt's nearest-neighbour matcher on a scalar distance, re-implemented
// (the algorithm of nn_matchC_vec / find_control_vec in MatchIt 4.7, Greifer
// et al., GPL-2+) for the propensity case: one control group, no exact or
// anti-exact constraints, a caliper on the distance only. Without
// replacement it matches in rounds (every treated unit gets its first control
// before any gets a second, treated taken in decreasing distance); with
// replacement each treated unit takes its `ratio` nearest controls in data
// order. Ties, the search order and the caliper behave as in MatchIt, so the
// match matrix is the same one.

static void nn_update_bounds(int& first_c, int& last_c, const std::vector<int>& ind_d_ord,
                             const std::vector<char>& eligible, const std::vector<int>& treat) {
  if (!eligible[ind_d_ord[first_c]]) {
    for (int c = first_c + 1; c <= last_c; ++c) {
      if (eligible[ind_d_ord[c]] && treat[ind_d_ord[c]] == 0) { first_c = c; break; }
    }
  }
  if (!eligible[ind_d_ord[last_c]]) {
    for (int c = last_c - 1; c >= first_c; --c) {
      if (eligible[ind_d_ord[c]] && treat[ind_d_ord[c]] == 0) { last_c = c; break; }
    }
  }
}

static std::vector<int> nn_find_controls(int t_id, const std::vector<int>& ind_d_ord,
                                         const std::vector<int>& match_d_ord,
                                         const std::vector<int>& treat, const std::vector<double>& dist,
                                         const std::vector<char>& eligible, int r,
                                         const std::vector<int>& prev, double caliper,
                                         int first_c, int last_c, int ratio) {
  const int ii = match_d_ord[t_id];
  int iil = ii, iir = ii;
  double min_dist = 0.0;
  if (r > 1 && !prev.empty()) {
    for (int m : prev) {
      iil = std::min(iil, match_d_ord[m]);
      iir = std::max(iir, match_d_ord[m]);
    }
    if (iil == ii) min_dist = std::abs(dist[t_id] - dist[ind_d_ord[iir]]);
    else if (iir == ii) min_dist = std::abs(dist[t_id] - dist[ind_d_ord[iil]]);
    else min_dist = std::max(std::abs(dist[t_id] - dist[ind_d_ord[iil]]),
                             std::abs(dist[t_id] - dist[ind_d_ord[iir]]));
  }
  const double di = dist[t_id];
  bool l_stop = false, r_stop = false, left = false;
  std::vector<int> pid;
  std::vector<double> pdist;
  int nl = 0, nr = 0;
  while (!l_stop || !r_stop) {
    if (l_stop) left = false;
    else if (r_stop) left = true;
    else left = !left;
    int iz;
    if (left) {
      if (iil <= first_c || nl == ratio) { l_stop = true; continue; }
      iz = ind_d_ord[--iil];
    } else {
      if (iir >= last_c || nr == ratio) { r_stop = true; continue; }
      iz = ind_d_ord[++iir];
    }
    if (!eligible[iz] || treat[iz] != 0) continue;
    if (r > 1 && std::find(prev.begin(), prev.end(), iz) != prev.end()) continue;
    const double dc = std::abs(di - dist[iz]);
    if (dc > caliper) {
      if (left) l_stop = true; else r_stop = true;
      continue;
    }
    if (dc < min_dist) continue;
    if (pid.size() >= static_cast<size_t>(ratio)) {
      int closer = 0;
      for (double d : pdist) {
        if (d < dc && ++closer == ratio) break;
      }
      if (closer >= ratio) {
        if (left) l_stop = true; else r_stop = true;
        continue;
      }
    }
    pid.push_back(iz);
    pdist.push_back(dc);
    if (left) { if (++nl == ratio) l_stop = true; }
    else { if (++nr == ratio) r_stop = true; }
  }
  const int n = static_cast<int>(pid.size());
  if (n <= 1) return pid;
  if (n <= ratio && std::is_sorted(pdist.begin(), pdist.end())) return pid;
  std::vector<int> ind(n);
  std::iota(ind.begin(), ind.end(), 0);
  auto by_dist = [&pdist](int a, int b) { return pdist[a] < pdist[b]; };
  std::vector<int> out;
  if (n > ratio) {
    std::partial_sort(ind.begin(), ind.begin() + ratio, ind.end(), by_dist);
    for (int k = 0; k < ratio; ++k) out.push_back(pid[ind[k]]);
  } else {
    std::sort(ind.begin(), ind.end(), by_dist);
    for (int k = 0; k < n; ++k) out.push_back(pid[ind[k]]);
  }
  return out;
}

// treat: 0/1 per unit in data order; dist: the distance per unit; ratio: matches
// wanted per treated unit (treated in data order); caliper: NA for none.
// Returns the match matrix (treated in data order x max ratio) of 1-based unit
// indices, NA where a slot is unfilled.
// [[Rcpp::export(.morie_match_nn_cpp)]]
IntegerMatrix morie_match_nn_cpp(IntegerVector treat_, NumericVector dist_, IntegerVector ratio_,
                                 bool replace, double caliper) {
  const int n = treat_.size();
  if (dist_.size() != n) stop("treat and distance must have the same length");
  std::vector<int> treat(n);
  std::vector<double> dist(n);
  std::vector<int> ind_focal;
  for (int i = 0; i < n; ++i) {
    if (treat_[i] != 0 && treat_[i] != 1) stop("treat must be 0/1");
    if (!R_FINITE(dist_[i])) stop("distance must be finite (unit %d is not)", i + 1);
    treat[i] = treat_[i];
    dist[i] = dist_[i];
    if (treat[i] == 1) ind_focal.push_back(i);
  }
  const int nf = static_cast<int>(ind_focal.size());
  if (ratio_.size() != nf) stop("ratio must have one entry per treated unit");
  std::vector<int> ratio(nf);
  int max_ratio = 1;
  for (int i = 0; i < nf; ++i) {
    if (ratio_[i] == NA_INTEGER || ratio_[i] < 1) stop("ratio must be positive");
    ratio[i] = ratio_[i];
    max_ratio = std::max(max_ratio, ratio[i]);
  }
  IntegerMatrix mm(nf, max_ratio);
  std::fill(mm.begin(), mm.end(), NA_INTEGER);
  if (nf == 0 || nf == n) return mm;
  // positions in the sort of all units by distance (stable: R's order())
  std::vector<int> ind_d_ord(n), match_d_ord(n);
  std::iota(ind_d_ord.begin(), ind_d_ord.end(), 0);
  std::stable_sort(ind_d_ord.begin(), ind_d_ord.end(), [&dist](int a, int b) { return dist[a] < dist[b]; });
  for (int k = 0; k < n; ++k) match_d_ord[ind_d_ord[k]] = k;
  if (ISNAN(caliper)) {
    caliper = *std::max_element(dist.begin(), dist.end()) - *std::min_element(dist.begin(), dist.end()) + 1;
  }
  std::vector<char> eligible(n, 1);
  int first_c = 0, last_c = n - 1;
  std::vector<std::vector<int>> rows(nf);
  if (!replace) {
    // treated in decreasing distance (R's order(decreasing = TRUE): stable)
    std::vector<int> ord(nf);
    std::iota(ord.begin(), ord.end(), 0);
    std::stable_sort(ord.begin(), ord.end(), [&](int a, int b) { return dist[ind_focal[a]] > dist[ind_focal[b]]; });
    std::vector<int> times(n, 0), allowed(n, 1);
    for (int i = 0; i < nf; ++i) allowed[ind_focal[i]] = ratio[i];
    int n_elig_c = n - nf;
    for (int r = 1; r <= max_ratio; ++r) {
      for (int ti : ord) {
        if (ratio[ti] < r) continue;
        if (n_elig_c == 0) break;
        const int t_id = ind_focal[ti];
        if (!eligible[t_id]) continue;
        nn_update_bounds(first_c, last_c, ind_d_ord, eligible, treat);
        std::vector<int> k = nn_find_controls(t_id, ind_d_ord, match_d_ord, treat, dist, eligible, r,
                                              rows[ti], caliper, first_c, last_c, 1);
        if (k.empty()) {
          eligible[t_id] = 0;
          continue;
        }
        rows[ti].push_back(k[0]);
        for (int ck : {k[0], t_id}) {
          if (!eligible[ck]) continue;
          if (++times[ck] >= allowed[ck]) {
            eligible[ck] = 0;
            if (treat[ck] == 0) --n_elig_c;
          }
        }
      }
    }
  } else {
    for (int ti = 0; ti < nf; ++ti) {
      std::vector<int> k = nn_find_controls(ind_focal[ti], ind_d_ord, match_d_ord, treat, dist, eligible, 1,
                                            rows[ti], caliper, first_c, last_c, ratio[ti]);
      rows[ti] = k;
    }
  }
  for (int ti = 0; ti < nf; ++ti)
    for (size_t k = 0; k < rows[ti].size(); ++k) mm(ti, static_cast<int>(k)) = rows[ti][k] + 1;
  return mm;
}
