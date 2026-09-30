// SPDX-License-Identifier: AGPL-3.0-or-later
// Exposure mapping on a place network (research P3).
//
// Given a treatment indicator per place and an edge list (0-based, either
// direction), count for every place the number of treated neighbours and
// assign the three-level exposure used by the identification theorems in
// research/lean/P3Interference.lean:
//   2 = treated, 1 = untreated with at least one treated neighbour,
//   0 = untreated with no treated neighbour.
// Distance-d exposure is obtained by passing the d-ring adjacency.
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(.morie_spillover_exposure_cpp)]]
List morie_spillover_exposure_cpp(IntegerVector treated, IntegerVector from, IntegerVector to) {
  const int n = treated.size();
  if (from.size() != to.size()) stop("from and to must have the same length");
  IntegerVector treated_neighbours(n, 0);
  for (int k = 0; k < from.size(); ++k) {
    const int a = from[k], b = to[k];
    if (a < 0 || a >= n || b < 0 || b >= n) stop("edge index out of range");
    if (a == b) continue;
    if (treated[b] == 1) treated_neighbours[a] += 1;
    if (treated[a] == 1) treated_neighbours[b] += 1;
  }
  IntegerVector exposure(n);
  for (int i = 0; i < n; ++i) {
    if (treated[i] == 1) exposure[i] = 2;
    else exposure[i] = treated_neighbours[i] > 0 ? 1 : 0;
  }
  return List::create(_["exposure"] = exposure,
                      _["treated_neighbours"] = treated_neighbours);
}
