// SPDX-License-Identifier: AGPL-3.0-or-later
// Predictive-policing feedback loop: two-region urn simulator (research P4).
//
// Each step the patrol is sent to region A with probability x = cA/(cA+cB)
// (region B otherwise). A crime is discovered at the visited region with
// probability equal to that region's true rate. With probability `rho` the
// step's crime is instead reported by the public, independently of where
// the patrol is, from region A with probability lamA/(lamA+lamB).
//
// update = 0 (naive): a discovered crime adds 1 to its region's count.
// update = 1 (corrected): a discovered crime adds 1/x_visited, so the
//   expected increment equals the true rate (Ensign et al. 2018 discount).
//
// The mean-field limits of both updates are proved in
// research/lean/P4Feedback.lean and P4Limit.lean:
//   naive, lamA > lamB : share -> 1   (Research.P4.naiveShare_tendsto_one)
//   corrected          : share -> lamA/(lamA+lamB)
//                        (Research.P4.corrected_share_tendsto)
// Returns the share path (length n_steps + 1) and the final counts.
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(.morie_feedback_urn_cpp)]]
List morie_feedback_urn_cpp(double lamA, double lamB, double cA0, double cB0,
                            int n_steps, int update, double rho) {
  if (!(lamA > 0.0 && lamA <= 1.0 && lamB > 0.0 && lamB <= 1.0))
    stop("lamA and lamB must lie in (0, 1]: they are per-visit discovery probabilities");
  if (!(cA0 > 0.0 && cB0 > 0.0)) stop("initial counts must be positive");
  if (n_steps < 0) stop("n_steps must be non-negative");
  if (!(rho >= 0.0 && rho <= 1.0)) stop("rho must lie in [0, 1]");
  NumericVector share(n_steps + 1);
  double cA = cA0, cB = cB0;
  const double pA_report = lamA / (lamA + lamB);
  share[0] = cA / (cA + cB);
  RNGScope scope;
  for (int t = 0; t < n_steps; ++t) {
    const double x = cA / (cA + cB);
    if (rho > 0.0 && R::unif_rand() < rho) {
      // public report: location follows the true rates, no presence effect
      if (R::unif_rand() < pA_report) cA += 1.0; else cB += 1.0;
    } else {
      const bool visitA = R::unif_rand() < x;
      const double lam = visitA ? lamA : lamB;
      if (R::unif_rand() < lam) {
        const double presence = visitA ? x : (1.0 - x);
        const double w = (update == 1) ? 1.0 / presence : 1.0;
        if (visitA) cA += w; else cB += w;
      }
    }
    share[t + 1] = cA / (cA + cB);
  }
  return List::create(_["share"] = share, _["cA"] = cA, _["cB"] = cB);
}
