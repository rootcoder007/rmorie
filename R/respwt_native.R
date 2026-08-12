# Weighting-class nonresponse adjustment.
# Source: Lohr (2010), Sampling: Design and Analysis, 2nd ed.,
# Sec. 8.5.1, Example 8.4 and Table 8.2
# (fetched-wave3/Sampling_Design_and_Analysis.pdf).  Mirrors Python
# morie.fn.respwt exactly.

#' Weighting-class adjustment for survey nonresponse
#'
#' phi_hat_c = sum(weights of respondents in class c) / sum(weights
#' of the selected sample in class c); respondent weights are
#' multiplied by 1/phi_hat_c, so adjusted class totals equal the
#' original full-sample class totals exactly.
#'
#' @param weights Positive sampling weights of the selected sample.
#' @param responded Logical response indicator.
#' @param classes Weighting-class label per unit.
#' @return A list with elements \code{adjusted} (NA for
#'   nonrespondents), \code{phi_hat}, \code{factors},
#'   \code{balance_error}, \code{n}, \code{method}.
#' @references Lohr, S. L. (2010). Sampling: Design and Analysis,
#'   2nd ed. Brooks/Cole, Sec. 8.5.1.
#' @export
morie_respwt <- function(weights, responded, classes) {
  w <- as.numeric(weights)
  n <- length(w)
  if (length(responded) != n || length(classes) != n || n == 0) {
    stop("weights, responded, classes must be paired")
  }
  if (any(w <= 0)) stop("weights must be positive")
  cl <- as.character(classes)
  responded <- as.logical(responded)
  tot <- tapply(w, cl, sum)
  resp <- tapply(w * responded, cl, sum)
  if (any(resp <= 0)) stop("a class has no respondents")
  phi <- resp / tot
  fac <- 1 / phi
  adjusted <- ifelse(responded, w * fac[cl], NA_real_)
  bal <- 0
  for (c_ in names(tot)) {
    s <- sum(adjusted[responded & cl == c_])
    bal <- max(bal, abs(s - tot[[c_]]))
  }
  list(adjusted = adjusted,
       phi_hat = as.list(phi),
       factors = as.list(fac),
       balance_error = bal, n = n,
       method = "weighting-class adjustment (Lohr 2010, Sec. 8.5.1)")
}
