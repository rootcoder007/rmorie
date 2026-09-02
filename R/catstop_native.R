# CAT precision stopping rule (Wainer & Mislevy; Magis & Raiche 2012).
# Source: Magis & Raiche (2012), JSS 48(8), Eqs. 1, 3, 4, 6
# (fetched-wave3/magis-raiche-2012-catR-JSS48.pdf): 4PL response
# function, item information I_j = P'^2/(PQ), se_ML = 1/sqrt(sum I_j),
# se_BM = 1/sqrt(1/sigma^2 + sum I_j).  Mirrors Python
# morie.fn.catstop exactly.

#' .catstop_info_4pl
#'
#' A step of the catstop_native implementation. Called by \code{morie_catstop}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param theta Numeric; combined arithmetically in the body.
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @param c Numeric; combined arithmetically in the body.
#' @param d Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.catstop_info_4pl <- function(theta, a, b, c, d) {
  e <- exp(a * (theta - b))
  p <- c + (d - c) * e / (1 + e)
  q <- 1 - p
  dp <- a * (d - c) * e / (1 + e)^2
  if (p <= 0 || q <= 0) return(0)
  dp * dp / (p * q)
}

#' CAT precision stopping rule
#'
#' Computes the provisional standard error of the ability estimate
#' from the administered 4PL items (catR Eqs. 1, 3, 4, 6) and reports
#' whether the precision criterion se <= se_target is met, i.e. the
#' standard-error stopping rule of computerized adaptive testing.
#'
#' @param items Matrix or list of item parameter vectors (a, b, c, d);
#'   use (a, b, 0, 1) for 2PL items.
#' @param theta Current provisional ability estimate.
#' @param se_target Positive precision target.
#' @param estimator "ML" (default) or "BM" (normal prior).
#' @param prior_var Prior variance sigma^2 for the BM estimator.
#' @return A list with elements \code{stop}, \code{se},
#'   \code{information}, \code{item_information}, \code{n_items},
#'   \code{estimator}, \code{se_target}, \code{method}.
#' @references Magis, D. and Raiche, G. (2012). Random generation of
#'   response patterns under computerized adaptive testing with the R
#'   package catR. Journal of Statistical Software, 48(8).  Wainer, H.
#'   and Mislevy, R. J. (2000). In Wainer (ed.), Computerized Adaptive
#'   Testing: A Primer, 2nd ed.
#' @export
#' @examples
#' items <- matrix(c(1, 0, 0.2, 0.95, 1.2, -0.5, 0.1, 0.98, 0.8, 1, 0.15, 0.9),
#'                 3, 4, byrow = TRUE)
#' morie_catstop(items, theta = 0.5, se_target = 0.5)
morie_catstop <- function(items, theta, se_target, estimator = "ML",
                          prior_var = 1) {
  if (is.matrix(items)) {
    items <- lapply(seq_len(nrow(items)), function(i) items[i, ])
  }
  th <- as.numeric(theta)
  tgt <- as.numeric(se_target)
  if (tgt <= 0) stop("se_target must be positive")
  est <- toupper(estimator)
  if (!est %in% c("ML", "BM")) stop("estimator must be 'ML' or 'BM'")
  infos <- vapply(items, function(it) {
    it <- as.numeric(it)
    a <- it[1]; b <- it[2]; c_ <- it[3]; d <- it[4]
    if (!(c_ >= 0 && c_ < d && d <= 1)) {
      stop("item parameters need 0 <= c < d <= 1")
    }
    .catstop_info_4pl(th, a, b, c_, d)
  }, numeric(1))
  total <- sum(infos)
  denom <- if (est == "BM") {
    pv <- as.numeric(prior_var)
    if (pv <= 0) stop("prior_var must be positive")
    1 / pv + total
  } else {
    total
  }
  se <- if (denom <= 0) Inf else 1 / sqrt(denom)
  list(stop = se <= tgt,
       se = se,
       information = total,
       item_information = infos,
       n_items = length(infos),
       estimator = est,
       se_target = tgt,
       method = "CAT precision stopping rule (catR Eqs. 3-4, 6)")
}
