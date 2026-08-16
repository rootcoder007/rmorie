# HAL-TMLE: efficiency under weak conditions.
# Sources: van der Laan, M. J. & Rose, S. (2018) *Targeted Learning
# in Data Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 7
# (a TMLE that uses the HAL estimator as initial estimator, or a
# super learner whose library contains it, is asymptotically
# efficient under very weak regularity conditions, as long as the
# strong positivity assumption holds). Chap. 6 (the HAL rate
# faster than n^{-1/4}). Chap. 4 (the second-order remainder of
# the longitudinal parameter and the role of the positivity bound
# in controlling it; and the note that the Donsker class
# assumption can be avoided by using cross-validated TMLE). van
# der Laan, M. J. (2017) "A generally efficient targeted minimum
# loss based estimator based on the highly adaptive lasso",
# International Journal of Biostatistics 13(2), 20150097,
# doi:10.1515/ijb-2015-0097. Zheng, W. & van der Laan, M. J.
# (2011) "Cross-Validated Targeted Minimum-Loss-Based Estimation",
# in *Targeted Learning*, Springer, 459-474,
# doi:10.1007/978-1-4419-9782-1_27. CV-TMLE, which removes the
# Donsker requirement.
#
# Native implementation mirroring Python morie.fn.tlhaltm exactly:
# the same rate condition, the same remainder bound with the same
# 1/delta factor, the same joint efficiency check that keeps the
# rate and the Donsker condition separate, and the same
# CV-TMLE split.

#' morie_tlhaltm
#'
#' A step of the tlhaltm_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param rate_Q Passed to \code{rate_condition}.
#' @param rate_g Passed to \code{rate_condition}.
#' @param n Passed to \code{rate_condition}.
#' @param err_Q Passed to \code{remainder_bound}.
#' @param err_g Passed to \code{remainder_bound}.
#' @param delta Passed to \code{remainder_bound}.
#' @param donsker Passed to \code{efficiency_check}. Defaults to \code{TRUE}.
#' @param mode One of \code{"rate"}, \code{"remainder"}, \code{"split"}.
#' @return The value of \code{efficiency_check}.
#' @export
morie_tlhaltm <- function(rate_Q = NULL, rate_g = NULL, n = NULL,
                          err_Q = NULL, err_g = NULL, delta = NULL,
                          donsker = TRUE,
                          mode = c("rate", "remainder",
                                   "efficiency", "split")) {
  mode <- match.arg(mode)
  if (mode == "rate") return(rate_condition(rate_Q, rate_g, n))
  if (mode == "remainder")
    return(remainder_bound(err_Q, err_g, delta))
  if (mode == "split") return(cv_tmle_split(n))
  efficiency_check(err_Q, err_g, delta, n, donsker = donsker)
}

#' rate_condition
#'
#' A step of the tlhaltm_native implementation. Called by \code{morie_tlhaltm}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param rate_Q Coerced to numeric by the body, with \code{as.numeric}.
#' @param rate_g Coerced to numeric by the body, with \code{as.numeric}.
#' @param n Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{sum}, \code{required}, \code{satisfied}, \code{product_order}, \code{root_n_order}, \code{note}.
#' @export
rate_condition <- function(rate_Q, rate_g, n) {
  a <- as.numeric(rate_Q); b <- as.numeric(rate_g)
  if (a <= 0 || b <= 0)
    stop("tlhaltm: rates must be positive exponents")
  list(sum = a + b, required = 0.5, satisfied = a + b > 0.5,
       product_order = (as.integer(n))^(-(a + b)),
       root_n_order = (as.integer(n))^(-0.5),
       note = "two estimators at exactly n^{-1/4} sit ON the boundary; HAL's n^{-1/3} clears it")
}

#' remainder_bound
#'
#' A step of the tlhaltm_native implementation. Called by \code{efficiency_check}, \code{morie_tlhaltm}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param err_Q Coerced to numeric by the body, with \code{as.numeric}.
#' @param err_g Coerced to numeric by the body, with \code{as.numeric}.
#' @param delta Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{bound}, \code{delta}, \code{err_Q}, \code{err_g}, \code{note}.
#' @export
remainder_bound <- function(err_Q, err_g, delta) {
  d <- as.numeric(delta)
  if (!(0 < d && d <= 1))
    stop(sprintf("tlhaltm: the positivity bound delta must lie in (0,1], got %g",
                 delta))
  list(bound = as.numeric(err_Q) * as.numeric(err_g) / d,
       delta = d, err_Q = as.numeric(err_Q),
       err_g = as.numeric(err_g),
       note = "the bound diverges as delta -> 0 however small the nuisance errors are")
}

#' efficiency_check
#'
#' A step of the tlhaltm_native implementation. Called by \code{morie_tlhaltm}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param err_Q Passed to \code{remainder_bound}.
#' @param err_g Passed to \code{remainder_bound}.
#' @param delta Coerced to numeric by the body, with \code{as.numeric}.
#' @param n Coerced to integer by the body, with \code{as.integer}.
#' @param donsker A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{estimate}, \code{remainder_bound}, \code{root_n}, \code{remainder_negligible}, \code{donsker_satisfied}, \code{efficient}, \code{positivity_delta}, \code{method}, \code{note}.
#' @export
efficiency_check <- function(err_Q, err_g, delta, n, donsker = TRUE) {
  r <- remainder_bound(err_Q, err_g, delta)
  root_n <- 1 / sqrt(as.integer(n))
  list(estimate = r$bound,
       remainder_bound = r$bound, root_n = root_n,
       remainder_negligible = r$bound < root_n,
       donsker_satisfied = isTRUE(donsker),
       efficient = r$bound < root_n && isTRUE(donsker),
       positivity_delta = as.numeric(delta),
       method = "HAL-TMLE efficiency conditions; van der Laan & Rose (2018) Chap. 7",
       note = "HAL supplies BOTH -- the rate, and a bounded variation-norm class that is Donsker; CV-TMLE removes the second requirement entirely")
}

#' cv_tmle_split
#'
#' A step of the tlhaltm_native implementation. Called by \code{morie_tlhaltm}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param V A count; the body uses it as \code{seq_len(...)}. Defaults to \code{10L}.
#' @param seed Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0L}.
#' @return A list with \code{folds}, \code{training}, \code{V}, \code{note}.
#' @export
cv_tmle_split <- function(n, V = 10L, seed = 0L) {
  n <- as.integer(n); V <- as.integer(V)
  if (V < 2L || V > n)
    stop(sprintf("tlhaltm: V must lie in 2..%d, got %d", n, V))
  e_rng <- .ghc_rng(as.numeric(seed))
  idx <- seq_len(n)
  for (i in n:2) {
    j <- as.integer(.ghc_unif(e_rng, 1L) * (i + 1)) %% (i + 1)
    if (j == 0L) j <- 1L
    if (j == i) j <- i - 1L
    tmp <- idx[i]; idx[i] <- idx[j]; idx[j] <- tmp
  }
  folds <- lapply(seq_len(V), function(v)
    sort(idx[seq(v, length(idx), by = V)]))
  list(folds = folds,
       training = lapply(folds, function(f)
         sort(setdiff(seq_len(n), f))),
       V = V,
       note = "the fit is independent of the validation sample, so no Donsker condition is needed")
}

#' .tlhaltm_cheatsheet
#'
#' A step of the tlhaltm_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
.tlhaltm_cheatsheet <- function() {
  paste("tlhaltm: TMLE is efficient when (a) the second-order ",
        "remainder -- a PRODUCT of the two nuisance errors -- is ",
        "o(n^-1/2), and (b) the fits stay in a Donsker class. HAL ",
        "supplies both: its rate beats n^{-1/4}, so the product ",
        "clears n^{-1/2}, and bounded-variation cadlag functions ",
        "form a Donsker class. Neither is assumed. What IS still ",
        "required is STRONG POSITIVITY: the remainder bound ",
        "carries a 1/delta, so it diverges as delta -> 0 no ",
        "matter how good the fits are. CV-TMLE drops (b) ",
        "outright.", sep = "")
}
