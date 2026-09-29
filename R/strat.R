# SPDX-License-Identifier: AGPL-3.0-or-later
#' Stratified mean estimator (Cochran 1977, Sampling Techniques, Ch. 5)
#'
#' Within-stratum means averaged with population (or proportional)
#' weights W_h, with the stratified simple random sampling variance
#'    var(y_bar_st) = sum_h W_h^2 (1 - n_h / N_h) s_h^2 / n_h
#' (finite population correction taken as 1 without population sizes), as
#' \code{survey::svymean} with strata and fpc.
#' Python parity: \code{morie.fn.strat.stratified_mean}.
#'
#' @param data data.frame containing outcome and stratum columns.
#' @param y character; outcome column.
#' @param strata character; stratum column.
#' @param pop_sizes optional named numeric vector mapping stratum -> N_h.
#'   If NULL, proportional weights W_h = n_h/sum(n_h) are used.
#' @return list: estimate, se, ci_lower, ci_upper, weights, strata_means,
#'   n_strata, method.
#' @references Cochran, W. G. (1977). Sampling Techniques, 3rd edn. Wiley.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(100), stratum = rep(c("a", "b"), each = 50))
#' morie_stratified_sampling(df, y = "y", strata = "stratum")
#' @keywords internal
#' @export
strat <- function(data, y = "y", strata = "stratum", pop_sizes = NULL) {
  yv <- as.numeric(data[[y]])
  sv <- data[[strata]]
  strata_names <- unique(sv)
  n_h <- vapply(strata_names, function(s) sum(sv == s), integer(1))
  yb_h <- vapply(strata_names, function(s) mean(yv[sv == s]), numeric(1))
  s2_h <- vapply(strata_names, function(s) stats::var(yv[sv == s]), numeric(1))
  if (is.null(pop_sizes)) {
    W_h <- n_h / sum(n_h)
    fpc <- rep(1, length(n_h))
  } else {
    N_h <- vapply(strata_names, function(s) as.numeric(pop_sizes[[as.character(s)]]), numeric(1))
    W_h <- N_h / sum(N_h)
    fpc <- 1 - n_h / N_h
  }
  est <- sum(W_h * yb_h)
  var_st <- sum(W_h^2 * fpc * s2_h / n_h)
  se <- sqrt(var_st)
  z <- stats::qnorm(0.975)
  names(W_h) <- as.character(strata_names)
  names(yb_h) <- as.character(strata_names)
  list(
    estimate = as.numeric(est), se = as.numeric(se),
    ci_lower = est - z * se, ci_upper = est + z * se,
    weights = as.list(W_h), strata_means = as.list(yb_h),
    n_strata = length(strata_names),
    method = "Stratified mean (Cochran 1977)"
  )
}

#' @rdname strat
#' @keywords internal
#' @export
morie_stratified_sampling <- strat
