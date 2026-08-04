# SPDX-License-Identifier: AGPL-3.0-or-later
#' Posterior predictive check and the Bayesian p-value
#'
#' Meng (1994), Posterior predictive p-values, Annals of Statistics 22(3),
#' 1142-1160, and Gelman, Meng and Stern (1996), Posterior predictive
#' assessment of model fitness via realized discrepancies, Statistica
#' Sinica 6(4), 733-760: p_B = Pr(T(y_rep, theta) >= T(y, theta) | y),
#' estimated as the proportion of replicated datasets whose discrepancy
#' exceeds the observed one.  Neither was retrievable here as a full text;
#' the definition is quoted in its standard published form.  p_B is
#' CONSERVATIVE -- under the model its distribution concentrates around
#' 1/2 -- so a value near 0.5 is evidence of nothing and only extremes are
#' informative; that is stated in the result rather than left to be
#' rediscovered.
#'
#' @param y observed data.
#' @param y_rep replicated datasets, one row per draw.
#' @param statistic "mean", "sd", "min", "max", "median", or a function.
#' @return list: estimate, p_value, t_obs, t_rep, mean_t_rep, n_rep, method.
#' @keywords internal
#' @examples
#' Ppcheck(c(1, 2, 3), matrix(c(1, 2, 4, 0, 2, 3), 2, 3, byrow = TRUE))$p_value
#' @export
Ppcheck <- function(y, y_rep, statistic = "mean") {
  v <- .s03vec(y); R <- .s03mat(y_rep)
  f <- if (is.function(statistic)) statistic else switch(
    as.character(statistic),
    mean = function(z) .s03mean(z),
    sd = function(z) .s03sd(z, 1L),
    min = function(z) min(z),
    max = function(z) max(z),
    median = function(z) .s03median(z),
    function(z) .s03mean(z))
  tobs <- as.numeric(f(v))
  trep <- numeric(nrow(R))
  for (i in seq_len(nrow(R))) trep[i] <- as.numeric(f(as.numeric(R[i, ])))
  ge <- 0
  for (t in trep) if (t >= tobs) ge <- ge + 1
  p <- if (length(trep)) ge / length(trep) else NaN
  list(estimate = p, p_value = p, t_obs = tobs, t_rep = trep,
       mean_t_rep = if (length(trep)) .s03mean(trep) else NaN,
       n_rep = length(trep),
       method = "Posterior predictive p-value (Meng 1994; Gelman, Meng and Stern 1996)")
}
