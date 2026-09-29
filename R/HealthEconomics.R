#' Years of life lost and the cost-effectiveness plane
#'
#' R arm of \code{morie.fn.cdyll} and \code{hecea}. \code{Cdyll}: years of life
#' lost, the sum of deaths times remaining life expectancy at the age of death
#' (undiscounted). \code{Hecea}: percentage of joint (cost, effect) differences
#' in each quadrant of the cost-effectiveness plane.
#'
#' @param deaths Deaths by age group.
#' @param life_expectancy_remaining Remaining life expectancy at those ages.
#' @param cost_diffs,effect_diffs Simulated incremental costs and effects.
#' @return A named list (the Python result's fields).
#' @references Murray, C. J. L. (1994). Quantifying the burden of disease. Bulletin of the World Health Organization 72, 429-445.
#'
#'   Black, W. C. (1990). The CE plane. Medical Decision Making 10, 212-214.
#' @examples
#' Cdyll(c(2, 1, 3), c(30.5, 12, 4))$estimate
#' Hecea(c(1, -2, 3, 0.5), c(0.1, 0.2, -0.3, 0.4))$value
#' @export
Cdyll <- function(deaths, life_expectancy_remaining) {
  if (length(deaths) != length(life_expectancy_remaining)) {
    stop("deaths and life_expectancy_remaining must match in shape")
  }
  list(estimate = sum(deaths * life_expectancy_remaining), total_deaths = sum(deaths),
       mean_le_remaining = mean(life_expectancy_remaining))
}

#' @rdname Cdyll
#' @export
Hecea <- function(cost_diffs, effect_diffs) {
  if (length(cost_diffs) != length(effect_diffs)) stop("cost_diffs and effect_diffs must match")
  cc <- cost_diffs > 0
  ee <- effect_diffs > 0
  n <- length(cc)
  list(value = list(NE = sum(cc & ee) / n * 100, NW = sum(cc & !ee) / n * 100, SE = sum(!cc & ee) / n * 100,
                    SW = sum(!cc & !ee) / n * 100),
       n_simulations = n, mean_delta_c = mean(cost_diffs), mean_delta_e = mean(effect_diffs))
}
