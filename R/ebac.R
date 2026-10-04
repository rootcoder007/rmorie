#' Calculate estimated Blood Alcohol Concentration (eBAC)
#'
#' Compute the continuous estimated Blood Alcohol Concentration using the
#' standard Widmark formula. Mirrors the Python `morie.calculate_ebac()`.
#'
#' The Widmark formula in US units (Matthews and Miller 1979) is:
#' \deqn{eBAC = (A \times 5.14) / (weight\_lbs \times r) - 0.015 \times hours}{eBAC
#' = (A x 5.14) / (weight\_lbs x r) - 0.015 x hours}
#' where \eqn{A} is the alcohol in fluid ounces of ethanol (a 14 g standard
#' drink is 0.6 fl oz, so \eqn{A = 0.6 \times drinks}) and \eqn{r} is the
#' gender constant (0.73 for men, 0.66 for women). Counting each drink as one
#' ounce overstated eBAC by a factor of 1/0.6.
#' Returned values are clipped at zero.
#'
#' @param drinks Number of standard drinks consumed (1 drink = 14 g alcohol).
#' @param weight_lbs Body weight in pounds.
#' @param hours Hours elapsed since drinking began.
#' @param gender_constant Widmark gender multiplier (0.73 men, 0.66 women).
#'
#' @return Non-negative numeric scalar: estimated BAC.
#' @export
#' @examples
#' morie_calculate_ebac(drinks = 4, weight_lbs = 180, hours = 2, gender_constant = 0.73)
morie_calculate_ebac <- function(drinks, weight_lbs, hours, gender_constant) {
  # a data-entry error must not read as a sober respondent (a negative result clamps to 0 below)
  if (!isTRUE(weight_lbs > 0)) stop(sprintf("weight_lbs must be > 0, not %s", format(weight_lbs)), call. = FALSE)
  if (isTRUE(drinks < 0) || isTRUE(hours < 0)) {
    stop(sprintf("drinks and hours cannot be negative (drinks = %s, hours = %s)", format(drinks), format(hours)), call. = FALSE)
  }
  if (!isTRUE(gender_constant > 0)) stop(sprintf("gender_constant must be > 0, not %s", format(gender_constant)), call. = FALSE)
  ounces <- drinks * 0.6  # a 14 g standard drink is 0.6 fl oz of ethanol (NIAAA)
  ebac <- (ounces * 5.14) / (weight_lbs * gender_constant) - (0.015 * hours)
  max(0.0, ebac)
}

#' Test whether an eBAC exceeds a legal driving limit
#'
#' @param ebac Numeric eBAC value (e.g. from [morie_calculate_ebac()]).
#' @param limit Legal threshold (default 0.08, the per-se limit in most
#'   Canadian and US jurisdictions).
#'
#' @return Integer 1 if `ebac >= limit`, 0 otherwise. (Integer, not
#'   logical, to match the Python sibling and ease binary-outcome modelling.)
#' @export
#' @examples
#' morie_is_over_legal_limit(0.09)
#' morie_is_over_legal_limit(0.05, limit = 0.05)
morie_is_over_legal_limit <- function(ebac, limit = 0.08) {
  if (ebac >= limit) 1L else 0L
}
