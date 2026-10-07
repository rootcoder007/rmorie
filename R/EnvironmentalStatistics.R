#' Climate and agreement statistics
#'

#' \code{AceIndex}: accumulated cyclone energy. \code{BudykoOlr}: linearised
#' outgoing longwave radiation and energy-balance equilibrium.
#' \code{PrewhitenedMannKendall}: Mann-Kendall test after prewhitening
#' (von Storch) or trend-free prewhitening (Yue et al.), as \code{modifiedmk}.
#' \code{FleissKappa}: Fleiss' kappa with the null standard error, as
#' \code{irr::kappam.fleiss}. \code{EmpiricalBreakdownPoint}: replacement
#' breakdown point of an estimator. Identical to the Python arm
#' \code{morie.fn.envstats}.
#'
#' @param T Temperature (degrees C).
#' @param A,B OLR intercept and slope.
#' @param S0 Solar constant.
#' @param albedo Planetary albedo.
#' @param x Series or sample.
#' @param method "pw" or "tfpw".
#' @param counts Subjects by categories matrix of rating counts.
#' @param estimator Function of a numeric vector.
#' @param magnitude,tol Contamination size and breakdown tolerance.
#' @return Number or list.
#' @references Bell, G. D. et al. (2000). Climate assessment for 1999.
#'   Bulletin of the American Meteorological Society 81, S1-S50.
#'
#'   North, G. R., Cahalan, R. F. and Coakley, J. A. (1981). Energy balance
#'   climate models. Reviews of Geophysics and Space Physics 19, 91-121.
#'
#'   Yue, S., Pilon, P., Phinney, B. and Cavadias, G. (2002). The influence
#'   of autocorrelation on the ability to detect trend in hydrological
#'   series. Hydrological Processes 16, 1807-1829.
#'
#'   Fleiss, J. L. (1971). Measuring nominal scale agreement among many
#'   raters. Psychological Bulletin 76, 378-382.
#'
#'   Donoho, D. L. and Huber, P. J. (1983). The notion of breakdown point. A
#'   Festschrift for Erich L. Lehmann, 157-184.
#' @examples
#' FleissKappa(rbind(c(3, 0), c(0, 3), c(2, 1), c(3, 0)))$kappa
#' EmpiricalBreakdownPoint(median, 1:9)$breakdown_point
#' @export
BudykoOlr <- function(T, A = 203.3, B = 2.09, S0 = 1361, albedo = 0.3) {
  .morie_arg(T, "n")
  list(olr = A + B * T, equilibrium_temperature = ((1 - albedo) * S0 / 4 - A) / B, sensitivity = 1 / B)
}

.es_sen <- function(x) {
  n <- length(x)
  v <- numeric(0)
  for (i in seq_len(n - 1)) for (j in (i + 1):n) v <- c(v, (x[j] - x[i]) / (j - i))
  stats::median(v)
}

.es_acf1 <- function(x) {
  d <- x - .es_ss(x) / length(x)
  .es_ss(d[-length(d)] * d[-1]) / .es_ss(d * d)
}

#' @rdname BudykoOlr
#' @export
PrewhitenedMannKendall <- function(x, method = "tfpw") {
  n <- length(x)
  if (n < 3) stop("need at least three values")
  if (method == "pw") {
    r1 <- .es_acf1(x)
    y <- x[-1] - r1 * x[-n]
    old <- .es_sen(x)
  } else if (method == "tfpw") {
    old <- .es_sen(x)
    xt <- x - old * seq_len(n)
    r1 <- .es_acf1(xt)
    y <- xt[-1] - r1 * xt[-n] + old * seq_len(n - 1)
  } else {
    stop("method must be 'pw' or 'tfpw'")
  }
  m <- length(y)
  S <- 0
  for (i in seq_len(m - 1)) for (j in (i + 1):m) S <- S + sign(y[j] - y[i])
  var <- m * (m - 1) * (2 * m + 5) / 18
  for (t in table(y)) if (t > 1) var <- var - t * (t - 1) * (2 * t + 5) / 18
  z <- if (S == 0) 0 else if (S > 0) (S - 1) / sqrt(var) else (S + 1) / sqrt(var)
  list(S = S, var_S = unname(var), Z = unname(z), p_value = unname(2 * stats::pnorm(-abs(z))), tau = S / (0.5 * m * (m - 1)),
       sen_slope = .es_sen(y), old_sen_slope = old, r1 = r1)
}

#' @rdname BudykoOlr
#' @export
FleissKappa <- function(counts) {
  Tt <- as.matrix(counts) + 0
  N <- nrow(Tt)
  m <- sum(Tt[1, ])
  if (any(abs(rowSums(Tt) - m) > 1e-9)) stop("every subject needs the same number of ratings")
  pbar <- .es_ss((rowSums(Tt^2) - m) / (m * (m - 1)) / N)
  pj <- colSums(Tt) / (N * m)
  pe <- .es_ss(pj^2)
  kappa <- (pbar - pe) / (1 - pe)
  pq <- .es_ss(pj * (1 - pj))
  var <- 2 / (pq^2 * (N * m * (m - 1))) * (pq^2 - .es_ss(pj * (1 - pj) * ((1 - pj) - pj)))
  se <- sqrt(var)
  z <- kappa / se
  list(kappa = kappa, se = se, z = z, p_value = 2 * stats::pnorm(-abs(z)))
}

#' @rdname BudykoOlr
#' @export
EmpiricalBreakdownPoint <- function(estimator, x, magnitude = 1e12, tol = 1e6) {
  x <- as.numeric(x)
  n <- length(x)
  base <- estimator(x)
  scale <- max(x) - min(x)
  if (scale == 0) scale <- 1
  ord <- order(-abs(x), seq_len(n))
  for (m in seq_len(n)) {
    z <- x
    z[ord[seq_len(m)]] <- magnitude * (1 + (seq_len(m) - 1))
    if (abs(estimator(z) - base) > tol * scale) return(list(m = m, breakdown_point = m / n))
  }
  list(m = n + 1, breakdown_point = 1)
}

.es_ss <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}
