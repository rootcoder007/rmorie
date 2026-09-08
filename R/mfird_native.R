# MIRT discrimination-to-factor-loading reparameterization.
# Source: Reckase (2009), Multidimensional Item Response Theory,
# Springer, Eq. 2.28, Sec. 3.3.3, Eqs. 6.11-6.12
# (fetched-wave3/Multidimensional_Item_Response_Theory.pdf).
# Mirrors Python morie.fn.mfird exactly.

#' MIRT a to factor-loading conversion (and inverse)
#'
#' lambda_il = a_il / sqrt(1 + a_i P a_i') (normal-ogive metric;
#' reduces to the inverse of Reckase Eq. 2.28 for m = 1), thresholds
#' tau_i = -d_i / sqrt(1 + a_i P a_i') per Eq. 6.12; the inverse map
#' is a_il = lambda_il / sqrt(1 - lambda_i R lambda_i').
#'
#' @param a Matrix (items x m) of discriminations (or loadings when
#'   \code{inverse}).
#' @param d Optional intercept vector (enables thresholds).
#' @param P Optional latent covariance (default identity).
#' @param inverse Convert loadings back to discriminations.
#' @return A list with elements \code{loadings} (or
#'   \code{discriminations}), \code{norming}, \code{thresholds},
#'   \code{communalities}, \code{inverse}, \code{method}.
#' @references Reckase, M. D. (2009). Multidimensional Item Response
#'   Theory. Springer.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_mfird(V)
morie_mfird <- function(a, d = NULL, P = NULL, inverse = FALSE) {
  A <- as.matrix(a)
  n <- nrow(A)
  m <- ncol(A)
  Pm <- if (is.null(P)) diag(m) else as.matrix(P)
  quad <- rowSums((A %*% Pm) * A)
  if (inverse) {
    if (any(quad >= 1)) stop("loadings imply communality >= 1")
    s <- sqrt(1 - quad)
    out <- A / s
    comms <- quad
  } else {
    s <- sqrt(1 + quad)
    out <- A / s
    comms <- rowSums((out %*% Pm) * out)
  }
  thresholds <- NULL
  if (!is.null(d) && !inverse) {
    dv <- as.numeric(d)
    if (length(dv) != n) stop("need one intercept per item")
    thresholds <- -dv / s
  }
  res <- list(norming = s, thresholds = thresholds,
              communalities = comms, inverse = inverse,
              method = "MIRT a <-> lambda (Reckase Eq. 2.28 / Eqs. 6.11-6.12)")
  if (inverse) res$discriminations <- out else res$loadings <- out
  res
}
