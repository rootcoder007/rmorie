# Manski worst-case (no-assumption) bounds.
# Source: Manski, C. F. (2007), Identification for Prediction and
# Decision, Harvard University Press: the missing-outcome bound of
# his eq. (2.4) and Sec. 2.1 (whose width is exactly the missing-data
# probability), and the average-treatment-effect bound of his eqs.
# (7.5)-(7.7) in Sec. 7.1.  Originally Manski, C. F. (1990),
# Nonparametric bounds on treatment effects, American Economic Review
# Papers and Proceedings 80(2), 319-323.
#
# Two facts from eq. (7.7) that this implementation reproduces and
# that the tests anchor on: with a BINARY treatment the two treatment
# fractions sum to one, so the ATE interval has width exactly
# y1 - y0, and it necessarily contains zero.  As Manski puts it, "the
# data alone confine the average treatment effect to exactly half its
# logically possible range" -- a no-assumption bound can never sign an
# effect by itself.
#
# Native implementation mirroring Python morie.fn.bndest exactly,
# including the support check on observed outcomes: the support is
# the single assumption being made, so violating it voids the bounds.

#' .mor_bnd_one_mean
#'
#' Part of the bndest_native implementation; see the file header for the
#' source it follows.
#'
#' @param yv See Usage.
#' @param seen See Usage.
#' @param k0 See Usage.
#' @param k1 See Usage.
#' @return A vector, from \code{c}.
#' @export
.mor_bnd_one_mean <- function(yv, seen, k0, k1) {
  p <- mean(seen)
  if (p > 0) {
    ys <- yv[seen]
    if (any(ys < k0 - 1e-12) || any(ys > k1 + 1e-12))
      stop(paste("an observed outcome lies outside the declared support;",
                 "the support is the one assumption here, so violating it",
                 "voids the bounds."))
    m <- mean(ys)
  } else {
    m <- 0
  }
  c(m * p + k0 * (1 - p), m * p + k1 * (1 - p), p)
}

#' Manski worst-case bounds
#'
#' With \code{treatment} left \code{NULL}, bounds the mean of a
#' partially observed outcome (Manski 2007, eq. 2.4 applied to the
#' mean): the missing part is set to each end of the support in turn,
#' so the interval has width \eqn{(K_1 - K_0) P(\mathrm{missing})}.
#' With a binary \code{treatment}, bounds the average treatment
#' effect by his eq. (7.6), each potential outcome being unobserved
#' for those who received the other treatment.
#'
#' @param y Outcomes.
#' @param observed Logical vector of observed outcomes; ignored (and
#'   may be \code{NULL}) when \code{treatment} is supplied.
#' @param support Length-2 vector \code{c(K0, K1)} with
#'   \code{K0 < K1}, the assumed outcome support.
#' @param treatment Optional binary 0/1 treatment.
#' @return For the mean: a list with \code{lower}, \code{upper},
#'   \code{width}, \code{p_observed}, \code{identified},
#'   \code{width_identity}, \code{assumptions}, \code{n},
#'   \code{method}.  For the ATE: \code{ate_lower}, \code{ate_upper},
#'   \code{ate_width}, \code{y1_bounds}, \code{y0_bounds},
#'   \code{p_treated}, \code{contains_zero}, \code{width_identity},
#'   \code{identified}, \code{n}, \code{method}.
#' @references Manski, C. F. (2007). Identification for Prediction and
#'   Decision. Harvard University Press, Sections 2.1 and 7.1.
#' @export
morie_bndest <- function(y, observed, support, treatment = NULL) {
  yv <- as.numeric(y)
  k0 <- as.numeric(support[1]); k1 <- as.numeric(support[2])
  if (!(k0 < k1))
    stop(sprintf("the support must satisfy K0 < K1, got (%g, %g).", k0, k1))
  n <- length(yv)
  if (is.null(treatment)) {
    obs <- as.logical(observed)
    if (length(obs) != n)
      stop(sprintf("observed has %d entries for %d.", length(obs), n))
    r <- .mor_bnd_one_mean(yv, obs, k0, k1)
    return(list(lower = r[1], upper = r[2], width = r[2] - r[1],
                p_observed = r[3], identified = isTRUE(r[3] == 1),
                width_identity = "(K1 - K0)(1 - P(obs)) exactly",
                assumptions = paste("the outcome's support alone;",
                                    "nothing about WHY data are missing"),
                n = n,
                method = "Manski worst-case bounds on a partially observed mean"))
  }
  Tv <- as.numeric(treatment)
  if (length(Tv) != n)
    stop(sprintf("treatment has %d entries for %d.", length(Tv), n))
  if (!all(Tv %in% c(0, 1))) stop("treatment must be binary 0/1.")
  r1 <- .mor_bnd_one_mean(yv, Tv == 1, k0, k1)
  r0 <- .mor_bnd_one_mean(yv, Tv == 0, k0, k1)
  ate_lo <- r1[1] - r0[2]
  ate_hi <- r1[2] - r0[1]
  list(ate_lower = ate_lo, ate_upper = ate_hi,
       ate_width = ate_hi - ate_lo,
       y1_bounds = c(r1[1], r1[2]), y0_bounds = c(r0[1], r0[2]),
       p_treated = r1[3],
       contains_zero = isTRUE(ate_lo <= 0 && 0 <= ate_hi),
       width_identity = paste("the ATE bounds always have width exactly",
                              "K1 - K0, so they always contain zero:",
                              "no-assumption bounds never sign an effect",
                              "on their own"),
       identified = FALSE, n = n,
       method = "Manski (1990) worst-case bounds on the average treatment effect")
}
