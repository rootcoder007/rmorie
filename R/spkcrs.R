# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cross K-function for bivariate point patterns
#'
#' eq (3.9): \eqn{\hat{K}_{ij}(h) = [\hat\lambda_i \hat\lambda_j
#' \nu(A)]^{-1} \sum_k \sum_l w(s_k, u_l)^{-1} I(h_{kl} \le h)}, with
#' \eqn{w} Ripley's isotropic weight -- the proportion of the circumference
#' of a circle centred at \eqn{s_k} with radius \eqn{h_{kl}} inside the
#' window, computed exactly rather than by sampling.
#'
#' Because \eqn{\hat{K}_{12}} and \eqn{\hat{K}_{21}} differ even though the
#' population functions are symmetric, the pooled estimator of Lotwick and
#' Silverman (1982) is returned as `K_star`, with
#' \eqn{L^* = \sqrt{K^*/\pi}}. Under independence \eqn{K_{ij}(h) = \pi h^2}
#' regardless of either pattern, so `L_minus_h` is the diagnostic: positive
#' means attraction, negative repulsion. Under the random labelling
#' hypothesis the relationship is instead eq (3.10),
#' \eqn{K_{11} = K_{22} = K_{12}}, and the statistic is Diggle and
#' Chetwynd's \eqn{D(h) = K_{ii}(h) - K_{jj}(h)}. The book is emphatic that
#' the two nulls are different, so `hypothesis` must name one.
#'
#' @param points1,points2 Event coordinates, (n, 2).
#' @param lambda1,lambda2 Accepted for signature compatibility; intensities
#'   are estimated as \eqn{n/\nu(A)} per eq (3.8), and supplied values are
#'   reported alongside rather than substituted.
#' @param r Distances at which to evaluate; a default grid is built from the
#'   window when omitted.
#' @param region Window as (xmin, ymin, xmax, ymax); defaults to the
#'   bounding box of both patterns.
#' @param correction "ripley" or "none".
#' @param hypothesis "independence" or "random_labelling".
#' @return A list with `estimate` (= `K_star`), `K_12`, `K_21`, `L_star`,
#'   `L_minus_h`, `K_independence`, `r`, `lambda_1`, `lambda_2`, and for
#'   random labelling `D`, `K_11`, `K_22`.
#' @references Schabenberger Ch 3, Sec 3.4.4, eqs (3.9)-(3.10), pp. 103-105.
#'   Lotwick and Silverman (1982), JRSS B 44:406-413. Diggle (1983). Diggle
#'   and Chetwynd (1991), Biometrics 47:1155-1163.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' spkcrs(V, V)
spkcrs <- function(points1, points2, lambda1 = NULL, lambda2 = NULL,
                   r = NULL, region = NULL, correction = "ripley",
                   hypothesis = "independence") {
  p1 <- matrix(as.numeric(as.matrix(points1)), ncol = 2)
  p2 <- matrix(as.numeric(as.matrix(points2)), ncol = 2)
  if (nrow(p1) == 0L || nrow(p2) == 0L) {
    stop("both patterns must contain at least one event")
  }
  if (is.null(region)) {
    all_ <- rbind(p1, p2)
    region <- c(min(all_[, 1]), min(all_[, 2]), max(all_[, 1]), max(all_[, 2]))
  }
  if (is.null(r)) {
    r <- seq(0, 0.25 * min(region[3] - region[1], region[4] - region[2]),
             length.out = 11L)[-1L]
  }
  r <- as.numeric(r)
  res <- .schab_cross_k_combined(p1, p2, region, r, correction = correction)
  res$estimate <- res$K_star
  res$correction <- correction
  res$hypothesis <- hypothesis
  if (!is.null(lambda1)) res$lambda_1_supplied <- as.numeric(lambda1)
  if (!is.null(lambda2)) res$lambda_2_supplied <- as.numeric(lambda2)
  if (identical(hypothesis, "random_labelling")) {
    d <- .schab_dc_d(p1, p2, region, r)
    res$D <- d$D
    res$K_11 <- d$K_11
    res$K_22 <- d$K_22
  } else if (!identical(hypothesis, "independence")) {
    stop("`hypothesis` must be 'independence' or 'random_labelling'")
  }
  res
}

