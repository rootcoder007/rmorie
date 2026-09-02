# SPDX-License-Identifier: AGPL-3.0-or-later
#' Network attributable fraction with spillover
#'
#' Under interference an individual's outcome depends on the exposure
#' of the people they are connected to, so the ordinary attributable
#' fraction -- which assumes each unit answers only to its own exposure
#' -- understates what removing the exposure would do. Following the
#' direct/indirect decomposition of Halloran & Hudgens, the
#' neighbourhood exposure of unit i is the fraction of its neighbours
#' exposed, \code{nu_i = sum_j A_ij e_j / sum_j A_ij} (zero if
#' isolated), and the outcome is regressed additively on both channels,
#' \code{E\[Y_i\] = b0 + b1 e_i + b2 nu_i}.
#'
#' Formula: \code{PAF = (mean(Y) - b0) / mean(Y)}, which splits exactly
#' into \code{b1 mean(e) / mean(Y)} and \code{b2 mean(nu) / mean(Y)}.
#' With \code{b2 = 0} it collapses to the ordinary attributable
#' fraction.
#'
#' @param y Outcome per unit, length n.
#' @param exposure Own exposure per unit, typically 0/1.
#' @param network Non-negative n by n adjacency; the diagonal is
#'   ignored, so a unit is not its own neighbour.
#' @return List with \code{estimate}, \code{paf}, \code{paf_direct},
#'   \code{paf_spillover}, \code{b0}, \code{b1}, \code{b2},
#'   \code{mean_y}, \code{mean_exposure}, \code{mean_nu}, \code{n}.
#' @references Halloran, M. E. & Hudgens, M. G. (2016). Dependent
#'   happenings: a recent methodological review. Current Epidemiology
#'   Reports, 3(4), 297-305. doi:10.1007/s40471-016-0086-4
#' @export
Netparf <- function(y, exposure, network) {
  yv <- as.numeric(y); ev <- as.numeric(exposure)
  n <- length(yv)
  if (n == 0L) stop("Netparf: y is empty")
  if (length(ev) != n) stop("Netparf: y and exposure have different lengths")
  A <- as.matrix(network)
  if (nrow(A) != n || ncol(A) != n) stop("Netparf: network must be n by n")
  if (any(A < 0)) stop("Netparf: network weights must be non-negative")
  diag(A) <- 0
  den <- rowSums(A)
  num <- as.numeric(A %*% ev)
  nu <- ifelse(den > 0, num / den, 0)
  Xd <- cbind(1, ev, nu)
  beta <- .t1_lstsq(Xd, yv)$beta
  b0 <- beta[1]; b1 <- beta[2]; b2 <- beta[3]
  my <- mean(yv)
  if (my == 0) stop("Netparf: mean outcome is zero, PAF undefined")
  me <- mean(ev); mn <- mean(nu)
  pd <- b1 * me / my
  ps <- b2 * mn / my
  .t1_result(estimate = pd + ps, paf = pd + ps, paf_direct = pd,
             paf_spillover = ps, b0 = b0, b1 = b1, b2 = b2,
             mean_y = my, mean_exposure = me, mean_nu = mn, n = n,
             method = "Network attributable fraction with spillover")
}
