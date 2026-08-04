# SPDX-License-Identifier: AGPL-3.0-or-later
#' Top-of-atmosphere radiation balance
#'
#' N = (1 - alpha) S / 4 - OLR, with the planetary equilibrium temperature
#' T_e = ((1 - alpha) S / (4 sigma))^(1/4).  Source consulted: Hartmann (1994),
#' Global Physical Climatology, Academic Press, chapter 2.
#'
#' @param S total solar irradiance in W/m^2.
#' @param alpha planetary albedo.
#' @param OLR outgoing longwave radiation in W/m^2, optional.
#' @return list: estimate, net, absorbed, olr, teq, n, method.
#' @keywords internal
#' @examples
#' recapTOA(1361, 0.30, 238.175)
#' @export
recapTOA <- function(S = 1361, alpha = 0.3, OLR = NULL) {
  sigma_sb <- 5.670374419e-8
  Sv <- as.numeric(S); av <- as.numeric(alpha)
  n <- max(length(Sv), length(av))
  absorbed <- numeric(n)
  for (i in seq_len(n)) {
    s <- Sv[((i - 1) %% length(Sv)) + 1]
    a <- av[((i - 1) %% length(av)) + 1]
    absorbed[i] <- (1 - a) * s / 4
  }
  ov <- if (is.null(OLR)) absorbed else {
    o <- as.numeric(OLR)
    o[((seq_len(n) - 1) %% length(o)) + 1]
  }
  net <- absorbed - ov
  teq <- (absorbed / sigma_sb)^0.25
  list(estimate = mean(net), net = net, absorbed = mean(absorbed),
       olr = mean(ov), teq = mean(teq), n = as.integer(n),
       method = "Top-of-atmosphere radiation balance (Hartmann 1994)")
}

# CANONICAL TEST
# r <- recapTOA(1361, 0.30, 238.175)
# stopifnot(abs(r$absorbed - 238.175) < 1e-9, abs(r$estimate) < 1e-9,
#           abs(r$teq - 254.6) < 0.2)

#' @rdname recapTOA
#' @keywords internal
#' @export
morie_toa_radiation_balance <- recapTOA
