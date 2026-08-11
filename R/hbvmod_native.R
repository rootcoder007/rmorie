# HBV conceptual rainfall-runoff model (Bergstrom; Seibert & Vis 2012).
# Source: Seibert & Vis (2012), HESS 16, 3315-3325, Eqs. 1-6
# (fetched-wave3/seibert-vis-2012-hbv-light-hess16-3315.pdf);
# Bergstrom (1995) in Singh (ed.), Computer Models of Watershed
# Hydrology (original description).  Mirrors Python morie.fn.hbvMod
# exactly (incl. 10% snowpack liquid-water holding and overflow
# handling).  Water is conserved to machine precision.

.hbv_maxbas_weights <- function(maxbas) {
  m <- as.numeric(maxbas)
  nw <- as.integer(ceiling(m))
  antider <- function(u) {
    if (u <= m / 2) return(2 * u * u / (m * m))
    4 * u / m - 2 * u * u / (m * m) - 1
  }
  w <- numeric(nw)
  for (i in seq_len(nw)) {
    lo <- i - 1
    hi <- min(i, m)
    w[i] <- antider(hi) - antider(lo)
  }
  w
}

#' HBV conceptual rainfall-runoff model (daily)
#'
#' Standard HBV structure per Seibert and Vis (2012), Eqs. 1-6:
#' degree-day snow routine with refreezing, soil-moisture accounting
#' with recharge fraction \eqn{(S/FC)^{BETA}}, actual evaporation
#' \eqn{E_{act} = E_{pot} \min(S/(FC \cdot LP), 1)}, two linear
#' groundwater boxes with threshold outflow and maximum percolation,
#' and triangular MAXBAS routing.  Mass balance closes to machine
#' precision.
#'
#' @param precip,temp,epot Numeric daily vectors: precipitation (mm),
#'   mean temperature (deg C), potential evaporation (mm).
#' @param params Named list: tt, cfmax, cfr (default 0.05), fc, lp,
#'   beta, k0, k1, k2, uzl, perc, maxbas.
#' @param init Optional named list of starting states: snow, swater,
#'   soil, suz, slz (mm, default 0).
#' @return List with daily vectors \code{q}, \code{q_gw},
#'   \code{snow}, \code{soil}, \code{suz}, \code{slz}, \code{e_act},
#'   plus \code{mass_balance_error}, \code{n_days},
#'   \code{params_used}, \code{method}.
#' @references Seibert, J. and Vis, M. J. P. (2012). Teaching
#'   hydrological modeling with a user-friendly catchment-runoff-model
#'   software package. Hydrology and Earth System Sciences, 16,
#'   3315-3325.  Bergstrom, S. (1995). The HBV model. In Singh (ed.),
#'   Computer Models of Watershed Hydrology, 443-476.
#' @export
morie_hbvmod <- function(precip, temp, epot, params, init = list()) {
  p <- as.numeric(precip); t <- as.numeric(temp); ep <- as.numeric(epot)
  n <- length(p)
  if (length(t) != n || length(ep) != n) {
    stop("precip, temp, epot must have equal length")
  }
  req <- c("tt", "cfmax", "fc", "lp", "beta", "k0", "k1", "k2",
           "uzl", "perc", "maxbas")
  miss <- setdiff(req, names(params))
  if (length(miss)) stop("params missing: ", paste(miss, collapse = ", "))
  tt <- as.numeric(params$tt); cfmax <- as.numeric(params$cfmax)
  cfr <- as.numeric(if (is.null(params$cfr)) 0.05 else params$cfr)
  fc <- as.numeric(params$fc); lp <- as.numeric(params$lp)
  beta <- as.numeric(params$beta)
  k0 <- as.numeric(params$k0); k1 <- as.numeric(params$k1)
  k2 <- as.numeric(params$k2)
  uzl <- as.numeric(params$uzl); perc <- as.numeric(params$perc)
  maxbas <- as.numeric(params$maxbas)
  if (fc <= 0 || maxbas < 1) stop("fc must be positive and maxbas >= 1")
  gs <- function(k) as.numeric(if (is.null(init[[k]])) 0 else init[[k]])
  snow <- gs("snow"); swater <- gs("swater"); soil <- gs("soil")
  suz <- gs("suz"); slz <- gs("slz")
  s0 <- snow + swater + soil + suz + slz

  w <- .hbv_maxbas_weights(maxbas)
  queue <- numeric(length(w))
  out <- list(q = numeric(n), q_gw = numeric(n), snow = numeric(n),
              soil = numeric(n), suz = numeric(n), slz = numeric(n),
              e_act = numeric(n))
  for (i in seq_len(n)) {
    if (t[i] <= tt) {
      snow <- snow + p[i]
      rain <- 0
    } else {
      rain <- p[i]
    }
    melt <- if (t[i] > tt) min(cfmax * (t[i] - tt), snow) else 0
    snow <- snow - melt
    swater <- swater + melt
    refreeze <- if (t[i] < tt) min(cfr * cfmax * (tt - t[i]), swater) else 0
    swater <- swater - refreeze
    snow <- snow + refreeze
    hold <- 0.1 * snow
    insoil <- rain + max(swater - hold, 0)
    swater <- min(swater, hold)
    recharge <- insoil * (soil / fc)^beta
    soil <- soil + insoil - recharge
    if (soil > fc) {
      recharge <- recharge + soil - fc
      soil <- fc
    }
    eact <- ep[i] * min(soil / (fc * lp), 1)
    eact <- min(eact, soil)
    soil <- soil - eact
    suz <- suz + recharge
    pc <- min(perc, suz)
    suz <- suz - pc
    slz <- slz + pc
    q0 <- k0 * max(suz - uzl, 0)
    q1 <- k1 * suz
    q2 <- k2 * slz
    qgw <- q0 + q1 + q2
    suz <- suz - q0 - q1
    slz <- slz - q2
    queue <- queue + w * qgw
    q <- queue[1]
    queue <- c(queue[-1], 0)
    out$q[i] <- q; out$q_gw[i] <- qgw
    out$snow[i] <- snow + swater; out$soil[i] <- soil
    out$suz[i] <- suz; out$slz[i] <- slz
    out$e_act[i] <- eact
  }
  s1 <- snow + swater + soil + suz + slz
  mbe <- sum(p) - sum(out$e_act) - sum(out$q) - (s1 - s0) - sum(queue)
  out$mass_balance_error <- mbe
  out$n_days <- n
  out$params_used <- list(tt = tt, cfmax = cfmax, cfr = cfr, fc = fc,
                          lp = lp, beta = beta, k0 = k0, k1 = k1,
                          k2 = k2, uzl = uzl, perc = perc,
                          maxbas = maxbas)
  out$method <- "HBV (Seibert & Vis 2012, Eqs. 1-6)"
  out
}
