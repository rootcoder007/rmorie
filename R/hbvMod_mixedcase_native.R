# HBV conceptual rainfall-runoff model (Bergstrom; Seibert & Vis 2012).
#
# Standard HBV structure as formalized in Seibert & Vis (2012),
# Eqs. 1-6: degree-day snow routine with refreezing (Eqs. 1-2),
# soil-moisture accounting with recharge fraction
# (S_soil/FC)^BETA (Eq. 3) and actual evaporation
# E_act = E_pot * min(S_soil/(FC*LP), 1) (Eq. 4), two linear
# groundwater boxes with threshold outflow
# Q_GW = K2*SLZ + K1*SUZ + K0*max(SUZ-UZL, 0) (Eq. 5) and maximum
# percolation PERC from the upper to the lower box, and a
# triangular routing filter of base MAXBAS (Eq. 6). Water is
# conserved to machine precision:
# sum(P) = sum(E_act) + sum(Q) + (final - initial storage)
# + water still in the routing queue.
#
# Sources
# -------
# Seibert, J. & Vis, M. J. P. (2012). Teaching hydrological
# modeling with a user-friendly catchment-runoff-model software
# package. Hydrology and Earth System Sciences, 16, 3315-3325,
# Eqs. 1-6.
# Bergstrom, S. (1995). The HBV model. In V. P. Singh (ed.),
# Computer Models of Watershed Hydrology, Water Resources
# Publications, 443-476.

#' Eq. 6 of Seibert & Vis (2012): triangular routing weights
#'
#' c(i) = int_\{i-1\}^\{i\} of (2/M - |u - M/2| * 4/M^2) du
#'
#' @param maxbas Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{w}, as built in the body.
#' @export
.hbvMod_maxbas_weights <- function(maxbas) {
  # Eq. 6 of Seibert & Vis (2012): triangular routing weights
  # c(i) = int_{i-1}^{i} of (2/M - |u - M/2| * 4/M^2) du
  m <- as.numeric(maxbas)
  nw <- as.integer(ceiling(m))
  antider <- function(u) {
    # antiderivative with F(0)=0, F(m)=1
    if (u <= m / 2.0) {
      return(2.0 * u * u / (m * m))
    }
    return(4.0 * u / m - 2.0 * u * u / (m * m) - 1.0)
  }
  w <- numeric(nw)
  for (i in seq_len(nw)) {
    lo <- as.numeric(i) - 1.0
    hi <- min(as.numeric(i), m)
    w[i] <- antider(hi) - antider(lo)
  }
  w
}

#' morie_hbvMod
#'
#' Part of the hbvMod_mixedcase_native implementation; see the file
#' header for the source it follows.
#'
#' @param precip Coerced to numeric by the body, with \code{as.numeric}.
#' @param temp Coerced to numeric by the body, with \code{as.numeric}.
#' @param epot Coerced to numeric by the body, with \code{as.numeric}.
#' @param params A list; the body reads \code{$beta}, \code{$cfmax}, \code{$cfr}, \code{$fc}, \code{$k0}, \code{$k1}, \code{$k2}, \code{$lp}, \code{$maxbas}, \code{$perc}, \code{$tt}, \code{$uzl} from it.
#' @param init Optional; may be \code{NULL}. A list; the body reads \code{$slz}, \code{$snow}, \code{$soil}, \code{$suz}, \code{$swater} from it.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_hbvMod <- function(precip, temp, epot, params, init = NULL) {
  p <- as.numeric(precip)
  t <- as.numeric(temp)
  ep <- as.numeric(epot)
  n <- length(p)
  if (!(length(t) == n && length(ep) == n)) {
    stop("precip, temp, epot must have equal length")
  }
  req <- c("tt", "cfmax", "fc", "lp", "beta", "k0", "k1", "k2",
           "uzl", "perc", "maxbas")
  miss <- setdiff(req, names(params))
  if (length(miss) > 0) {
    stop("params missing: ", paste(miss, collapse = ", "))
  }
  tt <- as.numeric(params[["tt"]])
  cfmax <- as.numeric(params[["cfmax"]])
  cfr <- if (!is.null(params[["cfr"]])) as.numeric(params[["cfr"]]) else 0.05
  fc <- as.numeric(params[["fc"]])
  lp <- as.numeric(params[["lp"]])
  beta <- as.numeric(params[["beta"]])
  k0 <- as.numeric(params[["k0"]])
  k1 <- as.numeric(params[["k1"]])
  k2 <- as.numeric(params[["k2"]])
  uzl <- as.numeric(params[["uzl"]])
  perc <- as.numeric(params[["perc"]])
  maxbas <- as.numeric(params[["maxbas"]])
  if (fc <= 0 || maxbas < 1) {
    stop("fc must be positive and maxbas >= 1")
  }
  if (is.null(init)) init <- list()
  snow   <- if (!is.null(init[["snow"]]))   as.numeric(init[["snow"]])   else 0.0
  swater <- if (!is.null(init[["swater"]])) as.numeric(init[["swater"]]) else 0.0
  soil   <- if (!is.null(init[["soil"]]))   as.numeric(init[["soil"]])   else 0.0
  suz    <- if (!is.null(init[["suz"]]))    as.numeric(init[["suz"]])    else 0.0
  slz    <- if (!is.null(init[["slz"]]))    as.numeric(init[["slz"]])    else 0.0
  s0 <- snow + swater + soil + suz + slz

  w <- .hbvMod_maxbas_weights(maxbas)
  queue <- rep(0.0, length(w))
  out <- list(
    q     = numeric(n),
    q_gw  = numeric(n),
    snow  = numeric(n),
    soil  = numeric(n),
    suz   = numeric(n),
    slz   = numeric(n),
    e_act = numeric(n)
  )

  for (i in seq_len(n)) {
    # snow routine (Eqs. 1-2); precipitation phase by TT
    if (t[i] <= tt) {
      snow <- snow + p[i]
      rain <- 0.0
    } else {
      rain <- p[i]
    }
    if (t[i] > tt) {
      melt <- min(cfmax * (t[i] - tt), snow)
    } else {
      melt <- 0.0
    }
    snow <- snow - melt
    swater <- swater + melt
    if (t[i] < tt) {
      refreeze <- min(cfr * cfmax * (tt - t[i]), swater)
    } else {
      refreeze <- 0.0
    }
    swater <- swater - refreeze
    snow <- snow + refreeze
    # liquid water above 10% of snowpack becomes soil input
    hold <- 0.1 * snow
    insoil <- rain + max(swater - hold, 0.0)
    swater <- min(swater, hold)
    # soil routine (Eqs. 3-4)
    recharge <- insoil * (soil / fc) ^ beta
    soil <- soil + insoil - recharge
    if (soil > fc) {                       # overflow to recharge
      recharge <- recharge + (soil - fc)
      soil <- fc
    }
    eact <- ep[i] * min(soil / (fc * lp), 1.0)
    eact <- min(eact, soil)
    soil <- soil - eact
    # groundwater boxes (Eq. 5) with max percolation PERC
    suz <- suz + recharge
    pc <- min(perc, suz)
    suz <- suz - pc
    slz <- slz + pc
    q0 <- k0 * max(suz - uzl, 0.0)
    q1 <- k1 * suz
    q2 <- k2 * slz
    qgw <- q0 + q1 + q2
    suz <- suz - q0 - q1
    slz <- slz - q2
    # triangular routing (Eq. 6)
    for (j in seq_along(w)) {
      queue[j] <- queue[j] + w[j] * qgw
    }
    q <- queue[1]
    queue <- c(queue[-1], 0.0)
    out$q[i]     <- q
    out$q_gw[i]  <- qgw
    out$snow[i]  <- snow + swater
    out$soil[i]  <- soil
    out$suz[i]   <- suz
    out$slz[i]   <- slz
    out$e_act[i] <- eact
  }

  s1 <- snow + swater + soil + suz + slz
  mbe <- sum(p) - sum(out$e_act) - sum(out$q) - (s1 - s0) - sum(queue)
  out$mass_balance_error <- mbe
  out$n_days <- n
  out$params_used <- list(tt = tt, cfmax = cfmax, cfr = cfr, fc = fc,
                          lp = lp, beta = beta, k0 = k0, k1 = k1, k2 = k2,
                          uzl = uzl, perc = perc, maxbas = maxbas)
  out$method <- "HBV (Seibert & Vis 2012, Eqs. 1-6)"
  out
}

# long descriptive alias (stub-era name)
hbv_hydrology <- morie_hbvMod

#' .hbvMod_cheatsheet
#'
#' Part of the hbvMod_mixedcase_native implementation; see the file
#' header for the source it follows.
#'
#' @return A character value.
#' @export
.hbvMod_cheatsheet <- function() {
  "hbvMod: HBV rainfall-runoff (snow/soil/2 GW boxes/MAXBAS routing)"
}

# public names resolved by fn/_lazy_map.json
hbvhydrology <- morie_hbvMod

# The 2026-08-11 arm of this module was a second
# implementation of the same paper; it has been removed and
# its exported name kept as an alias. The formals were
# identical, so this is exact and the man page still applies.
morie_hbvmod <- morie_hbvMod
