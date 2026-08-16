# R arm of droPDSI -- Palmer Drought Severity Index from a two-layer water
# balance. Palmer, W. C. (1965) Meteorological Drought, Research Paper No. 45,
# U.S. Weather Bureau. Mirrors src/morie/fn/droPDSI.py.

.droPDSI_EPS <- 1e-12

#' morie_droPDSI_palmer_pdsi
#'
#' A step of the droPDSI_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param precip Coerced to numeric by the body, with \code{as.numeric}.
#' @param pet Coerced to numeric by the body, with \code{as.numeric}.
#' @param awc Numeric; passed to \code{min}. Defaults to \code{100}.
#' @param month Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{estimate}, \code{pdsi}, \code{z_index}, \code{departure}, \code{cafec_precip}, \code{alpha}, \code{beta}, \code{gamma}, \code{delta}, \code{K}, \code{K_month}, \code{mean_abs_departure}, \code{evapotranspiration}, \code{recharge}, \code{runoff}, \code{loss}, \code{soil_surface_capacity}, \code{soil_under_capacity}, \code{n}, \code{duration_factor}, \code{duration_divisor}, \code{method}, \code{note}.
#' @export
morie_droPDSI_palmer_pdsi <- function(precip, pet, awc = 100.0,
                                      month = NULL) {
  P <- as.numeric(precip); PE <- as.numeric(pet)
  n <- length(P)
  if (n == 0L) stop("droPDSI: an empty series has no water balance")
  if (length(PE) != n)
    stop(sprintf("droPDSI: %d precipitation but %d PET values", n, length(PE)))
  awc <- as.numeric(awc)
  if (awc <= 0.0)
    stop("droPDSI: the available water capacity must be positive")
  su_cap <- min(25.4, awc); sl_cap <- awc - su_cap

  Ss <- su_cap; Su <- sl_cap
  ET <- numeric(n); R <- numeric(n); RO <- numeric(n); L <- numeric(n)
  PR <- numeric(n); PRO <- numeric(n); PL <- numeric(n)
  for (i in seq_len(n)) {
    PR[i] <- (su_cap - Ss) + (sl_cap - Su)
    PRO[i] <- Ss + Su
    pls <- min(PE[i], Ss)
    plu <- min(if (awc > .droPDSI_EPS) (PE[i] - pls) * Su / awc else 0.0, Su)
    PL[i] <- pls + plu

    if (P[i] >= PE[i]) {
      et <- PE[i]
      excess <- P[i] - PE[i]
      rec_s <- min(su_cap - Ss, excess); Ss <- Ss + rec_s; excess <- excess - rec_s
      rec_u <- min(sl_cap - Su, excess); Su <- Su + rec_u; excess <- excess - rec_u
      ro <- excess; loss <- 0.0
      R[i] <- rec_s + rec_u
    } else {
      need <- PE[i] - P[i]
      loss_s <- min(Ss, need); Ss <- Ss - loss_s; need <- need - loss_s
      loss_u <- min(Su, if (awc > .droPDSI_EPS) need * Su / awc else 0.0)
      Su <- Su - loss_u
      et <- P[i] + loss_s + loss_u
      ro <- 0.0; loss <- loss_s + loss_u
      R[i] <- 0.0
    }
    ET[i] <- et; RO[i] <- ro; L[i] <- loss
  }

  ratio <- function(num, den) {
    sd <- sum(den)
    if (sd > .droPDSI_EPS) sum(num) / sd else 0.0
  }
  alpha <- ratio(ET, PE); beta <- ratio(R, PR)
  gamma <- ratio(RO, PRO); delta <- ratio(L, PL)

  Phat <- alpha * PE + beta * PR + gamma * PRO - delta * PL
  d <- P - Phat

  # Palmer's climatic characteristic is computed PER CALENDAR MONTH and then
  # rescaled across months; a single record-wide K collapses to zero on
  # ordinary seasonal data and takes the whole index with it.
  mon <- if (is.null(month)) (seq_len(n) - 1L) %% 12L else
    as.integer(month) %% 12L
  if (length(mon) != n)
    stop(sprintf("droPDSI: %d observations but %d month labels", n,
                 length(mon)))
  Kp_month <- numeric(12); D_month <- numeric(12)
  for (j in 0:11) {
    idx <- which(mon == j)
    if (length(idx) == 0L) next
    cnt <- length(idx)
    Dj <- sum(abs(d[idx])) / cnt
    mPE <- sum(PE[idx]) / cnt; mR <- sum(R[idx]) / cnt
    mRO <- sum(RO[idx]) / cnt; mP <- sum(P[idx]) / cnt
    mL <- sum(L[idx]) / cnt
    ratio_j <- (mPE + mR + mRO) / (mP + mL + .droPDSI_EPS) + 2.8
    arg <- ratio_j / (Dj + .droPDSI_EPS)
    Kp_month[j + 1L] <- 1.5 * log10(if (arg > .droPDSI_EPS) arg else
                                    .droPDSI_EPS) + 0.5
    D_month[j + 1L] <- Dj
  }
  denom <- sum(D_month * Kp_month)
  if (abs(denom) > .droPDSI_EPS) Kp_month <- 17.67 * Kp_month / denom
  Kp <- sum(Kp_month) / 12.0
  Z <- vapply(seq_len(n), function(i) Kp_month[mon[i] + 1L] * d[i], numeric(1))

  X <- numeric(n); prev <- 0.0
  for (i in seq_len(n)) { cur <- 0.897 * prev + Z[i] / 3.0; X[i] <- cur; prev <- cur }

  list(estimate = X, pdsi = X, z_index = Z, departure = d,
       cafec_precip = Phat,
       alpha = alpha, beta = beta, gamma = gamma, delta = delta,
       K = Kp, K_month = Kp_month,
       mean_abs_departure = D_month, evapotranspiration = ET, recharge = R, runoff = RO,
       loss = L, soil_surface_capacity = su_cap,
       soil_under_capacity = sl_cap, n = as.integer(n),
       duration_factor = 0.897, duration_divisor = 3.0,
       method = paste0("Palmer Drought Severity Index from a two-layer ",
                       "water balance (Palmer 1965, Research Paper 45)"),
       note = paste0("the 0.897 and the /3 are Palmer's fitted duration ",
                     "factors, chosen so the index is comparable BETWEEN ",
                     "climates -- a locally re-tuned version is no longer ",
                     "PDSI"))
}

#' .droPDSI_cheatsheet
#'
#' A step of the droPDSI_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
.droPDSI_cheatsheet <- function() {
  paste0("droPDSI: morie_droPDSI_palmer_pdsi(precip, pet, awc) -> PDSI, Z ",
         "index and the CAFEC water balance (Palmer 1965)")
}

morie_droPDSI <- morie_droPDSI_palmer_pdsi
