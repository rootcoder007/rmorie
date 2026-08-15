# R arm of droPDSI -- Palmer Drought Severity Index from a two-layer water
# balance. Palmer, W. C. (1965) Meteorological Drought, Research Paper No. 45,
# U.S. Weather Bureau. Mirrors src/morie/fn/droPDSI.py.

.droPDSI_EPS <- 1e-12

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

  md <- sum(abs(d)) / n
  mP <- sum(P) / n; mPE <- sum(PE) / n
  mL <- sum(L) / n; mR <- sum(R) / n; mRO <- sum(RO) / n
  num <- (mPE + mR + mRO) / (mP + mL + .droPDSI_EPS) + 2.8
  Kp <- 1.5 * log10(if (num > 0) num / (md + .droPDSI_EPS) else 1.0) + 0.5
  Kp <- max(Kp, 0.0)
  Z <- Kp * d

  X <- numeric(n); prev <- 0.0
  for (i in seq_len(n)) { cur <- 0.897 * prev + Z[i] / 3.0; X[i] <- cur; prev <- cur }

  list(estimate = X, pdsi = X, z_index = Z, departure = d,
       cafec_precip = Phat,
       alpha = alpha, beta = beta, gamma = gamma, delta = delta,
       K = Kp, evapotranspiration = ET, recharge = R, runoff = RO,
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

.droPDSI_cheatsheet <- function() {
  paste0("droPDSI: morie_droPDSI_palmer_pdsi(precip, pet, awc) -> PDSI, Z ",
         "index and the CAFEC water balance (Palmer 1965)")
}

morie_droPDSI <- morie_droPDSI_palmer_pdsi
