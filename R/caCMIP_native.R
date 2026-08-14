# CMIP multi-model ensemble weighting (Knutti et al. 2017).
# Sources: Knutti, R., Sedlacek, J., Sanderson, B. M., Lorenz, R.,
# Fischer, E. M. & Eyring, V. (2017). A climate model projection
# weighting scheme accounting for performance and interdependence.
# *Geophysical Research Letters*, 44, 1909-1918, Eq. 1 and
# surrounding text. Sanderson, B. M., Knutti, R. & Caldwell, P.
# (2015). A representative democracy to reduce interdependency in a
# multimodel ensemble. *Journal of Climate*, 28, 5171-5194 (the
# scheme's basis, Eqs. 10-16, as cited by Knutti et al.).

.ca_rms <- function(a, b) {
  n <- length(a)
  sqrt(sum((as.numeric(a) - as.numeric(b))^2) / n)
}

caCMIP <- function(models, obs, sigma_d, sigma_s, projections = NULL) {
  mods <- lapply(models, function(m) as.numeric(m))
  ob <- as.numeric(obs)
  m_count <- length(mods)
  if (m_count < 1L)
    stop("need at least one model")
  lens <- vapply(mods, length, integer(1))
  if (any(lens != length(ob)))
    stop("all models must match obs length")
  sd <- as.numeric(sigma_d)
  ss <- as.numeric(sigma_s)
  if (sd <= 0 || ss <= 0)
    stop("sigma_d and sigma_s must be positive")
  if (is.null(projections)) {
    proj <- vapply(mods, function(mm) sum(mm) / length(mm), numeric(1))
  } else {
    proj <- as.numeric(projections)
    if (length(proj) != m_count)
      stop("projections must have one value per model")
  }
  d <- vapply(mods, function(mm) .ca_rms(mm, ob), numeric(1))
  s <- matrix(0, m_count, m_count)
  for (i in 1:(m_count - 1L)) {
    for (j in (i + 1L):m_count) {
      v <- .ca_rms(mods[[i]], mods[[j]])
      s[i, j] <- v
      s[j, i] <- v
    }
  }
  w <- numeric(m_count)
  for (i in seq_len(m_count)) {
    num <- exp(-d[i]^2 / sd^2)
    den <- 1 + sum(exp(-s[i, -i]^2 / ss^2))
    w[i] <- num / den
  }
  tot <- sum(w)
  if (tot <= 0)
    stop("all weights vanished; increase sigma_d")
  w <- w / tot
  est <- sum(w * proj)
  list(estimate = est,
       weights = w,
       unweighted_mean = sum(proj) / m_count,
       d = d,
       n_models = m_count,
       effective_n = 1 / sum(w * w),
       method = "Knutti et al. (2017) Eq. 1 weighting")
}

cmip_ensemble <- function(models, weights) {
  mods <- as.numeric(models)
  ws <- as.numeric(weights)
  if (length(mods) != length(ws))
    stop("models and weights must have equal length")
  tot <- sum(ws)
  if (tot <= 0)
    stop("weights must sum to a positive value")
  est <- sum(ws * mods) / tot
  list(estimate = est, n = length(mods),
       method = "weighted ensemble mean")
}

cmipensemble <- cmip_ensemble

morie_caCMIP <- caCMIP

caCMIP_cheatsheet <- function() {
  "caCMIP: Knutti 2017 performance+independence CMIP weighting"
}
