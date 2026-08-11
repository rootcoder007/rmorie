# CMIP multi-model ensemble weighting (Knutti et al. 2017).
# Source: Knutti, Sedlacek, Sanderson, Lorenz, Fischer & Eyring
# (2017), GRL 44, 1909-1918, Eq. 1
# (fetched-wave3/knutti-2017-model-weighting-grl44.pdf):
#   w_i = exp(-D_i^2/sigma_d^2) / (1 + sum_{j!=i} exp(-S_ij^2/sigma_s^2)),
# normalized; D_i and S_ij are RMS distances.  Mirrors Python
# morie.fn.caCMIP exactly.

.cacmip_rms <- function(a, b) sqrt(mean((a - b)^2))

#' Performance-and-independence weighted CMIP ensemble mean
#'
#' Implements the model-projection weighting scheme of Knutti et al.
#' (2017), Eq. 1: models are weighted down for distance to
#' observations (performance, radius \code{sigma_d}) and for
#' similarity to other models (independence, radius \code{sigma_s});
#' the weighted mean of the per-model projections is returned.  Two
#' identical models each receive half weight, so duplicating a model
#' leaves the estimate unchanged when the other models are dissimilar.
#'
#' @param models List of numeric vectors (per-model historical fields,
#'   equal lengths).
#' @param obs Numeric vector, observed field of the same length.
#' @param sigma_d,sigma_s Positive performance and similarity radii.
#' @param projections Optional numeric vector, one projection per
#'   model; defaults to each model's own field mean.
#' @return A list with elements \code{estimate}, \code{weights},
#'   \code{unweighted_mean}, \code{d}, \code{n_models},
#'   \code{effective_n}, \code{method}.
#' @references Knutti, R., Sedlacek, J., Sanderson, B. M., Lorenz, R.,
#'   Fischer, E. M. and Eyring, V. (2017). A climate model projection
#'   weighting scheme accounting for performance and interdependence.
#'   Geophysical Research Letters, 44, 1909-1918.  Sanderson, B. M.,
#'   Knutti, R. and Caldwell, P. (2015). A representative democracy to
#'   reduce interdependency in a multimodel ensemble. Journal of
#'   Climate, 28, 5171-5194.
#' @export
morie_cacmip <- function(models, obs, sigma_d, sigma_s,
                         projections = NULL) {
  mods <- lapply(models, as.numeric)
  ob <- as.numeric(obs)
  m_count <- length(mods)
  if (m_count < 1) stop("need at least one model")
  if (any(vapply(mods, length, 1L) != length(ob))) {
    stop("all models must match obs length")
  }
  sd_ <- as.numeric(sigma_d)
  ss_ <- as.numeric(sigma_s)
  if (sd_ <= 0 || ss_ <= 0) stop("sigma_d and sigma_s must be positive")
  if (is.null(projections)) {
    proj <- vapply(mods, mean, numeric(1))
  } else {
    proj <- as.numeric(projections)
    if (length(proj) != m_count) {
      stop("projections must have one value per model")
    }
  }
  d <- vapply(mods, function(mm) .cacmip_rms(mm, ob), numeric(1))
  s <- matrix(0, m_count, m_count)
  if (m_count > 1) {
    for (i in seq_len(m_count - 1)) {
      for (j in (i + 1):m_count) {
        s[i, j] <- s[j, i] <- .cacmip_rms(mods[[i]], mods[[j]])
      }
    }
  }
  w <- numeric(m_count)
  for (i in seq_len(m_count)) {
    num <- exp(-d[i]^2 / sd_^2)
    den <- 1 + sum(exp(-s[i, -i]^2 / ss_^2))
    w[i] <- num / den
  }
  tot <- sum(w)
  if (tot <= 0) stop("all weights vanished; increase sigma_d")
  w <- w / tot
  list(estimate = sum(w * proj),
       weights = w,
       unweighted_mean = mean(proj),
       d = d,
       n_models = m_count,
       effective_n = 1 / sum(w^2),
       method = "Knutti et al. (2017) Eq. 1 weighting")
}
