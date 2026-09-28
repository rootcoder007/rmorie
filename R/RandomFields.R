.rf_unit <- function(model) {
  nested <- is.null(model$model)
  parts <- if (nested) model else list(model)
  tot <- sum(vapply(parts, function(p) (if (is.null(p$psill)) 0 else p$psill) +
                      (if (is.null(p$nugget)) 0 else p$nugget), 0))
  parts <- lapply(parts, function(p) {
    p$psill <- (if (is.null(p$psill)) 0 else p$psill) / tot
    if (!is.null(p$nugget)) p$nugget <- p$nugget / tot
    p
  })
  if (nested) parts else parts[[1]]
}

#' Non-Gaussian random fields
#'
#' \code{TransformedField}: pointwise transforms of standard Gaussian fields
#' simulated by \code{CholeskySim} with the model rescaled to unit sill
#' (independent field \code{k} uses seed \code{seed + 7919 k}): log-normal,
#' chi-square, Student t (Worsley 1994), gamma anamorphosis, binary,
#' truncated pluri-Gaussian categories (Matheron et al. 1987), Poisson counts
#' and Cox intensities of a log-Gaussian field, white noise and mixtures.
#' \code{MaxStableField}: Schlather (2002) extremal Gaussian field.
#' \code{AnisotropicCoords}: gstat-style geometric anisotropy transform.
#' Identical to the Python arm \code{morie.fn.rfields}.
#'
#' @param coords Two-column coordinates.
#' @param model Covariance model (list or list of nested structures).
#' @param kind Transform (see Python documentation).
#' @param seed Philox seed.
#' @param nsim Number of realisations.
#' @param mean,sd Location and scale of the Gaussian predictor.
#' @param df Degrees of freedom.
#' @param threshold Binary threshold.
#' @param proportions Category proportions.
#' @param shape,rate Gamma parameters.
#' @param scale Poisson/Cox intensity scale.
#' @param weights,models Mixture weights and component models.
#' @param n_fields Number of Gaussian fields in the max-stable series.
#' @param angle Major-axis direction (degrees clockwise from north).
#' @param ratio Minor/major range ratio.
#' @return List (\code{field}) or matrix.
#' @references Schlather, M. (2002). Models for stationary max-stable random
#'   fields. Extremes 5, 33-44.
#'
#'   Worsley, K. J. (1994). Local maxima and the expected Euler characteristic
#'   of excursion sets of chi-squared, F and t fields. Advances in Applied
#'   Probability 26, 13-42.
#' @examples
#' m <- list(model = "Exp", psill = 1, range = 1)
#' TransformedField(rbind(c(0, 0), c(1, 0)), m, "binary", seed = 3)$field
#' AnisotropicCoords(rbind(c(1, 0), c(0, 1)), 90, 0.5)
#' @export
TransformedField <- function(coords, model, kind, seed = 1, nsim = 1, mean = 0, sd = 1, df = 3, threshold = 0,
                             proportions = NULL, shape = 2, rate = 1, scale = 1, weights = NULL, models = NULL) {
  P <- as.matrix(coords)
  n <- nrow(P)
  unit <- .rf_unit(model)
  gauss <- function(k, mdl = unit) CholeskySim(P, mdl, nsim = nsim, seed = seed + 7919 * k, mean = 0)$simulations
  if (kind == "white") {
    out <- t(vapply(seq_len(nsim) - 1, function(s) mean + sd * .morie_random_normal(n, seed = seed, stream = s),
                    numeric(n)))
    return(list(field = out, kind = kind))
  }
  Z <- gauss(0)
  out <- switch(kind,
    lognormal = exp(mean + sd * Z),
    cox_intensity = scale * exp(mean + sd * Z),
    binary = (Z > threshold) + 0,
    gamma = array(stats::qgamma(stats::pnorm(Z), shape, rate), dim(Z)),
    categorical = {
      if (abs(sum(proportions) - 1) > 1e-12 || any(proportions <= 0)) {
        stop("proportions must be positive and sum to 1")
      }
      cuts <- stats::qnorm(cumsum(proportions)[-length(proportions)])
      array(vapply(Z, function(z) sum(z > cuts), 0), dim(Z))
    },
    chi2 = Reduce(`+`, lapply(c(list(Z), lapply(seq_len(df - 1), gauss)), function(f) f^2)),
    student_t = Z / sqrt(Reduce(`+`, lapply(seq_len(df), function(k) gauss(k)^2)) / df),
    poisson = t(vapply(seq_len(nsim), function(s) {
      u <- .morie_random_uniform(n, seed = seed, stream = 500 + s - 1)
      stats::qpois(u, scale * exp(mean + sd * Z[s, ]))
    }, numeric(n))),
    mixture = {
      if (abs(sum(weights) - 1) > 1e-12 || any(weights < 0)) stop("weights must be nonnegative and sum to 1")
      Reduce(`+`, lapply(seq_along(models), function(k) sqrt(weights[k]) * gauss(k - 1, .rf_unit(models[[k]]))))
    },
    stop("unknown kind")
  )
  list(field = matrix(out, nsim), kind = kind)
}

#' @rdname TransformedField
#' @export
MaxStableField <- function(coords, model, n_fields = 200, seed = 1) {
  W <- CholeskySim(as.matrix(coords), .rf_unit(model), nsim = n_fields, seed = seed, mean = 0)$simulations
  u <- .morie_random_uniform(n_fields, seed = seed, stream = 2000)
  zeta <- 1 / cumsum(-log(u))
  Z <- sqrt(2 * pi) * apply(zeta * pmax(W, 0), 2, max)
  list(field = Z, n_fields = n_fields)
}

#' @rdname TransformedField
#' @export
AnisotropicCoords <- function(coords, angle, ratio) {
  P <- as.matrix(coords)
  a <- angle * pi / 180
  major <- P[, 1] * sin(a) + P[, 2] * cos(a)
  minor <- P[, 1] * cos(a) - P[, 2] * sin(a)
  cbind(minor / ratio, major)
}
