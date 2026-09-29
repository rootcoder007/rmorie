.stc_g <- function(model, h) {
  nug <- if (is.null(model$nugget)) 0 else model$nugget
  r <- h / model$range
  f <- switch(model$model, Exp = 1 - exp(-r), Gau = 1 - exp(-r^2), Sph = ifelse(r >= 1, 1, 1.5 * r - 0.5 * r^3),
              Lin = r, stop("marginal model must be Exp, Gau, Sph or Lin"))
  ifelse(h == 0, 0, nug + model$psill * f)
}

.stc_sill <- function(model) model$psill + (if (is.null(model$nugget)) 0 else model$nugget)

#' Space-time variograms and covariance families
#'
#' \code{StModelVariogram}: separable, product-sum, metric and sum-metric
#' variograms in the parametrisation of \code{gstat::vgmST} (marginals are
#' lists \code{psill}, \code{model} (Exp, Gau, Sph, Lin), \code{range},
#' \code{nugget}). \code{StCovarianceFamily}: Gneiting (2002), Cressie-Huang (1999),
#' Iaco-Cesare (De Iaco, Myers and Posa 2002), periodic, separable
#' exponential and Porcu (Porcu, Gregori and Mateu 2006, quasi-arithmetic
#' mean: \code{(0.5 (1 + (h/scale_s)^power_s)^sep + 0.5 (1 +
#' (|u|/scale_t)^power_t)^sep)^(-1/sep)}; at \code{sep = 0} the continuous
#' limit, the separable geometric mean, or with \code{method = "GeoModels"}
#' the package's squared product) covariances. \code{StLinearCombination}: nonnegative
#' combinations. Identical to the Python arm \code{morie.fn.stcovar}.
#'
#' @param h Spatial distances.
#' @param u Time lags.
#' @param model \code{"separable"}, \code{"productSum"}, \code{"metric"} or
#'   \code{"sumMetric"}.
#' @param space,time,joint Marginal structures.
#' @param sill Separable sill.
#' @param k Product-sum parameter.
#' @param stani Space-time anisotropy.
#' @param family Covariance family.
#' @param ... Family parameters.
#' @param components List of \code{list(family, params)}.
#' @param weights Nonnegative weights.
#' @return Numeric or list.
#' @references Gneiting, T. (2002). Nonseparable, stationary covariance
#'   functions for space-time data. JASA 97, 590-600.
#'
#'   Cressie, N. and Huang, H.-C. (1999). Classes of nonseparable,
#'   spatio-temporal stationary covariance functions. JASA 94, 1330-1340.
#'
#'   Graeler, B., Pebesma, E. and Heuvelink, G. (2016). Spatio-temporal
#'   interpolation using gstat. The R Journal 8, 204-218.
#'
#'   Porcu, E., Gregori, P. and Mateu, J. (2006). Nonseparable stationary
#'   anisotropic space-time covariance functions. SERRA 21, 113-122.
#' @examples
#' s <- list(psill = 2, model = "Exp", range = 100)
#' tm <- list(psill = 3, model = "Sph", range = 5)
#' StModelVariogram(c(0, 50, 120), c(1, 3, 10), "productSum", space = s, time = tm, k = 0.1)
#' StCovarianceFamily(1, 2, "gneiting", a = 1, alpha = 0.5, beta = 1, gamma = 0.5, c = 1, tau = 1)
#' @export
StModelVariogram <- function(h, u, model, space = NULL, time = NULL, joint = NULL, sill = NULL, k = NULL, stani = NULL) {
  switch(model,
    separable = {
      gs <- .stc_g(space, h)
      gt <- .stc_g(time, u)
      sill * (gs + gt - gs * gt)
    },
    productSum = {
      gs <- .stc_g(space, h)
      gt <- .stc_g(time, u)
      (k * .stc_sill(time) + 1) * gs + (k * .stc_sill(space) + 1) * gt - k * gs * gt
    },
    metric = .stc_g(joint, sqrt(h^2 + (stani * u)^2)),
    sumMetric = .stc_g(space, h) + .stc_g(time, u) + .stc_g(joint, sqrt(h^2 + (stani * u)^2)),
    stop("model must be separable, productSum, metric or sumMetric")
  )
}

#' @rdname StModelVariogram
#' @export
StCovarianceFamily <- function(h, u, family, ...) {
  p <- list(...)
  s2 <- if (is.null(p$sigma2)) 1 else p$sigma2
  switch(family,
    gneiting = {
      psi <- p$a * abs(u)^(2 * p$alpha) + 1
      s2 / psi^p$tau * exp(-p$c * h^(2 * p$gamma) / psi^(p$beta * p$gamma))
    },
    cressie_huang = {
      q <- p$a^2 * u^2 + 1
      s2 / q^((if (is.null(p$d)) 2 else p$d) / 2) * exp(-p$b^2 * h^2 / q)
    },
    iaco_cesare = s2 * (1 + (h / p$a)^p$alpha + (abs(u) / p$b)^p$beta)^(-p$delta),
    periodic = s2 * exp(-h / p$range) * exp(-abs(u) / (if (is.null(p$tau)) Inf else p$tau)) * cos(2 * pi * u / p$period),
    separable_exp = s2 * exp(-h / p$range_s - abs(u) / p$range_t),
    porcu = {
      d <- if (is.null(p$sep)) 0.5 else p$sep
      if (!(p$power_s > 0 && p$power_s <= 2 && p$power_t > 0 && p$power_t <= 2 && p$scale_s > 0 && p$scale_t > 0 &&
              d >= 0 && d <= 1)) {
        stop("porcu needs power_s, power_t in (0, 2], scale_s, scale_t > 0 and sep in [0, 1]")
      }
      a1 <- 1 + (h / p$scale_s)^p$power_s
      a2 <- 1 + (abs(u) / p$scale_t)^p$power_t
      # the sep -> 0 limit is the geometric mean; GeoModels/CompRandFld return 1 / (a1 a2) at sep = 0 exactly
      if (d == 0) {
        if (identical(p$method, "GeoModels")) s2 / (a1 * a2) else s2 / sqrt(a1 * a2)
      } else {
        s2 * (0.5 * a1^d + 0.5 * a2^d)^(-1 / d)
      }
    },
    stop("unknown covariance family")
  )
}

#' @rdname StModelVariogram
#' @export
StLinearCombination <- function(h, u, components, weights) {
  if (any(weights < 0)) stop("weights must be nonnegative")
  parts <- lapply(components, function(cm) do.call(StCovarianceFamily, c(list(h, u, cm[[1]]), cm[[2]])))
  list(covariance = Reduce(`+`, Map(`*`, weights, parts)), components = parts)
}
