# Prophet's additive decomposition: the components, separated.
# Sources: Taylor, S. J. & Letham, B. (2018) "Forecasting at Scale",
# *The American Statistician* 72(1), 37-45,
# doi:10.1080/00031305.2017.1380080; preprint *PeerJ Preprints*
# 5:e3190v2, doi:10.7287/peerj.preprints.3190v2. Eq. (1); the
# decomposition is its Sec. 3. Cleveland, R. B., Cleveland, W. S.,
# McRae, J. E. & Terpenning, I. (1990) "STL: A Seasonal-Trend
# Decomposition Procedure Based on Loess", *Journal of Official
# Statistics* 6(1), 3-73. The decomposition idea in its nonparametric
# form. Harvey, A. C. & Peters, S. (1990) "Estimation procedures for
# structural time series models", *Journal of Forecasting* 9(2), 89-108,
# doi:10.1002/for.3980090203.
#
# Native implementation mirroring Python morie.fn.prophe exactly: the
# same model fit through prphet, the same component-by-component
# extraction from the fitted coefficients, the same reconstruction
# check that trend + seasonalities + holidays sum back to the fitted
# values to machine precision, and the same component-wise sd ranking
# (not a variance decomposition -- the components overlap).

prophe_additive_components <- function(t, y, seasonalities = NULL,
                                       holidays = NULL,
                                       holiday_window = c(0, 0), ...) {
  fit <- prphet_prophet_fit(t, y, seasonalities = seasonalities,
                            holidays = holidays,
                            holiday_window = holiday_window, ...)
  tv <- fit$t
  n <- length(tv)
  coef <- fit$coef
  trend <- fit$trend

  comps <- list(trend = trend)
  if (!is.null(seasonalities) && length(seasonalities) > 0L) {
    for (s in seasonalities) {
      name <- s[[1L]]
      period <- s[[2L]]
      order <- as.integer(s[[3L]])
      Fmat <- prphet_fourier_terms(tv, period, order)
      vals <- numeric(n)
      for (i in seq_len(n)) {
        acc <- 0.0
        for (nn in seq_len(order)) {
          cos_key <- paste0(name, "_cos", nn)
          sin_key <- paste0(name, "_sin", nn)
          acc <- acc + (coef[[cos_key]] * Fmat[[i]][2L * nn - 2L]
                        + coef[[sin_key]] * Fmat[[i]][2L * nn - 1L])
        }
        vals[i] <- acc
      }
      comps[[name]] <- vals
    }
  }
  if (!is.null(holidays) && length(holidays) > 0L) {
    H <- prphet_holiday_matrix(tv, holidays, holiday_window[1L],
                               holiday_window[2L])
    Hmat <- H$matrix
    names <- H$names
    vals <- numeric(n)
    for (i in seq_len(n)) {
      acc <- 0.0
      for (j in seq_along(names)) {
        key <- paste0("holiday_", names[j])
        acc <- acc + coef[[key]] * Hmat[[i]][j]
      }
      vals[i] <- acc
    }
    comps[["holidays"]] <- vals
  }

  total <- numeric(n)
  for (i in seq_len(n)) {
    acc <- 0.0
    for (cn in names(comps)) {
      acc <- acc + comps[[cn]][i]
    }
    total[i] <- acc
  }
  gap <- max(abs(total - fit$fitted))
  list(estimate = comps, components = comps, total = total,
       fitted = fit$fitted, residual = fit$residual,
       reconstruction_error = gap, reconstructs = gap < 1e-8,
       coef = coef, changepoints = fit$changepoints,
       sigma = fit$sigma, n = n,
       component_names = sort(names(comps)),
       method = paste0("Prophet additive decomposition, Taylor & ",
                       "Letham (2018) eq. (1)"))
}

prophe_component_shares <- function(components) {
  out <- list()
  for (nm in names(components)) {
    vals <- components[[nm]]
    out[[nm]] <- if (length(vals) > 1L) sd(vals) else 0.0
  }
  tot <- sum(unlist(out))
  rel <- if (tot > 0) {
    r <- list()
    for (nm in names(out)) r[[nm]] <- out[[nm]] / tot
    r
  } else {
    r <- list()
    for (nm in names(out)) r[[nm]] <- 0.0
    r
  }
  ranked <- names(out)[order(-unlist(out))]
  list(sd = out, relative = rel, ranked = ranked,
       note = paste0("standard deviations, not an orthogonal variance ",
                     "split -- the components overlap"))
}

prophe_cheatsheet <- function() {
  paste0("prophe: same model and source as prphet (Taylor & Letham ",
         "2018 eq. 1) -- this is the DECOMPOSITION view. Fit once, ",
         "return g(t), each s(t) and h(t) separately, and check they ",
         "sum back to the fitted values exactly. Component sds rank ",
         "them but do NOT partition variance: trend and Fourier ",
         "terms both absorb slow drift.")
}

morie_prophe <- function(t, y, seasonalities = NULL, holidays = NULL,
                         holiday_window = c(0, 0), ...) {
  prophe_additive_components(t, y, seasonalities = seasonalities,
                             holidays = holidays,
                             holiday_window = holiday_window, ...)
}

# compact alias per ledger/NAMING.md
additivecomponents <- prophe_additive_components

# public names resolved by fn/_lazy_map.json
facebook_prophet <- prophe_additive_components
