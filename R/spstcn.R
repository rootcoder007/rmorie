# SPDX-License-Identifier: AGPL-3.0-or-later

#' Non-separable spatio-temporal covariance functions
#'
#' Separable models cannot represent space-time interaction: under product
#' separability the spatial covariance has the same shape at every time lag.
#' Sec. 9.3 names four constructions that can, and all four are available
#' here, because they are not interchangeable.
#'
#' @details
#' `monotone` -- Gneiting (2002), Sec. 9.3.1, eqs (9.7)-(9.9).
#' \eqn{C(h,k) = \sigma^2 \psi(k^2)^{-d/2} \phi(||h||^2 / \psi(k^2))} with
#' \eqn{\phi} completely monotone and \eqn{\psi} positive with a completely
#' monotone derivative. With \eqn{\phi(t) = \exp(-c t^\gamma)} and
#' \eqn{\psi(t) = (a t^\alpha + 1)^\beta} this is the closed form (9.8), valid
#' for \eqn{a, c > 0}, \eqn{0 < \gamma, \alpha \le 1} and
#' \eqn{0 \le \beta \le 1}. Those are Gneiting's *sufficient* conditions;
#' Zastavnyi and Porcu (2011) later gave the necessary ones, so enforcing these
#' bounds is conservative rather than exact.
#'
#' `monotone` is also the only one of the four that yields a test for
#' separability: (9.9) is separable at \eqn{\beta = 0} and non-separable
#' otherwise, and the two are nested. Supply `neg2_loglik` and
#' `neg2_loglik_separable` to get the likelihood-ratio test with the boundary
#' correction of Sec. 6.2.3.
#'
#' `power_mixture` -- Ma (2002), eqs (9.13)-(9.14). Mixing product correlation
#' functions over a discrete law. The univariate case is the probability
#' generating function of that law evaluated at \eqn{w = R_s(h) R_t(k)}, so a
#' pgf is all that is needed; Example 9.1 gives the Binomial and Poisson cases.
#'
#' `scale_mixture` -- Ma (2002), eqs (9.15)-(9.16).
#' \eqn{Z(s,t) = Z_s(sU) Z_t(tV)}: the coordinates themselves are randomly
#' rescaled, so \eqn{C(h,k) = \int C_s(hu) C_t(kv) dF(u,v)}.
#'
#' `differential` -- Jones and Zhang (1997), eq (9.17), the covariance implied
#' by a stochastic partial differential equation, obtained as a zero-order
#' Hankel transform. `p` governs the smoothness and must exceed
#' \eqn{\max(1, d/2)}. The transform is evaluated by panel quadrature and the
#' result carries its own truncation diagnostics, because at \eqn{k = 0} the
#' integrand decays only algebraically and a fixed cutoff silently returns a
#' wrong number.
#'
#'
#' The Cressie and Huang (1999) spectral route of Sec. 9.3.2 is deliberately
#' not offered. It requires choosing \eqn{R(\omega, k)} and \eqn{k(\omega)}
#' and integrating (9.12), and the text records that Gneiting (2002) showed
#' some of the published examples are invalid because \eqn{R} did not satisfy
#' the needed conditions. A construction whose validity cannot be checked from
#' its arguments does not belong behind a keyword.
#'
#' @param spatial_h,temporal_u Spatial and temporal lags, carried separately.
#' @param params A list of method parameters; see the method descriptions.
#' @param method One of "monotone", "power_mixture", "scale_mixture",
#'   "differential".
#' @param coords,times Optional design on which to check eq (9.5).
#' @return A list with `st_covariance`, `method`, `equation`, `separable`,
#'   method-specific diagnostics, and when a design is supplied `valid` and
#'   `min_eigenvalue`.
#' @references Schabenberger Ch 9, Sec 9.3, eqs (9.7)-(9.17)
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' spstcn(V, V)
spstcn <- function(spatial_h, temporal_u, params = list(), method = "monotone",
                   coords = NULL, times = NULL) {
  methods <- c("monotone", "power_mixture", "scale_mixture", "differential")
  if (!method %in% methods) {
    stop(sprintf("`method` must be one of %s", paste(methods, collapse = ", ")),
         call. = FALSE)
  }
  p <- params
  gv <- function(nm, default) if (is.null(p[[nm]])) default else p[[nm]]
  out <- list(method = method, separable = FALSE)
  model <- NULL

  if (identical(method, "monotone")) {
    beta <- gv("beta", 1)
    args <- list(sigma2 = gv("sigma2", 1), a = gv("a", 1), c = gv("c", 1),
                 alpha = gv("alpha", 1), beta = beta, gamma = gv("gamma", 1),
                 d = gv("d", 2))
    if (!is.null(p$beta_t)) {
      cvals <- do.call(.schab_st_gneiting_with_temporal,
                       c(list(spatial_h, temporal_u, beta_t = p$beta_t), args))
      model <- function(d, u) do.call(.schab_st_gneiting_with_temporal,
                                      c(list(d, u, beta_t = p$beta_t), args))
      out$equation <- "9.9"
    } else {
      cvals <- do.call(.schab_st_gneiting, c(list(spatial_h, temporal_u), args))
      model <- function(d, u) do.call(.schab_st_gneiting, c(list(d, u), args))
      out$equation <- "9.8"
    }
    out$separable <- isTRUE(beta == 0)
    if (!is.null(p$neg2_loglik) && !is.null(p$neg2_loglik_separable)) {
      out$separability_test <- .schab_st_separability_test(
        p$neg2_loglik, p$neg2_loglik_separable)
    }
  } else if (identical(method, "power_mixture")) {
    if (!is.null(p$pmf)) {
      cvals <- .schab_st_bivariate_power_mixture(p$rs, p$rt, p$pmf)
      out$equation <- "9.13"
    } else {
      dist <- gv("distribution", "poisson")
      extra <- p[intersect(names(p), c("lam", "n", "pi"))]
      cvals <- do.call(.schab_st_power_mixture,
                       c(list(p$rs, p$rt, dist), extra))
      out$equation <- "9.14"
      out$distribution <- dist
    }
  } else if (identical(method, "scale_mixture")) {
    cvals <- .schab_st_scale_mixture(spatial_h, temporal_u, p$cov_spatial,
                                     p$cov_temporal, p$nodes, p$weights)
    model <- function(d, u) .schab_st_scale_mixture(d, u, p$cov_spatial,
                                                    p$cov_temporal, p$nodes,
                                                    p$weights)
    out$equation <- "9.16"
  } else {
    args <- list(sigma2 = gv("sigma2", 1), theta = gv("theta", 1),
                 c = gv("c", 1), p = gv("p", 1.5), d = gv("d", 2),
                 n_quad = gv("n_quad", 40L))
    r <- do.call(.schab_st_jones_zhang, c(list(spatial_h, temporal_u), args))
    cvals <- r$covariance
    out$quadrature <- r$quadrature
    model <- function(d, u) do.call(.schab_st_jones_zhang,
                                    c(list(d, u), args))$covariance
    out$equation <- "9.17"
  }

  out$st_covariance <- cvals
  if (!is.null(coords) && !is.null(times) && !is.null(model)) {
    v <- .schab_st_is_valid_covariance(coords, times, model)
    out$valid <- v$valid
    out$min_eigenvalue <- v$min_eigenvalue
    if (!isTRUE(v$valid)) {
      out$warning <- paste("eq (9.5) fails on this design -- the construction",
                           "is not a valid covariance function here")
    }
  }
  out
}
