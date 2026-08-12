# Variance components: ANOVA, REML, and the combined front end.
# Source: Searle, S. R., Casella, G. and McCulloch, C. E. (1992),
# Variance Components, Wiley: one-way ANOVA estimators Eq. (21)
# Sec. 2.2 pp. 26-27; balanced-data ANOVA Ch. 4; unbalanced data /
# Henderson's Method I Ch. 5; REML Sec. 3.8 and Ch. 6 (Sec. 6.6);
# the balanced-data identity "REML solutions = ANOVA estimators"
# Sec. 4.8; interval estimation Sec. 3.5.  Local source:
# fetched-wave3/Variance_components_FULL.pdf.  Patterson & Thompson
# (1971), Biometrika 58(3), 545-554.
# Mirrors Python morie.fn.ranova / remlfn / vcomp exactly.

.vc_groups <- function(y, group) {
  k <- as.character(group)
  keys <- sort(unique(k))
  list(keys = keys, gs = lapply(keys, function(u) as.numeric(y[k == u])))
}

#' ANOVA (method-of-moments) estimation of variance components
#'
#' One-way random model y_ij = mu + a_i + e_ij.  With
#' SSA = sum_i n_i (ybar_i - ybar)^2, SSE = sum_ij (y_ij - ybar_i)^2,
#' MSA = SSA/(a-1) and MSE = SSE/(N-a), the ANOVA estimators are
#' sigma_e^2 = MSE and sigma_a^2 = (MSA - MSE)/n_0 with
#' n_0 = (N - sum_i n_i^2 / N)/(a - 1), which reduces to Searle et
#' al.'s Eq. (21) form (MSA - MSE)/n for balanced data.  Unbiased but
#' possibly negative, so both the raw and the zero-truncated values
#' are returned.
#'
#' @param y Observations.
#' @param group Class label per observation.
#' @return A list with elements \code{sigma2_a}, \code{sigma2_e},
#'   \code{sigma2_a_raw}, \code{msa}, \code{mse}, \code{ssa},
#'   \code{sse}, \code{n0}, \code{a}, \code{N}, \code{n_i},
#'   \code{balanced}, \code{icc}, \code{method}.
#' @references Searle, S. R., Casella, G. and McCulloch, C. E.
#'   (1992). Variance Components. Wiley, Eq. (21) and Chapters 4-5.
#' @export
morie_ranova <- function(y, group) {
  y <- as.numeric(y)
  if (length(y) != length(group))
    stop("y and group must have equal length")
  z <- .vc_groups(y, group)
  gs <- z$gs
  a <- length(gs)
  if (a < 2) stop("need at least two classes")
  ns <- vapply(gs, length, integer(1))
  N <- sum(ns)
  if (N == a) stop("need replication within classes")
  grand <- sum(y) / N
  means <- vapply(gs, mean, numeric(1))
  ssa <- sum(ns * (means - grand)^2)
  sse <- sum(vapply(seq_len(a), function(i) sum((gs[[i]] - means[i])^2),
                    numeric(1)))
  msa <- ssa / (a - 1)
  mse <- sse / (N - a)
  balanced <- length(unique(ns)) == 1L
  n0 <- (N - sum(ns * ns) / N) / (a - 1)
  s2a_raw <- (msa - mse) / n0
  s2a <- if (s2a_raw > 0) s2a_raw else 0
  s2e <- mse
  denom <- s2a + s2e
  list(sigma2_a = s2a, sigma2_e = s2e, sigma2_a_raw = s2a_raw,
       msa = msa, mse = mse, ssa = ssa, sse = sse, n0 = n0,
       a = a, N = N, n_i = as.integer(ns), balanced = balanced,
       icc = if (denom > 0) s2a / denom else 0,
       method = "ANOVA variance components (Searle et al. 1992, Eq. 21)")
}

.reml_loglik <- function(gs, ns, s2a, s2e) {
  logdetV <- 0; xvx <- 0; xvy <- 0; yvy <- 0
  for (i in seq_along(gs)) {
    g <- gs[[i]]; n <- ns[i]
    d <- s2e + n * s2a
    logdetV <- logdetV + (n - 1) * log(s2e) + log(d)
    s <- sum(g); ss <- sum(g * g)
    xvx <- xvx + n / d
    xvy <- xvy + s / d
    yvy <- yvy + ss / s2e - (s2a / (s2e * d)) * s * s
  }
  mu <- xvy / xvx
  ypy <- yvy - xvy * xvy / xvx
  list(ll = -0.5 * (logdetV + log(xvx) + ypy), mu = mu)
}

#' REML estimation of variance components (one-way random model)
#'
#' Maximises the restricted log-likelihood
#' -2 l_R = log|V| + log|X'V^-1 X| + y'Py with X = 1 (Searle et al.
#' Sec. 6.6).  For BALANCED data their Sec. 4.8 proves the REML
#' solutions equal the ANOVA estimators, so the closed form is used
#' there -- exact, where a numerical maximiser cannot resolve the
#' argmax because l_R is flat to within double precision.  For
#' unbalanced data l_R is maximised numerically over the log
#' variances.
#'
#' On unbalanced data the two language arms agree on l_R to
#' ~6e-14 but on the argmax only to ~3e-7, because l_R is very flat
#' near its maximum; the objective is the verified quantity.
#'
#' @param y Observations.
#' @param group Class label per observation.
#' @param tol Convergence tolerance.
#' @param max_iter Maximum optimiser iterations.
#' @return A list with elements \code{sigma2_a}, \code{sigma2_e},
#'   \code{mu}, \code{loglik}, \code{n_iter}, \code{converged},
#'   \code{icc}, \code{a}, \code{N}, \code{closed_form},
#'   \code{method}.
#' @references Searle, S. R., Casella, G. and McCulloch, C. E.
#'   (1992). Variance Components. Wiley, Sections 4.8 and 6.6.
#'   Patterson, H. D. and Thompson, R. (1971). Biometrika, 58,
#'   545-554.
#' @export
morie_remlfn <- function(y, group, tol = 1e-10, max_iter = 5000) {
  y <- as.numeric(y)
  if (length(y) != length(group))
    stop("y and group must have equal length")
  z <- .vc_groups(y, group); gs <- z$gs
  a <- length(gs)
  if (a < 2) stop("need at least two classes")
  ns <- vapply(gs, length, integer(1))
  N <- sum(ns)
  if (N == a) stop("need replication within classes")
  st <- morie_ranova(y, group)
  s2e <- st$mse; if (s2e <= 0) s2e <- 1e-8
  s2a <- st$sigma2_a; if (s2a <= 0) s2a <- s2e / max(a, 2)
  if (isTRUE(st$balanced) && st$sigma2_a_raw > 0) {
    s2a <- st$sigma2_a_raw; s2e <- st$mse
    r <- .reml_loglik(gs, ns, s2a, s2e)
    denom <- s2a + s2e
    return(list(sigma2_a = s2a, sigma2_e = s2e, mu = r$mu,
                loglik = r$ll, n_iter = 0L, converged = TRUE,
                icc = if (denom > 0) s2a / denom else 0,
                a = a, N = N, closed_form = TRUE,
                method = paste("REML variance components (Searle et al.",
                  "1992, Sec. 4.8 closed form: REML = ANOVA on",
                  "balanced data)")))
  }
  neg <- function(par) {
    va <- exp(par[1]); ve <- exp(par[2])
    if (!is.finite(va) || !is.finite(ve) || va <= 0 || ve <= 0)
      return(1e300)
    v <- tryCatch(.reml_loglik(gs, ns, va, ve)$ll,
                  error = function(e) NA_real_)
    if (!is.finite(v)) return(1e300)
    -v
  }
  x0 <- c(log(max(s2a, 1e-12)), log(max(s2e, 1e-12)))
  op <- stats::optim(x0, neg, method = "Nelder-Mead",
                     control = list(reltol = tol, maxit = as.integer(max_iter)))
  xb <- op$par
  # coordinate-wise golden-section polish (mirrors the Python arm)
  gr <- (sqrt(5) - 1) / 2
  for (round in seq_len(60)) {
    moved <- 0
    for (k in 1:2) {
      lo <- xb[k] - 0.5; hi <- xb[k] + 0.5
      cc <- hi - gr * (hi - lo); dd <- lo + gr * (hi - lo)
      pc <- xb; pc[k] <- cc; pd <- xb; pd[k] <- dd
      fc <- neg(pc); fd <- neg(pd)
      for (jj in seq_len(200)) {
        if (fc < fd) {
          hi <- dd; dd <- cc; fd <- fc
          cc <- hi - gr * (hi - lo); pc <- xb; pc[k] <- cc; fc <- neg(pc)
        } else {
          lo <- cc; cc <- dd; fc <- fd
          dd <- lo + gr * (hi - lo); pd <- xb; pd[k] <- dd; fd <- neg(pd)
        }
        if (hi - lo < 1e-14) break
      }
      best <- 0.5 * (lo + hi)
      moved <- max(moved, abs(best - xb[k]))
      xb[k] <- best
    }
    if (moved < 1e-13) break
  }
  s2a <- exp(xb[1]); s2e <- exp(xb[2])
  r <- .reml_loglik(gs, ns, s2a, s2e)
  denom <- s2a + s2e
  list(sigma2_a = s2a, sigma2_e = s2e, mu = r$mu, loglik = r$ll,
       n_iter = as.integer(op$counts[1]), converged = op$convergence == 0,
       icc = if (denom > 0) s2a / denom else 0,
       a = a, N = N, closed_form = FALSE,
       method = "REML variance components (Searle et al. 1992, Sec. 6.6)")
}

#' Variance components with an exact ICC interval on balanced data
#'
#' Dispatches to \code{morie_ranova} or \code{morie_remlfn}.  On
#' balanced data the exact interval for the intraclass correlation
#' inverts the pivot MSA/MSE ~ (1 + n rho/(1-rho)) F_{a-1, N-a}
#' (Searle et al. Sec. 3.5): with F = MSA/MSE, F_L = F/F_{1-alpha/2}
#' and F_U = F/F_{alpha/2}, the limits are (F_L-1)/(F_L-1+n) and
#' (F_U-1)/(F_U-1+n).
#'
#' @param y Observations.
#' @param group Class label per observation.
#' @param method "reml" (default) or "anova".
#' @param conf_level Confidence level for the ICC interval.
#' @return A list with elements \code{sigma2_a}, \code{sigma2_e},
#'   \code{icc}, \code{icc_lower}, \code{icc_upper},
#'   \code{method_used}, \code{balanced}, \code{a}, \code{N},
#'   \code{fit}, \code{method}.
#' @references Searle, S. R., Casella, G. and McCulloch, C. E.
#'   (1992). Variance Components. Wiley, Sections 3.5, 4.8, 6.6.
#' @export
morie_vcomp <- function(y, group, method = "reml", conf_level = 0.95) {
  if (!method %in% c("reml", "anova"))
    stop("method must be 'reml' or 'anova'")
  av <- morie_ranova(y, group)
  fit <- if (method == "reml") morie_remlfn(y, group) else av
  s2a <- fit$sigma2_a; s2e <- fit$sigma2_e
  denom <- s2a + s2e
  icc <- if (denom > 0) s2a / denom else 0
  lo <- NULL; hi <- NULL
  if (isTRUE(av$balanced)) {
    n <- av$n_i[1]; a <- av$a; N <- av$N
    alpha <- 1 - as.numeric(conf_level)
    Fv <- if (av$mse > 0) av$msa / av$mse else Inf
    FL <- Fv / stats::qf(1 - alpha / 2, a - 1, N - a)
    FU <- Fv / stats::qf(alpha / 2, a - 1, N - a)
    lo <- (FL - 1) / (FL - 1 + n)
    hi <- (FU - 1) / (FU - 1 + n)
    if (lo < 0) lo <- 0
    if (hi > 1) hi <- 1
  }
  list(sigma2_a = s2a, sigma2_e = s2e, icc = icc,
       icc_lower = lo, icc_upper = hi, method_used = method,
       balanced = isTRUE(av$balanced), a = av$a, N = av$N, fit = fit,
       method = sprintf("variance components, %s (Searle et al. 1992)",
                        method))
}
