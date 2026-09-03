# morie.fn -- function file (rootcoder007/morie)
# Sources:
#   Hyndman, R. J. & Khandakar, Y. (2008) "Automatic Time Series
#   Forecasting: The forecast Package for R", Journal of Statistical
#   Software 27(3), 1-22, doi:10.18637/jss.v027.i03. Sec. 3.1, 3.2.
#   Box, G. E. P., Jenkins, G. M., Reinsel, G. C. & Ljung, G. M. (2016)
#   Time Series Analysis: Forecasting and Control, 5th edn, Wiley,
#   ISBN 978-1-118-67502-1, Sec. 9.5.

MAX_P <- 5
MAX_Q <- 5
MAX_SEASONAL <- 2
ROOT_TOL <- 1.001
.sarimax_state <- new.env(parent = emptyenv())
.sarimax_state$searching <- FALSE

#' .filter_column
#'
#' A step of the sarimax_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param w A vector; its length is taken and its elements indexed.
#' @param ar Passed to \code{.sarima_state_space}.
#' @param ma Passed to \code{.sarima_state_space}.
#' @return A list with \code{v}, \code{f}.
#' @export
.filter_column <- function(w, ar, ma) {
  ss <- .sarima_state_space(ar, ma)
  T <- ss$T
  R <- ss$R
  r <- ss$r
  P <- .sarima_initial_covariance(T, R, r)
  a <- rep(0.0, r)
  v_out <- numeric(0)
  f_out <- numeric(0)
  for (t in seq_along(w)) {
    f <- P[1L, 1L]
    if (f <= 0.0) stop("sarimax: non-positive prediction variance")
    v <- w[t] - a[1L]
    PZ <- P[, 1L]
    a <- a + PZ * v / f
    P <- P - (PZ %*% t(PZ)) / f
    v_out <- c(v_out, v)
    f_out <- c(f_out, f)
    a <- as.numeric(T %*% a)
    TP <- T %*% P
    P <- TP %*% t(T) + R %o% R
  }
  list(v = v_out, f = f_out)
}

#' .residual_column
#'
#' A step of the sarimax_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param w A vector; its length is taken.
#' @param ar Passed to \code{css}.
#' @param ma Passed to \code{css}.
#' @return A list with \code{v}, \code{f}.
#' @export
.residual_column <- function(w, ar, ma) {
  r <- css(w, ar, ma, full = TRUE)
  list(v = r$residuals, f = rep(1.0, length(w)))
}

#' profile_beta
#'
#' A step of the sarimax_native implementation. Called by \code{.sarimax_fit}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param wy A vector; its length is taken.
#' @param wX A vector; its length is taken and its elements indexed.
#' @param ar Passed to \code{.col}. Defaults to \code{numeric(0)}.
#' @param ma Passed to \code{.col}. Defaults to \code{numeric(0)}.
#' @param filter One of \code{"conditional"}, \code{"exact"}. Defaults to \code{"exact"}.
#' @return A list with \code{beta}, \code{ssq}, \code{v}, \code{f}, \code{information},
#' \code{sum_log_f}.
#' @export
profile_beta <- function(wy, wX, ar = numeric(0), ma = numeric(0),
                         filter = "exact") {
  n <- length(wy)
  if (any(lengths(wX) != n)) {
    stop(sprintf("sarimax: regressor columns must match the differenced series length %d", n))
  }
  if (!filter %in% c("exact", "conditional")) {
    stop(sprintf("sarimax: filter must be 'exact' or 'conditional', got %s", filter))
  }
  .col <- if (filter == "exact") .filter_column else .residual_column
  rwy <- .col(wy, ar, ma)
  vy <- rwy$v
  f <- rwy$f
  if (length(wX) == 0L) {
    ssq <- sum(vy * vy / f)
    return(list(beta = numeric(0), ssq = ssq, v = vy, f = f,
                information = list(),
                sum_log_f = sum(log(f))))
  }
  for (j in seq_along(wX)) {
    if (max(abs(wX[[j]])) <= 1e-12) {
      stop(sprintf("sarimax: regressor %d is annihilated by the differencing operator (a linear trend vanishes under nabla, a seasonal dummy under nabla_s), so beta is not identified", j - 1L))
    }
  }
  vx <- lapply(wX, function(c) .col(c, ar, ma)$v)
  k <- length(vx)
  A <- matrix(0.0, k, k)
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      A[i, j] <- sum(vx[[i]] * vx[[j]] / f)
    }
  }
  b <- numeric(k)
  for (i in seq_len(k)) b[i] <- sum(vx[[i]] * vy / f)
  beta <- as.numeric(solve(A, b))
  resid <- numeric(n)
  for (t in seq_len(n)) {
    s <- 0
    for (i in seq_len(k)) s <- s + beta[i] * vx[[i]][t]
    resid[t] <- vy[t] - s
  }
  ssq <- sum(resid * resid / f)
  list(beta = beta, ssq = ssq, v = resid, f = f,
       information = A, sum_log_f = sum(log(f)))
}

#' .columns
#'
#' A step of the sarimax_native implementation. Called by \code{.sarimax_fit}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @param n Passed to \code{!=}.
#' @return The value of \code{cols}, as built in the body.
#' @export
#' @examples
#' b <- c(1.5, 2.5, 3.5)
#' res <- .columns(X = b, n = 3L)
#' res
.columns <- function(X, n) {
  if (is.null(X)) return(list())
  if (is.list(X)) {
    cols <- lapply(X, function(c) as.numeric(c))
  } else {
    M <- as.matrix(X)
    cols <- lapply(seq_len(ncol(M)), function(j) as.numeric(M[, j]))
  }
  for (c in cols) {
    if (length(c) != n) {
      stop(sprintf("sarimax: regressor has %d rows but the series has %d",
                   length(c), n))
    }
  }
  cols
}

#' .sarimax_fit
#'
#' A step of the sarimax_native implementation. Called by \code{.try_fit}, \code{morie_sarimax}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y A vector; its length is taken.
#' @param X Passed to \code{.columns}.
#' @param order A vector; indexed elementwise. Defaults to \code{c(0, 1, 1)}.
#' @param seasonal_order A vector; indexed elementwise. Defaults to \code{c(0, 1, 1)}.
#' @param s Coerced to integer by the body, with \code{as.integer}. Defaults to \code{12}.
#' @param include_constant Optional; may be \code{NULL}. A flag; the body branches on it.
#' @param method One of \code{"css"}, \code{"ml"}, \code{"uls"}. Defaults to \code{"ml"}.
#' @return A list with \code{beta_se}, \code{estimate}, \code{beta}, \code{phi},
#' \code{theta}, \code{Phi}, \code{Theta}, \code{ar}, \code{ma}, \code{sigma2},
#' \code{loglik}, \code{aic}, \code{n_par}, \code{n_used}, \code{residuals},
#' \code{innovation_variance}, \code{include_constant}, \code{order},
#' \code{seasonal_order}, \code{s}, \code{fit_method}, \code{method}.
#' @export
.sarimax_fit <- function(y, X = NULL, order = c(0, 1, 1), seasonal_order = c(0, 1, 1),
                s = 12, include_constant = NULL, method = "ml") {
  if (!method %in% c("ml", "uls", "css")) {
    stop(sprintf("sarimax: method must be 'ml', 'uls' or 'css', got %s", method))
  }
  y <- as.numeric(y)
  p <- as.integer(order[1])
  d <- as.integer(order[2])
  q <- as.integer(order[3])
  P <- as.integer(seasonal_order[1])
  D <- as.integer(seasonal_order[2])
  Q <- as.integer(seasonal_order[3])
  s <- as.integer(s)
  cols <- .columns(X, length(y))
  if (is.null(include_constant)) include_constant <- (d + D) < 2
  if (include_constant && (d + D) >= 2) {
    stop(sprintf("sarimax: a constant is admitted only when d + D < 2 (Hyndman-Khandakar 2008 Sec. 3.1), got d = %d, D = %d",
                 d, D))
  }
  if (include_constant) cols <- c(list(rep(1.0, length(y))), cols)
  wy <- difference(y, d, D, s)
  wX <- lapply(cols, function(c) difference(c, d, D, s))
  if (include_constant) wX[[1L]] <- rep(1.0, length(wy))
  npar <- p + q + P + Q
  if (npar == 0 && length(wX) == 0L) stop("sarimax: nothing to estimate")
  unpack <- function(v) {
    # seq_len, not (i+1):(i+p): with p = 0 the colon form is 1:0,
    # which in R is c(1, 0) and returns the FIRST element instead of
    # none. Every block after it then reads the wrong slice, so an
    # order like (0,1,1)(0,1,1) estimated the wrong parameters
    # entirely. sarima_native.R already guards this; sarimax did not.
    i <- 0L
    phi <- v[i + seq_len(p)]
    i <- i + p
    th  <- v[i + seq_len(q)]
    i <- i + q
    Ph  <- v[i + seq_len(P)]
    i <- i + P
    Th  <- v[i + seq_len(Q)]
    list(phi = phi, th = th, Ph = Ph, Th = Th)
  }
  objective <- function(v) {
    u <- unpack(v)
    if (!(.sarima_roots_ok(u$phi, ROOT_TOL) && .sarima_roots_ok(u$Ph, ROOT_TOL))) return(1e10)
    ep <- expand_polynomials(u$phi, u$Ph, u$th, u$Th, s)
    if (!.sarima_roots_ok(ep$ma, ROOT_TOL)) return(1e10)
    tryCatch({
      r <- profile_beta(wy, wX, ep$ar, ep$ma,
                        if (method == "css") "conditional" else "exact")
      n <- length(wy)
      s2 <- r$ssq / n
      if (s2 <= 0.0) return(1e10)
      if (method %in% c("uls", "css")) return(r$ssq)
      0.5 * n * (log(2.0 * pi * s2) + 1.0) + 0.5 * r$sum_log_f
    }, error = function(e) 1e10)
  }
  x0 <- rep(0.1, npar)
  best <- objective(x0)
  xhat <- x0
  res <- NULL
  if (npar > 0L) {
    niter <- if (isTRUE(.sarimax_state$searching)) 1L else 8L
    for (i in seq_len(niter)) {
      res <- .sarima_minimize_nm(objective, xhat)
      cand <- as.numeric(res$x)
      val <- objective(cand)
      if (val < best - 1e-11) { best <- val
      xhat <- cand } else break
    }
  }
  u <- unpack(xhat)
  ep <- expand_polynomials(u$phi, u$Ph, u$th, u$Th, s)
  r <- profile_beta(wy, wX, ep$ar, ep$ma)
  n <- length(wy)
  sigma2 <- r$ssq / n
  ll <- -0.5 * n * (log(2.0 * pi * sigma2) + 1.0) - 0.5 * r$sum_log_f
  k <- npar + length(r$beta) + 1L
  if (length(r$beta) > 0L) {
    cov <- solve(r$information)
    beta_se <- sapply(seq_along(r$beta), function(i) {
      sqrt(max(sigma2 * cov[i, i], 0.0))
    })
  } else {
    beta_se <- numeric(0)
  }
  list(beta_se = beta_se,
       estimate = if (length(r$beta) > 0L) r$beta[1L] else sigma2,
       beta = r$beta, phi = u$phi, theta = u$th,
       Phi = u$Ph, Theta = u$Th, ar = ep$ar, ma = ep$ma,
       sigma2 = sigma2, loglik = ll,
       aic = -2.0 * ll + 2.0 * k, n_par = k, n_used = n,
       residuals = r$v, innovation_variance = r$f,
       include_constant = isTRUE(include_constant),
       order = c(p, d, q), seasonal_order = c(P, D, Q), s = s,
       fit_method = method,
       method = "regression with seasonal ARIMA errors, beta profiled out by exact GLS; Box et al. (2016) Sec. 9.5, Hyndman & Khandakar (2008) Sec. 3.1")
}

#' aic
#'
#' A step of the sarimax_native implementation. Called by \code{aicc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param loglik Coerced to numeric by the body, with \code{as.numeric}.
#' @param n_par Coerced to integer by the body, with \code{as.integer}.
#' @return A numeric value.
#' @export
aic <- function(loglik, n_par) {
  -2.0 * as.numeric(loglik) + 2.0 * as.integer(n_par)
}

#' aicc
#'
#' A step of the sarimax_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param loglik Passed to \code{aic}.
#' @param n_par Coerced to integer by the body, with \code{as.integer}.
#' @param n Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
aicc <- function(loglik, n_par, n) {
  k <- as.integer(n_par)
  n <- as.integer(n)
  if (n - k - 1L <= 0L) return(Inf)
  aic(loglik, k) + 2.0 * k * (k + 1L) / (n - k - 1L)
}

#' starting_models
#'
#' A step of the sarimax_native implementation. Called by \code{auto_order}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param d Passed to \code{c}.
#' @param D Accepted by the signature and not used anywhere in the body.
#' @param s Coerced to integer by the body, with \code{as.integer}.
#' @return One of two values, depending on the branch taken.
#' @export
starting_models <- function(d, D, s) {
  # Each candidate is a PAIR: the non-seasonal order and the seasonal
  # one. Returning only the first half made the seasonal branch
  # identical to the non-seasonal one, so a seasonal search began from
  # four models none of which had a seasonal term.
  if (as.integer(s) > 1L) {
    list(list(c(2, d, 2), c(1, D, 1)),
         list(c(0, d, 0), c(0, D, 0)),
         list(c(1, d, 0), c(1, D, 0)),
         list(c(0, d, 1), c(0, D, 1)))
  } else {
    list(list(c(2, d, 2), c(0, D, 0)),
         list(c(0, d, 0), c(0, D, 0)),
         list(c(1, d, 0), c(0, D, 0)),
         list(c(0, d, 1), c(0, D, 0)))
  }
}

#' neighbours
#'
#' A step of the sarimax_native implementation. Called by \code{auto_order}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param order A vector; indexed elementwise.
#' @param seasonal_order A vector; indexed elementwise.
#' @param constant A flag; the body branches on it.
#' @param s Coerced to integer by the body, with \code{as.integer}.
#' @return The value of \code{out}, as built in the body.
#' @export
neighbours <- function(order, seasonal_order, constant, s) {
  p <- order[1]
  d <- order[2]
  q <- order[3]
  P <- seasonal_order[1]
  D <- seasonal_order[2]
  Q <- seasonal_order[3]
  out <- list()
  deltas <- list(c(1, 0, 0, 0), c(-1, 0, 0, 0),
                 c(0, 1, 0, 0), c(0, -1, 0, 0),
                 c(0, 0, 1, 0), c(0, 0, -1, 0),
                 c(0, 0, 0, 1), c(0, 0, 0, -1),
                 c(1, 1, 0, 0), c(-1, -1, 0, 0),
                 c(0, 0, 1, 1), c(0, 0, -1, -1))
  for (d_ in deltas) {
    np_ <- p + d_[1L]
    nq <- q + d_[2L]
    nP <- P + d_[3L]
    nQ <- Q + d_[4L]
    if (min(np_, nq, nP, nQ) < 0) next
    if (np_ > MAX_P || nq > MAX_Q) next
    if (nP > MAX_SEASONAL || nQ > MAX_SEASONAL) next
    if (as.integer(s) <= 1L && (nP > 0 || nQ > 0)) next
    out[[length(out) + 1L]] <- list(c(np_, d, nq), c(nP, D, nQ), constant)
  }
  out[[length(out) + 1L]] <- list(c(p, d, q), c(P, D, Q), !constant)
  out
}

#' .try_fit
#'
#' A step of the sarimax_native implementation. Called by \code{auto_order}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y Passed to \code{.sarimax_fit}.
#' @param X Optional; may be \code{NULL}. Passed to \code{.sarimax_fit}.
#' @param order A vector; indexed elementwise.
#' @param seasonal_order A vector; indexed elementwise.
#' @param s Passed to \code{.sarimax_fit}.
#' @param constant A flag; the body branches on it.
#' @param method Passed to \code{.sarimax_fit}.
#' @return The value of \code{tryCatch}.
#' @export
.try_fit <- function(y, X, order, seasonal_order, s, constant, method) {
  if (constant && (order[2L] + seasonal_order[2L]) >= 2L) return(NULL)
  if (sum(order) + sum(seasonal_order) - order[2L] - seasonal_order[2L] == 0 &&
      !constant && is.null(X)) return(NULL)
  tryCatch(.sarimax_fit(y, X, order, seasonal_order, s,
               include_constant = constant, method = method),
           error = function(e) NULL)
}

#' auto_order
#'
#' A step of the sarimax_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y Passed to \code{.try_fit}.
#' @param X Passed to \code{.try_fit}.
#' @param d Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param D Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param s Passed to \code{.try_fit}. Defaults to \code{1}.
#' @param method Passed to \code{.try_fit}. Defaults to \code{"css"}.
#' @param max_steps Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20}.
#' @return A list with \code{estimate}, \code{aic}, \code{fit}, \code{order},
#' \code{seasonal_order}, \code{constant}, \code{steps}, \code{n_models_tried},
#' \code{tried}, \code{s}, \code{search_method}, \code{differencing_note}, \code{method}.
#' @export
auto_order <- function(y, X = NULL, d = 0, D = 0, s = 1, method = "css",
                       max_steps = 20) {
  d <- as.integer(d)
  D <- as.integer(D)
  s <- as.integer(s)
  visited <- new.env(hash = TRUE, parent = emptyenv())
  tried <- list()
  score <- function(order, seasonal, constant) {
    key <- paste(order, collapse = ",") %||% ""  # fallback; we'll use list
    key <- list(order = as.integer(order), seasonal = as.integer(seasonal), constant = isTRUE(constant))
    kstr <- paste(unlist(key), collapse = "|")
    if (exists(kstr, envir = visited, inherits = FALSE)) {
      return(get(kstr, envir = visited, inherits = FALSE))
    }
    r <- .try_fit(y, X, order, seasonal, s, constant, method)
    tried[[length(tried) + 1L]] <<- list(
      order = as.integer(order), seasonal_order = as.integer(seasonal),
      constant = isTRUE(constant),
      aic = if (is.null(r)) NULL else r$aic,
      rejected = is.null(r))
    assign(kstr, r, envir = visited)
    r
  }
  constant <- (d + D) < 2
  best <- NULL
  .sarimax_state$searching <- TRUE
  for (smod in starting_models(d, D, s)) {
    r <- score(smod[[1L]], smod[[2L]], constant)
    if (!is.null(r) && (is.null(best) || r$aic < best[[1L]]$aic)) {
      best <- list(r, smod[[1L]], smod[[2L]], constant)
    }
  }
  if (is.null(best)) stop("sarimax: every starting model was rejected")
  steps <- 0
  repeat {
    steps <- steps + 1
    if (steps > as.integer(max_steps)) break
    improved <- FALSE
    cur <- best[[1L]]
    order <- best[[2L]]
    seasonal <- best[[3L]]
    constant <- best[[4L]]
    for (nb in neighbours(order, seasonal, constant, s)) {
      r <- score(nb[[1L]], nb[[2L]], nb[[3L]])
      if (!is.null(r) && r$aic < cur$aic - 1e-8) {
        best <- list(r, nb[[1L]], nb[[2L]], nb[[3L]])
        improved <- TRUE
        break
      }
    }
    if (!improved) break
  }
  .sarimax_state$searching <- FALSE
  r <- best[[1L]]
  order <- best[[2L]]
  seasonal <- best[[3L]]
  constant <- best[[4L]]
  final <- .try_fit(y, X, order, seasonal, s, constant, method)
  if (!is.null(final)) r <- final
  list(estimate = r$aic, aic = r$aic, fit = r,
       order = order, seasonal_order = seasonal,
       constant = isTRUE(constant), steps = steps,
       n_models_tried = length(tried), tried = tried,
       s = s, search_method = method,
       differencing_note = "d and D are inputs; Hyndman & Khandakar select them with KPSS and Canova-Hansen tests, which are not implemented here",
       method = "step-wise order selection; Hyndman & Khandakar (2008) Sec. 3.2")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' .sarimax_cheatsheet
#'
#' A step of the sarimax_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .sarimax_cheatsheet()
#' res
.sarimax_cheatsheet <- function() {
  paste("sarimax: y = beta'x + n with seasonal ARIMA errors. beta",
        "is profiled out by exact GLS on the Kalman innovations,",
        "so only the ARIMA parameters go to the optimiser.",
        "auto_order is Hyndman-Khandakar's step-wise search: four",
        "starting models, thirteen neighbours, AIC, and the four",
        "stated constraints (p,q<=5, P,Q<=2, root >= 1.001, drop",
        "anything that will not fit). d and D are inputs -- the",
        "KPSS and Canova-Hansen tests that choose them are not",
        "implemented.")
}

#' morie_sarimax
#'
#' A step of the sarimax_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param ... Passed through.
#' @return The value of \code{.sarimax_fit}.
#' @export
morie_sarimax <- function(...) {
  .sarimax_fit(...)
}

sarimax <- .sarimax_fit
