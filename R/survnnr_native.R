# survnnr.R -- DeepSurv: a neural network trained on the Cox partial likelihood.
#
# The model: Cox's proportional hazards writes lambda(t|x) = lambda0(t) * exp(h(x)).
# Cox regression takes h(x) = beta'x; DeepSurv replaces that linear term with the
# single-node output of a multilayer perceptron and keeps everything else, so the
# training objective is the average negative log partial likelihood over the
# uncensored cases, with L2 regularisation.
#
# The linear case is the anchor: with no hidden layers, DeepSurv IS Cox
# regression by gradient descent, so the network must land on the coefficients
# that coxph finds by Newton-Raphson.
#
# The partial likelihood never estimates lambda0, so the network output is a
# log-risk on an arbitrary scale: only differences between subjects mean
# anything. The fit result reports that invariance and baseline_hazard
# supplies Breslow's estimator when an absolute survival curve is wanted.
#
# References
# ----------
# Katzman, J. L., Shaham, U., Cloninger, A., Bates, J., Jiang, T. & Kluger, Y.
# (2018) "DeepSurv: personalized treatment recommender system using a Cox
# proportional hazards deep neural network", BMC Medical Research Methodology
# 18, 24, doi:10.1186/s12874-018-0482-1. Sec. "DeepSurv" for the architecture
# and for the objective, the average negative log partial likelihood with L2
# regularisation.
#
# Cox, D. R. (1972) "Regression Models and Life-Tables", Journal of the Royal
# Statistical Society. Series B 34(2), 187-220, doi:10.1111/j.2517-6161.1972
# .tb00899.x, for the partial likelihood and the fact that it leaves the
# baseline hazard unspecified.
#
# Breslow, N. (1974) "Covariance Analysis of Censored Survival Data",
# Biometrics 30(1), 89-99, doi:10.2307/2529620, for the tie handling and the
# baseline cumulative hazard estimator used here.

ACTIVATIONS <- c("tanh", "relu", "identity")

#' .survnnr_act
#'
#' A step of the survnnr_native implementation. Called by \code{morie_survnnr_forward}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param v Passed to \code{tanh}.
#' @param kind One of \code{"relu"}, \code{"tanh"}.
#' @return The value of \code{v}, as built in the body.
#' @export
.survnnr_act <- function(v, kind) {
  if (kind == "tanh") return(tanh(v))
  if (kind == "relu") return(if (v > 0) v else 0)
  v
}

#' .survnnr_dact
#'
#' A step of the survnnr_native implementation. Called by \code{morie_survnnr_fit}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param v Passed to \code{tanh}.
#' @param kind One of \code{"relu"}, \code{"tanh"}.
#' @return A numeric value.
#' @export
.survnnr_dact <- function(v, kind) {
  if (kind == "tanh") {
    t <- tanh(v)
    return(1.0 - t * t)
  }
  if (kind == "relu") return(if (v > 0) 1.0 else 0.0)
  1.0
}

#' .survnnr_init
#'
#' A step of the survnnr_native implementation. Called by \code{morie_survnnr_fit}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param d Passed to \code{c}.
#' @param hidden Coerced to integer by the body, with \code{as.integer}.
#' @param seed Passed to \code{.ghc_rng}.
#' @return A list with \code{W}, \code{b}.
#' @export
.survnnr_init <- function(d, hidden, seed) {
  sizes <- c(d, as.integer(hidden), 1L)
  L <- length(sizes) - 1
  state <- .ghc_rng(seed)
  total_n <- 0L
  for (k in seq_len(L)) total_n <- total_n + sizes[k + 1L] * sizes[k]
  all_u <- .ghc_unif(state, total_n)
  W <- list()
  b <- list()
  idx <- 1L
  for (k in seq_len(L)) {
    scale <- sqrt(2.0 / sizes[k])
    n_rows <- sizes[k + 1L]
    n_cols <- sizes[k]
    end_idx <- idx + n_rows * n_cols - 1L
    layer_u <- all_u[idx:end_idx]
    M <- matrix((layer_u - 0.5) * 2.0 * scale,
                nrow = n_rows, ncol = n_cols, byrow = TRUE)
    W[[k]] <- M
    b[[k]] <- rep(0.0, n_rows)
    idx <- end_idx + 1L
  }
  list(W = W, b = b)
}

#' morie_survnnr_forward
#'
#' A step of the survnnr_native implementation. Called by \code{morie_survnnr_fit}, \code{morie_survnnr_risk_score}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param W A vector; its length is taken and its elements indexed.
#' @param b A vector; indexed elementwise.
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param activation Passed to \code{.survnnr_act}. Defaults to \code{"tanh"}.
#' @return A list with \code{output}, \code{pre}, \code{acts}.
#' @export
morie_survnnr_forward <- function(W, b, x, activation = "tanh") {
  a <- as.numeric(x)
  pre <- list()
  acts <- list(list(a))
  for (k in seq_along(W)) {
    z <- as.numeric(W[[k]] %*% a + b[[k]])
    pre[[k]] <- z
    if (k == length(W)) {
      a <- z
    } else {
      a <- sapply(z, function(v) .survnnr_act(v, activation))
    }
    acts[[k + 1L]] <- a
  }
  list(output = a[1], pre = pre, acts = acts)
}

#' morie_survnnr_partial_loglik
#'
#' A step of the survnnr_native implementation. Called by \code{morie_survnnr_fit}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param times A vector; its length is taken and its elements indexed.
#' @param events A vector; its length is taken and its elements indexed.
#' @param risk A vector; its length is taken and its elements indexed.
#' @return A list with \code{loglik}, \code{average}, \code{n_events}, \code{ties}.
#' @export
morie_survnnr_partial_loglik <- function(times, events, risk) {
  n <- length(times)
  if (!(n == length(events) && n == length(risk))) {
    stop("survnnr: times, events and risks must have the same length")
  }
  n_events <- sum(events)
  if (n_events == 0) {
    stop("survnnr: the partial likelihood needs at least one event")
  }
  total <- 0.0
  for (i in seq_len(n)) {
    if (!events[i]) next
    at_risk <- which(times >= times[i])
    if (length(at_risk) == 0L) next
    m <- max(risk[at_risk])
    lse <- m + log(sum(exp(risk[at_risk] - m)))
    total <- total + risk[i] - lse
  }
  list(loglik = total, average = total / n_events,
       n_events = n_events, ties = "Breslow")
}

#' .survnnr_grad_wrt_risk
#'
#' A step of the survnnr_native implementation. Called by \code{morie_survnnr_fit}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param times A vector; its length is taken and its elements indexed.
#' @param events A vector; indexed elementwise.
#' @param risk A vector; indexed elementwise.
#' @return The value of \code{g}, as built in the body.
#' @export
.survnnr_grad_wrt_risk <- function(times, events, risk) {
  n <- length(times)
  n_events <- as.numeric(sum(events))
  g <- rep(0.0, n)
  for (i in seq_len(n)) {
    if (!events[i]) next
    at_risk <- which(times >= times[i])
    m <- max(risk[at_risk])
    w <- exp(risk[at_risk] - m)
    s <- sum(w)
    g[i] <- g[i] - 1.0 / n_events
    g[at_risk] <- g[at_risk] + (w / s) / n_events
  }
  g
}

#' morie_survnnr_fit
#'
#' A step of the survnnr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A vector; its length is taken and its elements indexed.
#' @param times A vector; its length is taken.
#' @param events A vector; its length is taken.
#' @param hidden A vector; its length is taken. Defaults to \code{c()}.
#' @param activation Passed to \code{morie_survnnr_forward}. Defaults to \code{"tanh"}.
#' @param l2 Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param lr Numeric; combined arithmetically in the body. Defaults to \code{0.1}.
#' @param n_epochs Coerced to integer by the body, with \code{as.integer}. Defaults to \code{400}.
#' @param seed Passed to \code{.survnnr_init}. Defaults to \code{0}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-10}.
#' @return A list with \code{estimate}, \code{W}, \code{b}, \code{activation}, \code{hidden}, \code{l2}, \code{loss_history}, \code{risk}, \code{centred_risk}, \code{coefficients}, \code{times}, \code{events}, \code{epochs}, \code{ties}, \code{scale_note}, \code{method}.
#' @export
morie_survnnr_fit <- function(X, times, events, hidden = c(),
                              activation = "tanh", l2 = 0.0, lr = 0.1,
                              n_epochs = 400, seed = 0, tol = 1e-10) {
  if (!(activation %in% ACTIVATIONS)) {
    stop(sprintf("survnnr: activation must be one of %s, got %s",
                 paste(ACTIVATIONS, collapse = ", "), activation))
  }
  n <- length(times)
  if (n != length(X) || n != length(events)) {
    stop("survnnr: X, times and events must have the same length")
  }
  if (n == 0) {
    stop("survnnr: no observations")
  }
  d <- length(X[[1]])
  init <- .survnnr_init(d, hidden, seed)
  W <- init$W
  b <- init$b
  history <- c()
  prev <- NULL
  for (epoch in seq_len(as.integer(n_epochs))) {
    fwd <- lapply(X, function(x) morie_survnnr_forward(W, b, x, activation))
    risk <- sapply(fwd, function(f) f$output)
    pl <- morie_survnnr_partial_loglik(times, events, risk)
    loss <- -pl$average
    l2_loss <- 0
    for (M in W) l2_loss <- l2_loss + sum(M * M)
    loss <- loss + l2 * l2_loss
    history <- c(history, loss)
    if (!is.null(prev) && abs(prev - loss) < tol) break
    prev <- loss
    gr <- .survnnr_grad_wrt_risk(times, events, risk)
    gW <- lapply(W, function(M) matrix(0.0, nrow = nrow(M), ncol = ncol(M)))
    gb <- lapply(W, function(M) rep(0.0, nrow(M)))
    for (i in seq_len(n)) {
      delta <- c(gr[i])
      for (k in length(W):1) {
        acts_k <- fwd[[i]]$acts[[k]]
        gb[[k]] <- gb[[k]] + delta
        gW[[k]] <- gW[[k]] + delta %o% acts_k
        if (k > 1L) {
          pre_prev <- fwd[[i]]$pre[[k - 1L]]
          delta <- as.numeric(t(W[[k]]) %*% delta) *
                   sapply(pre_prev, function(v) .survnnr_dact(v, activation))
        }
      }
    }
    for (k in seq_along(W)) {
      b[[k]] <- b[[k]] - lr * gb[[k]]
      W[[k]] <- W[[k]] - lr * (gW[[k]] + 2.0 * l2 * W[[k]])
    }
  }
  risk <- sapply(X, function(x) morie_survnnr_forward(W, b, x, activation)$output)
  mean_risk <- mean(risk)
  list(
    estimate = if (length(history) > 0L) history[length(history)] else NaN,
    W = W, b = b, activation = activation,
    hidden = as.integer(hidden),
    l2 = as.numeric(l2), loss_history = history,
    risk = risk,
    centred_risk = risk - mean_risk,
    coefficients = if (length(hidden) == 0L) as.numeric(W[[1]][1, ]) else NULL,
    times = as.numeric(times), events = as.logical(events),
    epochs = length(history), ties = "Breslow",
    scale_note = paste("the partial likelihood is invariant to adding",
                       "a constant to every risk; only differences are",
                       "identified"),
    method = paste("DeepSurv: MLP trained on the average negative log",
                   "partial likelihood; Katzman et al. (2018) Eq. 4")
  )
}

#' morie_survnnr_risk_score
#'
#' A step of the survnnr_native implementation. Called by \code{morie_survnnr_concordance}, \code{morie_survnnr_survival_function}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param fit_result A list; the body reads \code{$activation}, \code{$b}, \code{$W} from it.
#' @param X Iterated over elementwise, with \code{sapply}.
#' @return A vector, from \code{sapply}.
#' @export
morie_survnnr_risk_score <- function(fit_result, X) {
  sapply(X, function(x) morie_survnnr_forward(fit_result$W, fit_result$b, x,
                                              fit_result$activation)$output)
}

#' morie_survnnr_baseline_hazard
#'
#' A step of the survnnr_native implementation. Called by \code{morie_survnnr_survival_function}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param fit_result A list; the body reads \code{$events}, \code{$risk}, \code{$times} from it.
#' @return A list with \code{time}, \code{cumulative_hazard}.
#' @export
morie_survnnr_baseline_hazard <- function(fit_result) {
  t <- fit_result$times
  e <- fit_result$events
  r <- exp(fit_result$risk)
  order_idx <- order(t)
  out_t <- c()
  out_h <- c()
  cum <- 0.0
  seen <- c()
  for (i in order_idx) {
    if (!e[i] || t[i] %in% seen) next
    seen <- c(seen, t[i])
    d <- sum(t == t[i] & e)
    denom <- sum(r[t >= t[i]])
    cum <- cum + d / denom
    out_t <- c(out_t, t[i])
    out_h <- c(out_h, cum)
  }
  list(time = out_t, cumulative_hazard = out_h)
}

#' morie_survnnr_survival_function
#'
#' A step of the survnnr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param fit_result Passed to \code{morie_survnnr_baseline_hazard}.
#' @param x Carried through into a list the body builds.
#' @param times Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{time}, \code{survival}.
#' @export
morie_survnnr_survival_function <- function(fit_result, x, times = NULL) {
  base <- morie_survnnr_baseline_hazard(fit_result)
  r <- exp(morie_survnnr_risk_score(fit_result, list(x))[[1]])
  ts <- if (is.null(times)) base$time else as.numeric(times)
  out <- numeric(length(ts))
  for (idx in seq_along(ts)) {
    t <- ts[idx]
    h <- 0.0
    for (k in seq_along(base$time)) {
      if (base$time[k] <= t) {
        h <- base$cumulative_hazard[k]
      } else {
        break
      }
    }
    out[idx] <- exp(-h * r)
  }
  list(time = ts, survival = out)
}

#' .survnnr_c_index
#'
#' A step of the survnnr_native implementation. Called by \code{morie_survnnr_concordance}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param times A vector; its length is taken and its elements indexed.
#' @param events A vector; indexed elementwise.
#' @param risk A vector; indexed elementwise.
#' @return A numeric value.
#' @export
.survnnr_c_index <- function(times, events, risk) {
  n <- length(times)
  num <- 0
  den <- 0
  for (i in seq_len(n)) {
    if (!events[i]) next
    for (j in seq_len(n)) {
      if (i == j) next
      if (times[i] < times[j]) {
        den <- den + 1
        if (risk[i] > risk[j]) {
          num <- num + 1
        } else if (risk[i] == risk[j]) {
          num <- num + 0.5
        }
      }
    }
  }
  if (den == 0) return(0)
  num / den
}

#' morie_survnnr_concordance
#'
#' A step of the survnnr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param fit_result Passed to \code{morie_survnnr_risk_score}.
#' @param X Passed to \code{morie_survnnr_risk_score}.
#' @param times Passed to \code{.survnnr_c_index}.
#' @param events Passed to \code{.survnnr_c_index}.
#' @return The value of \code{.survnnr_c_index}.
#' @export
morie_survnnr_concordance <- function(fit_result, X, times, events) {
  .survnnr_c_index(times, events, morie_survnnr_risk_score(fit_result, X))
}

#' morie_survnnr_cheatsheet
#'
#' A step of the survnnr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
morie_survnnr_cheatsheet <- function() {
  paste("survnnr: DeepSurv = Cox's partial likelihood with the linear",
        "predictor replaced by an MLP output. Loss is the AVERAGE negative",
        "log partial likelihood over events, Breslow ties. With no hidden",
        "layer it is Cox regression and must reproduce coxph. The baseline",
        "hazard is never estimated by the likelihood, so risks are",
        "identified only up to an additive constant; Breslow's estimator",
        "supplies it when an absolute survival curve is wanted.")
}

# compact alias per ledger/NAMING.md
morie_survnnr_deep_surv <- morie_survnnr_fit

# entry point
morie_survnnr <- morie_survnnr_fit













