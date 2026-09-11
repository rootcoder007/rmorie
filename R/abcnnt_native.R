# Sequential Neural Likelihood, Papamakarios, Sterratt & Murray (2019).
# Sources: Papamakarios, G., Sterratt, D. C. & Murray, I. (2019)
# "Sequential Neural Likelihood: Fast Likelihood-free Inference with
# Autoregressive Flows", AISTATS 22, PMLR 89:837-848, arXiv:1805.07226
# -- Algorithm 1 (propose from current posterior estimate, simulate,
# add pair to D, retrain flow on ALL of D, set new posterior to
# q(x_o|theta) p(theta)) and the Masked Autoregressive Flow body
# (5 affine autoregressive layers with reversed variable order).
# Papamakarios, G., Pavlakou, T. & Murray, I. (2017) "Masked
# Autoregressive Flow for Density Estimation", NIPS 30, for the
# affine autoregressive transform and the change of variables.
#
# Native implementation mirroring morie.fn.abcnnt exactly: the same
# MADE masks (degrees 1..dim_x for inputs and outputs, 0..dim_x-1 for
# hidden units, conditioning columns always unmasked), the same SGD
# with central-difference gradients, the same Algorithm 1 loop. All
# random numbers come from the shared .ghc_rng.

# A single Masked Autoregressive layer as a list of named matrices. The
# masks are stored as 0/1 floats so element-wise multiply drops blocked
# connections, which is what the masked matrix product reduces to in
# any case.
#' A single Masked Autoregressive layer as a list of named matrices. The
#'
#' masks are stored as 0/1 floats so element-wise multiply drops blocked
#' connections, which is what the masked matrix product reduces to in
#' any case.
#'
#' @param dim_x A count; the body uses it as \code{seq_len(...)}.
#' @param dim_t A count; the body uses it as \code{seq_len(...)}.
#' @param hidden A count; the body uses it as \code{seq_len(...)}.
#' @param e Passed to \code{.ghc_norm}.
#' @param reverse A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return A list with \code{W1}, \code{b1}, \code{Wm}, \code{bm}, \code{Wa}, \code{ba},
#' \code{M1}, \code{M2}, \code{dim_x}, \code{dim_t}, \code{hidden}, \code{order}.
#' @export
#' @examples
#' L <- .abcnnt_made_layer(dim_x = 3L, dim_t = 2L, hidden = 6L,
#'                         e = .ghc_rng(1L), reverse = FALSE)
#' dim(L$M1)   # hidden by dim_x + dim_t
#' dim(L$M2)   # dim_x by hidden
#' L$order
.abcnnt_made_layer <- function(dim_x, dim_t, hidden, e, reverse = FALSE) {
  order <- seq_len(dim_x)
  if (reverse) order <- rev(order)
  deg_in <- order # 0-based, used as integers
  deg_out <- order
  deg_h <- (seq_len(hidden) - 1L) %% dim_x
  s1 <- 1 / sqrt(dim_x + dim_t)
  s2 <- 1 / sqrt(hidden)
  W1 <- matrix(.ghc_norm(e, hidden * (dim_x + dim_t), 0, s1),
    nrow = hidden, ncol = dim_x + dim_t
  )
  b1 <- rep(0, hidden)
  Wm <- matrix(.ghc_norm(e, dim_x * hidden, 0, s2), nrow = dim_x, ncol = hidden)
  Wa <- matrix(.ghc_norm(e, dim_x * hidden, 0, s2), nrow = dim_x, ncol = hidden)
  bm <- rep(0, dim_x)
  ba <- rep(0, dim_x)
  M1 <- matrix(0, hidden, dim_x + dim_t)
  for (k in seq_len(hidden)) {
    for (j in seq_len(dim_x)) M1[k, j] <- if (deg_h[k] >= deg_in[j]) 1 else 0
    for (j in seq_len(dim_t)) M1[k, dim_x + j] <- 1
  }
  M2 <- matrix(0, dim_x, hidden)
  for (i in seq_len(dim_x)) {
    for (k in seq_len(hidden)) {
      M2[i, k] <- if (deg_out[i] > deg_h[k]) 1 else 0
    }
  }
  list(
    W1 = W1, b1 = b1, Wm = Wm, bm = bm, Wa = Wa, ba = ba,
    M1 = M1, M2 = M2, dim_x = dim_x, dim_t = dim_t,
    hidden = hidden, order = order
  )
}

# Returns list(mu, al, h). Tanh hidden activations, clipped log-scale.
#' Returns list(mu, al, h). Tanh hidden activations, clipped log-scale
#'
#' A step of the abcnnt_native implementation. Called by \code{flow_forward}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param layer A list; the body reads \code{$b1}, \code{$ba}, \code{$bm}, \code{$dim_t},
#' \code{$dim_x}, \code{$hidden}, \code{$M1}, \code{$M2}, \code{$W1}, \code{$Wa},
#' \code{$Wm} from it.
#' @param x Passed to \code{c}.
#' @param t Passed to \code{c}.
#' @return A list with \code{mu}, \code{al}, \code{h}.
#' @export
#' @examples
#' L <- .abcnnt_made_layer(3L, 2L, 6L, .ghc_rng(2L), reverse = FALSE)
#' st <- .abcnnt_layer_stats(L, x = c(0.5, -0.2, 0.9), t = c(0.1, -0.4))
#' st$mu
#' st$al       # the log-scale, clamped to [-5, 5]
.abcnnt_layer_stats <- function(layer, x, t) {
  dx <- layer$dim_x
  dt <- layer$dim_t
  H <- layer$hidden
  inp <- c(x, t)
  # MADE masks the WEIGHTS: the autoregressive structure is (M * W) %*% x.
  # Masking the product instead multiplies a hidden x (dim_x + dim_t) mask
  # by a hidden x 1 vector, which is non-conformable for dim_x + dim_t > 1
  # and, where rowSums hid it, computed rowSums(M) * (W %*% x).
  z <- layer$b1 + as.numeric((layer$M1 * layer$W1) %*% matrix(inp, ncol = 1))
  h <- tanh(z)
  mu <- layer$bm + as.numeric((layer$M2 * layer$Wm) %*% matrix(h, ncol = 1))
  a <- layer$ba + as.numeric((layer$M2 * layer$Wa) %*% matrix(h, ncol = 1))
  al <- pmin(pmax(a, -5), 5)
  list(mu = as.numeric(mu), al = as.numeric(al), h = as.numeric(h))
}

#' Push x through every MAF layer; returns (u, total_alpha)
#' @param flow See Usage.
#' @param x See Usage.
#' @param t See Usage.
#' @export
#' @examples
#' flow <- MAF(dim_x = 2L, dim_t = 2L, n_layers = 2L, hidden = 4L, seed = 1L)
#' fw <- flow_forward(flow, x = c(0.4, -0.7), t = c(0.1, 0.2))
#' fw$u        # the latent point x maps to
#' fw$total    # the log-Jacobian of the map
flow_forward <- function(flow, x, t) {
  u <- as.numeric(x)
  total <- 0
  for (layer in flow$layers) {
    st <- .abcnnt_layer_stats(layer, u, t)
    u <- (u - st$mu) * exp(-st$al)
    total <- total + sum(st$al)
  }
  list(u = u, total = total)
}

#' log q_phi(x | theta), change of variables
#' @param flow See Usage.
#' @param x See Usage.
#' @param t See Usage.
#' @export
#' @examples
#' flow <- MAF(dim_x = 2L, dim_t = 2L, n_layers = 2L, hidden = 4L, seed = 1L)
#' flow_logprob(flow, x = c(0.4, -0.7), t = c(0.1, 0.2))
#' # for one dimension the density integrates to one
#' f1 <- MAF(dim_x = 1L, dim_t = 1L, n_layers = 3L, hidden = 5L, seed = 4L)
#' g <- seq(-12, 12, length.out = 2001)
#' d <- vapply(g, function(x) exp(flow_logprob(f1, x, 0.3)), numeric(1))
#' round(sum((d[-1] + d[-length(d)]) / 2) * (g[2] - g[1]), 3)
flow_logprob <- function(flow, x, t) {
  fw <- flow_forward(flow, x, t)
  d <- length(fw$u)
  -0.5 * sum(fw$u^2) - 0.5 * d * log(2 * pi) - fw$total
}

#' Build a Masked Autoregressive Flow
#' @param dim_x See Usage.
#' @param dim_t See Usage.
#' @param n_layers See Usage.
#' @param hidden See Usage.
#' @param seed See Usage.
#' @export
#' @examples
#' flow <- MAF(dim_x = 2L, dim_t = 2L, n_layers = 3L, hidden = 6L, seed = 1L)
#' length(flow$layers)
#' # the autoregressive ordering alternates between layers
#' flow$layers[[1]]$order
#' flow$layers[[2]]$order
MAF <- function(dim_x, dim_t, n_layers = 5L, hidden = 20L, seed = 0L) {
  if (dim_x < 1L || dim_t < 1L) stop("abcnnt: dimensions must be positive")
  if (n_layers < 1L || hidden < 1L) {
    stop("abcnnt: n_layers and hidden must be positive")
  }
  e <- .ghc_rng(seed + 17L)
  layers <- vector("list", as.integer(n_layers))
  for (k in seq_len(as.integer(n_layers))) {
    layers[[k]] <- .abcnnt_made_layer(dim_x, dim_t, hidden, e,
      reverse = (k %% 2L == 1L)
    )
  }
  list(layers = layers, dim_x = dim_x, dim_t = dim_t)
}

# Walk every (container, index) the gradient touches. A parameter is
# in the active set iff its mask entry is 1; theta columns are always
# unmasked, hidden units are masked by deg_in, output by deg_out.
#' Walk every (container, index) the gradient touches. A parameter is
#'
#' in the active set iff its mask entry is 1; theta columns are always
#' unmasked, hidden units are masked by deg_in, output by deg_out.
#'
#' @param flow A list; the body reads \code{$layers} from it.
#' @return A list of addresses, each \code{list(layer=, field=, index=)},
#' naming one trainable parameter of the flow.
#' @export
#' @examples
#' flow <- MAF(dim_x = 2L, dim_t = 2L, n_layers = 2L, hidden = 4L, seed = 1L)
#' ps <- .abcnnt_params(flow)
#' length(ps)
#' # each entry addresses one trainable parameter
#' ps[[1]]
.abcnnt_params <- function(flow) {
  # An address, not the value: R hands out a copy of L$W1, so perturbing
  # what this returned could never reach the flow and every
  # finite-difference gradient came out as exactly zero.
  out <- list()
  at <- function(layer, field, index)
    list(layer = layer, field = field, index = index)
  for (li in seq_along(flow$layers)) {
    L <- flow$layers[[li]]
    for (k in seq_len(L$hidden)) {
      for (j in seq_len(L$dim_x + L$dim_t)) {
        if (L$M1[k, j] == 1) out[[length(out) + 1L]] <- at(li, "W1", c(k, j))
      }
      out[[length(out) + 1L]] <- at(li, "b1", k)
    }
    for (i in seq_len(L$dim_x)) {
      for (k in seq_len(L$hidden)) {
        if (L$M2[i, k] == 1) {
          out[[length(out) + 1L]] <- at(li, "Wm", c(i, k))
          out[[length(out) + 1L]] <- at(li, "Wa", c(i, k))
        }
      }
      out[[length(out) + 1L]] <- at(li, "bm", i)
      out[[length(out) + 1L]] <- at(li, "ba", i)
    }
  }
  out
}

#' Read the parameter an address points at
#' @param flow A flow as built by \code{MAF}.
#' @param a An address from \code{.abcnnt_params}.
#' @return The scalar parameter value.
#' @export
#' @examples
#' flow <- MAF(dim_x = 2L, dim_t = 2L, n_layers = 2L, hidden = 4L, seed = 1L)
#' a <- .abcnnt_params(flow)[[1]]
#' .abcnnt_param_get(flow, a)
.abcnnt_param_get <- function(flow, a) {
  x <- flow$layers[[a$layer]][[a$field]]
  if (length(a$index) == 2L) x[a$index[1L], a$index[2L]] else x[a$index[1L]]
}

#' Write the parameter an address points at
#' @param flow A flow as built by \code{MAF}.
#' @param a An address from \code{.abcnnt_params}.
#' @param v The replacement value.
#' @return The flow, with that one parameter replaced.
#' @export
#' @examples
#' flow <- MAF(dim_x = 2L, dim_t = 2L, n_layers = 2L, hidden = 4L, seed = 1L)
#' a <- .abcnnt_params(flow)[[1]]
#' moved <- .abcnnt_param_set(flow, a, 0.5)
#' .abcnnt_param_get(moved, a)
.abcnnt_param_set <- function(flow, a, v) {
  if (length(a$index) == 2L) {
    flow$layers[[a$layer]][[a$field]][a$index[1L], a$index[2L]] <- v
  } else {
    flow$layers[[a$layer]][[a$field]][a$index[1L]] <- v
  }
  flow
}

#' Train a MAF by central-difference SGD
#'
#' The paper backpropagates through autograd; we run central
#' differences on the masked parameters so the two arms agree
#' bit-for-bit at the cost of a few extra forward passes.
#' @param flow See Usage.
#' @param D See Usage.
#' @param epochs See Usage.
#' @param lr See Usage.
#' @param seed See Usage.
#' @param batch See Usage.
#' @export
#' @examples
#' flow <- MAF(dim_x = 2L, dim_t = 2L, n_layers = 2L, hidden = 4L, seed = 1L)
#' set.seed(1)
#' D <- lapply(1:30, function(k) {
#'   th <- rnorm(2)
#'   list(th, th * 0.5 + rnorm(2, 0, 0.2))
#' })
#' ll <- function(fl) mean(vapply(D, function(p)
#'   flow_logprob(fl, p[[2]], p[[1]]), numeric(1)))
#' trained <- train_flow(flow, D, epochs = 10L, lr = 0.02, seed = 1L)
#' round(c(before = ll(flow), after = ll(trained)), 3)
train_flow <- function(flow, D, epochs = 40L, lr = 0.01, seed = 0L,
                       batch = NULL) {
  if (length(D) == 0L) stop("abcnnt: no training pairs")
  if (epochs < 1L || lr <= 0) {
    stop("abcnnt: epochs must be >= 1 and lr positive")
  }
  e <- .ghc_rng(seed + 23L)
  ps <- .abcnnt_params(flow)
  n <- length(D)
  bs <- if (is.null(batch)) n else max(1L, min(as.integer(batch), n))
  total <- function(fl, sample) {
    sum(vapply(
      sample, function(p) flow_logprob(fl, p[[2]], p[[1]]),
      numeric(1)
    )) / length(sample)
  }
  h <- 1e-4
  for (ep in seq_len(as.integer(epochs))) {
    idx <- as.integer(.ghc_unif(e, bs) * n) + 1L
    idx <- pmin(idx, n)
    sample <- D[idx]
    for (a in ps) {
      old <- .abcnnt_param_get(flow, a)
      up <- total(.abcnnt_param_set(flow, a, old + h), sample)
      dn <- total(.abcnnt_param_set(flow, a, old - h), sample)
      # ascend the log-likelihood
      flow <- .abcnnt_param_set(flow, a, old + lr * (up - dn) / (2 * h))
    }
  }
  flow
}

#' Random-walk Metropolis, used to draw from the current posterior
#' @param logpdf See Usage.
#' @param x0 See Usage.
#' @param n See Usage.
#' @param burn See Usage.
#' @param step See Usage.
#' @param seed See Usage.
#' @export
#' @examples
#' # a standard normal target
#' lp <- function(x) -0.5 * sum(x^2)
#' mc <- mcmc_sample(lp, 0, 2000L, burn = 200L, step = 1.5, seed = 1L)
#' round(c(mean = mean(mc$samples[, 1]), sd = sd(mc$samples[, 1])), 2)
mcmc_sample <- function(logpdf, x0, n, burn = 100L, step = 0.5,
                        seed = 0L) {
  if (n < 1L) stop("abcnnt: n must be positive")
  if (step <= 0) stop("abcnnt: step must be positive")
  e <- .ghc_rng(seed + 31L)
  cur <- as.numeric(x0)
  lc <- logpdf(cur)
  out <- matrix(0, as.integer(n), length(cur))
  acc <- 0L
  for (it in seq_len(as.integer(burn) + as.integer(n))) {
    prop <- cur + step * .ghc_norm(e, length(cur))
    lp <- logpdf(prop)
    if (log(max(.ghc_unif(e, 1L), 1e-300)) < lp - lc) {
      cur <- prop
      lc <- lp
      acc <- acc + 1L
    }
    if (it > as.integer(burn)) out[it - as.integer(burn), ] <- cur
  }
  list(samples = out, acceptance = acc / (as.integer(burn) + as.integer(n)))
}

#' Algorithm 1: Sequential Neural Likelihood
#' @param simulator See Usage.
#' @param x_o See Usage.
#' @param log_prior See Usage.
#' @param theta0 See Usage.
#' @param n_rounds See Usage.
#' @param n_per_round See Usage.
#' @param n_layers See Usage.
#' @param hidden See Usage.
#' @param epochs See Usage.
#' @param lr See Usage.
#' @param mcmc_burn See Usage.
#' @param mcmc_step See Usage.
#' @param seed See Usage.
#' @param n_posterior See Usage.
#' @export
#' @examples
#' simulator <- function(theta, e) theta * 0.5 + rnorm(length(theta), 0, 0.2)
#' log_prior <- function(th) if (any(abs(th) > 5)) -Inf else -0.5 * sum(th^2)
#' set.seed(1)
#' res <- abcnnt(simulator, x_o = 0.25, log_prior = log_prior, theta0 = 0.5,
#'               n_rounds = 2L, n_per_round = 25L, epochs = 5L, lr = 0.02,
#'               seed = 1L, mcmc_burn = 50L, mcmc_step = 0.8)
#' names(res)
abcnnt <- function(simulator, x_o, log_prior, theta0, n_rounds = 3L,
                   n_per_round = 50L, n_layers = 5L, hidden = 20L,
                   epochs = 40L, lr = 0.01, mcmc_burn = 100L,
                   mcmc_step = 0.5, seed = 0L, n_posterior = 200L) {
  x_o <- as.numeric(x_o)
  theta0 <- as.numeric(theta0)
  if (length(x_o) == 0L || length(theta0) == 0L) {
    stop("abcnnt: x_o and theta0 must be non-empty")
  }
  if (n_rounds < 1L || n_per_round < 1L) {
    stop("abcnnt: n_rounds and n_per_round must be positive")
  }
  e_round <- .ghc_rng(seed + 5L)
  flow <- MAF(length(x_o), length(theta0), n_layers, hidden, seed)
  D <- list()
  history <- list()
  logpost <- log_prior
  for (r in seq_len(as.integer(n_rounds))) {
    mc <- mcmc_sample(
      logpost, theta0, as.integer(n_per_round),
      mcmc_burn, mcmc_step, seed + r
    )
    for (k in seq_len(nrow(mc$samples))) {
      th <- mc$samples[k, ]
      D[[length(D) + 1L]] <- list(th, as.numeric(simulator(th, e_round)))
    }
    flow <- train_flow(flow, D, epochs, lr, seed + r, batch = NULL)
    logpost <- function(th, f = flow) {
      lp <- log_prior(th)
      if (lp == -Inf) {
        return(-Inf)
      }
      lp + flow_logprob(f, x_o, th)
    }
    tail_ll <- sum(vapply(
      D[max(1L, length(D) - 9L):length(D)],
      function(p) flow_logprob(flow, x_o, p[[1]]),
      numeric(1)
    )) / 10
    history[[length(history) + 1L]] <- list(
      round = r, n_total = length(D),
      acceptance = mc$acceptance,
      loglik_at_xo = tail_ll
    )
  }
  post <- mcmc_sample(
    logpost, theta0, as.integer(n_posterior), mcmc_burn,
    mcmc_step, seed + 999L
  )
  d <- length(theta0)
  M <- nrow(post$samples)
  m <- colMeans(post$samples)
  v <- apply(post$samples, 2, function(z) {
    mu <- mean(z)
    sum((z - mu)^2) / max(M - 1, 1)
  })
  list(
    estimate = m, posterior_mean = m,
    posterior_sd = sqrt(v), posterior_samples = post$samples, flow = flow,
    D = D, n_simulations = length(D), history = history,
    acceptance = post$acceptance, n_rounds = as.integer(n_rounds),
    method = "Sequential Neural Likelihood (Papamakarios, Sterratt & Murray 2019) with a Masked Autoregressive Flow",
    note = paste0(
      "Algorithm 1 retrains on the whole of D each round, not the n",
      "ewest round, and proposes from the current posterior estimat",
      "e; round 1 proposes from the prior since p_hat_0 = p(theta).",
      " Flow gradients are central differences, which is slow and a",
      "voids a hand-rolled backward pass"
    )
  )
}

#' Compact alias for abcnnt
#' @export
#' @noRd
sequential_neural_likelihood <- abcnnt

# house entry point: the package exports one morie_<module>
morie_abcnnt <- abcnnt
