# bayrjmcmc -- reversible-jump MCMC across models of differing dimension
# Green, P. J. (1995) Biometrika 82(4), 711-732.
# Base R only.

.JACOBIAN_ROUTES <- c("analytic", "numeric")
.MOVE_KEYS <- c("frm", "to", "n_u", "n_u_rev", "propose", "transform")

# --------------------------------------------------------------------------
# Philox-like uniform stream
# --------------------------------------------------------------------------

#' .unif_stream
#'
#' A step of the bayrjmcmc_native implementation. Called by \code{changepoint_rjmcmc}, \code{reversible_jump_mcmc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param seed Coerced to integer by the body, with \code{as.integer}.
#' @param block Defaults to \code{8192L}.
#' @return The value of \code{uni}, as built in the body.
#' @export
.unif_stream <- function(seed, block = 8192L) {
  st <- list(buf = numeric(0), i = 0L, stream = 0L)
  uni <- function() {
    if (st$i >= length(st$buf)) {
      # LCG, sufficient here (used only for MH accept/reject)
      n <- block
      out <- numeric(n)
      x <- as.integer(seed) + as.integer(st$stream) * 1009L
      for (i in 1:n) {
        x <- .ghc_lcg31(x)
        out[i] <- x / 2147483648
      }
      st$buf <- out
      st$stream <- st$stream + 1L
      st$i <- 0L
    }
    v <- st$buf[st$i + 1L]
    st$i <- st$i + 1L
    if (v <= 0) v <- 1e-15
    if (v >= 1) v <- 1 - 1e-15
    v
  }
  uni
}

# --------------------------------------------------------------------------
# log |det| by Gaussian elimination
# --------------------------------------------------------------------------

#' .logabsdet
#'
#' A step of the bayrjmcmc_native implementation. Called by \code{numeric_log_jacobian}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param a A matrix; passed to \code{nrow}.
#' @return The value of \code{total}, as built in the body.
#' @export
.logabsdet <- function(a) {
  n <- nrow(a)
  if (n == 0) return(0)
  m <- a
  total <- 0
  for (col in 1:n) {
    piv <- col
    best <- abs(m[col, col])
    for (r in (col + 1):n) {
      v <- abs(m[r, col])
      if (v > best) { best <- v; piv <- r }
    }
    if (best < 1e-300) return(-Inf)
    if (piv != col) { tmp <- m[col, ]; m[col, ] <- m[piv, ]; m[piv, ] <- tmp }
    total <- total + log(abs(m[col, col]))
    for (r in (col + 1):n) {
      f <- m[r, col] / m[col, col]
      if (f == 0) next
      m[r, (col):n] <- m[r, (col):n] - f * m[col, (col):n]
    }
  }
  total
}

#' numeric_log_jacobian
#'
#' A step of the bayrjmcmc_native implementation. Called by \code{reversible_jump_mcmc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param mapfun Accepted by the signature and not used anywhere in the body.
#' @param z A vector; its length is taken and its elements indexed.
#' @param h Numeric; combined arithmetically in the body. Defaults to \code{1e-06}.
#' @return The value of \code{.logabsdet}.
#' @export
numeric_log_jacobian <- function(mapfun, z, h = 1e-6) {
  z <- as.numeric(z)
  n <- length(z)
  out0 <- mapfun(z)
  if (length(out0) != n)
    stop(sprintf(paste("bayrjmcmc: the bijection maps %d values to %d;",
                       "dimension matching requires n1 + m1 == n2 + m2"),
                 n, length(out0)))
  if (n == 0) return(0)
  jac <- matrix(0, n, n)
  for (j in 1:n) {
    step <- h * max(1, abs(z[j]))
    zp <- z; zm <- z
    zp[j] <- zp[j] + step
    zm[j] <- zm[j] - step
    fp <- mapfun(zp)
    fm <- mapfun(zm)
    jac[, j] <- (fp - fm) / (2 * step)
  }
  .logabsdet(jac)
}

# --------------------------------------------------------------------------
# dimension matching
# --------------------------------------------------------------------------

#' check_dimension_matching
#'
#' A step of the bayrjmcmc_native implementation. Called by \code{reversible_jump_mcmc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param models A vector; its length is taken and its elements indexed.
#' @param moves See Usage.
#' @return The value of \code{by_pair}, as built in the body.
#' @export
check_dimension_matching <- function(models, moves) {
  if (length(models) == 0) stop("bayrjmcmc: no models given")
  for (nm in names(models)) {
    spec <- models[[nm]]
    if (is.null(spec$dim) || is.null(spec$logpost))
      stop(sprintf("bayrjmcmc: model %s needs 'dim' and 'logpost'", nm))
    if (as.integer(spec$dim) < 0)
      stop(sprintf("bayrjmcmc: model %s has negative dim", nm))
  }
  by_pair <- list()
  for (mv in moves) {
    for (key in .MOVE_KEYS)
      if (is.null(mv[[key]]))
        stop(sprintf("bayrjmcmc: a move is missing %s", key))
    if (!(mv$frm %in% names(models)) || !(mv$to %in% names(models)))
      stop(sprintf("bayrjmcmc: move %s -> %s names an unknown model",
                   mv$frm, mv$to))
    if (mv$frm == mv$to)
      stop(sprintf(paste("bayrjmcmc: %s -> %s is a within-model move;",
                         "give it as 'within', not as a jump"),
                   mv$frm, mv$to))
    k <- paste(mv$frm, mv$to, sep = "->")
    if (!is.null(by_pair[[k]]))
      stop(sprintf("bayrjmcmc: two moves given for %s -> %s", mv$frm, mv$to))
    by_pair[[k]] <- mv
  }
  for (mv in moves) {
    rev <- by_pair[[paste(mv$to, mv$frm, sep = "->")]]
    if (is.null(rev))
      stop(sprintf(paste("bayrjmcmc: move %s -> %s has no reverse move;",
                         "detailed balance is imposed within each move",
                         "type, so the reverse must be supplied"),
                   mv$frm, mv$to))
    n1 <- as.integer(models[[mv$frm]]$dim)
    n2 <- as.integer(models[[mv$to]]$dim)
    m1 <- as.integer(mv$n_u)
    m2 <- as.integer(mv$n_u_rev)
    if (m1 < 0 || m2 < 0)
      stop(sprintf("bayrjmcmc: move %s -> %s has negative n_u",
                   mv$frm, mv$to))
    if (n1 + m1 != n2 + m2)
      stop(sprintf(paste("bayrjmcmc: move %s -> %s violates dimension",
                         "matching: n1 + m1 = %d + %d != %d + %d = n2 + m2"),
                   mv$frm, mv$to, n1, m1, n2, m2))
    if (as.integer(rev$n_u) != m2 || as.integer(rev$n_u_rev) != m1)
      stop(sprintf(paste("bayrjmcmc: move %s -> %s declares u of length %d",
                         "and a reverse u of length %d, but the reverse",
                         "move declares %d and %d"),
                   mv$frm, mv$to, m1, m2,
                   as.integer(rev$n_u), as.integer(rev$n_u_rev)))
  }
  by_pair
}

#' rj_log_acceptance
#'
#' A step of the bayrjmcmc_native implementation. Called by \code{reversible_jump_mcmc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param logpost_from Numeric; combined arithmetically in the body.
#' @param logpost_to Numeric; combined arithmetically in the body.
#' @param log_j_from Numeric; combined arithmetically in the body.
#' @param log_j_to Numeric; combined arithmetically in the body.
#' @param logq_u Numeric; combined arithmetically in the body.
#' @param logq_u_rev Numeric; combined arithmetically in the body.
#' @param log_jacobian Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
rj_log_acceptance <- function(logpost_from, logpost_to, log_j_from,
                              log_j_to, logq_u, logq_u_rev, log_jacobian) {
  (logpost_to - logpost_from) + (log_j_to - log_j_from) +
    (logq_u_rev - logq_u) + log_jacobian
}

# --------------------------------------------------------------------------
# symmetric Gaussian random walk
# --------------------------------------------------------------------------

#' .rw_within
#'
#' A step of the bayrjmcmc_native implementation. Called by \code{reversible_jump_mcmc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param theta A vector; its length is taken and its elements indexed.
#' @param uni Accepted by the signature and not used anywhere in the body.
#' @param scale Numeric; combined arithmetically in the body.
#' @return The value of \code{out}, as built in the body.
#' @export
.rw_within <- function(theta, uni, scale) {
  out <- numeric(length(theta))
  for (i in seq_along(theta)) {
    u1 <- uni(); u2 <- uni()
    z <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
    out[i] <- theta[i] + scale * z
  }
  out
}

# --------------------------------------------------------------------------
# general reversible jump sampler
# --------------------------------------------------------------------------

#' reversible_jump_mcmc
#'
#' A step of the bayrjmcmc_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param models A vector; its length is taken and its elements indexed.
#' @param moves Passed to \code{check_dimension_matching}.
#' @param init_model Passed to \code{\%in\%}.
#' @param init_theta Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{c()}.
#' @param n_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{10000L}.
#' @param burn_in Numeric; combined arithmetically in the body. Defaults to \code{0L}.
#' @param thin Numeric; combined arithmetically in the body. Defaults to \code{1L}.
#' @param seed Passed to \code{.unif_stream}. Defaults to \code{0L}.
#' @param within Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param within_scale Passed to \code{.rw_within}. Defaults to \code{0.5}.
#' @param within_weight Carried through into a list the body builds. Defaults to \code{1}.
#' @param move_weight Defaults to \code{1}.
#' @param jacobian One of \code{"analytic"}, \code{"numeric"}. Defaults to \code{"analytic"}.
#' @param keep_chain A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{model_freq}, \code{visits}, \code{n_kept}, \code{accept}, \code{tried}, \code{chain}, \code{jacobian}, \code{method}, \code{note}.
#' @export
reversible_jump_mcmc <- function(models, moves, init_model, init_theta = c(),
                                 n_iter = 10000L, burn_in = 0L, thin = 1L,
                                 seed = 0L, within = NULL,
                                 within_scale = 0.5, within_weight = 1,
                                 move_weight = 1, jacobian = "analytic",
                                 keep_chain = TRUE) {
  if (!(jacobian %in% .JACOBIAN_ROUTES))
    stop("bayrjmcmc: jacobian must be one of analytic, numeric")
  by_pair <- check_dimension_matching(models, moves)
  if (!(init_model %in% names(models)))
    stop(sprintf("bayrjmcmc: init_model %s is not a model", init_model))
  n_iter <- as.integer(n_iter)
  burn_in <- as.integer(burn_in)
  thin <- as.integer(thin)
  if (n_iter < 1) stop("bayrjmcmc: n_iter must be at least 1")
  if (burn_in < 0 || burn_in >= n_iter)
    stop("bayrjmcmc: burn_in must be in [0, n_iter)")
  if (thin < 1) stop("bayrjmcmc: thin must be at least 1")

  theta <- as.numeric(init_theta)
  if (length(theta) != as.integer(models[[init_model]]$dim))
    stop(sprintf(paste("bayrjmcmc: init_theta has %d values but model %s",
                       "has dim %d"),
                 length(theta), init_model,
                 as.integer(models[[init_model]]$dim)))

  avail <- list()
  for (name in names(models)) {
    opts <- list(list(kind = "within", mv = NULL, w = within_weight))
    for (mv in moves) if (mv$frm == name) {
      opts[[length(opts) + 1L]] <- list(kind = "jump", mv = mv,
                                       w = if (is.null(mv$weight)) move_weight
                                           else mv$weight)
    }
    tot <- sum(sapply(opts, function(o) o$w))
    if (tot <= 0)
      stop(sprintf("bayrjmcmc: model %s has no move with positive weight",
                   name))
    avail[[name]] <- list(opts = opts, tot = tot)
  }

  uni <- .unif_stream(seed)
  cur <- init_model
  logp <- as.numeric(models[[cur]]$logpost(theta))
  visits <- setNames(rep(0L, length(models)), names(models))
  tried <- list(); accepted <- list()
  chain <- list()

  for (it in seq_len(n_iter) - 1L) {
    opts <- avail[[cur]]$opts
    tot <- avail[[cur]]$tot
    pick <- uni() * tot
    acc_ <- 0
    chosen <- opts[[length(opts)]]
    for (o in opts) {
      acc_ <- acc_ + o$w
      if (pick < acc_) { chosen <- o; break }
    }

    if (chosen$kind == "within") {
      label <- paste0("within:", cur)
      tried[[label]] <- if (is.null(tried[[label]])) 1L else tried[[label]] + 1L
      if (length(theta) > 0) {
        if (!is.null(within) && !is.null(within[[cur]])) {
          out <- within[[cur]](theta, uni)
          prop <- as.numeric(out[[1]])
          log_ratio <- as.numeric(out[[2]])
        } else {
          prop <- .rw_within(theta, uni, within_scale)
          log_ratio <- 0
        }
        lp_new <- as.numeric(models[[cur]]$logpost(prop))
        if (log(uni()) < (lp_new - logp) + log_ratio) {
          theta <- prop; logp <- lp_new
          accepted[[label]] <- if (is.null(accepted[[label]])) 1L
                               else accepted[[label]] + 1L
        }
      }
    } else {
      mv <- chosen$mv
      label <- if (is.null(mv$name)) paste(mv$frm, mv$to, sep = "->") else mv$name
      tried[[label]] <- if (is.null(tried[[label]])) 1L else tried[[label]] + 1L
      u <- as.numeric(mv$propose(theta, uni))
      if (length(u) != as.integer(mv$n_u))
        stop(sprintf(paste("bayrjmcmc: move %s proposed %d values of u",
                           "but declares n_u = %d"),
                     label, length(u), as.integer(mv$n_u)))
      tr <- mv$transform(theta, u)
      theta2 <- as.numeric(tr[[1]])
      u2 <- as.numeric(tr[[2]])
      dim2 <- as.integer(models[[mv$to]]$dim)
      if (length(theta2) != dim2 || length(u2) != as.integer(mv$n_u_rev))
        stop(sprintf(paste("bayrjmcmc: move %s produced theta of length %d",
                           "and u2 of length %d; the model has dim %d and",
                           "the move declares n_u_rev = %d"),
                     label, length(theta2), length(u2), dim2,
                     as.integer(mv$n_u_rev)))
      if (jacobian == "numeric" || is.null(mv$logjac)) {
        if (jacobian == "analytic" && is.null(mv$logjac)) {
          logjac <- 0
        } else {
          n_from <- length(theta)
          tf <- mv$transform
          flat <- function(z) {
            ab <- tf(z[1:n_from], z[(n_from + 1):length(z)])
            c(as.numeric(ab[[1]]), as.numeric(ab[[2]]))
          }
          logjac <- numeric_log_jacobian(flat, c(theta, u))
        }
      } else {
        logjac <- as.numeric(mv$logjac(theta, u, theta2, u2))
      }
      rev <- by_pair[[paste(mv$to, mv$frm, sep = "->")]]
      opts2 <- avail[[mv$to]]$opts; tot2 <- avail[[mv$to]]$tot
      w_rev <- if (is.null(rev$weight)) move_weight else rev$weight
      log_j_from <- log(chosen$w) - log(tot)
      log_j_to <- log(w_rev) - log(tot2)
      logq_u <- if (is.null(mv$logq)) 0 else as.numeric(mv$logq(theta, u))
      logq_rev <- if (is.null(mv$logq_rev)) 0
                  else as.numeric(mv$logq_rev(theta2, u2))
      lp_new <- as.numeric(models[[mv$to]]$logpost(theta2))
      log_alpha <- rj_log_acceptance(logp, lp_new, log_j_from, log_j_to,
                                     logq_u, logq_rev, logjac)
      if (log(uni()) < log_alpha) {
        cur <- mv$to; theta <- theta2; logp <- lp_new
        accepted[[label]] <- if (is.null(accepted[[label]])) 1L
                             else accepted[[label]] + 1L
      }
    }
    if (it >= burn_in) {
      visits[[cur]] <- visits[[cur]] + 1L
      if (keep_chain && ((it - burn_in) %% thin) == 0)
        chain[[length(chain) + 1L]] <- list(model = cur, theta = theta)
    }
  }
  kept <- sum(unlist(visits))
  freq <- lapply(visits, function(v) v / kept)
  rates <- list()
  for (k in names(tried)) {
    v <- tried[[k]]
    rates[[k]] <- if (is.null(accepted[[k]])) 0 else accepted[[k]] / v
  }
  list(model_freq = freq, visits = visits, n_kept = kept, accept = rates,
       tried = tried, chain = chain, jacobian = jacobian,
       method = paste("reversible-jump MCMC, Green (1995) eq. 7;",
                      "hybrid sampler with detailed balance within",
                      "each move type"),
       note = paste("dimension matching n1 + m1 == n2 + m2 is enforced",
                    "for every move; j(x) is computed from the move",
                    "weights available in each model, in both directions,",
                    "because eq. 7 needs it at x' as well as x"))
}

bayrjmcmc <- reversible_jump_mcmc

# --------------------------------------------------------------------------
# §4: step-function rate for a point process
# --------------------------------------------------------------------------

#' step_function_loglik
#'
#' A step of the bayrjmcmc_native implementation. Called by \code{changepoint_rjmcmc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y See Usage.
#' @param s Coerced to numeric by the body, with \code{as.numeric}.
#' @param h A vector; its length is taken and its elements indexed.
#' @param L Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{out}, as built in the body.
#' @export
step_function_loglik <- function(y, s, h, L) {
  edges <- c(0, as.numeric(s), as.numeric(L))
  if (length(h) != length(edges) - 1)
    stop(sprintf("bayrjmcmc: %d heights for %d intervals",
                 length(h), length(edges) - 1))
  counts <- rep(0L, length(h))
  for (v in y) {
    v <- as.numeric(v)
    if (v < 0 || v > as.numeric(L))
      stop(sprintf("bayrjmcmc: point %g lies outside [0, %g]", v, as.numeric(L)))
    j <- 1L
    while (j + 1 < length(edges) - 1L && v >= edges[j + 1]) j <- j + 1L
    counts[j] <- counts[j] + 1L
  }
  out <- 0
  for (j in seq_along(h)) {
    hj <- as.numeric(h[j])
    if (hj <= 0) return(-Inf)
    if (counts[j] > 0) out <- out + counts[j] * log(hj)
    out <- out - hj * (edges[j + 1] - edges[j])
  }
  out
}

#' changepoint_move_probabilities
#'
#' A step of the bayrjmcmc_native implementation. Called by \code{changepoint_rjmcmc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param lam Numeric; combined arithmetically in the body.
#' @param k_max Numeric; combined arithmetically in the body.
#' @param cap Numeric; combined arithmetically in the body. Defaults to \code{0.9}.
#' @return A list with \code{eta}, \code{pi}, \code{b}, \code{d}, \code{c}.
#' @export
changepoint_move_probabilities <- function(lam, k_max, cap = 0.9) {
  lam <- as.numeric(lam)
  k_max <- as.integer(k_max)
  if (lam <= 0) stop("bayrjmcmc: lam must be positive")
  if (k_max < 1) stop("bayrjmcmc: k_max must be at least 1")
  raw_b <- vapply(0:k_max, function(k) min(1, lam / (k + 1)), numeric(1))
  raw_d <- vapply(0:k_max, function(k) if (k == 0) 0 else min(1, k / lam),
                  numeric(1))
  raw_b[k_max + 1] <- 0
  worst <- max(raw_b + raw_d)
  c_ <- if (worst > 0) cap / worst else cap
  b <- c_ * raw_b
  d <- c_ * raw_d
  eta <- numeric(k_max + 1)
  pi_ <- numeric(k_max + 1)
  for (k in 0:k_max) {
    rest <- 1 - b[k + 1] - d[k + 1]
    if (k == 0) { eta[k + 1] <- rest; pi_[k + 1] <- 0 }
    else { eta[k + 1] <- 0.5 * rest; pi_[k + 1] <- 0.5 * rest }
  }
  list(eta = eta, pi = pi_, b = b, d = d, c = c_)
}

#' birth_split_heights
#'
#' A step of the bayrjmcmc_native implementation. Called by \code{changepoint_rjmcmc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param h_j Coerced to numeric by the body, with \code{as.numeric}.
#' @param u Coerced to numeric by the body, with \code{as.numeric}.
#' @param s_left Coerced to numeric by the body, with \code{as.numeric}.
#' @param s_star Coerced to numeric by the body, with \code{as.numeric}.
#' @param s_right Coerced to numeric by the body, with \code{as.numeric}.
#' @return A vector, from \code{c}.
#' @export
birth_split_heights <- function(h_j, u, s_left, s_star, s_right) {
  span <- as.numeric(s_right) - as.numeric(s_left)
  if (span <= 0) stop("bayrjmcmc: empty interval in a birth move")
  w1 <- (as.numeric(s_star) - as.numeric(s_left)) / span
  r <- as.numeric(u) / (1 - as.numeric(u))
  lr <- log(r)
  hj <- as.numeric(h_j) * exp(-(1 - w1) * lr)
  hj1 <- as.numeric(h_j) * exp(w1 * lr)
  c(hj, hj1)
}

#' birth_log_jacobian
#'
#' A step of the bayrjmcmc_native implementation. Called by \code{changepoint_rjmcmc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param h_j Coerced to numeric by the body, with \code{as.numeric}.
#' @param h_new_left Coerced to numeric by the body, with \code{as.numeric}.
#' @param h_new_right Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
birth_log_jacobian <- function(h_j, h_new_left, h_new_right) {
  2 * log(as.numeric(h_new_left) + as.numeric(h_new_right)) -
    log(as.numeric(h_j))
}

#' .merge_height
#'
#' A step of the bayrjmcmc_native implementation. Called by \code{changepoint_rjmcmc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param s_left Coerced to numeric by the body, with \code{as.numeric}.
#' @param s_mid Coerced to numeric by the body, with \code{as.numeric}.
#' @param s_right Coerced to numeric by the body, with \code{as.numeric}.
#' @param h_left Numeric; passed to \code{log}.
#' @param h_right Numeric; passed to \code{log}.
#' @return A numeric value.
#' @export
.merge_height <- function(s_left, s_mid, s_right, h_left, h_right) {
  span <- as.numeric(s_right) - as.numeric(s_left)
  exp(((as.numeric(s_mid) - as.numeric(s_left)) * log(h_left) +
       (as.numeric(s_right) - as.numeric(s_mid)) * log(h_right)) / span)
}

#' .log_k_prior_ratio
#'
#' A step of the bayrjmcmc_native implementation. Called by \code{changepoint_rjmcmc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param lam Numeric; passed to \code{log}.
#' @param k Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.log_k_prior_ratio <- function(lam, k) {
  log(lam) - log(k + 1)
}

#' changepoint_rjmcmc
#'
#' A step of the bayrjmcmc_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{numeric(0)}.
#' @param L Numeric; passed to \code{log}. Defaults to \code{1}.
#' @param n_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{40000L}.
#' @param burn_in Numeric; combined arithmetically in the body. Defaults to \code{4000L}.
#' @param lam Passed to \code{.log_k_prior_ratio}. Defaults to \code{3}.
#' @param k_max Numeric; combined arithmetically in the body. Defaults to \code{30L}.
#' @param alpha Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param beta Numeric; passed to \code{log}. Defaults to \code{200}.
#' @param seed Passed to \code{.unif_stream}. Defaults to \code{0L}.
#' @param cap Passed to \code{changepoint_move_probabilities}. Defaults to \code{0.9}.
#' @param use_likelihood A flag; the body branches on it. Defaults to \code{TRUE}.
#' @param k_init A count; the body uses it as \code{seq_len(...)}. Defaults to \code{0L}.
#' @param thin Numeric; combined arithmetically in the body. Defaults to \code{1L}.
#' @param keep_chain A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return A list with \code{k_posterior}, \code{k_counts}, \code{k_mean}, \code{s}, \code{h}, \code{accept}, \code{tried}, \code{c}, \code{b}, \code{d}, \code{eta}, \code{pi}, \code{mean_s1_given_k1}, \code{var_s1_given_k1}, \code{mean_height}, \code{chain}, \code{n_kept}, \code{use_likelihood}, \code{method}, \code{note}.
#' @export
changepoint_rjmcmc <- function(y = numeric(0), L = 1, n_iter = 40000L,
                               burn_in = 4000L, lam = 3, k_max = 30L,
                               alpha = 1, beta = 200, seed = 0L, cap = 0.9,
                               use_likelihood = TRUE, k_init = 0L,
                               thin = 1L, keep_chain = FALSE) {
  L <- as.numeric(L)
  if (L <= 0) stop("bayrjmcmc: L must be positive")
  alpha <- as.numeric(alpha); beta <- as.numeric(beta)
  if (alpha <= 0 || beta <= 0)
    stop("bayrjmcmc: alpha and beta must be positive")
  n_iter <- as.integer(n_iter); burn_in <- as.integer(burn_in)
  if (n_iter < 1) stop("bayrjmcmc: n_iter must be at least 1")
  if (burn_in < 0 || burn_in >= n_iter)
    stop("bayrjmcmc: burn_in must be in [0, n_iter)")
  thin <- as.integer(thin)
  if (thin < 1) stop("bayrjmcmc: thin must be at least 1")
  y <- as.numeric(y)
  for (v in y)
    if (v < 0 || v > L)
      stop(sprintf("bayrjmcmc: point %g lies outside [0, %g]", v, L))
  mp <- changepoint_move_probabilities(lam, k_max, cap = cap)
  eta <- mp$eta; pi_ <- mp$pi; b <- mp$b; d <- mp$d; c_ <- mp$c
  k_init <- as.integer(k_init)
  if (k_init < 0 || k_init > k_max)
    stop("bayrjmcmc: k_init must be in [0, k_max]")

  uni <- .unif_stream(seed)
  s <- vapply(seq_len(k_init), function(i) L * i / (k_init + 1), numeric(1))
  h <- rep(alpha / beta, k_init + 1)
  loglik_fn <- function(s_, h_) {
    if (use_likelihood) step_function_loglik(y, s_, h_, L) else 0
  }
  cur_ll <- loglik_fn(s, h)
  counts <- rep(0L, k_max + 1)
  tried <- list(height = 0L, position = 0L, birth = 0L, death = 0L)
  acc <- list(height = 0L, position = 0L, birth = 0L, death = 0L)
  chain <- list()
  s1_sum <- 0; s1_sq <- 0; s1_n <- 0
  h_sum <- 0; h_n <- 0L

  for (it in seq_len(n_iter) - 1L) {
    k <- length(s)
    pick <- uni()
    edges <- c(0, s, L)
    if (pick < b[k + 1]) {
      # birth
      tried$birth <- tried$birth + 1L
      s_star <- L * uni()
      j <- 1L
      while (j + 1 < length(edges) - 1L && s_star >= edges[j + 1]) j <- j + 1L
      u <- uni()
      hh <- birth_split_heights(h[j], u, edges[j], s_star, edges[j + 1])
      hl <- hh[1]; hr <- hh[2]
      s_new <- c(s[1:(j - 1)], s_star, s[j:length(s)])
      h_new <- c(h[1:(j - 1)], hl, hr, h[(j + 1):length(h)])
      new_ll <- loglik_fn(s_new, h_new)
      log_prior <- (.log_k_prior_ratio(lam, k)
        + log(2 * (k + 1) * (2 * k + 3)) - 2 * log(L)
        + log((s_star - edges[j]) * (edges[j + 1] - s_star) /
              (edges[j + 1] - edges[j]))
        + alpha * log(beta) - lgamma(alpha)
        + (alpha - 1) * log(hl * hr / h[j])
        - beta * (hl + hr - h[j]))
      log_prop <- log(d[k + 1]) + log(L) - log(b[k + 1]) - log(k + 1)
      log_jac <- birth_log_jacobian(h[j], hl, hr)
      log_alpha <- (new_ll - cur_ll) + log_prior + log_prop + log_jac
      if ((k + 1) <= k_max && log(uni()) < log_alpha) {
        s <- s_new; h <- h_new; cur_ll <- new_ll
        acc$birth <- acc$birth + 1L
      }
    } else if (pick < b[k + 1] + d[k + 1]) {
      # death
      tried$death <- tried$death + 1L
      i <- as.integer(uni() * k)
      if (i >= k) i <- k - 1L
      j <- i + 1L
      s_new <- c(s[1:i], s[(i + 2):length(s)])
      h_merged <- .merge_height(edges[i + 1], edges[i + 2], edges[i + 3],
                                 h[j], h[j + 1])
      h_new <- c(h[1:i], h_merged, h[(j + 2):length(h)])
      new_ll <- loglik_fn(s_new, h_new)
      kk <- k - 1
      s_star <- edges[i + 2]
      left <- edges[i + 1]; right <- edges[i + 3]
      log_prior <- (.log_k_prior_ratio(lam, kk)
        + log(2 * (kk + 1) * (2 * kk + 3)) - 2 * log(L)
        + log((s_star - left) * (right - s_star) / (right - left))
        + alpha * log(beta) - lgamma(alpha)
        + (alpha - 1) * log(h[j] * h[j + 1] / h_merged)
        - beta * (h[j] + h[j + 1] - h_merged))
      log_prop <- log(d[kk + 1]) + log(L) - log(b[kk + 1]) - log(kk + 1)
      log_jac <- birth_log_jacobian(h_merged, h[j], h[j + 1])
      log_alpha <- -((cur_ll - new_ll) + log_prior + log_prop + log_jac)
      if (log(uni()) < log_alpha) {
        s <- s_new; h <- h_new; cur_ll <- new_ll
        acc$death <- acc$death + 1L
      }
    } else if (pick < b[k + 1] + d[k + 1] + eta[k + 1]) {
      # height
      tried$height <- tried$height + 1L
      j <- as.integer(uni() * (k + 1)); if (j > k) j <- k
      hj <- h[j + 1] * exp(uni() - 0.5)
      h_new <- h
      h_new[j + 1] <- hj
      new_ll <- loglik_fn(s, h_new)
      log_alpha <- (new_ll - cur_ll + alpha * log(hj / h[j + 1]) -
                    beta * (hj - h[j + 1]))
      if (log(uni()) < log_alpha) {
        h <- h_new; cur_ll <- new_ll
        acc$height <- acc$height + 1L
      }
    } else if (k >= 1) {
      # position
      tried$position <- tried$position + 1L
      j <- as.integer(uni() * k); if (j >= k) j <- k - 1L
      lo <- edges[j + 1]; hi <- edges[j + 3]
      s_star <- lo + (hi - lo) * uni()
      s_new <- s
      s_new[j + 1] <- s_star
      new_ll <- loglik_fn(s_new, h)
      log_alpha <- (new_ll - cur_ll +
        log((hi - s_star) * (s_star - lo) / ((hi - s[j + 1]) * (s[j + 1] - lo))))
      if (log(uni()) < log_alpha) {
        s <- s_new; cur_ll <- new_ll
        acc$position <- acc$position + 1L
      }
    }
    if (it >= burn_in) {
      counts[length(s) + 1] <- counts[length(s) + 1] + 1L
      if (length(s) == 1) {
        s1_sum <- s1_sum + s[1]
        s1_sq <- s1_sq + s[1]^2
        s1_n <- s1_n + 1L
      }
      for (v in h) { h_sum <- h_sum + v; h_n <- h_n + 1L }
      if (keep_chain && ((it - burn_in) %% thin) == 0)
        chain[[length(chain) + 1L]] <- list(s = s, h = h)
    }
  }
  kept <- sum(counts)
  k_post <- counts / kept
  rates <- list()
  for (key in names(tried))
    rates[[key]] <- if (tried[[key]] > 0) acc[[key]] / tried[[key]] else 0
  list(k_posterior = k_post, k_counts = counts,
       k_mean = sum((0:k_max) * k_post),
       s = s, h = h, accept = rates, tried = tried, c = c_,
       b = b, d = d, eta = eta, pi = pi_,
       mean_s1_given_k1 = if (s1_n > 0) s1_sum / s1_n else NaN,
       var_s1_given_k1 = if (s1_n > 1)
         s1_sq / s1_n - (s1_sum / s1_n)^2 else NaN,
       mean_height = if (h_n > 0) h_sum / h_n else NaN,
       chain = chain, n_kept = kept, use_likelihood = use_likelihood,
       method = paste("Green (1995) §4 change-point sampler: height,",
                      "position, birth and death moves on a step-function",
                      "rate, with the §4-3 acceptance ratios"),
       note = paste("with use_likelihood=False the target is the prior,",
                    "so k must come back Poisson(lam) truncated at k_max",
                    "and the heights Gamma(alpha, beta)"))
}

# house entry point: the package exports one morie_<module>
morie_bayrjmcmc <- reversible_jump_mcmc
