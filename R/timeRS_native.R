# morie.fn -- function file (rootcoder007/morie)
# timeSVD++: preferences drift, and drift is not noise.
#
# Two temporal effects in the Netflix data motivate the whole model, and
# both are measured rather than posited: the mean rating **jumped** from
# about 3.4 to above 3.6 stars in early 2004, and ratings **rise with a
# movie's age** -- older films score higher than newer ones.
#
# **Why the usual remedies fail here.** Concept-drift work tracks a
# single concept; here many characteristics of many users and items
# shift at once. A time window or instance decay would discard the older
# data, and the paper is explicit that this loses too many signals. The
# alternative is to *model* the drift and keep every instance.
#
# **Different effects drift at different rates, so they get different
# treatments.**
#
# * **Item bias** moves slowly -- a film's perception changes over
#   months -- so it is captured with **time bins**:
#   :math:`b_i(t) = b_i + b_{i,\mathrm{Bin}(t)}`.
# * **User bias** can shift gradually and also spike on a single day, so
#   it gets a smooth term plus a per-day term:
#   :math:`b_u(t) = b_u + \alpha_u\,\mathrm{dev}_u(t) + b_{u,t}`.
#
# **The deviation function is the mechanism.** With :math:`t_u` the
# user's mean rating date,
#
# .. math:: \mathrm{dev}_u(t) = \mathrm{sign}(t - t_u)\,
#           |t - t_u|^{\beta},\qquad \beta = 0.4,
#
# so the effect is **signed** -- before and after the user's centre pull
# in opposite directions -- and **concave**, so a rating 400 days out is
# not four times the deviation of one 100 days out. :math:`\beta` was
# set by cross-validation, not derived, and ``deviation`` exposes it so
# the concavity can be checked instead of assumed.
#
# **A single-day term is not overfitting, it is the point.** Ratings
# made in one session share a mood; :math:`b_{u,t}` absorbs that so it
# does not contaminate the long-run parameters.
#
# References
# ----------
# Koren, Y. (2010) "Collaborative filtering with temporal dynamics",
# *Communications of the ACM* 53(4), 89-97,
# doi:10.1145/1721654.1721677. [PDF supplied by Vee.] The two measured
# effects in the Netflix data -- the abrupt shift of rating scale in
# early 2004 from around 3.4 to above 3.6 stars, and ratings increasing
# with movie age; the argument that this differs from concept drift
# because many characteristics shift simultaneously and that classical
# time-window or instance-decay approaches cannot work as they lose too
# many signals when discarding data instances; the time deviation
# dev_u(t) = sign(t - t_u) |t - t_u|^beta with t_u the user's mean
# rating date and beta = 0.4 set by cross-validation; the resulting
# time-dependent user bias; item bias in time bins; and the per-day user
# term.
#
# Koren, Y. (2009) "Collaborative Filtering with Temporal Dynamics",
# *KDD '09*, 447-456, doi:10.1145/1557019.1557072. The full conference
# treatment, including the spline alternative to the linear drift.
#
# Koren, Y. (2008) "Factorization Meets the Neighborhood", *KDD '08*,
# 426-434, doi:10.1145/1401890.1401944. The SVD++ base this extends;
# implemented in :mod:`svdpp`.

.timeRS_EPS <- 1e-12
.timeRS_BETA <- 0.4

# Private helpers (prefixed .timeRS_ to avoid collisions in the shared R/ env)

.timeRS_deviation <- function(t, t_user, beta = .timeRS_BETA) {
  d <- as.numeric(t) - as.numeric(t_user)
  b <- as.numeric(beta)
  if (b <= 0.0) {
    stop("timeRS: beta must be positive")
  }
  sgn <- if (d > 0) 1.0 else if (d < 0) -1.0 else 0.0
  sgn * (abs(d) ^ b)
}

.timeRS_time_bin <- function(t, bin_days = 70, n_bins = 30) {
  w <- as.integer(bin_days)
  if (w < 1L) {
    stop("timeRS: the bin width must be positive")
  }
  idx <- as.integer(max(as.numeric(t), 0.0) %/% w)
  nb <- as.integer(n_bins)
  min(idx, nb - 1L)
}

.timeRS_user_bias <- function(b_u, alpha_u, t, t_user, per_day = NULL,
                              beta = .timeRS_BETA) {
  dev <- .timeRS_deviation(t, t_user, beta)
  day <- if (is.null(per_day)) {
    0.0
  } else {
    val <- per_day[[as.character(as.integer(t))]]
    if (is.null(val)) 0.0 else as.numeric(val)
  }
  list(
    bias = as.numeric(b_u) + as.numeric(alpha_u) * dev + day,
    deviation = dev,
    per_day = day,
    note = paste(
      "a single-day term absorbs session mood, so it ",
      "does not contaminate the long-run parameters",
      sep = ""
    )
  )
}

.timeRS_item_bias <- function(b_i, bins, t, bin_days = 70, n_bins = 30) {
  idx <- .timeRS_time_bin(t, bin_days, n_bins)
  nb <- length(bins)
  bias <- if (idx < nb) {
    as.numeric(b_i) + as.numeric(bins[[idx + 1L]])
  } else {
    as.numeric(b_i)
  }
  list(bias = bias, bin = idx)
}

.timeRS_predict_time <- function(mu, b_u, alpha_u, t_user, b_i, item_bins, t,
                                 p_u = NULL, q_i = NULL, per_day = NULL,
                                 bin_days = 70, beta = .timeRS_BETA) {
  ub <- .timeRS_user_bias(b_u, alpha_u, t, t_user, per_day, beta)
  ib <- .timeRS_item_bias(b_i, item_bins, t, bin_days, length(item_bins))
  inner <- 0.0
  if (!is.null(p_u) && !is.null(q_i)) {
    p <- as.numeric(p_u)
    q <- as.numeric(q_i)
    if (length(p) != length(q)) {
      stop("timeRS: the factors differ in width")
    }
    inner <- sum(p * q)
  }
  list(
    prediction = as.numeric(mu) + ub$bias + ib$bias + inner,
    user_bias  = ub$bias,
    item_bias  = ib$bias,
    deviation  = ub$deviation,
    bin        = ib$bin
  )
}

.timeRS_fit_time_bias <- function(ratings, n_users, n_items, bin_days = 70,
                                  n_bins = 30, epochs = 40, lr = 0.005,
                                  reg = 0.02, beta = .timeRS_BETA) {
  # Normalise ratings to a list of (u, i, t, r) records
  if (is.matrix(ratings)) {
    R <- lapply(seq_len(nrow(ratings)), function(k) list(
      u = as.integer(ratings[k, 1L]),
      i = as.integer(ratings[k, 2L]),
      t = as.numeric(ratings[k, 3L]),
      r = as.numeric(ratings[k, 4L])
    ))
  } else {
    R <- lapply(ratings, function(x) list(
      u = as.integer(x[[1L]]),
      i = as.integer(x[[2L]]),
      t = as.numeric(x[[3L]]),
      r = as.numeric(x[[4L]])
    ))
  }

  if (length(R) == 0L) {
    stop("timeRS: no ratings given")
  }

  nu <- as.integer(n_users)
  ni <- as.integer(n_items)
  nb <- as.integer(n_bins)

  mu <- sum(vapply(R, function(x) x$r, numeric(1))) / length(R)

  # t_user: mean rating date per user (named numeric vector)
  days <- list()
  for (entry in R) {
    key <- as.character(entry$u)
    if (is.null(days[[key]])) {
      days[[key]] <- entry$t
    } else {
      days[[key]] <- c(days[[key]], entry$t)
    }
  }
  t_user <- vapply(days, function(v) sum(v) / length(v), numeric(1))

  bu   <- rep(0.0, nu)
  al   <- rep(0.0, nu)
  bi   <- rep(0.0, ni)
  bins <- matrix(0.0, nrow = ni, ncol = nb)
  hist <- numeric(0)

  n_eps <- as.integer(epochs)
  for (ep in seq_len(n_eps)) {
    se <- 0.0
    for (entry in R) {
      u <- entry$u
      i <- entry$i
      t <- entry$t
      r <- entry$r

      tu <- t_user[[as.character(u)]]
      if (is.null(tu)) tu <- t  # safety fallback (cannot occur in well-formed data)

      dev <- .timeRS_deviation(t, tu, beta)
      idx <- .timeRS_time_bin(t, bin_days, n_bins)

      # Python uses 0-based indices; R is 1-based
      ui <- u + 1L
      ii <- i + 1L
      bi_idx <- idx + 1L

      pred <- mu + bu[ui] + al[ui] * dev + bi[ii] + bins[ii, bi_idx]
      e <- r - pred
      se <- se + e * e

      bu[ui] <- bu[ui] + lr * (e - reg * bu[ui])
      al[ui] <- al[ui] + lr * (e * dev - reg * al[ui])
      bi[ii] <- bi[ii] + lr * (e - reg * bi[ii])
      bins[ii, bi_idx] <- bins[ii, bi_idx] +
        lr * (e - reg * bins[ii, bi_idx])
    }
    hist <- c(hist, sqrt(se / length(R)))
  }

  list(
    estimate      = hist[length(hist)],
    rmse          = hist[length(hist)],
    rmse_history  = hist,
    mu            = mu,
    b_user        = bu,
    alpha_user    = al,
    b_item        = bi,
    item_bins     = bins,
    t_user        = t_user,
    beta          = as.numeric(beta),
    n_instances   = length(R),
    method        = "time-dependent biases; Koren (2010) eq. (8)",
    note          = paste(
      "every instance is kept; windows and decay would ",
      "discard the signals this models",
      sep = ""
    )
  )
}

# Public API ----------------------------------------------------------------

# Main entry point: fit the time-dependent biases via SGD.
#' Main entry point: fit the time-dependent biases via SGD
#'
#' Part of the timeRS_native implementation; see the file header for the
#' source it follows.
#'
#' @param ratings See Usage.
#' @param n_users See Usage.
#' @param n_items See Usage.
#' @param bin_days Defaults to \code{70}.
#' @param n_bins Defaults to \code{30}.
#' @param epochs Defaults to \code{40}.
#' @param lr Defaults to \code{0.005}.
#' @param reg Defaults to \code{0.02}.
#' @param beta Defaults to \code{.timeRS_BETA}.
#' @return The value of \code{.timeRS_fit_time_bias}.
#' @export
morie_timeRS <- function(ratings, n_users, n_items, bin_days = 70,
                         n_bins = 30, epochs = 40, lr = 0.005, reg = 0.02,
                         beta = .timeRS_BETA) {
  .timeRS_fit_time_bias(ratings, n_users, n_items, bin_days, n_bins,
                        epochs, lr, reg, beta)
}

# Exported helpers
#' Exported helpers
#'
#' Part of the timeRS_native implementation; see the file header for the
#' source it follows.
#'
#' @param t See Usage.
#' @param t_user See Usage.
#' @param beta Defaults to \code{.timeRS_BETA}.
#' @return The value of \code{.timeRS_deviation}.
#' @export
morie_timeRS_deviation <- function(t, t_user, beta = .timeRS_BETA) {
  .timeRS_deviation(t, t_user, beta)
}

#' morie_timeRS_time_bin
#'
#' Part of the timeRS_native implementation; see the file header for the
#' source it follows.
#'
#' @param t See Usage.
#' @param bin_days Defaults to \code{70}.
#' @param n_bins Defaults to \code{30}.
#' @return The value of \code{.timeRS_time_bin}.
#' @export
morie_timeRS_time_bin <- function(t, bin_days = 70, n_bins = 30) {
  .timeRS_time_bin(t, bin_days, n_bins)
}

#' morie_timeRS_user_bias
#'
#' Part of the timeRS_native implementation; see the file header for the
#' source it follows.
#'
#' @param b_u See Usage.
#' @param alpha_u See Usage.
#' @param t See Usage.
#' @param t_user See Usage.
#' @param per_day Defaults to \code{NULL}.
#' @param beta Defaults to \code{.timeRS_BETA}.
#' @return The value of \code{.timeRS_user_bias}.
#' @export
morie_timeRS_user_bias <- function(b_u, alpha_u, t, t_user, per_day = NULL,
                                   beta = .timeRS_BETA) {
  .timeRS_user_bias(b_u, alpha_u, t, t_user, per_day, beta)
}

#' morie_timeRS_item_bias
#'
#' Part of the timeRS_native implementation; see the file header for the
#' source it follows.
#'
#' @param b_i See Usage.
#' @param bins See Usage.
#' @param t See Usage.
#' @param bin_days Defaults to \code{70}.
#' @param n_bins Defaults to \code{30}.
#' @return The value of \code{.timeRS_item_bias}.
#' @export
morie_timeRS_item_bias <- function(b_i, bins, t, bin_days = 70, n_bins = 30) {
  .timeRS_item_bias(b_i, bins, t, bin_days, n_bins)
}

#' morie_timeRS_predict_time
#'
#' Part of the timeRS_native implementation; see the file header for the
#' source it follows.
#'
#' @param mu See Usage.
#' @param b_u See Usage.
#' @param alpha_u See Usage.
#' @param t_user See Usage.
#' @param b_i See Usage.
#' @param item_bins See Usage.
#' @param t See Usage.
#' @param p_u Defaults to \code{NULL}.
#' @param q_i Defaults to \code{NULL}.
#' @param per_day Defaults to \code{NULL}.
#' @param bin_days Defaults to \code{70}.
#' @param beta Defaults to \code{.timeRS_BETA}.
#' @return The value of \code{.timeRS_predict_time}.
#' @export
morie_timeRS_predict_time <- function(mu, b_u, alpha_u, t_user, b_i, item_bins,
                                      t, p_u = NULL, q_i = NULL,
                                      per_day = NULL, bin_days = 70,
                                      beta = .timeRS_BETA) {
  .timeRS_predict_time(mu, b_u, alpha_u, t_user, b_i, item_bins, t,
                       p_u, q_i, per_day, bin_days, beta)
}

#' morie_timeRS_fit_time_bias
#'
#' Part of the timeRS_native implementation; see the file header for the
#' source it follows.
#'
#' @param ratings See Usage.
#' @param n_users See Usage.
#' @param n_items See Usage.
#' @param bin_days Defaults to \code{70}.
#' @param n_bins Defaults to \code{30}.
#' @param epochs Defaults to \code{40}.
#' @param lr Defaults to \code{0.005}.
#' @param reg Defaults to \code{0.02}.
#' @param beta Defaults to \code{.timeRS_BETA}.
#' @return The value of \code{.timeRS_fit_time_bias}.
#' @export
morie_timeRS_fit_time_bias <- function(ratings, n_users, n_items, bin_days = 70,
                                       n_bins = 30, epochs = 40, lr = 0.005,
                                       reg = 0.02, beta = .timeRS_BETA) {
  .timeRS_fit_time_bias(ratings, n_users, n_items, bin_days, n_bins,
                        epochs, lr, reg, beta)
}

# Compact aliases per ledger/NAMING.md
#' Compact aliases per ledger/NAMING.md
#'
#' Part of the timeRS_native implementation; see the file header for the
#' source it follows.
#'
#' @param ratings See Usage.
#' @param n_users See Usage.
#' @param n_items See Usage.
#' @param bin_days Defaults to \code{70}.
#' @param n_bins Defaults to \code{30}.
#' @param epochs Defaults to \code{40}.
#' @param lr Defaults to \code{0.005}.
#' @param reg Defaults to \code{0.02}.
#' @param beta Defaults to \code{.timeRS_BETA}.
#' @return The value of \code{.timeRS_fit_time_bias}.
#' @export
morie_timeRS_timesvdpp <- function(ratings, n_users, n_items, bin_days = 70,
                                   n_bins = 30, epochs = 40, lr = 0.005,
                                   reg = 0.02, beta = .timeRS_BETA) {
  .timeRS_fit_time_bias(ratings, n_users, n_items, bin_days, n_bins,
                        epochs, lr, reg, beta)
}

#' morie_timeRS_timesvd
#'
#' Part of the timeRS_native implementation; see the file header for the
#' source it follows.
#'
#' @param ratings See Usage.
#' @param n_users See Usage.
#' @param n_items See Usage.
#' @param bin_days Defaults to \code{70}.
#' @param n_bins Defaults to \code{30}.
#' @param epochs Defaults to \code{40}.
#' @param lr Defaults to \code{0.005}.
#' @param reg Defaults to \code{0.02}.
#' @param beta Defaults to \code{.timeRS_BETA}.
#' @return The value of \code{.timeRS_fit_time_bias}.
#' @export
morie_timeRS_timesvd <- function(ratings, n_users, n_items, bin_days = 70,
                                 n_bins = 30, epochs = 40, lr = 0.005,
                                 reg = 0.02, beta = .timeRS_BETA) {
  .timeRS_fit_time_bias(ratings, n_users, n_items, bin_days, n_bins,
                        epochs, lr, reg, beta)
}

#' morie_timeRS_cheatsheet
#'
#' Part of the timeRS_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
morie_timeRS_cheatsheet <- function() {
  paste0(
    "timeRS: preferences DRIFT -- the Netflix mean rating ",
    "jumped 3.4 to 3.6 in early 2004 and ratings rise with ",
    "movie age. This is not ordinary concept drift (many ",
    "things shift at once), and windows or decay would discard ",
    "too much, so MODEL the drift and keep every instance. ",
    "Different effects, different rates: item bias in slow ",
    "TIME BINS (months), user bias smooth PLUS a per-day term ",
    "for session mood. The mechanism is dev_u(t) = ",
    "sign(t - t_u)|t - t_u|^0.4 -- SIGNED, so the two sides of ",
    "the user's centre pull oppositely, and CONCAVE, so 400 ",
    "days out is not 4x 100 days out. beta was cross-",
    "validated, not derived."
  )
}
