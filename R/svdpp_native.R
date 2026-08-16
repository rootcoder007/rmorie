# morie.fn -- function file (rootcoder007/morie)
# SVD++: which items a user rated is itself a signal.
# (R translation of the Python svdpp module)
#
# A latent factor model predicts r_ui = b_ui + q_i' p_u and learns
# p_u only from the ratings the user gave. A rating dataset carries
# a second, weaker signal: WHICH items the user chose to rate at
# all, regardless of the score. SVD++ adds that signal to the user
# factor INSIDE the inner product:
#
#   r_ui = b_ui + q_i' ( p_u + |N(u)|^{-1/2} sum_{j in N(u)} y_j )
#
# so it modifies the user's TASTE vector rather than adding a bias.
# The |N(u)|^{-1/2} normalisation is load-bearing: at exponent 0 a
# heavy rater's term swamps p_u; at -1 it becomes a mean and stops
# distinguishing a user with 5 ratings from one with 500.
#
# References
# ----------
# Koren, Y. (2008) "Factorization Meets the Neighborhood: a
# Multifaceted Collaborative Filtering Model", KDD '08, 426-434,
# doi:10.1145/1401890.1401944. Sec. 1 and 4, eq. (15).
#
# Koren, Y., Bell, R. & Volinsky, C. (2009) "Matrix Factorization
# Techniques for Recommender Systems", Computer 42(8), 30-37,
# doi:10.1109/MC.2009.263. The baseline decomposition mu + b_u + b_i.
#
# Hu, Y., Koren, Y. & Volinsky, C. (2008) "Collaborative Filtering for
# Implicit Feedback Datasets", ICDM 2008, 263-272,
# doi:10.1109/ICDM.2008.22. The purely implicit alternative.

.svdpp_eps <- 1e-12

#' .svdpp_baseline
#'
#' A step of the svdpp_native implementation. Called by \code{.svdpp_predict}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param mu See Usage.
#' @param b_user See Usage.
#' @param b_item See Usage.
#' @return A numeric value.
#' @export
.svdpp_baseline <- function(mu, b_user, b_item) {
  return(as.numeric(mu) + as.numeric(b_user) + as.numeric(b_item))
}

#' .svdpp_implicit_term
#'
#' A step of the svdpp_native implementation. Called by \code{.svdpp_predict}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param rated_items See Usage.
#' @param y A vector; its length is taken and its elements indexed.
#' @param exponent Defaults to \code{-0.5}.
#' @return A list with \code{term}, \code{n_rated}, \code{scale}, \code{exponent}, \code{raw_sum}.
#' @export
.svdpp_implicit_term <- function(rated_items, y, exponent = -0.5) {
  N <- as.list(rated_items)
  if (length(N) == 0) {
    width <- 0L
    if (length(y) > 0) {
      width <- length(y[[1]])
    }
    return(list(
      term = rep(0.0, width),
      n_rated = 0L,
      note = "a user with no ratings gets no implicit signal"
    ))
  }
  first_j <- as.character(N[[1]])
  d <- length(y[[first_j]])
  s <- numeric(d)
  for (j in N) {
    yj <- y[[as.character(j)]]
    for (a in seq_len(d)) {
      s[a] <- s[a] + as.numeric(yj[a])
    }
  }
  scale <- as.numeric(length(N)) ^ as.numeric(exponent)
  return(list(
    term = scale * s,
    n_rated = as.integer(length(N)),
    scale = scale,
    exponent = as.numeric(exponent),
    raw_sum = s
  ))
}

#' .svdpp_predict
#'
#' A step of the svdpp_native implementation. Called by \code{.svdpp_sgd_step}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param mu Passed to \code{.svdpp_baseline}.
#' @param b_user Passed to \code{.svdpp_baseline}.
#' @param b_item Passed to \code{.svdpp_baseline}.
#' @param p_u See Usage.
#' @param q_i See Usage.
#' @param rated_items Optional; may be \code{NULL}. A vector; its length is taken.
#' @param y Optional; may be \code{NULL}. A vector; its length is taken.
#' @param exponent Passed to \code{.svdpp_implicit_term}. Defaults to \code{-0.5}.
#' @return A list with \code{prediction}, \code{effective_user_factor}, \code{implicit}, \code{n_rated}, \code{note}.
#' @export
.svdpp_predict <- function(mu, b_user, b_item, p_u, q_i,
                          rated_items = NULL, y = NULL,
                          exponent = -0.5) {
  p <- as.numeric(p_u)
  q <- as.numeric(q_i)
  if (length(p) != length(q)) {
    stop(sprintf("svdpp: the user and item factors differ in width (%d, %d)",
                 length(p), length(q)))
  }
  d <- length(p)
  imp <- rep(0.0, d)
  n_rated <- 0L
  if (!is.null(rated_items) && length(rated_items) > 0 &&
      !is.null(y) && length(y) > 0) {
    r <- .svdpp_implicit_term(rated_items, y, exponent)
    if (length(r$term) == d) {
      imp <- r$term
    }
    n_rated <- r$n_rated
  }
  eff <- p + imp
  return(list(
    prediction = .svdpp_baseline(mu, b_user, b_item) + sum(q * eff),
    effective_user_factor = eff,
    implicit = imp,
    n_rated = n_rated,
    note = "inside the inner product, so it modifies the user's TASTE rather than adding a bias"
  ))
}

#' .svdpp_sgd_step
#'
#' A step of the svdpp_native implementation. Called by \code{.svdpp_fit}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param rating See Usage.
#' @param mu Passed to \code{.svdpp_predict}.
#' @param b_user Passed to \code{.svdpp_predict}.
#' @param b_item Passed to \code{.svdpp_predict}.
#' @param p_u Passed to \code{.svdpp_predict}.
#' @param q_i Passed to \code{.svdpp_predict}.
#' @param rated_items A vector; its length is taken.
#' @param y A vector; indexed elementwise.
#' @param lr Defaults to \code{0.007}.
#' @param reg Defaults to \code{0.015}.
#' @param exponent Passed to \code{.svdpp_predict}. Defaults to \code{-0.5}.
#' @return A list with \code{error}, \code{b_user}, \code{b_item}, \code{p_u}, \code{q_i}, \code{y}, \code{note}.
#' @export
.svdpp_sgd_step <- function(rating, mu, b_user, b_item, p_u, q_i,
                           rated_items, y,
                           lr = 0.007, reg = 0.015, exponent = -0.5) {
  pr <- .svdpp_predict(mu, b_user, b_item, p_u, q_i,
                       rated_items, y, exponent)
  e <- as.numeric(rating) - pr$prediction
  a_ <- as.numeric(lr)
  r_ <- as.numeric(reg)
  p <- as.numeric(p_u)
  q <- as.numeric(q_i)
  d <- length(p)
  nb_u <- as.numeric(b_user) + a_ * (e - r_ * as.numeric(b_user))
  nb_i <- as.numeric(b_item) + a_ * (e - r_ * as.numeric(b_item))
  nq <- q + a_ * (e * pr$effective_user_factor - r_ * q)
  npu <- p + a_ * (e * q - r_ * p)
  ny <- list()
  scale <- max(length(rated_items), 1) ^ as.numeric(exponent)
  for (j in rated_items) {
    jname <- as.character(j)
    yj <- y[[jname]]
    ny[[jname]] <- yj + a_ * (e * scale * q - r_ * yj)
  }
  return(list(
    error = e,
    b_user = nb_u,
    b_item = nb_i,
    p_u = npu,
    q_i = nq,
    y = ny,
    note = "the y update carries the same |N(u)|^-1/2 the forward pass uses"
  ))
}

#' .svdpp_fit
#'
#' A step of the svdpp_native implementation. Called by \code{morie_svdpp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ratings See Usage.
#' @param n_users See Usage.
#' @param n_items See Usage.
#' @param factors Defaults to \code{4}.
#' @param epochs A count; the body uses it as \code{seq_len(...)}. Defaults to \code{30}.
#' @param lr Passed to \code{.svdpp_sgd_step}. Defaults to \code{0.007}.
#' @param reg Passed to \code{.svdpp_sgd_step}. Defaults to \code{0.015}.
#' @param exponent Passed to \code{.svdpp_sgd_step}. Defaults to \code{-0.5}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @param implicit A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{estimate}, \code{rmse}, \code{rmse_history}, \code{mu}, \code{b_user}, \code{b_item}, \code{P}, \code{Q}, \code{Y}, \code{implicit}, \code{method}, \code{note}.
#' @export
.svdpp_fit <- function(ratings, n_users, n_items,
                      factors = 4, epochs = 30,
                      lr = 0.007, reg = 0.015,
                      exponent = -0.5, seed = 0,
                      implicit = TRUE) {
  R <- lapply(ratings, function(x) {
    list(as.integer(x[[1]]), as.integer(x[[2]]), as.numeric(x[[3]]))
  })
  if (length(R) == 0) {
    stop("svdpp: no ratings given")
  }
  nu <- as.integer(n_users)
  ni <- as.integer(n_items)
  d <- as.integer(factors)
  mu <- mean(sapply(R, function(x) x[[3]]))
  rng <- .ghc_rng(seed)
  small <- function() {
    u <- .ghc_unif(rng, d)
    return((u - 0.5) * 0.1)
  }
  bu <- rep(0.0, nu)
  bi <- rep(0.0, ni)
  P <- lapply(seq_len(nu), function(x) small())
  Q <- lapply(seq_len(ni), function(x) small())
  Y <- lapply(seq_len(ni), function(x) small())
  names(Y) <- as.character(seq_len(ni) - 1L)
  N <- list()
  for (rating in R) {
    u <- as.character(rating[[1]])
    i <- rating[[2]]
    N[[u]] <- c(N[[u]], i)
  }
  hist <- numeric(epochs)
  for (epoch in seq_len(epochs)) {
    se <- 0.0
    for (rating in R) {
      u <- rating[[1]]
      i <- rating[[2]]
      r <- rating[[3]]
      items <- if (implicit) N[[as.character(u)]] else NULL
      st <- .svdpp_sgd_step(r, mu, bu[u + 1L], bi[i + 1L],
                            P[[u + 1L]], Q[[i + 1L]],
                            if (!is.null(items)) items else list(),
                            if (implicit) Y else list(),
                            lr, reg, exponent)
      se <- se + st$error ^ 2
      bu[u + 1L] <- st$b_user
      bi[i + 1L] <- st$b_item
      P[[u + 1L]] <- st$p_u
      Q[[i + 1L]] <- st$q_i
      if (implicit) {
        for (jname in names(st$y)) {
          Y[[jname]] <- st$y[[jname]]
        }
      }
    }
    hist[epoch] <- sqrt(se / length(R))
  }
  return(list(
    estimate = hist[length(hist)],
    rmse = hist[length(hist)],
    rmse_history = hist,
    mu = mu,
    b_user = bu,
    b_item = bi,
    P = P,
    Q = Q,
    Y = if (implicit) Y else NULL,
    implicit = as.logical(implicit),
    method = "SVD++; Koren (2008) eq. (15)",
    note = "which items were rated is a signal even when the ratings themselves are not used"
  ))
}

#' morie_svdpp
#'
#' A step of the svdpp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ratings Passed to \code{.svdpp_fit}.
#' @param n_users Passed to \code{.svdpp_fit}.
#' @param n_items Passed to \code{.svdpp_fit}.
#' @param factors Passed to \code{.svdpp_fit}. Defaults to \code{4}.
#' @param epochs Passed to \code{.svdpp_fit}. Defaults to \code{30}.
#' @param lr Passed to \code{.svdpp_fit}. Defaults to \code{0.007}.
#' @param reg Passed to \code{.svdpp_fit}. Defaults to \code{0.015}.
#' @param exponent Passed to \code{.svdpp_fit}. Defaults to \code{-0.5}.
#' @param seed Passed to \code{.svdpp_fit}. Defaults to \code{0}.
#' @param implicit Passed to \code{.svdpp_fit}. Defaults to \code{TRUE}.
#' @return The value of \code{.svdpp_fit}.
#' @export
morie_svdpp <- function(ratings, n_users, n_items,
                        factors = 4, epochs = 30,
                        lr = 0.007, reg = 0.015,
                        exponent = -0.5, seed = 0,
                        implicit = TRUE) {
  return(.svdpp_fit(ratings, n_users, n_items,
                    factors, epochs, lr, reg,
                    exponent, seed, implicit))
}

#' .svdpp_cheatsheet
#'
#' A step of the svdpp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.svdpp_cheatsheet <- function() {
  return(paste0(
    "svdpp: a rating dataset carries a SECOND signal for free -- ",
    "WHICH items a user rated, regardless of the score. Add it to ",
    "the user factor INSIDE the inner product: r_ui = b_ui + ",
    "q_i'(p_u + |N(u)|^-1/2 sum_{j in N(u)} y_j), so it modifies ",
    "taste rather than adding a bias. The |N(u)|^-1/2 is load-bearing: ",
    "at exponent 0 a heavy rater's term swamps p_u, at -1 it becomes ",
    "a mean and forgets how much evidence there was. Baselines mu + ",
    "b_u + b_i come first, or the factors waste capacity relearning them."
  ))
}
