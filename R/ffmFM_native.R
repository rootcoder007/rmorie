# Field-aware factorization machines.
#
# Sources: Juan, Y., Zhuang, Y., Chin, W.-S. & Lin, C.-J. (2016)
# "Field-aware Factorization Machines for CTR Prediction",
# Proceedings of the Tenth ACM Conference on Recommender Systems
# (RecSys '16), 43-50, doi:10.1145/2959100.2959134. Sec. 2 (FM and
# its limitation), Sec. 3 (the FFM model equation with the crossed
# field indices <w_{j1,f2}, w_{j2,f1}>, the nfk parameter count and
# the consequence that FFM's k is much smaller than FM's, the
# logistic loss with y in {-1,1}, and the AdaGrad updates of eqs.
# (8)-(9)), and Sec. 3.3 (overfitting and early stopping). Rendle, S.
# (2010) "Factorization Machines", ICDM 2010, 995-1000,
# doi:10.1109/ICDM.2010.127. The model FFM specialises; implemented
# in fmFM.
#
# Native implementation mirroring Python morie.fn.ffmFM exactly: the
# same crossed field indices, the same nfk parameter count, the same
# logistic loss with y in {-1,1}, the same AdaGrad updates of eqs.
# (8)-(9), and the same RichResult-style payload as a named list.
# Randomness is drawn from .ghc_rng / .ghc_unif so both arms produce
# the same stream.

.FFMFM_EPS <- 1e-12

#' n_parameters
#'
#' A step of the ffmFM_native implementation. Called by \code{fit_ffm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_features Coerced to integer by the body, with \code{as.integer}.
#' @param n_fields Coerced to integer by the body, with \code{as.integer}.
#' @param k_dim Coerced to integer by the body, with \code{as.integer}.
#' @param model One of \code{"ffm"}, \code{"fm"}. Defaults to \code{"ffm"}.
#' @return One of two values, depending on the branch taken.
#' @export
n_parameters <- function(n_features, n_fields, k_dim,
                         model = "ffm") {
  n <- as.integer(n_features)
  F <- as.integer(n_fields)
  kk <- as.integer(k_dim)
  if (!(model %in% c("ffm", "fm")))
    stop("ffmFM: model must be ffm or fm, got ", model)
  if (model == "ffm") n * F * kk else n * kk
}

#' phi
#'
#' A step of the ffmFM_native implementation. Called by \code{explor}, \code{fit_ffm},
#' \code{gated_update} and 15 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x See Usage.
#' @param fields A vector; indexed elementwise.
#' @param W A vector; indexed elementwise.
#' @return The value of \code{tot}, as built in the body.
#' @export
phi <- function(x, fields, W) {
  nz <- list()
  for (pair in x) {
    j <- as.integer(pair[[1]])
    v <- as.numeric(pair[[2]])
    if (v != 0) nz[[length(nz) + 1L]] <- list(j = j, v = v)
  }
  tot <- 0
  for (a in seq_along(nz)) {
    # seq_len guard: seq.int DESCENDS when a is already the last
    # non-zero and reads nz[[length + 1]]
    for (b in seq_len(length(nz) - a) + a) {
      j1 <- nz[[a]]$j
      v1 <- nz[[a]]$v
      j2 <- nz[[b]]$j
      v2 <- nz[[b]]$v
      f1 <- as.integer(fields[j1 + 1L])
      f2 <- as.integer(fields[j2 + 1L])
      v1j <- W[[j1 + 1L]][[f2 + 1L]]
      v2j <- W[[j2 + 1L]][[f1 + 1L]]
      tot <- tot + sum(v1j * v2j) * v1 * v2
    }
  }
  tot
}

#' logistic_loss
#'
#' A step of the ffmFM_native implementation. Called by \code{fit_ffm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param phi_val Coerced to numeric by the body, with \code{as.numeric}.
#' @return One of two values, depending on the branch taken.
#' @export
logistic_loss <- function(y, phi_val) {
  yv <- as.numeric(y)
  if (!(yv == -1 || yv == 1))
    stop("ffmFM: the label must be -1 or 1, got ", y)
  z <- -yv * as.numeric(phi_val)
  if (z < 700) log(1 + exp(z)) else z
}

#' fit_ffm
#'
#' A step of the ffmFM_native implementation. Called by \code{field_aware_fm},
#' \code{fieldawarefm}, \code{morie_ffmFM}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param rows A vector; its length is taken and its elements indexed.
#' @param labels A vector; its length is taken and its elements indexed.
#' @param fields A vector; indexed elementwise.
#' @param n_features Coerced to integer by the body, with \code{as.integer}.
#' @param n_fields Coerced to integer by the body, with \code{as.integer}.
#' @param k_dim Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4}.
#' @param eta Numeric; combined arithmetically in the body. Defaults to \code{0.1}.
#' @param lam Numeric; combined arithmetically in the body. Defaults to \code{2e-05}.
#' @param epochs Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10}.
#' @param seed Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @return A list with \code{estimate}, \code{W}, \code{loss_history}, \code{final_loss},
#' \code{k}, \code{n_parameters}, \code{n_parameters_fm}, \code{method}, \code{caveat}.
#' @export
fit_ffm <- function(rows, labels, fields, n_features, n_fields,
                    k_dim = 4, eta = 0.1, lam = 2e-5, epochs = 10,
                    seed = 0) {
  n <- as.integer(n_features)
  F <- as.integer(n_fields)
  kk <- as.integer(k_dim)
  if (n < 1L || F < 1L || kk < 1L)
    stop("ffmFM: n_features, n_fields and k must all be at least 1")
  if (length(rows) != length(labels))
    stop("ffmFM: ", length(rows), " rows but ", length(labels),
         " labels")
  rng <- .ghc_rng(as.numeric(seed))
  scale <- 1 / sqrt(kk)
  u <- .ghc_unif(rng, n * F * kk)
  W <- vector("list", n)
  for (j in seq_len(n)) {
    W[[j]] <- vector("list", F)
    for (f in seq_len(F)) {
      W[[j]][[f]] <- numeric(kk)
      for (d in seq_len(kk)) {
        idx <- ((j - 1L) * F + (f - 1L)) * kk + d
        W[[j]][[f]][d] <- u[idx] * scale
      }
    }
  }
  G <- lapply(seq_len(n), function(j)
    lapply(seq_len(F), function(f) rep(1, kk)))
  hist <- numeric(0)
  for (ep in seq_len(as.integer(epochs))) {
    tot <- 0
    for (r in seq_along(rows)) {
      yv <- as.numeric(labels[r])
      p <- phi(rows[[r]], fields, W)
      tot <- tot + logistic_loss(yv, p)
      g0 <- -yv / (1 + exp(min(700, yv * p)))
      nz <- list()
      for (pair in rows[[r]]) {
        j <- as.integer(pair[[1]])
        v <- as.numeric(pair[[2]])
        if (v != 0) nz[[length(nz) + 1L]] <- list(j = j, v = v)
      }
      for (a in seq_along(nz)) {
        # seq_len guard: seq.int DESCENDS when a is already the last
    # non-zero and reads nz[[length + 1]]
    for (b in seq_len(length(nz) - a) + a) {
          j1 <- nz[[a]]$j
          v1 <- nz[[a]]$v
          j2 <- nz[[b]]$j
          v2 <- nz[[b]]$v
          f1 <- as.integer(fields[j1 + 1L])
          f2 <- as.integer(fields[j2 + 1L])
          for (d in seq_len(kk)) {
            g1 <- lam * W[[j1 + 1L]][[f2 + 1L]][d] +
              g0 * W[[j2 + 1L]][[f1 + 1L]][d] * v1 * v2
            g2 <- lam * W[[j2 + 1L]][[f1 + 1L]][d] +
              g0 * W[[j1 + 1L]][[f2 + 1L]][d] * v1 * v2
            G[[j1 + 1L]][[f2 + 1L]][d] <- G[[j1 + 1L]][[f2 + 1L]][d] +
              g1 * g1
            G[[j2 + 1L]][[f1 + 1L]][d] <- G[[j2 + 1L]][[f1 + 1L]][d] +
              g2 * g2
            W[[j1 + 1L]][[f2 + 1L]][d] <- W[[j1 + 1L]][[f2 + 1L]][d] -
              eta / sqrt(G[[j1 + 1L]][[f2 + 1L]][d]) * g1
            W[[j2 + 1L]][[f1 + 1L]][d] <- W[[j2 + 1L]][[f1 + 1L]][d] -
              eta / sqrt(G[[j2 + 1L]][[f1 + 1L]][d]) * g2
          }
        }
      }
    }
    hist <- c(hist, tot / length(rows))
  }
  list(estimate = W, W = W, loss_history = hist,
       final_loss = hist[length(hist)], k = kk,
       n_parameters = n_parameters(n, F, kk),
       n_parameters_fm = n_parameters(n, F, kk, "fm"),
       method = "FFM with AdaGrad; Juan, Zhuang, Chin & Lin (2016) eqs. (8)-(9)",
       caveat = "FFM overfits readily -- the paper stops early on a validation set")
}

#' .ffmFM_cheatsheet
#'
#' A step of the ffmFM_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .ffmFM_cheatsheet()
#' res
.ffmFM_cheatsheet <- function() {
  paste("ffmFM: one latent vector per feature PER FIELD, because a",
        "feature interacts differently with an advertiser than",
        "with a gender. The interaction is <w_{j1,f2}, w_{j2,f1}>",
        "-- each vector indexed by the OTHER feature's field, and",
        "swapping that is the classic bug. nfk parameters against",
        "FM's nk, but each vector specialises so k is much smaller.",
        "No linear-time trick: the FM identity needs one vector per",
        "feature. Logistic loss with y in {-1,1}, AdaGrad, early",
        "stopping.",
        sep = " ")
}

#' morie_ffmFM
#'
#' A step of the ffmFM_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param rows Passed to \code{fit_ffm}.
#' @param labels Passed to \code{fit_ffm}.
#' @param fields Passed to \code{fit_ffm}.
#' @param n_features Passed to \code{fit_ffm}.
#' @param n_fields Passed to \code{fit_ffm}.
#' @param k_dim Passed to \code{fit_ffm}. Defaults to \code{4}.
#' @param eta Passed to \code{fit_ffm}. Defaults to \code{0.1}.
#' @param lam Passed to \code{fit_ffm}. Defaults to \code{2e-05}.
#' @param epochs Passed to \code{fit_ffm}. Defaults to \code{10}.
#' @param seed Passed to \code{fit_ffm}. Defaults to \code{0}.
#' @return The value of \code{fit_ffm}.
#' @export
morie_ffmFM <- function(rows, labels, fields, n_features, n_fields,
                        k_dim = 4, eta = 0.1, lam = 2e-5,
                        epochs = 10, seed = 0) {
  fit_ffm(rows, labels, fields, n_features, n_fields, k_dim, eta,
          lam, epochs, seed)
}

#' fieldawarefm
#'
#' A step of the ffmFM_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param rows Passed to \code{fit_ffm}.
#' @param labels Passed to \code{fit_ffm}.
#' @param fields Passed to \code{fit_ffm}.
#' @param n_features Passed to \code{fit_ffm}.
#' @param n_fields Passed to \code{fit_ffm}.
#' @param k_dim Passed to \code{fit_ffm}. Defaults to \code{4}.
#' @param eta Passed to \code{fit_ffm}. Defaults to \code{0.1}.
#' @param lam Passed to \code{fit_ffm}. Defaults to \code{2e-05}.
#' @param epochs Passed to \code{fit_ffm}. Defaults to \code{10}.
#' @param seed Passed to \code{fit_ffm}. Defaults to \code{0}.
#' @return The value of \code{fit_ffm}.
#' @export
fieldawarefm <- function(rows, labels, fields, n_features, n_fields,
                         k_dim = 4, eta = 0.1, lam = 2e-5,
                         epochs = 10, seed = 0) {
  fit_ffm(rows, labels, fields, n_features, n_fields, k_dim, eta,
          lam, epochs, seed)
}

#' field_aware_fm
#'
#' A step of the ffmFM_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param rows Passed to \code{fit_ffm}.
#' @param labels Passed to \code{fit_ffm}.
#' @param fields Passed to \code{fit_ffm}.
#' @param n_features Passed to \code{fit_ffm}.
#' @param n_fields Passed to \code{fit_ffm}.
#' @param k_dim Passed to \code{fit_ffm}. Defaults to \code{4}.
#' @param eta Passed to \code{fit_ffm}. Defaults to \code{0.1}.
#' @param lam Passed to \code{fit_ffm}. Defaults to \code{2e-05}.
#' @param epochs Passed to \code{fit_ffm}. Defaults to \code{10}.
#' @param seed Passed to \code{fit_ffm}. Defaults to \code{0}.
#' @return The value of \code{fit_ffm}.
#' @export
field_aware_fm <- function(rows, labels, fields, n_features, n_fields,
                           k_dim = 4, eta = 0.1, lam = 2e-5,
                           epochs = 10, seed = 0) {
  fit_ffm(rows, labels, fields, n_features, n_fields, k_dim, eta,
          lam, epochs, seed)
}
