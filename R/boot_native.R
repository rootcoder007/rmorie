# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native nonparametric bootstrap + boot.ci (feat/native-specializations,
# module 28). Replaces the boot and simpleboot packages: ordinary and
# block (moving / stationary) resampling, two-sample bootstrap, and the
# normal / basic / percentile / BCa confidence-interval algebra.
#
# Index generation, block resampling and the interval machinery
# reproduce boot's RNG stream and formulae exactly, so native and
# boot/simpleboot results agree to machine precision under a common
# seed. tests/cross validates this.

#' .boot_n
#'
#' A step of the boot_native implementation. Called by \code{.morie_empinf_reg}, \code{morie_boot}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param data A matrix; passed to \code{nrow}.
#' @return One of two values, depending on the branch taken.
#' @export
.boot_n <- function(data) if (is.null(dim(data))) length(data) else nrow(data)

# boot:::ordinary.array -- single- or multi-stratum R x n index matrix.
#' Boot:::ordinary.array -- single- or multi-stratum R x n index matrix
#'
#' A step of the boot_native implementation. Called by \code{morie_boot}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param R A count; the body uses it as \code{rep(...)}.
#' @param strata Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return The value of \code{output}, as built in the body.
#' @export
.morie_boot_index <- function(n, R, strata = NULL) {
  if (is.null(strata) || length(unique(strata)) == 1L) {
    out <- sample.int(n, n * R, replace = TRUE)
    dim(out) <- c(R, n)
    return(out)
  }
  inds <- as.integer(names(table(strata)))
  output <- matrix(0L, R, n)
  for (s in inds) {
    gp <- seq_len(n)[strata == s]
    output[, gp] <- if (length(gp) == 1L) {
      rep(gp, R)
    } else {
      gp[sample.int(length(gp), R * length(gp), replace = TRUE)]
    }
  }
  output
}

#' Native nonparametric bootstrap (ordinary, stratified)
#'
#' Reproduces \code{boot::boot(..., sim = "ordinary", stype = "i")}.
#' The statistic is called as \code{statistic(data, indices)} with
#' \code{indices} an integer resample of rows; under a common seed the
#' replicate matrix \code{t} matches \code{boot::boot} to the RNG
#' stream.
#'
#' @param data A vector or data.frame (rows are the sampling units).
#' @param statistic Function \code{(data, indices) -> numeric}.
#' @param R Number of bootstrap replicates.
#' @param strata Optional grouping vector for stratified resampling.
#' @param ... Passed on to \code{statistic}.
#' @return An object of class \code{morie_boot} with \code{t0}
#'   (observed), \code{t} (R x k replicates), \code{data},
#'   \code{strata} and the resample \code{index} matrix.
#' @references Davison, A. C., & Hinkley, D. V. (1997).
#'   \emph{Bootstrap Methods and their Application}. Cambridge.
#' @examples
#' set.seed(1)
#' b <- morie_boot(mtcars$mpg, function(d, i) mean(d[i]), R = 200)
#' morie_boot_ci(b, type = "perc")
#' @export
morie_boot <- function(data, statistic, R, strata = NULL, ...) {
  n <- .boot_n(data)
  idx <- .morie_boot_index(n, R, strata)
  t0 <- as.numeric(statistic(data, seq_len(n), ...))
  k <- length(t0)
  t <- matrix(NA_real_, R, k)
  for (r in seq_len(R)) t[r, ] <- as.numeric(statistic(data, idx[r, ], ...))
  structure(
    list(
      t0 = t0, t = t, R = R, data = data,
      statistic = statistic, strata = strata, index = idx
    ),
    class = "morie_boot"
  )
}

# boot:::freq.array -- R x n resample frequency counts.
#' Boot:::freq.array -- R x n resample frequency counts
#'
#' A step of the boot_native implementation. Called by \code{.morie_empinf_reg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param boot_obj A list; the body reads \code{$index} from it.
#' @return A matrix, from \code{t}.
#' @export
.morie_boot_freq <- function(boot_obj) {
  idx <- boot_obj$index
  n <- ncol(idx)
  t(apply(idx, 1, tabulate, nbins = n))
}

# boot:::empinf.reg -- regression estimate of empirical influence.
#' Boot:::empinf.reg -- regression estimate of empirical influence
#'
#' A step of the boot_native implementation. Called by \code{.morie_ci_bca}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param boot_obj A list; the body reads \code{$data}, \code{$strata} from it.
#' @param t A vector; its length is taken and its elements indexed. Defaults to \code{boot_obj$t[, 1L]}.
#' @return A vector, from \code{as.numeric}.
#' @export
.morie_empinf_reg <- function(boot_obj, t = boot_obj$t[, 1L]) {
  fins <- which(is.finite(t))
  t <- t[fins]
  R <- length(t)
  n <- .boot_n(boot_obj$data)
  strata <- boot_obj$strata
  if (is.null(strata)) strata <- rep(1, n)
  strata <- as.integer(factor(strata))
  ns <- table(strata)
  f <- .morie_boot_freq(boot_obj)[fins, , drop = FALSE]
  X <- f / matrix(ns[strata], R, n, byrow = TRUE)
  out <- tapply(seq_len(n), strata, min)
  inc <- seq_len(n)[-out]
  X <- X[, inc, drop = FALSE]
  beta <- stats::coefficients(stats::glm(t ~ X))[-1L]
  l <- rep(0, n)
  l[inc] <- beta
  l <- l - tapply(l, strata, mean)[strata]
  as.numeric(l)
}

# boot:::norm.inter -- interpolation of order statistics on the normal
# quantile scale (shared by percentile / basic / BCa).
#' Boot:::norm.inter -- interpolation of order statistics on the normal
#'
#' quantile scale (shared by percentile / basic / BCa).
#'
#' @param t A vector; its length is taken and its elements indexed.
#' @param alpha A vector; its length is taken and its elements indexed.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_norm_inter <- function(t, alpha) {
  t <- sort(t[is.finite(t)])
  R <- length(t)
  rk <- (R + 1) * alpha
  k <- trunc(rk)
  out <- numeric(length(alpha))
  for (i in seq_along(alpha)) {
    ki <- k[i]
    if (ki == 0) {
      out[i] <- t[1L]
      next
    }
    if (ki >= R) {
      out[i] <- t[R]
      next
    }
    if (ki == rk[i]) {
      out[i] <- t[ki]
      next
    }
    q1 <- stats::qnorm(alpha[i])
    q2 <- stats::qnorm(ki / (R + 1))
    q3 <- stats::qnorm((ki + 1) / (R + 1))
    out[i] <- t[ki] + (q1 - q2) / (q3 - q2) * (t[ki + 1L] - t[ki])
  }
  out
}

#' .morie_ci_perc
#'
#' A step of the boot_native implementation. Called by \code{morie_boot_ci}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t Passed to \code{.morie_norm_inter}.
#' @param conf Numeric; combined arithmetically in the body.
#' @return The value of \code{.morie_norm_inter}.
#' @export
.morie_ci_perc <- function(t, conf) .morie_norm_inter(t, (1 + c(-conf, conf)) / 2)

#' .morie_ci_basic
#'
#' A step of the boot_native implementation. Called by \code{morie_boot_ci}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t0 Numeric; combined arithmetically in the body.
#' @param t Passed to \code{.morie_norm_inter}.
#' @param conf Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.morie_ci_basic <- function(t0, t, conf) {
  2 * t0 - .morie_norm_inter(t, (1 + c(conf, -conf)) / 2)
}

#' .morie_ci_norm
#'
#' A step of the boot_native implementation. Called by \code{morie_boot_ci}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t0 Numeric; combined arithmetically in the body.
#' @param t A vector; indexed elementwise.
#' @param conf Numeric; combined arithmetically in the body.
#' @return A vector, from \code{c}.
#' @export
.morie_ci_norm <- function(t0, t, conf) {
  t <- t[is.finite(t)]
  bias <- mean(t) - t0
  merr <- sqrt(stats::var(t)) * stats::qnorm((1 + conf) / 2)
  c(t0 - bias - merr, t0 - bias + merr)
}

#' .morie_ci_bca
#'
#' A step of the boot_native implementation. Called by \code{morie_boot_ci}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param boot_obj A list; the body reads \code{$t}, \code{$t0} from it.
#' @param index See Usage.
#' @param conf Numeric; combined arithmetically in the body.
#' @return The value of \code{.morie_norm_inter}.
#' @export
.morie_ci_bca <- function(boot_obj, index, conf) {
  t_all <- boot_obj$t[, index]
  t0 <- boot_obj$t0[index]
  t <- t_all[is.finite(t_all)]
  w <- stats::qnorm(sum(t < t0) / length(t))
  if (!is.finite(w)) stop("estimated BCa bias 'w' is infinite")
  zalpha <- stats::qnorm((1 + c(-conf, conf)) / 2)
  L <- .morie_empinf_reg(boot_obj, t_all)
  a <- sum(L^3) / (6 * sum(L^2)^1.5)
  if (!is.finite(a)) stop("estimated BCa acceleration 'a' is NA")
  adj <- stats::pnorm(w + (w + zalpha) / (1 - a * (w + zalpha)))
  .morie_norm_inter(t, adj)
}

#' Native bootstrap confidence intervals
#'
#' Reproduces \code{boot::boot.ci} for the \code{"norm"},
#' \code{"basic"}, \code{"perc"} and \code{"bca"} interval types
#' (BCa via the regression empirical-influence estimate, matching
#' \code{boot}'s default).
#'
#' @param boot_obj A \code{morie_boot} object from \code{morie_boot()}.
#' @param conf Confidence level (default 0.95).
#' @param type Character vector: any of \code{"perc"}, \code{"norm"},
#'   \code{"basic"}, \code{"bca"}.
#' @param index Which column of \code{t} to summarise (default 1).
#' @return Named list; each element is \code{c(lower, upper)}.
#' @references DiCiccio, T. J., & Efron, B. (1996). Bootstrap
#'   confidence intervals. \emph{Statistical Science}, 11(3), 189-228.
#' @examples
#' set.seed(1)
#' b <- morie_boot_run(rnorm(50), statistic = mean, R = 200L)
#' morie_boot_ci(b)
#' @export
morie_boot_ci <- function(boot_obj, conf = 0.95,
                          type = c("perc", "norm", "basic", "bca"),
                          index = 1L) {
  t <- boot_obj$t[, index]
  t0 <- boot_obj$t0[index]
  res <- lapply(type, function(ty) {
    switch(ty,
      perc  = .morie_ci_perc(t, conf),
      norm  = .morie_ci_norm(t0, t, conf),
      basic = .morie_ci_basic(t0, t, conf),
      bca   = .morie_ci_bca(boot_obj, index, conf),
      stop("unknown CI type: ", ty)
    )
  })
  names(res) <- type
  res
}

# boot:::ts.array -- block start/length arrays for the block bootstrap.
#' Boot:::ts.array -- block start/length arrays for the block bootstrap
#'
#' A step of the boot_native implementation. Called by \code{morie_tsboot}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Numeric; combined arithmetically in the body.
#' @param n.sim Numeric; combined arithmetically in the body.
#' @param R A count; the body uses it as \code{rep(...)}.
#' @param l A count; the body uses it as \code{rep(...)}.
#' @param sim Compared against \code{"geom"}.
#' @param endcorr A flag; the body branches on it.
#' @return A list with \code{starts}, \code{lengths}.
#' @export
.morie_ts_array <- function(n, n.sim, R, l, sim, endcorr) {
  endpt <- if (endcorr) n else n - l + 1
  if (sim == "geom") {
    len.tot <- rep(0, R)
    lens <- NULL
    cont <- TRUE
    while (cont) {
      temp <- 1 + stats::rgeom(R, 1 / l)
      temp <- pmin(temp, n.sim - len.tot)
      lens <- cbind(lens, temp)
      len.tot <- len.tot + temp
      cont <- any(len.tot < n.sim)
    }
    dimnames(lens) <- NULL
    nn <- ncol(lens)
    st <- matrix(sample.int(endpt, nn * R, replace = TRUE), R)
  } else {
    nn <- ceiling(n.sim / l)
    lens <- c(rep(l, nn - 1), 1 + (n.sim - 1) %% l)
    st <- matrix(sample.int(endpt, nn * R, replace = TRUE), R)
  }
  list(starts = st, lengths = lens)
}

#' .morie_make_ends
#'
#' A step of the boot_native implementation. Called by \code{morie_tsboot}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; indexed elementwise.
#' @param n Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.morie_make_ends <- function(a, n) {
  if (a[2L] == 0) {
    return(numeric())
  }
  1 + (seq.int(a[1L], a[1L] + a[2L] - 1, length.out = a[2L]) - 1) %% n
}

#' Native block bootstrap for time series
#'
#' Reproduces \code{boot::tsboot} for the \code{sim = "fixed"} (moving
#' block) and \code{sim = "geom"} (stationary) resampling schemes.
#'
#' @param tseries A numeric vector / matrix (the series).
#' @param statistic Function \code{(series) -> numeric}.
#' @param R Number of replicates.
#' @param l Block length.
#' @param sim \code{"fixed"} (moving block, default) or \code{"geom"}
#'   (stationary bootstrap).
#' @param endcorr Apply end-correction (default TRUE).
#' @param n.sim Length of each simulated series (default \code{NROW}).
#' @param ... Passed on to \code{statistic}.
#' @return A \code{morie_boot} object (\code{t0}, \code{t}, \code{R}).
#' @references Politis, D. N., & Romano, J. P. (1994). The stationary
#'   bootstrap. \emph{JASA}, 89(428), 1303-1313.
#' @examples
#' set.seed(1)
#' b <- morie_tsboot(stats::as.ts(rnorm(60)),
#'   statistic = mean,
#'   R = 100L, l = 5
#' )
#' str(b, max.level = 1)
#' @export
morie_tsboot <- function(tseries, statistic, R, l, sim = "fixed",
                         endcorr = TRUE, n.sim = NROW(tseries), ...) {
  ts.orig <- if (is.null(dim(tseries))) as.matrix(tseries) else tseries
  n <- nrow(ts.orig)
  t0 <- as.numeric(statistic(tseries, ...))
  ia <- .morie_ts_array(n, n.sim, R, l, sim, endcorr)
  k <- length(t0)
  t <- matrix(NA_real_, R, k)
  for (r in seq_len(R)) {
    ends <- if (sim == "geom") {
      cbind(ia$starts[r, ], ia$lengths[r, ])
    } else {
      cbind(ia$starts[r, ], ia$lengths)
    }
    inds <- unlist(lapply(
      seq_len(nrow(ends)),
      function(j) .morie_make_ends(ends[j, ], n)
    ))
    inds <- inds[seq_len(n.sim)]
    series <- ts.orig[inds, ]
    t[r, ] <- as.numeric(statistic(series, ...))
  }
  structure(list(t0 = t0, t = t, R = R), class = "morie_boot")
}

#' Native two-sample bootstrap
#'
#' Reproduces \code{simpleboot::two.boot}: bootstraps the difference
#' \code{FUN(sample1) - FUN(sample2)} by stratified resampling of the
#' pooled data (one stratum per sample), matching \code{boot}'s RNG.
#'
#' @param x,y Numeric vectors.
#' @param statistic A scalar summary applied to each sample (default
#'   \code{mean}).
#' @param R Number of replicates (default 1000).
#' @param ... Passed on to \code{statistic}.
#' @return A \code{morie_boot} object for the difference statistic.
#' @seealso \code{\link{morie_boot_ci}}.
#' @examples
#' set.seed(1)
#' b <- morie_two_boot(rnorm(25), rnorm(25, 0.5), statistic = mean, R = 100L)
#' str(b, max.level = 1)
#' @export
morie_two_boot <- function(x, y, statistic = mean, R = 1000L, ...) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  ind <- c(rep(1L, length(x)), rep(2L, length(y)))
  func <- match.fun(statistic)
  boot.func <- function(d, idx) {
    func(d[idx[ind == 1L]], ...) - func(d[idx[ind == 2L]], ...)
  }
  morie_boot(c(x, y), boot.func, R, strata = ind)
}
