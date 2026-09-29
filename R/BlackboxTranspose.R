.bbt_miss <- function(v) abs(v + 999) <= 0.001

.bbt_jacobi <- function(A) {
  n <- nrow(A)
  a <- A
  v <- diag(n)
  for (sweep in 1:100) {
    off <- 0
    tot <- 0
    for (i in seq_len(n)) {
      tot <- tot + a[i, i] * a[i, i]
      if (i < n) for (j in (i + 1):n) off <- off + a[i, j] * a[i, j]
    }
    if (off <= 1e-32 * tot || off == 0) break
    if (n < 2) break
    for (p in 1:(n - 1)) {
      for (q in (p + 1):n) {
        apq <- a[p, q]
        if (apq == 0) next
        theta <- (a[q, q] - a[p, p]) / (2 * apq)
        t <- (if (theta >= 0) 1 else -1) / (abs(theta) + sqrt(theta * theta + 1))
        c <- 1 / sqrt(t * t + 1)
        s <- t * c
        akp <- a[, p]
        akq <- a[, q]
        a[, p] <- c * akp - s * akq
        a[, q] <- s * akp + c * akq
        apk <- a[p, ]
        aqk <- a[q, ]
        a[p, ] <- c * apk - s * aqk
        a[q, ] <- s * apk + c * aqk
        vkp <- v[, p]
        vkq <- v[, q]
        v[, p] <- c * vkp - s * vkq
        v[, q] <- s * vkp + c * vkq
      }
    }
  }
  d <- diag(a)
  ord <- order(-d, seq_len(n))
  list(values = d[ord], vectors = v[, ord, drop = FALSE])
}

.bbt_pinv <- function(a, thr) {
  n <- nrow(a)
  e <- .bbt_jacobi(a)
  w <- e$values
  z <- e$vectors
  out <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (kx in seq_len(n)) {
      s <- 0
      for (j in seq_len(n)) if (abs(w[j]) > thr) s <- s + z[kx, j] * (1 / w[j]) * z[i, j]
      out[i, kx] <- s
    }
  }
  out
}

.bbt_svd_v <- function(M, ncol) {
  G <- matrix(0, ncol, ncol)
  for (a in seq_len(ncol)) {
    for (b in seq_len(ncol)) {
      s <- 0
      for (r in seq_len(nrow(M))) s <- s + M[r, a] * M[r, b]
      G[a, b] <- s
    }
  }
  e <- .bbt_jacobi(G)
  V <- e$vectors
  sv <- ifelse(e$values > 0, sqrt(pmax(e$values, 0)), 0)
  for (j in seq_len(ncol)) {
    big <- 1
    for (i in seq_len(ncol)) if (abs(V[i, j]) > abs(V[big, j])) big <- i
    if (V[big, j] < 0) V[, j] <- -V[, j]
  }
  list(sv = sv, V = V)
}

.bbt_corr3 <- function(x, np, ny) {
  sa <- matrix(0, ny, ny)
  sb <- matrix(0, ny, ny)
  sc <- matrix(0, ny, ny)
  sd <- matrix(0, ny, ny)
  for (i in seq_len(np)) {
    xi <- x[i, ]
    for (j in seq_len(ny)) {
      for (jj in seq_len(j)) {
        # the Fortran jumps to the end of the outer loop (label 31), abandoning this j
        if (.bbt_miss(xi[j]) || .bbt_miss(xi[jj])) break
        sa[j, jj] <- sa[j, jj] + xi[j]
        if (j != jj) sa[jj, j] <- sa[jj, j] + xi[jj]
        sb[j, jj] <- sb[j, jj] + xi[j] * xi[j]
        if (j != jj) sb[jj, j] <- sb[jj, j] + xi[jj] * xi[jj]
        sc[j, jj] <- sc[j, jj] + xi[j] * xi[jj]
        sc[jj, j] <- sc[j, jj]
        sd[j, jj] <- sd[j, jj] + 1
      }
    }
  }
  r <- matrix(0, ny, ny)
  for (j in seq_len(ny)) {
    for (jj in seq_len(j)) {
      aa <- sd[j, jj] * sc[j, jj] - sa[j, jj] * sa[jj, j]
      bb <- sd[j, jj] * sb[j, jj] - sa[j, jj] * sa[j, jj]
      cc <- sd[j, jj] * sb[jj, j] - sa[jj, j] * sa[jj, j]
      r[jj, j] <- if (bb * cc <= 0) 0 else aa / sqrt(bb * cc)
      r[j, jj] <- r[jj, j]
    }
  }
  best <- -99
  ks <- 1
  for (j in seq_len(ny)) {
    s <- 0
    for (jj in seq_len(ny)) s <- s + abs(r[j, jj])
    if (s > best) {
      best <- s
      ks <- j
    }
  }
  ll <- ifelse(r[ks, ] <= 0, -1, 1)
  half <- (ny - 1) %/% 2
  # the Fortran always makes ny passes; a pass without a flip is a fixed point, so stop there
  for (pass in seq_len(ny)) {
    flipped <- FALSE
    for (j in seq_len(ny)) {
      kk <- sum(r[j, ] * ll * ll[j] < 0)
      if (kk > half) {
        ll[j] <- -ll[j]
        flipped <- TRUE
      }
    }
    if (!flipped) break
  }
  ll
}

.bbt_regt <- function(np, nf, ny, st) {
  nf1 <- nf + 1
  tsum <- 0
  for (k in seq_len(ny)) {
    st$w[k, seq_len(nf1)] <- 0
    rows <- which(!.bbt_miss(st$xs[, k]) & !.bbt_miss(st$psi[, 1]))
    a <- matrix(0, nf1, nf1)
    for (j in seq_len(nf1)) {
      for (jj in seq_len(nf1)) {
        s <- 0
        for (i in rows) s <- s + st$psi[i, j] * st$psi[i, jj]
        a[j, jj] <- s
      }
    }
    b <- .bbt_pinv(a, 0.001)
    for (i in rows) {
      cc <- numeric(nf1)
      for (j in seq_len(nf1)) {
        s <- 0
        for (jj in seq_len(nf1)) s <- s + b[j, jj] * st$psi[i, jj]
        cc[j] <- s
      }
      for (j in seq_len(nf1)) st$w[k, j] <- st$w[k, j] + cc[j] * st$xs[i, k]
    }
    esum <- 0
    for (i in rows) {
      s <- 0
      for (j in seq_len(nf1)) s <- s + st$psi[i, j] * st$w[k, j]
      d <- s - st$xs[i, k]
      esum <- esum + d * d
      st$x[i, k] <- d
    }
    st$w[k, nf + 2] <- esum
    tsum <- tsum + esum
  }
  st$tsum <- tsum
  st
}

.bbt_regat <- function(a, y, nf) {
  ns <- length(y)
  b <- matrix(0, nf, nf)
  for (j in seq_len(nf)) {
    for (jj in seq_len(nf)) {
      s <- 0
      for (i in seq_len(ns)) s <- s + a[i, j] * a[i, jj]
      b[j, jj] <- s
    }
  }
  cm <- .bbt_pinv(b, 0.00001)
  bb <- matrix(0, nf, ns)
  for (i in seq_len(ns)) {
    for (j in seq_len(nf)) {
      s <- 0
      for (jj in seq_len(nf)) s <- s + cm[j, jj] * a[i, jj]
      bb[j, i] <- s
    }
  }
  v <- numeric(nf)
  for (jj in seq_len(nf)) {
    s <- 0
    for (j in seq_len(ns)) s <- s + bb[jj, j] * y[j]
    v[jj] <- s
  }
  v
}

.bbt_reg2t <- function(np, nf, ny, st, nwho) {
  esum <- 0
  pxb <- 0
  pxs <- 0
  xns <- 0
  for (i in seq_len(np)) {
    if (.bbt_miss(st$psi[i, 1])) next
    js <- which(!.bbt_miss(st$xs[i, ]))
    y <- st$xs[i, js] - st$w[js, nwho + 1]
    a <- st$w[js, seq_len(nf), drop = FALSE]
    v <- .bbt_regat(a, y, nf)
    for (k in js) {
      s <- 0
      for (j in seq_len(nf)) {
        st$psi[i, j] <- v[j]
        s <- s + st$psi[i, j] * st$w[k, j]
      }
      s <- s + st$w[k, nwho + 1]
      d <- s - st$xs[i, k]
      st$x[i, k] <- d
      esum <- esum + d * d
    }
    pxb <- pxb + st$psi[i, 1]
    pxs <- pxs + st$psi[i, 1] * st$psi[i, 1]
    xns <- xns + 1
  }
  pxb <- pxb / xns
  st$pxs <- pxs - xns * pxb * pxb
  st$pxb <- pxb
  st$esum <- esum
  st
}

.bbt_blackbt <- function(xb, np, ny, nf, nfx) {
  x <- xb
  xs <- xb
  xss <- xb
  ll <- numeric(ny)
  dc <- numeric(ny)
  ktot <- 0
  svsum <- 0
  swsum <- 0
  for (i in seq_len(np)) {
    s <- 0
    sa <- 0
    kk <- 0
    for (j in seq_len(ny)) {
      if (!.bbt_miss(x[i, j])) {
        s <- s + x[i, j] * x[i, j]
        sa <- sa + x[i, j]
        kk <- kk + 1
      }
    }
    ktot <- ktot + kk
    svsum <- svsum + s
    swsum <- swsum + sa
    for (j in seq_len(ny)) {
      if (!.bbt_miss(x[i, j])) {
        ll[j] <- ll[j] + 1
        dc[j] <- dc[j] + x[i, j]
      }
    }
  }
  svsum <- svsum - (swsum * swsum) / ktot
  ltot <- np * ny
  fits2 <- c(np, ny, ktot, ltot - ktot, (ltot - ktot) / ltot * 100, svsum)
  dc <- dc / ll
  ll <- .bbt_corr3(x, np, ny)
  psix <- matrix(0, np, nf + 1)
  xt <- matrix(0, np, 2)
  w <- matrix(0, ny, nf + 2)
  for (jjj in seq_len(nf)) {
    xxk <- 0
    txb <- 0
    kkt <- 0
    for (i in seq_len(np)) {
      kk <- 0
      s <- 0
      for (j in seq_len(ny)) {
        if (.bbt_miss(x[i, j])) next
        s <- s + (x[i, j] + (if (jjj == 1) -dc[j] else 0)) * ll[j]
        kk <- kk + 1
      }
      xt[i, 1] <- -999
      if (kk < nfx) next
      wxb <- s / kk
      kkt <- kkt + 1
      txb <- txb + wxb
      xt[i, 1] <- wxb
      xxk <- xxk + wxb * wxb
    }
    txb <- txb / kkt
    xxk <- xxk - kkt * txb * txb
    for (i in seq_len(np)) {
      if (!.bbt_miss(xt[i, 1])) {
        xt[i, 1] <- xt[i, 1] - txb
        xt[i, 2] <- 1
      }
    }
    for (mm in 1:4) {
      st <- .bbt_regt(np, 1, ny, list(w = w, xs = xs, x = x, psi = xt))
      st <- .bbt_reg2t(np, 1, ny, st, 1)
      w <- st$w
      x <- st$x
      xt <- st$psi
      xcor <- sqrt(xxk / st$pxs)
      for (i in seq_len(np)) {
        psix[i, jjj] <- (xt[i, 1] - st$pxb) * xcor
        xt[i, 1] <- (xt[i, 1] - st$pxb) * xcor
        if (psix[i, jjj] <= -99) {
          xt[i, 1] <- -999
          psix[i, jjj] <- -999
        }
      }
    }
    last <- jjj == nf
    if (last) {
      psix[, nf + 1] <- 1
      x <- xss
    }
    xs <- x
    if (!last) ll <- .bbt_corr3(x, np, ny)
  }
  for (nn in 1:5) {
    st <- .bbt_regt(np, nf, ny, list(w = w, xs = xs, x = x, psi = psix))
    areg <- st$tsum
    st <- .bbt_reg2t(np, nf, ny, st, nf)
    w <- st$w
    x <- st$x
    psix <- st$psi
    for (k in seq_len(nf)) {
      s <- 0
      for (i in seq_len(np)) s <- s + psix[i, k]
      s <- s / np
      psix[, k] <- psix[, k] - s
    }
    if (abs(areg - st$esum) < 0.01) break
  }
  for (i in seq_len(np)) {
    if (.bbt_miss(psix[i, 1])) next
    for (k in seq_len(ny)) {
      s <- 0
      for (j in seq_len(nf + 1)) s <- s + psix[i, j] * w[k, j]
      x[i, k] <- s
    }
  }
  pw <- x - matrix(w[, nf + 1], np, ny, byrow = TRUE)
  sv <- .bbt_svd_v(t(pw), np)
  xdata <- matrix(0, np, nf)
  for (i in seq_len(np)) for (jj in seq_len(nf)) xdata[i, jj] <- sv$V[i, jj] * sqrt(sv$sv[jj])
  wout <- matrix(0, ny, nf + 2)
  for (k in seq_len(ny)) {
    wout[k, 1] <- w[k, nf + 1]
    for (jj in seq_len(nf)) {
      u <- 0
      for (i in seq_len(np)) u <- u + pw[i, k] * sv$V[i, jj]
      u <- if (sv$sv[jj] > 0) u / sv$sv[jj] else 0
      wout[k, jj + 1] <- u * sqrt(sv$sv[jj])
    }
    wout[k, nf + 2] <- w[k, nf + 2]
  }
  list(xdata = xdata, w = wout, svsum = svsum, fits2 = fits2)
}

.bbt_r2 <- function(acc) {
  a3 <- acc[1] * acc[6] - acc[2] * acc[3]
  b3 <- acc[1] * acc[4] - acc[2] * acc[2]
  c3 <- acc[1] * acc[5] - acc[3] * acc[3]
  if (abs(b3 * c3) > 0) (a3 * a3) / (b3 * c3) else 0
}

#' Blackbox-transpose scaling of a respondent-by-stimulus rating matrix
#'
#' A line-for-line port of the BLACKBOXT Fortran routine of the basicspace
#' package. The stimuli-by-respondents matrix is decomposed as
#' \code{X = P W' + J c' + E} one dimension at a time by alternating least
#' squares (starting values from sign-corrected row means, column signs from
#' the pairwise-complete correlation matrix), refined with all dimensions
#' jointly, and the fitted \code{P W'} is re-expressed by its singular value
#' decomposition. Stimulus coordinates are the unit right singular vectors;
#' respondent weights are \code{U sqrt(s)} with intercepts \code{c}. In two
#' dimensions the solution is rotated so its first axis matches the
#' one-dimensional solution, as in the Fortran. Respondents with fewer than
#' \code{dims + 2} answers are dropped (\code{NULL} rows); stimuli with fewer
#' than 8 answers are excluded from the starting values. Needs more scaled
#' respondents than stimuli. Singular vectors are signed so that each one's
#' largest entry is positive (LAPACK's signs are arbitrary), so columns can
#' differ in sign from basicspace, which also rounds to three decimals.
#' Identical to the Python arm \code{morie.fn.blackboxt}.
#'
#' @param data Respondents (rows) by stimuli (columns) numeric matrix;
#'   \code{NA} or any value in \code{missing} is missing.
#' @param missing Optional vector of missing-value codes.
#' @param dims Number of dimensions (solutions for 1 to dims are returned).
#' @return A list with \code{stimuli} (per dimension, rows
#'   \code{n, coord_1..coord_d, R2}), \code{individuals} (per dimension, rows
#'   \code{c, w_1..w_d, R2} or \code{NULL}), \code{fits} (per dimension: SSE,
#'   SSE_explained, percent, cumulative_percent, R2, SE, singular),
#'   \code{n_row} (stimuli), \code{n_col} (scaled respondents), \code{n_data},
#'   \code{n_miss}, \code{ss_mean} and \code{dims}.
#' @references Poole, K. T. (1998). Recovering a basic space from a set of
#'   issue scales. American Journal of Political Science 42, 954-993.
#'
#'   Poole, K., Lewis, J., Rosenthal, H., Lo, J. and Carroll, R. (2016).
#'   Recovering a basic space from issue scales in R. Journal of Statistical
#'   Software 69(7), 1-21.
#' @examples
#' X <- outer(1:12, 1:4, function(i, j) (i * 7 + j * 3) %% 10 + ifelse(i %% 2 == 1, -j, j) * 0.3)
#' r <- BlackboxTranspose(X, dims = 1)
#' r$fits[[1]]$R2
#' @export
BlackboxTranspose <- function(data, missing = NULL, dims = 1) {
  if (dims < 1) stop("dims must be positive")
  X <- unname(as.matrix(data))
  storage.mode(X) <- "double"
  bad <- is.na(X)
  for (m in missing) bad <- bad | (abs(X - m) <= 0.001 & !is.na(X))
  X[bad] <- -999
  n <- nrow(X)
  nq <- ncol(X)
  keep <- which(rowSums(!.bbt_miss(X)) >= dims + 2)
  ny <- length(keep)
  if (ny <= nq) stop("BlackboxTranspose needs more scaled respondents than stimuli")
  xb <- t(X[keep, , drop = FALSE])
  stimuli <- list()
  individuals <- list()
  rsave <- list()
  work5 <- NULL
  psisave <- NULL
  for (kkk in seq_len(dims)) {
    bt <- .bbt_blackbt(xb, nq, ny, kkk, 8)
    xdata <- bt$xdata
    w <- bt$w
    xt <- matrix(0, ny, nq)
    tot <- numeric(6)
    sume <- 0
    for (j in seq_len(ny)) {
      acc <- numeric(6)
      for (i in seq_len(nq)) {
        s <- 0
        for (k in seq_len(kkk)) s <- s + xdata[i, k] * w[j, k + 1]
        xt[j, i] <- s
        aa <- s + w[j, 1]
        if (.bbt_miss(xb[i, j])) next
        bb <- xb[i, j]
        sume <- sume + (aa - bb) * (aa - bb)
        acc <- acc + c(1, aa, bb, aa * aa, bb * bb, aa * bb)
      }
      w[j, kkk + 2] <- .bbt_r2(acc)
      tot <- tot + acc
    }
    den <- tot[1] - kkk * (nq + ny) - ny
    rsave[[kkk]] <- c(sume, .bbt_r2(tot), if (den > 0 && sume >= 0) sqrt(sume / den) else NaN)
    sv <- .bbt_svd_v(xt, nq)
    V <- sv$V
    work5 <- sv$sv
    coords <- V[, seq_len(kkk), drop = FALSE]
    if (kkk == 2) {
      rot <- matrix(0, 2, 2)
      for (i in seq_len(nq)) for (jj in 1:2) rot[jj, 1] <- rot[jj, 1] + V[i, jj] * psisave[i]
      s <- rot[1, 1] * rot[1, 1] + rot[2, 1] * rot[2, 1]
      rot[, 1] <- rot[, 1] / sqrt(s)
      rot[1, 2] <- -rot[2, 1]
      rot[2, 2] <- rot[1, 1]
      w2 <- w
      for (j in seq_len(ny)) {
        for (ijj in 1:2) {
          s <- 0
          for (jj in 1:2) s <- s + rot[jj, ijj] * w[j, jj + 1]
          w2[j, ijj + 1] <- s
        }
      }
      for (i in seq_len(nq)) {
        for (ijj in 1:2) {
          s <- 0
          for (jj in 1:2) s <- s + V[i, jj] * rot[jj, ijj]
          coords[i, ijj] <- s
        }
      }
    }
    stim <- list()
    for (i in seq_len(nq)) {
      acc <- numeric(6)
      for (j in seq_len(ny)) {
        s <- 0
        for (k in seq_len(kkk)) s <- s + xdata[i, k] * w[j, k + 1]
        aa <- s + w[j, 1]
        if (.bbt_miss(xb[i, j])) next
        bb <- xb[i, j]
        acc <- acc + c(1, aa, bb, aa * aa, bb * bb, aa * bb)
      }
      stim[[i]] <- c(acc[1], coords[i, ], .bbt_r2(acc))
    }
    if (kkk == 1) psisave <- V[, 1]
    if (kkk == 2) w <- w2
    ind <- vector("list", n)
    for (ki in seq_along(keep)) ind[[keep[ki]]] <- w[ki, seq_len(kkk + 2)]
    stimuli[[kkk]] <- stim
    individuals[[kkk]] <- ind
  }
  svsum <- bt$svsum
  fits <- list()
  for (j in seq_len(dims)) {
    prev <- if (j == 1) svsum else rsave[[j - 1]][1]
    fits[[j]] <- list(SSE = rsave[[j]][1], SSE_explained = svsum - rsave[[j]][1],
                      percent = (prev - rsave[[j]][1]) / svsum * 100,
                      cumulative_percent = (svsum - rsave[[j]][1]) / svsum * 100,
                      R2 = rsave[[j]][2], SE = rsave[[j]][3], singular = work5[j])
  }
  f2 <- bt$fits2
  list(stimuli = stimuli, individuals = individuals, fits = fits, n_row = f2[1], n_col = f2[2], n_data = f2[3],
       n_miss = f2[4], ss_mean = f2[6], dims = dims)
}
