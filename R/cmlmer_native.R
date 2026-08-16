# R arm of cmlmer -- compressed mixed linear model: average-linkage grouping
# on 1 - K, a per-group random effect, the variance ratio profiled out by
# REML, and a GLS t-test per marker.
# Zhang, Z. et al. (2010) Nature Genetics 42(4), 355-360; Yu, J. et al.
# (2006) Nature Genetics 38(2), 203-208.
# Mirrors src/morie/fn/cmlmer.py.

.cmlmer_EPS <- 1e-12


# Maximise f over [lo, hi] by a staged fixed-grid argmax.
#
# A golden-section search is PATH-DEPENDENT. Each arm walks its own sequence
# of brackets, and near a flat maximum the fc > fd branch is decided by the
# last bits of two nearly equal likelihoods, so the two languages take
# different paths and land on different answers. Quantising the result
# afterwards hides that only when the answer does not fall near a cell
# boundary, which is a coincidence rather than a guarantee: the measured
# failure was two arms landing on ADJACENT points of a 1e-6 grid.
#
# Here both arms evaluate the SAME list of points -- a + (i - 1) * step is
# the same double in both languages -- and take the argmax BY INDEX, ties to
# the lowest index. The winning index is therefore the same by construction,
# and the value returned is an exact grid point rather than a bracket
# midpoint, so the two arms return bit-identical doubles. R indexes from one
# and Python from zero; the indexing stays native in each because the index
# is internal and never reported.
#
# Refinement stops while adjacent grid values still differ by far more than
# floating-point noise. Going finer would push the comparison back below the
# noise floor and reintroduce exactly the disagreement this exists to remove.
#' Refinement stops while adjacent grid values still differ by far more
#' than
#'
#' floating-point noise. Going finer would push the comparison back
#' below the noise floor and reintroduce exactly the disagreement this
#' exists to remove.
#'
#' @param f See Usage.
#' @param lo See Usage.
#' @param hi See Usage.
#' @param points Defaults to \code{201L}.
#' @param stages Defaults to \code{4L}.
#' @return The value of \code{a}, as built in the body.
#' @export
.cmlmer_gridmax <- function(f, lo, hi, points = 201L, stages = 4L) {
  a <- as.numeric(lo); b <- as.numeric(hi)
  npt <- as.integer(points)
  nst <- as.integer(stages)
  for (s in seq_len(nst)) {
    step <- (b - a) / (npt - 1L)
    vals <- vapply(seq_len(npt),
                   function(i) f(a + (i - 1L) * step), numeric(1))
    best <- 1L
    for (i in seq_len(npt)) if (vals[i] > vals[best]) best <- i
    if (s == nst) return(a + (best - 1L) * step)
    lo_i <- if (best > 1L) best - 1L else 1L
    hi_i <- if (best < npt) best + 1L else npt
    a2 <- a + (lo_i - 1L) * step
    b <- a + (hi_i - 1L) * step
    a <- a2
    npt <- 21L
  }
  a
}

# Quantise a UPGMA distance to a 1e-12 grid. Average linkage merges the
# closest pair, and a tie broken differently in the two languages would
# give different groups and therefore a different model. The grid is far
# finer than any distance the caller can distinguish and far coarser than
# the last-bit noise that would otherwise decide a tie.
#' Quantise a UPGMA distance to a 1e-12 grid. Average linkage merges the
#'
#' closest pair, and a tie broken differently in the two languages would
#' give different groups and therefore a different model. The grid is
#' far finer than any distance the caller can distinguish and far
#' coarser than the last-bit noise that would otherwise decide a tie.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.cmlmer_snap12 <- function(x) floor(x * 1e12 + 0.5) / 1e12

#' .cmlmer_rows
#'
#' A step of the cmlmer_native implementation. Called by \code{morie_cmlmer_compressed_lmm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A matrix; passed to \code{as.matrix}.
#' @return The value of \code{m}, as built in the body.
#' @export
.cmlmer_rows <- function(x) {
  if (is.matrix(x)) m <- x
  else if (is.data.frame(x)) m <- as.matrix(x)
  else if (is.list(x)) m <- do.call(rbind, lapply(x, as.numeric))
  else m <- matrix(as.numeric(x), ncol = 1L)
  storage.mode(m) <- "double"
  m
}

#' .cmlmer_chol
#'
#' A step of the cmlmer_native implementation. Called by \code{.cmlmer_reml_at}, \code{morie_cmlmer_compressed_lmm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; indexed by row and column.
#' @return The value of \code{L}, as built in the body.
#' @export
.cmlmer_chol <- function(A) {
  n <- nrow(A)
  L <- matrix(0.0, n, n)
  jit <- 1e-12 * max(abs(sum(diag(A)) / n), 1.0)
  for (i in seq_len(n)) {
    for (j in seq_len(i)) {
      s <- A[i, j]
      if (j > 1L) s <- s - sum(L[i, seq_len(j - 1L)] * L[j, seq_len(j - 1L)])
      if (i == j) {
        s <- s + jit
        if (s <= 0.0)
          stop("cmlmer: the covariance matrix is not positive definite")
        L[i, i] <- sqrt(s)
      } else {
        L[i, j] <- s / L[j, j]
      }
    }
  }
  L
}

#' .cmlmer_solve
#'
#' A step of the cmlmer_native implementation. Called by \code{.cmlmer_reml_at}, \code{morie_cmlmer_compressed_lmm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param L A matrix; indexed by row and column.
#' @param b A vector; indexed elementwise.
#' @return The value of \code{x}, as built in the body.
#' @export
.cmlmer_solve <- function(L, b) {
  n <- nrow(L)
  z <- numeric(n)
  for (i in seq_len(n)) {
    s <- b[i]
    if (i > 1L) s <- s - sum(L[i, seq_len(i - 1L)] * z[seq_len(i - 1L)])
    z[i] <- s / L[i, i]
  }
  x <- numeric(n)
  for (i in seq.int(n, 1L)) {
    s <- z[i]
    if (i < n) s <- s - sum(L[seq.int(i + 1L, n), i] * x[seq.int(i + 1L, n)])
    x[i] <- s / L[i, i]
  }
  x
}

#' .cmlmer_logdet
#'
#' A step of the cmlmer_native implementation. Called by \code{.cmlmer_reml_at}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param L A matrix; passed to \code{diag}.
#' @return A numeric value.
#' @export
.cmlmer_logdet <- function(L) 2.0 * sum(log(diag(L)))

# Average-linkage clustering on 1 - K, cut at g groups. Deterministic
# throughout: ties are broken by the smaller index pair, so two
# implementations agree on the dendrogram, not merely on its quality.
#' Average-linkage clustering on 1 - K, cut at g groups. Deterministic
#'
#' throughout: ties are broken by the smaller index pair, so two
#' implementations agree on the dendrogram, not merely on its quality.
#'
#' @param K A matrix; passed to \code{nrow}.
#' @param g See Usage.
#' @return A list with \code{lab}, \code{groups}.
#' @export
.cmlmer_upgma <- function(K, g) {
  n <- nrow(K)
  members <- lapply(seq_len(n), function(i) i)
  # the distances are quantised before any comparison. Two candidate merges
  # separated by about 1e-15 are a coin flip between languages that compute
  # 1 - K to different last bits, and one different merge changes the whole
  # dendrogram below it.
  D <- .cmlmer_snap12(1.0 - K)
  alive <- seq_len(n)
  while (length(alive) > g) {
    bi <- -1L; bj <- -1L; best <- NULL
    for (ai in seq_len(length(alive) - 1L)) {
      for (aj in seq.int(ai + 1L, length(alive))) {
        d <- D[alive[ai], alive[aj]]
        if (is.null(best) || d < best - 1e-15) {
          best <- d; bi <- alive[ai]; bj <- alive[aj]
        }
      }
    }
    na <- length(members[[bi]]); nb <- length(members[[bj]])
    for (cc in alive) {
      if (cc == bi || cc == bj) next
      nd <- .cmlmer_snap12((na * D[bi, cc] + nb * D[bj, cc]) / (na + nb))
      D[bi, cc] <- nd; D[cc, bi] <- nd
    }
    members[[bi]] <- c(members[[bi]], members[[bj]])
    members[[bj]] <- integer(0)
    alive <- alive[alive != bj]
  }
  groups <- lapply(alive, function(i) members[[i]])
  # canonical group order: by the smallest member index, so the labels are a
  # property of the data and not of the merge order
  groups <- groups[order(vapply(groups, min, 0L))]
  lab <- integer(n)
  for (j in seq_along(groups)) for (i in groups[[j]]) lab[i] <- j - 1L
  list(lab = lab, groups = groups)
}

#' .cmlmer_reml_at
#'
#' A step of the cmlmer_native implementation. Called by \code{morie_cmlmer_compressed_lmm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param logdelta Numeric; passed to \code{exp}.
#' @param Vk See Usage.
#' @param y A vector; its length is taken.
#' @param X A matrix; indexed by row and column.
#' @return A list with \code{ll}, \code{delta}, \code{beta}, \code{s2g}, \code{L}.
#' @export
.cmlmer_reml_at <- function(logdelta, Vk, y, X) {
  n <- length(y); p <- ncol(X)
  delta <- exp(logdelta)
  V <- Vk
  diag(V) <- diag(V) + delta
  L <- .cmlmer_chol(V)
  Viy <- .cmlmer_solve(L, y)
  ViX <- vapply(seq_len(p), function(a) .cmlmer_solve(L, X[, a]), numeric(n))
  if (!is.matrix(ViX)) ViX <- matrix(ViX, nrow = n)
  XtViX <- crossprod(X, ViX)
  Lx <- .cmlmer_chol(XtViX)
  beta <- .cmlmer_solve(Lx, as.numeric(crossprod(X, Viy)))
  r <- y - as.numeric(X %*% beta)
  Vir <- .cmlmer_solve(L, r)
  dfr <- n - p
  s2g <- sum(r * Vir) / dfr
  ll <- -0.5 * (dfr * log(max(s2g, 1e-300)) + .cmlmer_logdet(L) +
                  .cmlmer_logdet(Lx) + dfr)
  list(ll = ll, delta = delta, beta = beta, s2g = s2g, L = L)
}

#' morie_cmlmer_compressed_lmm
#'
#' A step of the cmlmer_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y See Usage.
#' @param M Optional; may be \code{NULL}. A vector; its length is taken.
#' @param K Passed to \code{.cmlmer_rows}.
#' @param clusters Defaults to \code{NULL}.
#' @param X Optional; may be \code{NULL}. Passed to \code{.cmlmer_rows}.
#' @param compare_levels Optional; may be \code{NULL}. A vector; its length is taken.
#' @param log_delta_lo Defaults to \code{-10}.
#' @param log_delta_hi Defaults to \code{10}.
#' @param max_iter Defaults to \code{200L}.
#' @return A list with \code{estimate}, \code{beta}, \code{se}, \code{t}, \code{p_value}, \code{group}, \code{n_groups}, \code{group_sizes}, \code{group_kinship}, \code{coefficients}, \code{delta}, \code{sigma2_g}, \code{sigma2_e}, \code{h2}, \code{reml_loglik}, \code{reml_profile}, \code{level_loglik}, \code{n}, \code{n_markers}, \code{p}, \code{clusters_requested}, \code{method}, \code{note}.
#' @export
morie_cmlmer_compressed_lmm <- function(y, M, K, clusters = NULL, X = NULL,
                                        compare_levels = NULL,
                                        log_delta_lo = -10.0,
                                        log_delta_hi = 10.0,
                                        max_iter = 200L) {
  yv <- as.numeric(y)
  n <- length(yv)
  if (n == 0L) stop("cmlmer: no observations")
  Km <- .cmlmer_rows(K)
  if (nrow(Km) != n || ncol(Km) != n)
    stop(sprintf("cmlmer: K must be %d by %d", n, n))
  asym <- max(abs(Km - t(Km)))
  if (asym > 1e-8)
    stop(sprintf("cmlmer: K is not symmetric (largest asymmetry %.3g)", asym))
  Mm <- if (is.null(M) || length(M) == 0L) NULL else .cmlmer_rows(M)
  if (!is.null(Mm) && nrow(Mm) != n)
    stop(sprintf("cmlmer: %d phenotypes but %d marker rows", n, nrow(Mm)))
  nm <- if (is.null(Mm)) 0L else ncol(Mm)
  Xm <- if (is.null(X)) matrix(1.0, n, 1L) else .cmlmer_rows(X)
  p <- ncol(Xm)
  if (n - p - 1L < 1L)
    stop(sprintf(paste0("cmlmer: too few observations for %d fixed effects ",
                        "plus a marker"), p))
  g <- if (is.null(clusters)) n else as.integer(clusters)
  if (g < 1L || g > n)
    stop(sprintf(paste0("cmlmer: the number of groups must be between 1 and ",
                        "%d, got %d"), n, g))

  cl <- .cmlmer_upgma(Km, g)
  lab <- cl$lab; groups <- cl$groups
  ng <- length(groups)
  Kg <- matrix(0.0, ng, ng)
  for (a in seq_len(ng)) for (b in seq_len(ng))
    Kg[a, b] <- sum(Km[groups[[a]], groups[[b]]]) /
      (length(groups[[a]]) * length(groups[[b]]))
  ZKZ <- Kg[lab + 1L, lab + 1L, drop = FALSE]

  # max_iter is accepted and ignored: the grid schedule fixes the
  # evaluation count, and dropping the argument would break callers.
  logdelta <- .cmlmer_gridmax(
    function(t) .cmlmer_reml_at(t, ZKZ, yv, Xm)$ll,
    as.numeric(log_delta_lo), as.numeric(log_delta_hi))
  fit <- .cmlmer_reml_at(logdelta, ZKZ, yv, Xm)
  ll <- fit$ll; delta <- fit$delta; beta0 <- fit$beta
  s2g <- fit$s2g; L <- fit$L
  s2e <- delta * s2g
  h2 <- s2g / (s2g + s2e)

  profile <- list()
  for (t in 0:20) {
    lt <- as.numeric(log_delta_lo) +
      (as.numeric(log_delta_hi) - as.numeric(log_delta_lo)) * t / 20.0
    profile[[length(profile) + 1L]] <-
      c(lt, .cmlmer_reml_at(lt, ZKZ, yv, Xm)$ll)
  }

  # ---- per-marker GLS test under the fitted covariance
  mb <- numeric(nm); mse <- numeric(nm); mt <- numeric(nm); mp <- numeric(nm)
  Viy <- .cmlmer_solve(L, yv)
  for (j in seq_len(nm)) {
    Xj <- cbind(Xm, Mm[, j])
    q <- p + 1L
    ViX <- vapply(seq_len(q), function(a) .cmlmer_solve(L, Xj[, a]),
                  numeric(n))
    if (!is.matrix(ViX)) ViX <- matrix(ViX, nrow = n)
    A <- crossprod(Xj, ViX)
    rhs <- as.numeric(crossprod(Xj, Viy))
    Lj <- tryCatch(.cmlmer_chol(A), error = function(e) NULL)
    if (is.null(Lj)) {
      mb[j] <- NaN; mse[j] <- NaN; mt[j] <- NaN; mp[j] <- NaN; next
    }
    bj <- .cmlmer_solve(Lj, rhs)
    r <- yv - as.numeric(Xj %*% bj)
    Vir <- .cmlmer_solve(L, r)
    s2 <- sum(r * Vir) / (n - q)
    e <- numeric(q); e[q] <- 1.0
    cjj <- .cmlmer_solve(Lj, e)[q]
    se <- sqrt(max(s2 * cjj, 0.0))
    tj <- if (se > .cmlmer_EPS) bj[q] / se else NaN
    mb[j] <- bj[q]; mse[j] <- se; mt[j] <- tj
    mp[j] <- if (!is.nan(tj)) 2.0 * (1.0 - pnorm(abs(tj))) else NaN
  }

  levels_ <- list()
  if (!is.null(compare_levels) && length(compare_levels)) {
    for (gl in as.integer(compare_levels)) {
      if (gl < 1L || gl > n)
        stop(sprintf("cmlmer: compare_levels entry %d is outside 1..%d",
                     gl, n))
      cl2 <- .cmlmer_upgma(Km, gl)
      gr2 <- cl2$groups; ng2 <- length(gr2)
      Kg2 <- matrix(0.0, ng2, ng2)
      for (a in seq_len(ng2)) for (b in seq_len(ng2))
        Kg2[a, b] <- sum(Km[gr2[[a]], gr2[[b]]]) /
          (length(gr2[[a]]) * length(gr2[[b]]))
      ZKZ2 <- Kg2[cl2$lab + 1L, cl2$lab + 1L, drop = FALSE]
      best <- NULL
      for (t in 0:40) {
        lt <- as.numeric(log_delta_lo) +
          (as.numeric(log_delta_hi) - as.numeric(log_delta_lo)) * t / 40.0
        v <- .cmlmer_reml_at(lt, ZKZ2, yv, Xm)$ll
        if (is.null(best) || v > best) best <- v
      }
      levels_[[length(levels_) + 1L]] <- c(gl, best)
    }
  }

  list(estimate = mb, beta = mb, se = mse, t = mt, p_value = mp,
       group = as.numeric(lab), n_groups = as.integer(ng),
       group_sizes = vapply(groups, length, 0L),
       group_kinship = Kg,
       coefficients = beta0, delta = delta,
       sigma2_g = s2g, sigma2_e = s2e, h2 = h2,
       reml_loglik = ll, reml_profile = do.call(rbind, profile),
       level_loglik = if (length(levels_)) do.call(rbind, levels_) else
         list(),
       n = as.integer(n), n_markers = as.integer(nm), p = as.integer(p),
       clusters_requested = as.integer(g),
       method = paste0("compressed mixed linear model: average-linkage ",
                       "grouping on 1 - K, a per-group random effect with ",
                       "the average between-group kinship, the variance ",
                       "ratio profiled out by REML, and a GLS t-test per ",
                       "marker (Zhang et al. 2010; Yu et al. 2006)"),
       note = paste0("with one group per individual Z is the identity and ",
                     "the group kinship is K itself, so the compressed ",
                     "model contains the uncompressed one exactly rather ",
                     "than approximating it"))
}

#' .cmlmer_cheatsheet
#'
#' A step of the cmlmer_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.cmlmer_cheatsheet <- function() {
  paste0("cmlmer: morie_cmlmer_compressed_lmm(y, M, K, clusters) -> ",
         "compressed MLM genome scan with REML variance components ",
         "(Zhang et al. 2010, Nature Genetics 42:355-360)")
}

morie_cmlmer <- morie_cmlmer_compressed_lmm
