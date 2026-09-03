# R arm of sxrhrt -- bivariate REML treating the sexes as two traits on
# disjoint individuals, with the genetic correlation parameterised directly
# and bounded to (-1, 1).
# Yang, J. et al. (2011) Am. J. Hum. Genet. 88(1), 76-82 (GCTA);
# Lee, S. H. et al. (2012) Bioinformatics 28(19), 2540-2542.
# Mirrors src/morie/fn/sxrhrt.py.

.sxrhrt_EPS <- 1e-12


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
#' @param f Accepted by the signature and not used anywhere in the body.
#' @param lo Coerced to numeric by the body, with \code{as.numeric}.
#' @param hi Coerced to numeric by the body, with \code{as.numeric}.
#' @param points Coerced to integer by the body, with \code{as.integer}. Defaults to \code{201L}.
#' @param stages Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4L}.
#' @return The value of \code{a}, as built in the body.
#' @export
.sxrhrt_gridmax <- function(f, lo, hi, points = 201L, stages = 4L) {
  a <- as.numeric(lo)
  b <- as.numeric(hi)
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

#' .sxrhrt_rows
#'
#' A step of the sxrhrt_native implementation. Called by \code{morie_sxrhrt_sex_specific_h2}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A matrix; passed to \code{as.matrix}.
#' @return The value of \code{m}, as built in the body.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .sxrhrt_rows(x = x)
#' res
.sxrhrt_rows <- function(x) {
  if (is.matrix(x)) m <- x
  else if (is.data.frame(x)) m <- as.matrix(x)
  else if (is.list(x)) m <- do.call(rbind, lapply(x, as.numeric))
  else m <- matrix(as.numeric(x), ncol = 1L)
  storage.mode(m) <- "double"
  m
}

#' .sxrhrt_chol
#'
#' A step of the sxrhrt_native implementation. Called by \code{.sxrhrt_reml}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; indexed by row and column.
#' @return The value of \code{L}, as built in the body.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' res <- .sxrhrt_chol(A = A)
#' res
.sxrhrt_chol <- function(A) {
  n <- nrow(A)
  L <- matrix(0.0, n, n)
  jit <- 1e-11 * max(abs(sum(diag(A)) / n), 1.0)
  for (i in seq_len(n)) {
    for (j in seq_len(i)) {
      s <- A[i, j]
      if (j > 1L) s <- s - sum(L[i, seq_len(j - 1L)] * L[j, seq_len(j - 1L)])
      if (i == j) {
        s <- s + jit
        if (s <= 0.0) return(NULL)
        L[i, i] <- sqrt(s)
      } else {
        L[i, j] <- s / L[j, j]
      }
    }
  }
  L
}

#' .sxrhrt_solve
#'
#' A step of the sxrhrt_native implementation. Called by \code{.sxrhrt_reml}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param L A matrix; indexed by row and column.
#' @param b A vector; indexed elementwise.
#' @return The value of \code{x}, as built in the body.
#' @export
.sxrhrt_solve <- function(L, b) {
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

#' .sxrhrt_logdet
#'
#' A step of the sxrhrt_native implementation. Called by \code{.sxrhrt_reml}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param L A matrix; passed to \code{diag}.
#' @return A numeric value.
#' @export
.sxrhrt_logdet <- function(L) 2.0 * sum(log(diag(L)))

# Restricted log likelihood at (s2gm, s2gf, rg, s2em, s2ef).
#' Restricted log likelihood at (s2gm, s2gf, rg, s2em, s2ef)
#'
#' A step of the sxrhrt_native implementation. Called by \code{morie_sxrhrt_sex_specific_h2}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param theta A vector; indexed elementwise.
#' @param y A vector; its length is taken.
#' @param X A matrix; indexed by row and column.
#' @param Km A matrix; indexed by row and column.
#' @param male A vector; indexed elementwise.
#' @return A list with \code{ll}, \code{beta}, \code{L}.
#' @export
.sxrhrt_reml <- function(theta, y, X, Km, male) {
  s2gm <- theta[1]
  s2gf <- theta[2]
  rg <- theta[3]
  s2em <- theta[4]
  s2ef <- theta[5]
  if (s2gm <= 0.0 || s2gf <= 0.0 || s2em <= 0.0 || s2ef <= 0.0) return(NULL)
  if (abs(rg) >= 1.0) return(NULL)
  n <- length(y)
  p <- ncol(X)
  cov2 <- rg * sqrt(s2gm * s2gf)
  S <- matrix(0.0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      S[i, j] <- if (male[i] && male[j]) s2gm * Km[i, j] else
        if (!male[i] && !male[j]) s2gf * Km[i, j] else cov2 * Km[i, j]
    }
    S[i, i] <- S[i, i] + (if (male[i]) s2em else s2ef)
  }
  L <- .sxrhrt_chol(S)
  if (is.null(L)) return(NULL)
  Viy <- .sxrhrt_solve(L, y)
  ViX <- vapply(seq_len(p), function(a) .sxrhrt_solve(L, X[, a]), numeric(n))
  if (!is.matrix(ViX)) ViX <- matrix(ViX, nrow = n)
  XtViX <- crossprod(X, ViX)
  Lx <- .sxrhrt_chol(XtViX)
  if (is.null(Lx)) return(NULL)
  beta <- .sxrhrt_solve(Lx, as.numeric(crossprod(X, Viy)))
  r <- y - as.numeric(X %*% beta)
  Vir <- .sxrhrt_solve(L, r)
  ll <- -0.5 * (.sxrhrt_logdet(L) + .sxrhrt_logdet(Lx) + sum(r * Vir))
  list(ll = ll, beta = beta, L = L)
}

#' morie_sxrhrt_sex_specific_h2
#'
#' A step of the sxrhrt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param sex Coerced to numeric by the body, with \code{as.numeric}.
#' @param K Passed to \code{.sxrhrt_rows}.
#' @param X Optional; may be \code{NULL}. Passed to \code{.sxrhrt_rows}.
#' @param max_cycles Coerced to integer by the body, with \code{as.integer}. Defaults to \code{60L}.
#' @param tol Accepted by the signature and not used anywhere in the body. Defaults to \code{1e-09}.
#' @param male_label Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{estimate}, \code{h2_male}, \code{h2_female}, \code{rg},
#' \code{sigma2_g_male}, \code{sigma2_g_female}, \code{sigma2_g_cross},
#' \code{sigma2_e_male}, \code{sigma2_e_female}, \code{coefficients}, \code{reml_loglik},
#' \code{reml_path}, \code{lrt_rg_equals_one}, \code{p_rg_equals_one},
#' \code{lrt_equal_h2}, \code{p_equal_h2}, \code{n}, \code{n_male}, \code{n_female},
#' \code{p}, \code{max_cross_sex_relatedness}, \code{cycles}, \code{converged},
#' \code{method}, \code{note}.
#' @export
morie_sxrhrt_sex_specific_h2 <- function(y, sex, K, X = NULL,
                                         max_cycles = 60L, tol = 1e-9,
                                         male_label = 1) {
  yv <- as.numeric(y)
  n <- length(yv)
  if (n == 0L) stop("sxrhrt: no observations")
  sv <- as.numeric(sex)
  if (length(sv) != n)
    stop(sprintf("sxrhrt: %d phenotypes but %d sex labels", n, length(sv)))
  male <- sv == as.numeric(male_label)
  nm <- sum(male)
  nf <- n - nm
  if (nm < 2L || nf < 2L)
    stop(sprintf(paste0("sxrhrt: %d in one sex and %d in the other -- a ",
                        "variance cannot be estimated from fewer than two"),
                 nm, nf))
  Km <- .sxrhrt_rows(K)
  if (nrow(Km) != n || ncol(Km) != n)
    stop(sprintf("sxrhrt: K must be %d by %d", n, n))
  asym <- max(abs(Km - t(Km)))
  if (asym > 1e-8)
    stop(sprintf("sxrhrt: K is not symmetric (largest asymmetry %.3g)",
                 asym))
  # the cross-sex block is the only source of information about rg
  cross <- max(abs(Km[outer(male, male, "!=")]))
  Xm <- if (is.null(X)) matrix(1.0, n, 1L) else .sxrhrt_rows(X)
  p <- ncol(Xm)

  mu <- sum(yv) / n
  vy <- sum((yv - mu) ^ 2) / max(n - 1L, 1L)
  theta <- c(max(vy / 2.0, 1e-6), max(vy / 2.0, 1e-6), 0.0,
             max(vy / 2.0, 1e-6), max(vy / 2.0, 1e-6))
  lo <- log(max(vy, 1e-8)) - 8.0
  hi <- log(max(vy, 1e-8)) + 4.0

  at <- function(th) {
    r <- .sxrhrt_reml(th, yv, Xm, Km, male)
    if (is.null(r)) -1e300 else r$ll
  }

  path <- at(theta)
  cycles <- 0L
  converged <- FALSE
  prev_theta <- NULL
  for (cycles in seq_len(as.integer(max_cycles))) {
    prev <- path[length(path)]
    for (idx in c(1L, 2L, 4L, 5L)) {
      local({
        ii <- idx
        f <- function(logv) {
          th <- theta
          th[ii] <- exp(logv)
          at(th)
        }
        theta[ii] <<- exp(.sxrhrt_gridmax(f, lo, hi))
      })
    }
    fr <- function(r) { th <- theta
    th[3] <- r
    at(th) }
    theta[3] <- .sxrhrt_gridmax(fr, -0.999, 0.999)
    cur <- at(theta)
    path <- c(path, cur)
    # convergence on the QUANTISED parameters, not on the likelihood -- see
    # hibrid: a last-bit difference makes the two languages stop one cycle
    # apart and land on different estimates
    if (!is.null(prev_theta) && all(theta == prev_theta)) {
      converged <- TRUE
      break
    }
    prev_theta <- theta
  }

  res <- .sxrhrt_reml(theta, yv, Xm, Km, male)
  if (is.null(res))
    stop(paste0("sxrhrt: the fitted covariance is not positive definite -- ",
                "the relationship matrix is probably not a valid GRM"))
  ll <- res$ll
  beta <- res$beta
  s2gm <- theta[1]
  s2gf <- theta[2]
  rg <- theta[3]
  s2em <- theta[4]
  s2ef <- theta[5]
  h2m <- s2gm / (s2gm + s2em)
  h2f <- s2gf / (s2gf + s2ef)

  # a likelihood-ratio test against rg = 1: does the architecture differ?
  th1 <- theta
  th1[3] <- 0.999999
  lrt_rg1 <- max(2.0 * (ll - at(th1)), 0.0)
  # and against equal heritabilities
  feq <- function(logv) { th <- theta
  th[1] <- exp(logv)
                          th[2] <- exp(logv)
                          at(th) }
  eq <- exp(.sxrhrt_gridmax(feq, lo, hi))
  th2 <- c(eq, eq, theta[3], theta[4], theta[5])
  for (i in seq_len(20L)) {
    for (idx in c(4L, 5L)) {
      local({
        ii <- idx
        f2 <- function(logv) { th <- th2
        th[ii] <- exp(logv)
        at(th) }
        th2[ii] <<- exp(.sxrhrt_gridmax(f2, lo, hi))
      })
    }
  }
  lrt_equal <- max(2.0 * (ll - at(th2)), 0.0)

  list(estimate = c(h2m, h2f), h2_male = h2m, h2_female = h2f, rg = rg,
       sigma2_g_male = s2gm, sigma2_g_female = s2gf,
       sigma2_g_cross = rg * sqrt(s2gm * s2gf),
       sigma2_e_male = s2em, sigma2_e_female = s2ef,
       coefficients = beta,
       reml_loglik = ll, reml_path = path,
       lrt_rg_equals_one = lrt_rg1,
       p_rg_equals_one = 0.5 * (1.0 - pnorm(sqrt(lrt_rg1))) * 2.0,
       lrt_equal_h2 = lrt_equal,
       p_equal_h2 = 2.0 * (1.0 - pnorm(sqrt(lrt_equal))),
       n = as.integer(n), n_male = as.integer(nm), n_female = as.integer(nf),
       p = as.integer(p),
       max_cross_sex_relatedness = cross,
       cycles = as.integer(cycles), converged = converged,
       method = paste0("bivariate REML treating the sexes as two traits on ",
                       "disjoint individuals, with the genetic correlation ",
                       "parameterised directly and bounded to (-1, 1) so ",
                       "the fitted covariance is admissible by construction ",
                       "(Yang et al. 2011 GCTA; Lee et al. 2012)"),
       note = paste0("max_cross_sex_relatedness is the diagnostic to read ",
                     "first: the cross-sex block of K is the only thing ",
                     "that identifies rg, and if it is near zero the ",
                     "correlation is not estimable however tight the ",
                     "likelihood looks"))
}

#' .sxrhrt_cheatsheet
#'
#' A step of the sxrhrt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .sxrhrt_cheatsheet()
#' res
.sxrhrt_cheatsheet <- function() {
  paste0("sxrhrt: morie_sxrhrt_sex_specific_h2(y, sex, K) -> per-sex ",
         "heritability and the cross-sex genetic correlation by bivariate ",
         "REML (Yang et al. 2011 GCTA; Lee et al. 2012)")
}

morie_sxrhrt <- morie_sxrhrt_sex_specific_h2
