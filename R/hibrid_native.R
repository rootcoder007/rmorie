# R arm of hibrid -- genomic hybrid prediction: additive GCA kernel from the
# sum of the parental relationship matrices, SCA kernel from their Hadamard
# product, variance ratios by a profiled cyclic fixed-grid search.
# Sprague, G. F. & Tatum, L. A. (1942) J. Am. Soc. Agron. 34(10), 923-932;
# Technow, F. et al. (2012) TAG 125(6), 1181-1194; Technow, F. et al. (2014)
# Genetics 197(4), 1343-1355.
# Mirrors src/morie/fn/hibrid.py.

.hibrid_EPS <- 1e-12
.hibrid_LO <- -14.0
.hibrid_HI <- 14.0


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
.hibrid_gridmax <- function(f, lo, hi, points = 201L, stages = 4L) {
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

.hibrid_rows <- function(x) {
  if (is.matrix(x)) m <- x
  else if (is.data.frame(x)) m <- as.matrix(x)
  else if (is.list(x)) m <- do.call(rbind, lapply(x, as.numeric))
  else m <- matrix(as.numeric(x), ncol = 1L)
  storage.mode(m) <- "double"
  m
}

.hibrid_chol <- function(A) {
  n <- nrow(A)
  L <- matrix(0.0, n, n)
  jit <- 1e-11 * max(abs(sum(diag(A)) / n), 1.0)
  for (i in seq_len(n)) {
    for (j in seq_len(i)) {
      s <- A[i, j]
      if (j > 1L) s <- s - sum(L[i, seq_len(j - 1L)] * L[j, seq_len(j - 1L)])
      if (i == j) {
        s <- s + jit
        if (s <= 0.0)
          stop("hibrid: the covariance matrix is not positive definite")
        L[i, i] <- sqrt(s)
      } else {
        L[i, j] <- s / L[j, j]
      }
    }
  }
  L
}

.hibrid_solve <- function(L, b) {
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

.hibrid_logdet <- function(L) 2.0 * sum(log(diag(L)))

# Restricted log likelihood at the two ratios, sigma_e^2 profiled out.
.hibrid_reml_at <- function(la, ls, Kg, Ks, y, X) {
  n <- length(y); p <- ncol(X)
  V <- Kg / la + Ks / ls
  diag(V) <- diag(V) + 1.0
  L <- .hibrid_chol(V)
  Viy <- .hibrid_solve(L, y)
  ViX <- vapply(seq_len(p), function(a) .hibrid_solve(L, X[, a]), numeric(n))
  if (!is.matrix(ViX)) ViX <- matrix(ViX, nrow = n)
  XtViX <- crossprod(X, ViX)
  Lx <- .hibrid_chol(XtViX)
  beta <- .hibrid_solve(Lx, as.numeric(crossprod(X, Viy)))
  r <- y - as.numeric(X %*% beta)
  Vir <- .hibrid_solve(L, r)
  dfr <- n - p
  s2e <- sum(r * Vir) / dfr
  ll <- -0.5 * (dfr * log(max(s2e, 1e-300)) + .hibrid_logdet(L) +
                  .hibrid_logdet(Lx) + dfr)
  list(ll = ll, beta = beta, s2e = s2e, L = L)
}

#' morie_hibrid_hibrid_prediction
#'
#' Part of the hibrid_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @param p1_geno See Usage.
#' @param p2_geno See Usage.
#' @param sigma2_sca Defaults to \code{NULL}.
#' @param X Defaults to \code{NULL}.
#' @param p1_new Defaults to \code{NULL}.
#' @param p2_new Defaults to \code{NULL}.
#' @param max_iter Defaults to \code{300L}.
#' @param tol Defaults to \code{1e-10}.
#' @return A list with \code{estimate}, \code{fitted}, \code{gca_effect}, \code{sca_effect}, \code{coefficients}, \code{sigma2_gca}, \code{sigma2_sca}, \code{sigma2_e}, \code{sca_share}, \code{h2}, \code{gca_kernel}, \code{sca_kernel}, \code{reml_path}, \code{reml_loglik}, \code{iterations}, \code{converged}, \code{sca_fixed}, \code{prediction_new}, \code{residuals}, \code{n}, \code{m}, \code{p}, \code{method}, \code{note}.
#' @export
morie_hibrid_hibrid_prediction <- function(y, p1_geno, p2_geno,
                                           sigma2_sca = NULL, X = NULL,
                                           p1_new = NULL, p2_new = NULL,
                                           max_iter = 300L, tol = 1e-10) {
  yv <- as.numeric(y)
  P1 <- .hibrid_rows(p1_geno); P2 <- .hibrid_rows(p2_geno)
  n <- length(yv)
  if (n == 0L) stop("hibrid: no crosses")
  if (nrow(P1) != n || nrow(P2) != n)
    stop(sprintf(paste0("hibrid: %d phenotypes but %d and %d parental ",
                        "genotype rows"), n, nrow(P1), nrow(P2)))
  m <- ncol(P1)
  if (ncol(P2) != m)
    stop(sprintf(paste0("hibrid: both parents must be typed at the same %d ",
                        "markers"), m))
  Xm <- if (is.null(X)) matrix(1.0, n, 1L) else .hibrid_rows(X)
  p <- ncol(Xm)
  if (n - p < 2L)
    stop(sprintf(paste0("hibrid: %d crosses and %d fixed effects leave too ",
                        "little information for two variance components"),
                 n, p))

  G1 <- tcrossprod(P1) / m
  G2 <- tcrossprod(P2) / m
  Kg <- G1 + G2
  Ks <- G1 * G2

  fixed_sca <- !is.null(sigma2_sca)
  path <- numeric(0)

  if (fixed_sca && as.numeric(sigma2_sca) <= .hibrid_EPS) {
    # no SCA at all: the model IS additive GBLUP on the GCA kernel, and is
    # fitted as exactly that -- one ratio, nothing else moving
    Kz <- matrix(0.0, n, n)
    f1 <- function(l) .hibrid_reml_at(exp(l), 1e300, Kg, Kz, yv, Xm)$ll
    la <- exp(.hibrid_gridmax(f1, .hibrid_LO, .hibrid_HI))
    path <- c(path, f1(log(la)))
    fit <- .hibrid_reml_at(la, 1e300, Kg, Kz, yv, Xm)
    ll <- fit$ll; beta <- fit$beta; s2e <- fit$s2e; L <- fit$L
    s2a <- s2e / la; s2s <- 0.0
    it <- 1L; conv <- TRUE
    Ks_used <- Kz
  } else {
    Ks_used <- Ks
    la <- 1.0; ls <- 1.0
    it <- 0L; conv <- FALSE
    path <- c(path, .hibrid_reml_at(la, ls, Kg, Ks_used, yv, Xm)$ll)
    prev_la <- NULL; prev_ls <- NULL
    for (it in seq_len(as.integer(max_iter))) {
      prev <- path[length(path)]
      fa <- function(l) .hibrid_reml_at(exp(l), ls, Kg, Ks_used, yv, Xm)$ll
      la <- exp(.hibrid_gridmax(fa, .hibrid_LO, .hibrid_HI))
      if (fixed_sca) {
        s2e_now <- .hibrid_reml_at(la, ls, Kg, Ks_used, yv, Xm)$s2e
        ls <- s2e_now / max(as.numeric(sigma2_sca), 1e-300)
      } else {
        fs <- function(l) .hibrid_reml_at(la, exp(l), Kg, Ks_used, yv, Xm)$ll
        ls <- exp(.hibrid_gridmax(fs, .hibrid_LO, .hibrid_HI))
      }
      cur <- .hibrid_reml_at(la, ls, Kg, Ks_used, yv, Xm)$ll
      path <- c(path, cur)
      # convergence on the QUANTISED ratios, not on the likelihood: the
      # likelihood test trips one iteration apart in the two languages
      # because cur and prev differ in their last bits.
      if (!is.null(prev_la) && la == prev_la && ls == prev_ls) {
        conv <- TRUE; break
      }
      prev_la <- la; prev_ls <- ls
    }
    fit <- .hibrid_reml_at(la, ls, Kg, Ks_used, yv, Xm)
    ll <- fit$ll; beta <- fit$beta; s2e <- fit$s2e; L <- fit$L
    s2a <- s2e / la
    s2s <- if (fixed_sca) as.numeric(sigma2_sca) else s2e / ls
  }

  r <- yv - as.numeric(Xm %*% beta)
  w <- .hibrid_solve(L, r)
  gca <- s2a * as.numeric(Kg %*% w)
  sca <- s2s * as.numeric(Ks_used %*% w)
  fitted <- as.numeric(Xm %*% beta) + gca + sca

  tot <- s2a + s2s + s2e
  pred_new <- NULL
  if (!is.null(p1_new) && !is.null(p2_new)) {
    Q1 <- .hibrid_rows(p1_new); Q2 <- .hibrid_rows(p2_new)
    if (nrow(Q1) != nrow(Q2))
      stop("hibrid: p1_new and p2_new must describe the same crosses")
    if (ncol(Q1) != m || ncol(Q2) != m)
      stop(sprintf(paste0("hibrid: new parents must be typed at the same %d ",
                          "markers"), m))
    pred_new <- numeric(nrow(Q1))
    for (u in seq_len(nrow(Q1))) {
      c1 <- as.numeric(P1 %*% Q1[u, ]) / m
      c2 <- as.numeric(P2 %*% Q2[u, ]) / m
      # an untested cross carries no covariate row, so the fixed part is the
      # intercept alone -- beta[1] by construction of X
      pred_new[u] <- beta[1] + s2a * sum((c1 + c2) * w) +
        s2s * sum((c1 * c2) * w)
    }
  }

  list(estimate = fitted, fitted = fitted,
       gca_effect = gca, sca_effect = sca, coefficients = beta,
       sigma2_gca = s2a, sigma2_sca = s2s, sigma2_e = s2e,
       sca_share = if (s2a + s2s > .hibrid_EPS) s2s / (s2a + s2s) else 0.0,
       h2 = if (tot > .hibrid_EPS) (s2a + s2s) / tot else NaN,
       gca_kernel = Kg, sca_kernel = Ks,
       reml_path = path, reml_loglik = ll, iterations = as.integer(it),
       converged = conv, sca_fixed = fixed_sca,
       prediction_new = pred_new,
       residuals = yv - fitted,
       n = as.integer(n), m = as.integer(m), p = as.integer(p),
       method = paste0("genomic hybrid prediction: additive GCA kernel from ",
                       "the sum of the parental relationship matrices, SCA ",
                       "kernel from their Hadamard product, variance ",
                       "components by profiled REML (Sprague & Tatum 1942; ",
                       "Technow et al. 2012, 2014)"),
       note = paste0("sca_share is the fraction of genetic variance that ",
                     "only the specific combination explains -- near zero ",
                     "means choosing good parents is enough, and large ",
                     "means the cross itself has to be tested; fixing ",
                     "sigma2_sca at 0 reduces this exactly to additive ",
                     "GBLUP. Separating the two needs a factorial design ",
                     "with many parents: with p lines per pool the GCA ",
                     "kernel already spans about 2p - 1 dimensions, so at ",
                     "6 by 6 it takes eleven of the thirty-six ",
                     "observations and no estimator can tell the remaining ",
                     "interaction from residual noise. Compare reml_loglik ",
                     "against the same fit with sigma2_sca = 0 before ",
                     "reporting an SCA variance."))
}

.hibrid_cheatsheet <- function() {
  paste0("hibrid: morie_hibrid_hibrid_prediction(y, p1_geno, p2_geno) -> ",
         "GCA and SCA variance components and hybrid predictions from ",
         "marker kernels (Sprague & Tatum 1942; Technow et al. 2014)")
}

morie_hibrid <- morie_hibrid_hibrid_prediction
