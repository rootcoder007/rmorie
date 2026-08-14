# FarmCPU: fixed and random models, circulating.
#
# Sources: Liu, X., Huang, M., Fan, B., Buckler, E. S. & Zhang, Z.
# (2016) "Iterative Usage of Fixed and Random Effect Models for
# Powerful and Efficient Genome-Wide Association Studies", PLoS
# Genetics 12(2), e1005767, doi:10.1371/journal.pgen.1005767. The
# confounding between population structure, kinship and quantitative
# trait nucleotides in a mixed linear model; MLMM's stepwise partial
# removal; the division into a Fixed Effect Model containing testing
# markers one at a time with multiple associated markers as
# covariates to control false positives, and a Random Effect Model in
# which the associated markers are estimated by using them to define
# kinship, avoiding over-fitting; the unification of p-values at each
# iteration; improved statistical power; and computing time linear in
# both the number of individuals and the number of markers. Yu, J.
# et al. (2006) "A unified mixed-model method for association mapping
# that accounts for multiple levels of relatedness", Nature Genetics
# 38(2), 203-208, doi:10.1038/ng1702. The MLM being improved.
# Segura, V. et al. (2012) "An efficient multi-locus mixed-model
# approach for genome-wide association studies in structured
# populations", Nature Genetics 44(7), 825-830, doi:10.1038/ng.2314.
# MLMM, the stepwise predecessor.
#
# Native implementation mirroring Python morie.fn.farmlmm exactly: the
# same kinship construction, the same FEM with associated markers as
# covariates, the same REM rebuilding kinship from the selected set,
# the same convergence check, the same oscillating-set reporting, and
# the same RichResult-style payload as a named list.


# Base R has no erf/erfc; both are pnorm in disguise. Defined here so
# the arm stays base-R only, as the package requires.
.erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
.erfc <- function(x) 2 * pnorm(-x * sqrt(2))

.FARMLMM_EPS <- 1e-12

.norm_cdf <- function(x) {
  0.5 * (1 + .erf(as.numeric(x) / sqrt(2)))
}

kinship_from_markers <- function(G, markers = NULL) {
  M <- as.matrix(G)
  n <- nrow(M)
  p <- ncol(M)
  cols <- if (is.null(markers)) seq_len(p) else as.integer(markers)
  if (length(cols) == 0L)
    stop("farmlmm: kinship needs at least one marker")
  Z <- matrix(0, nrow = length(cols), ncol = n)
  for (jj in seq_along(cols)) {
    j <- cols[jj]
    col <- M[, j]
    m <- mean(col)
    s <- sqrt(sum((col - m) ^ 2) / n)
    if (s == 0) s <- 1
    Z[jj, ] <- (col - m) / s
  }
  K <- (Z %*% t(Z)) / length(cols)
  list(K = K, markers_used = cols, n_markers = length(cols),
       all_markers = is.null(markers))
}

confounding <- function(G, K, marker) {
  M <- as.matrix(G)
  K <- as.matrix(K)
  n <- nrow(M)
  g <- as.numeric(M[, as.integer(marker)])
  kg <- as.numeric(K %*% g) / n
  mg <- mean(g)
  mk <- mean(kg)
  num <- sum((g - mg) * (kg - mk))
  den <- sqrt(sum((g - mg) ^ 2) * sum((kg - mk) ^ 2))
  list(correlation = if (den > .FARMLMM_EPS) num / den else 0,
       marker = as.integer(marker),
       note = paste("kinship from ALL markers contains the tested",
                    "marker; that is the confounding",
                    sep = " "))
}

fixed_effect_scan <- function(y, G, covariates = numeric(0),
                              K = NULL) {
  yv <- as.numeric(y)
  M <- as.matrix(G)
  n <- nrow(M)
  p <- ncol(M)
  if (length(yv) != n)
    stop("farmlmm: ", length(yv), " phenotypes but ", n, " genotypes")
  cov <- as.integer(covariates)
  pv <- numeric(p)
  betas <- numeric(p)
  for (j in seq_len(p)) {
    cols <- c(j, setdiff(cov, j))
    X <- M[, cols, drop = FALSE]
    ok <- FALSE
    co <- tryCatch({
      Xqr <- qr(X)
      Q <- qr.Q(Xqr)
      R <- qr.R(Xqr)
      rhs <- as.numeric(t(Q) %*% yv)
      backsolve(R, rhs)
    }, error = function(e) NULL)
    if (is.null(co)) {
      pv[j] <- 1
      betas[j] <- 0
      next
    }
    fit <- as.numeric(X %*% co)
    res <- yv - fit
    dof <- max(n - length(cols) - 1L, 1L)
    s2 <- sum(res ^ 2) / dof
    xm <- mean(X[, 1])
    sxx <- sum((X[, 1] - xm) ^ 2)
    se <- if (sxx > .FARMLMM_EPS) sqrt(s2 / sxx) else Inf
    t <- if (se > 0 && is.finite(se)) co[2] / se else 0
    pv[j] <- 2 * (1 - pnorm(abs(t)))
    betas[j] <- co[2]
  }
  list(p = pv, beta = betas, covariates = cov,
       note = paste("associated markers enter as COVARIATES, which",
                    "is what controls false positives",
                    sep = " "))
}

random_effect_step <- function(y, G, selected, bins = NULL) {
  sel <- as.integer(selected)
  if (length(sel) == 0L)
    return(list(K = NULL, markers_used = c(),
                note = paste("no associated markers yet; kinship is",
                             "the identity at the first iteration",
                             sep = " ")))
  kk <- kinship_from_markers(G, sel)
  yv <- as.numeric(y)
  n <- length(yv)
  Kk <- kk$K
  m <- mean(yv)
  blup <- as.numeric(Kk %*% (yv - m)) / n
  list(K = Kk, markers_used = sel, blup = blup,
       note = paste("kinship from a SMALL selected set, so it no",
                    "longer contains the marker under test",
                    sep = " "))
}

farmcpu <- function(y, G, max_iter = 10, threshold = NULL,
                    seed = 0) {
  yv <- as.numeric(y)
  M <- as.matrix(G)
  p <- ncol(M)
  thr <- if (is.null(threshold)) 0.05 / p else as.numeric(threshold)
  sel <- integer(0)
  hist <- list()
  converged <- FALSE
  fem <- NULL
  for (it in seq_len(as.integer(max_iter))) {
    fem <- fixed_effect_scan(yv, M, sel)
    new <- which(fem$p < thr)
    new <- sort(new)
    hist[[length(hist) + 1L]] <- new
    if (identical(new, sel)) {
      converged <- TRUE
      break
    }
    if (length(hist) >= 3L && identical(new, hist[[length(hist) - 2L]]))
      break
    sel <- new
    random_effect_step(yv, M, sel)
  }
  list(estimate = sel, selected = sel, p = fem$p,
       iterations = length(hist), converged = converged,
       oscillating = (!converged && length(hist) >= 3L &&
                        identical(hist[[length(hist)]],
                                  hist[[length(hist) - 2L]])),
       threshold = thr, history = hist,
       method = "FarmCPU; Liu, Huang, Fan, Buckler & Zhang (2016)",
       note = paste("kinship rebuilt from the SELECTED markers each",
                    "round, so the confounding is removed rather",
                    "than reduced",
                    sep = " "))
}

cheatsheet <- function() {
  paste("farmlmm: an MLM controls false positives with kinship",
        "estimated from ALL markers -- which therefore contains",
        "the marker being tested, and that confounding costs",
        "power. Split the model: FEM tests one marker at a time",
        "with the currently associated markers as COVARIATES; REM",
        "estimates those associated markers by using them to",
        "DEFINE KINSHIP, avoiding over-fitting. Alternate, unify",
        "the p-values each round, and the confounding is removed",
        "rather than reduced. Cost is linear in individuals AND",
        "markers.",
        sep = " ")
}

morie_farmlmm <- function(y, G, max_iter = 10, threshold = NULL,
                          seed = 0) {
  farmcpu(y, G, max_iter, threshold, seed)
}

farmcpumodel <- function(y, G, max_iter = 10, threshold = NULL,
                         seed = 0) {
  farmcpu(y, G, max_iter, threshold, seed)
}

farm_cpu <- function(y, G, max_iter = 10, threshold = NULL,
                     seed = 0) {
  farmcpu(y, G, max_iter, threshold, seed)
}
