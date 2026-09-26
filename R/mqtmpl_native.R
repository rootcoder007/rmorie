# Genome scans for QTL, R/qtl style: one scan, several methods.
# Sources: Broman, K. W. et al. (2003), "R/qtl: QTL mapping in
# experimental crosses", Bioinformatics 19(7), 889-890;
# Lander, E. S. & Botstein, D. (1989), "Mapping Mendelian Factors
# Underlying Quantitative Traits Using RFLP Linkage Maps", Genetics
# 121(1), 185-199; Churchill, G. A. & Doerge, R. W. (1994),
# "Empirical Threshold Values for Quantitative Trait Mapping",
# Genetics 138(3), 963-971; Baum, L. E. et al. (1970); Sen, S. &
# Churchill, G. A. (2001), "A statistical framework for quantitative
# trait mapping", Genetics 159(1), 371-387.
#
# Native implementation mirroring Python morie.fn.mqtmpl exactly: the
# same forward-backward HMM with the same emission/transition logic,
# the same EM/marker-regression/multiple-imputation scans, the same
# permutation threshold, and the same refusal of Haley-Knott with the
# same reason.

.GHC_MQTMPL_LOG10E <- 0.4342944819032518

#' @keywords internal
#' @noRd
.ghc_haldane <- function(d) {
  d <- as.numeric(d)
  if (d <= 0) return(0)
  0.5 * (1 - exp(-2 * d / 100))
}

#' @keywords internal
#' @noRd
.ghc_geno_prob <- function(gL, gR, rL, rR) {
  p1 <- if (gL == 1L) 1 - rL else rL
  p2 <- if (gR == 1L) 1 - rR else rR
  p10 <- if (gL == 1L) rL else 1 - rL
  p20 <- if (gR == 1L) rR else 1 - rR
  c(p1 * p2 / (p1 * p2 + p10 * p20), p10 * p20 / (p1 * p2 + p10 * p20))
}

#' Available scan methods
#'
#' @param method Optional single method name; if NULL, returns the
#'   full status dictionary.
#' @return A list with the available methods, or a per-method status.
#' @export
#' @examples
#' morie_mqtmpl_method_status()
#' @keywords internal
morie_mqtmpl_method_status <- function(method = NULL) {
  avail <- c("em", "mr", "imp")
  unsourced <- list(
    hk = paste0("Haley-Knott regression is named but not defined in ",
                "Broman et al. (2003); the primary source, Haley, C. ",
                "S. & Knott, S. A. (1992) 'A simple regression ",
                "method for mapping quantitative trait loci in line ",
                "crosses using flanking markers', Heredity 69(4), ",
                "315-324, doi:10.1038/hdy.1992.131, is not in the ",
                "corpus"))
  if (is.null(method))
    return(list(methods = c("em", "mr", "hk", "imp"),
                available = avail, unavailable = unsourced))
  if (!(method %in% c("em", "mr", "hk", "imp")))
    stop(paste0("mqtmpl: method must be one of em, mr, hk, imp, got ",
                method))
  list(method = method, available = method %in% avail,
       reason = if (is.null(unsourced[[method]])) "" else unsourced[[method]])
}

#' @keywords internal
#' @noRd
.ghc_mqtmpl_check <- function(method) {
  if (!(method %in% c("em", "mr", "hk", "imp")))
    stop(paste0("mqtmpl: method must be one of em, mr, hk, imp, got ",
                method))
  if (!(method %in% c("em", "mr", "imp")))
    stop(paste0("mqtmpl: the '", method,
                "' scan method is not implemented -- ",
                morie_mqtmpl_method_status(method)$reason))
}

#' Forward-backward posterior genotype probabilities
#'
#' @param genotypes One row per individual: 0, 1 or NA.
#' @param positions Marker positions in cM.
#' @param error_rate Genotyping error rate in [0, 0.5).
#' @return A list of per-individual posterior matrices.
#' @export
#' @examples
#' set.seed(1)
#' g <- lapply(1:6, function(i) c(rbinom(1, 1, 0.5), NA, rbinom(1, 1, 0.5)))
#' pos <- c(0, 0.1, 0.2)
#' r <- morie_mqtmpl_hmm_genotype_probabilities(g, pos)
#' str(r[[1]])
#' @keywords internal
morie_mqtmpl_hmm_genotype_probabilities <- function(genotypes, positions,
                                                      error_rate = 0) {
  e <- as.numeric(error_rate)
  if (e < 0 || e >= 0.5)
    stop(paste0("mqtmpl: the genotyping error rate must lie in ",
                "[0, 0.5), got ", e))
  m <- length(positions)
  if (any(vapply(genotypes, function(r) length(r), integer(1)) != m))
    stop("mqtmpl: every individual needs one call per marker")
  trans <- numeric(m - 1L)
  for (j in seq_len(m - 1L)) {
    d <- as.numeric(positions[j + 1L]) - as.numeric(positions[j])
    if (d <= 0)
      stop("mqtmpl: marker positions must increase")
    trans[j] <- .ghc_haldane(d)
  }
  out <- list()
  for (row in genotypes) {
    emit <- function(j, s) {
      v <- row[[j]]
      if (is.null(v) || is.na(v)) return(1)
      if (as.integer(v) == s) 1 - e else e
    }
    f <- matrix(0, m, 2)
    f[1, ] <- c(0.5 * emit(1L, 0L), 0.5 * emit(1L, 1L))
    for (j in 2:m) {
      r <- trans[j - 1L]
      for (s in 0:1) {
        f[j, s + 1L] <- emit(j, s) *
          (f[j - 1L, s + 1L] * (1 - r) + f[j - 1L, 2L - s] * r)
      }
      tot <- sum(f[j, ])
      if (tot <= 0)
        stop(paste0("mqtmpl: an individual's marker data have ",
                    "probability zero; raise the error rate"))
      f[j, ] <- f[j, ] / tot
    }
    b <- matrix(1, m, 2)
    for (j in (m - 1L):1) {
      r <- trans[j]
      for (s in 0:1) {
        b[j, s + 1L] <- (b[j + 1L, s + 1L] * (1 - r) * emit(j + 1L, s) +
                         b[j + 1L, 2L - s] * r * emit(j + 1L, 1L - s))
      }
      tot <- sum(b[j, ])
      if (tot > 0) b[j, ] <- b[j, ] / tot
    }
    post <- matrix(0, m, 2)
    for (j in seq_len(m)) {
      p0 <- f[j, 1] * b[j, 1]
      p1 <- f[j, 2] * b[j, 2]
      post[j, ] <- c(p0 / (p0 + p1), p1 / (p0 + p1))
    }
    out[[length(out) + 1L]] <- post
  }
  out
}

#' Draw pseudomarker genotypes from p(g | m)
#'
#' @param genotypes,positions,error_rate As above.
#' @param grid Grid positions.
#' @param n_imp Number of imputations.
#' @param seed Seed for the shared generator.
#' @return A list of draws.
#' @export
#' @examples
#' set.seed(1)
#' g <- lapply(1:6, function(i) c(rbinom(1, 1, 0.5), NA, rbinom(1, 1, 0.5)))
#' pos <- c(0, 0.1, 0.2)
#' r <- morie_mqtmpl_sample_genotypes(g, pos, grid = pos, n_imp = 4)
#' str(r, max.level = 1)
#' @keywords internal
morie_mqtmpl_sample_genotypes <- function(genotypes, positions, grid,
                                           n_imp = 16, error_rate = 0,
                                           seed = 0) {
  m <- length(positions)
  n <- length(genotypes)
  e <- .ghc_rng(seed)
  post <- morie_mqtmpl_hmm_genotype_probabilities(genotypes, positions,
                                                   error_rate)
  out <- list()
  for (rep in seq_len(as.integer(n_imp))) {
    draw <- list()
    for (i in seq_len(n)) {
      states <- integer(m)
      states[m] <- if (.ghc_unif(e, 1L) < post[[i]][m, 2]) 1L else 0L
      for (j in (m - 1L):1) {
        r <- .ghc_haldane(as.numeric(positions[j + 1L]) -
                          as.numeric(positions[j]))
        w <- c(post[[i]][j, 1] * (1 - r) +
               post[[i]][j, 2] * r,
               post[[i]][j, 1] * r +
               post[[i]][j, 2] * (1 - r))
        states[j] <- if (.ghc_unif(e, 1L) < w[2] / sum(w)) 1L else 0L
      }
      row <- numeric(length(grid))
      for (gi in seq_along(grid)) {
        gpos <- grid[gi]
        j <- max(which(as.numeric(positions) <= gpos + 1e-12))
        if (j == m) { row[gi] <- states[j]
        next }
        d1 <- max(gpos - as.numeric(positions[j]), 0)
        d2 <- max(as.numeric(positions[j + 1L]) - gpos, 0)
        pr <- .ghc_geno_prob(states[j], states[j + 1L],
                             .ghc_haldane(d1), .ghc_haldane(d2))
        row[gi] <- if (.ghc_unif(e, 1L) < pr[2]) 1 else 0
      }
      draw[[length(draw) + 1L]] <- row
    }
    out[[length(out) + 1L]] <- draw
  }
  out
}

#' Normal-model imputation weight
#'
#' @param y Phenotype.
#' @param genotype_column A column of imputed genotypes.
#' @param model_dimension Degrees of freedom in the model.
#' @return The log weight.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_mqtmpl_imputation_weights(V, V)
#' @keywords internal
morie_mqtmpl_imputation_weights <- function(y, genotype_column,
                                             model_dimension = 2) {
  n <- length(y)
  if (n != length(genotype_column))
    stop("mqtmpl: one genotype per phenotype")
  g <- as.numeric(genotype_column)
  my <- mean(y)
  mg <- mean(g)
  sgg <- sum((g - mg)^2)
  if (sgg <= 0) {
    rss <- sum((y - my)^2)
  } else {
    b <- sum((g - mg) * (y - my)) / sgg
    a <- my - b * mg
    rss <- sum((y - (a + b * g))^2)
  }
  rss <- max(rss, 1e-300)
  -0.5 * as.numeric(model_dimension) * log(n) - 0.5 * n * log(rss)
}

#' @keywords internal
#' @noRd
.ghc_mqtmpl_scan_imp <- function(y, markers, positions, step, n_imp,
                                  error_rate, seed) {
  n <- length(y)
  grid <- seq(from = as.numeric(positions[1]),
              to = as.numeric(positions[length(positions)]),
              by = as.numeric(step))
  geno <- vector("list", n)
  for (i in seq_len(n)) geno[[i]] <- vapply(seq_along(markers),
                                              function(j) markers[[j]][i],
                                              numeric(1))
  draws <- morie_mqtmpl_sample_genotypes(geno, positions, grid, n_imp,
                                          error_rate, seed)
  null <- morie_mqtmpl_imputation_weights(y, rep(0, n), model_dimension = 1)
  lods <- numeric(length(grid))
  for (gi in seq_along(grid)) {
    ws <- vapply(seq_along(draws), function(k)
      morie_mqtmpl_imputation_weights(y,
        vapply(seq_len(n), function(i) draws[[k]][[i]][gi], numeric(1))),
      numeric(1))
    top <- max(ws)
    avg <- top + log(sum(exp(ws - top)) / length(ws))
    lods[gi] <- (avg - null) * .GHC_MQTMPL_LOG10E
  }
  k <- which.max(lods)
  list(estimate = lods[k], peak_lod = lods[k],
       peak_position = grid[k], position = grid, lod = lods,
       method_used = "imp", n_imputations = as.integer(n_imp),
       note = paste0("weights are n^(-v/2) RSS^(-n/2) on the log ",
                     "scale; the draws depend on the markers only, ",
                     "so a new model reuses them and only the ",
                     "weights change"),
       method = paste0("multiple-imputation scan; Sen & Churchill ",
                       "(2001) eqs (3)-(4)"))
}

#' Single-marker LOD (marker regression)
#'
#' @param y Phenotypes for typed individuals.
#' @param g Genotypes for typed individuals.
#' @return The LOD.
#' @noRd
.ghc_mqtmpl_single_marker <- function(y, g) {
  n <- length(y)
  my <- mean(y)
  mg <- mean(g)
  sgg <- sum((g - mg)^2)
  rss0 <- sum((y - my)^2)
  if (sgg <= 0) return(list(lod = 0))
  b <- sum((g - mg) * (y - my)) / sgg
  a <- my - b * mg
  rss1 <- sum((y - (a + b * g))^2)
  rss1 <- max(rss1, 1e-300)
  list(lod = 0.5 * (n - 2) * log(rss0 / rss1) * .GHC_MQTMPL_LOG10E)
}

#' @keywords internal
#' @noRd
#' @keywords internal
#' @noRd
# EM interval / composite interval mapping at one QTL position -- the
# Zeng (1994) eqs (5)-(6) composite likelihood, mirrored line for line
# from the Python arm (morie.fn.cqtmpl.cim). An empty `cofactors` is
# exactly Lander-Botstein interval mapping, which is what
# Broman et al. (2003) scanone(method = "em") computes.
.ghc_mqtmpl_cim_em <- function(y, left, right, r_left, r_right, cofactors = list(),
                     max_iter = 200L, tol = 1e-10) {
  n <- length(y)
  y <- as.numeric(y)
  cof <- lapply(cofactors, as.numeric)
  gL <- vapply(seq_len(n), function(i) if (is.na(left[i]))  0L else as.integer(left[i]),  integer(1))
  gR <- vapply(seq_len(n), function(i) if (is.na(right[i])) 0L else as.integer(right[i]), integer(1))
  # G[i, ] = c(P(q = 0), P(q = 1)) given the flanking markers (backcross)
  G <- t(vapply(seq_len(n), function(i) {
    out <- numeric(2)
    for (q in 0:1) {
      p <- if (gL[i] != q) r_left  else 1 - r_left
      p <- p * (if (gR[i] != q) r_right else 1 - r_right)
      out[q + 1L] <- p
    }
    tot <- out[1] + out[2]
    if (tot <= 0) stop("mqtmpl: the flanking marker configuration has probability zero")
    out / tot
  }, numeric(2)))
  my <- sum(y) / n
  beta <- c(my, 0.1 * (max(y) - min(y) + 1e-12), rep(0, length(cof)))
  s2 <- sum((y - my)^2) / n
  history <- numeric(0)
  post <- rep(0.5, n)
  cofmat <- if (length(cof)) do.call(cbind, cof) else matrix(0, n, 0)
  wls <- function(X, yy, w) { A <- crossprod(X * w, X)
  b <- crossprod(X * w, yy)
  as.numeric(solve(A, b)) }
  for (iter in seq_len(as.integer(max_iter))) {
    base <- beta[1] + if (length(cof)) as.numeric(cofmat %*% beta[-(1:2)]) else 0
    m0 <- base
    m1 <- base + beta[2]
    d0 <- exp(-((y - m0)^2) / (2 * s2))
    d1 <- exp(-((y - m1)^2) / (2 * s2))
    w0 <- G[, 1] * d0
    w1 <- G[, 2] * d1
    tot <- w0 + w1
    if (any(tot <= 0)) stop("mqtmpl: the mixture vanished at individual ", which(tot <= 0)[1])
    post <- w1 / tot
    ll <- sum(log(tot / sqrt(2 * pi * s2)))
    history <- c(history, ll)
    if (length(history) > 1 && abs(history[length(history)] - history[length(history) - 1]) < tol) break
    X <- rbind(cbind(1, 0, cofmat), cbind(1, 1, cofmat))
    Y <- c(y, y)
    W <- c(1 - post, post)
    beta <- wls(X, Y, W)
    s2 <- sum(W * (Y - as.numeric(X %*% beta))^2) / n
  }
  X0 <- cbind(1, cofmat)
  b0 <- wls(X0, y, rep(1, n))
  r0 <- y - as.numeric(X0 %*% b0)
  s0 <- sum(r0^2) / n
  ll0 <- -0.5 * n * (log(2 * pi * s0) + 1)
  lod <- (history[length(history)] - ll0) * (1 / log(10))
  list(lod = lod, b0 = beta[1], b = beta[2], cofactor_coefficients = beta[-(1:2)],
       sigma2 = s2, sigma2_null = s0, loglik = history[length(history)], loglik_null = ll0,
       iterations = length(history), posterior = post, rss = s2 * n, coef = beta)
}

.ghc_mqtmpl_scan_em <- function(y, markers, positions, step, cof) {
  n <- length(y)
  m <- length(markers)
  out_pos <- c()
  out_lod <- c()
  fits <- list()
  for (j in seq_len(m - 1L)) {
    span <- as.numeric(positions[j + 1L]) - as.numeric(positions[j])
    d <- 0
    while (d <= span + 1e-12) {
      gL <- vapply(seq_len(n), function(i)
        if (is.na(markers[[j]][i])) 0 else as.integer(markers[[j]][i]),
        integer(1))
      gR <- vapply(seq_len(n), function(i)
        if (is.na(markers[[j + 1L]][i])) 0 else as.integer(markers[[j + 1L]][i]),
        integer(1))
      rL <- .ghc_haldane(min(d, span))
      rR <- .ghc_haldane(max(span - d, 0))
      # the caller passes an intercept column when there are no covariates;
      # the EM adds its own intercept, so keep only non-constant columns
      cofl <- if (is.null(cof) || NCOL(cof) == 0L) list() else
        Filter(function(v) length(unique(v)) > 1L, lapply(seq_len(ncol(cof)), function(c) cof[, c]))
      fit <- .ghc_mqtmpl_cim_em(y, gL, gR, rL, rR, cofl)
      lod <- fit$lod
      out_pos <- c(out_pos, as.numeric(positions[j]) + d)
      out_lod <- c(out_lod, lod)
      fits[[length(fits) + 1L]] <- fit
      d <- d + as.numeric(step)
    }
  }
  k <- which.max(out_lod)
  list(estimate = out_lod[k], peak_lod = out_lod[k],
       peak_position = out_pos[k], position = out_pos, lod = out_lod,
       fit = fits[[k]])
}

#' Single-QTL genome scan
#'
#' @param y Phenotype vector.
#' @param markers List of per-marker genotype vectors.
#' @param positions Marker positions in cM.
#' @param method "em", "mr", "imp" or "hk".
#' @param step Grid step in cM.
#' @param covariates Optional list of covariate columns.
#' @param error_rate Genotyping error rate.
#' @return A list with peak LOD, position, full grid, and bookkeeping.
#' @export
#' @examples
#' set.seed(2)
#' n <- 40
#' m1 <- rbinom(n, 1, 0.5)
#' m2 <- ifelse(runif(n) < 0.9, m1, 1 - m1)
#' y <- 1 + 0.8 * m1 + rnorm(n, 0, 0.5)
#' r <- morie_mqtmpl_scanone(y, list(m1, m2), positions = c(0, 0.1),
#'                           method = "mr")
#' str(r, max.level = 1)
#' @keywords internal
morie_mqtmpl_scanone <- function(y, markers, positions,
                                  method = "em", step = 0.02,
                                  covariates = list(),
                                  error_rate = 0) {
  .ghc_mqtmpl_check(method)
  n <- length(y)
  if (any(vapply(markers, function(r) length(r) != n, logical(1))))
    stop(paste0("mqtmpl: every marker must be typed on all ", n,
                " individuals"))
  if (method == "imp")
    return(.ghc_mqtmpl_scan_imp(y, markers, positions, step,
                                if (length(covariates) > 0L) 64L else 64L,
                                error_rate, 0))
  if (method == "mr") {
    out_pos <- c()
    out_lod <- c()
    for (j in seq_along(markers)) {
      typed <- which(!vapply(seq_along(y),
                              function(i) is.na(markers[[j]][i]),
                              logical(1)))
      if (length(typed) < 3L) next
      sm <- .ghc_mqtmpl_single_marker(y[typed],
                                      as.numeric(markers[[j]][typed]))
      out_pos <- c(out_pos, as.numeric(positions[j]))
      out_lod <- c(out_lod, sm$lod)
    }
    if (length(out_lod) == 0L)
      stop("mqtmpl: no marker has enough typed individuals")
    k <- which.max(out_lod)
    return(list(estimate = out_lod[k], peak_lod = out_lod[k],
                peak_position = out_pos[k], position = out_pos,
                lod = out_lod, method_used = "mr",
                note = paste0("marker regression reports LOD at ",
                              "markers only, and drops individuals ",
                              "not typed there"),
                method = "marker regression scan; Broman et al. (2003)"))
  }
  cof <- if (length(covariates) == 0L) matrix(1, n, 1) else do.call(cbind, covariates)
  res <- .ghc_mqtmpl_scan_em(y, markers, positions, step, cof)
  list(estimate = res$peak_lod, peak_lod = res$peak_lod,
       peak_position = res$peak_position,
       position = res$position, lod = res$lod,
       method_used = "em", n_covariates = ncol(cof),
       error_rate = as.numeric(error_rate),
       method = paste0("EM genome scan; Lander & Botstein (1989) via ",
                       "Broman et al. (2003)"))
}

#' Permutation threshold
#'
#' @param y,markers,positions,method,step As in \code{scanone}.
#' @param n_perm Number of permutations.
#' @param alpha Significance level.
#' @param seed Seed.
#' @return A list with threshold, null maxima, median, alpha, n_perm.
#' @export
#' @examples
#' set.seed(2)
#' n <- 40
#' m1 <- rbinom(n, 1, 0.5)
#' m2 <- ifelse(runif(n) < 0.9, m1, 1 - m1)
#' y <- 1 + 0.8 * m1 + rnorm(n, 0, 0.5)
#' r <- morie_mqtmpl_permutation_threshold(y, list(m1, m2),
#'                                         positions = c(0, 0.1),
#'                                         n_perm = 20, method = "mr")
#' str(r, max.level = 1)
#' @keywords internal
morie_mqtmpl_permutation_threshold <- function(y, markers, positions,
                                                n_perm = 100, alpha = 0.05,
                                                method = "em",
                                                step = 0.05, seed = 0) {
  a <- as.numeric(alpha)
  if (a <= 0 || a >= 1)
    stop("mqtmpl: alpha must lie in (0, 1)")
  e <- .ghc_rng(seed)
  maxima <- numeric(as.integer(n_perm))
  ys <- as.numeric(y)
  for (k in seq_len(as.integer(n_perm))) {
    perm <- sample.int(length(ys))
    perm_ys <- ys[perm]
    sc <- morie_mqtmpl_scanone(perm_ys, markers, positions, method, step)
    maxima[k] <- sc$peak_lod
  }
  maxima <- sort(maxima)
  idx <- min(length(maxima) - 1L,
             max(0L, as.integer(ceiling((1 - a) * length(maxima))) - 1L))
  list(estimate = maxima[idx + 1L], threshold = maxima[idx + 1L],
       alpha = a, n_perm = as.integer(n_perm), null_maxima = maxima,
       median_null = maxima[length(maxima) %/% 2L + 1L],
       method = paste0("permutation threshold; Churchill & Doerge ",
                       "(1994) via Broman et al. (2003)"))
}

#' LOD-drop support interval
#'
#' @param scan_result Result of \code{scanone}.
#' @param drop LOD drop.
#' @return A list with peak, lower, upper, drop, peak_lod.
#' @export
#' @examples
#' set.seed(2)
#' n <- 40
#' m1 <- rbinom(n, 1, 0.5)
#' m2 <- ifelse(runif(n) < 0.9, m1, 1 - m1)
#' y <- 1 + 0.8 * m1 + rnorm(n, 0, 0.5)
#' sc <- morie_mqtmpl_scanone(y, list(m1, m2), positions = c(0, 0.1),
#'                            method = "mr")
#' r <- morie_mqtmpl_lod_support_interval(sc)
#' str(r, max.level = 1)
#' @keywords internal
morie_mqtmpl_lod_support_interval <- function(scan_result, drop = 1.5) {
  lod <- scan_result$lod
  pos <- scan_result$position
  k <- which.max(lod)
  cut <- lod[k] - as.numeric(drop)
  lo <- k
  while (lo > 1 && lod[lo - 1L] >= cut) lo <- lo - 1L
  hi <- k
  while (hi < length(lod) && lod[hi + 1L] >= cut) hi <- hi + 1L
  list(peak = pos[k], lower = pos[lo], upper = pos[hi],
       drop = as.numeric(drop), peak_lod = lod[k])
}

morie_mqtmpl <- morie_mqtmpl_scanone
morie_mqtmpl_qtl_genome_scan <- morie_mqtmpl_scanone

# -- restored: morie-only definition kept through the rmorie sync --
#' mqtmpl_cheatsheet
#'
#' A step of the mqtmpl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
mqtmpl_cheatsheet <- function() {
  paste(paste0(
    "mqtmpl: the scanning layer. Genotypes come from a forward-ba",
    "ckward HMM that tolerates missing calls and a genotyping err",
    "or rate, and collapses to the flanking-marker formula when b",
    "oth are absent. Scans by EM or marker regression; Haley-Knot",
    "t and multiple imputation are named and REFUSED, with citati",
    "ons. Genome-wide significance is a permutation threshold, be",
    "cause the maximum over correlated positions is not chi-squar",
    "ed anything."
  ))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mqtmpl_check_method
#'
#' A step of the mqtmpl_native implementation. Called by \code{mqtmpl_scanone}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param method Passed to \code{\%in\%}.
#' @return One of two values, depending on the branch taken.
#' @export
mqtmpl_check_method <- function(method) {
  if (!(method %in% mqtmpl_METHODS)) {
    stop(sprintf("mqtmpl: method must be one of %s, got %s",
                 paste(mqtmpl_METHODS, collapse = ", "), method))
  }
  if (!(method %in% mqtmpl_AVAILABLE)) {
    stop(sprintf("mqtmpl: the '%s' scan method is not implemented -- %s",
                 method, mqtmpl_UNSOURCED[[method]]))
  }
}

# -- restored: morie-only definition kept through the rmorie sync --
mqtmpl_cim_em <- function(y, left, right, r_left, r_right, cofactors = list(),
                     max_iter = 200L, tol = 1e-10) {
  n <- length(y)
  y <- as.numeric(y)
  cof <- lapply(cofactors, as.numeric)
  gL <- vapply(seq_len(n), function(i) if (is.na(left[i]))  0L else as.integer(left[i]),  integer(1))
  gR <- vapply(seq_len(n), function(i) if (is.na(right[i])) 0L else as.integer(right[i]), integer(1))
  # G[i, ] = c(P(q = 0), P(q = 1)) given the flanking markers (backcross)
  G <- t(vapply(seq_len(n), function(i) {
    out <- numeric(2)
    for (q in 0:1) {
      p <- if (gL[i] != q) r_left  else 1 - r_left
      p <- p * (if (gR[i] != q) r_right else 1 - r_right)
      out[q + 1L] <- p
    }
    tot <- out[1] + out[2]
    if (tot <= 0) stop("mqtmpl: the flanking marker configuration has probability zero")
    out / tot
  }, numeric(2)))
  my <- sum(y) / n
  beta <- c(my, 0.1 * (max(y) - min(y) + 1e-12), rep(0, length(cof)))
  s2 <- sum((y - my)^2) / n
  history <- numeric(0)
  post <- rep(0.5, n)
  cofmat <- if (length(cof)) do.call(cbind, cof) else matrix(0, n, 0)
  wls <- function(X, yy, w) { A <- crossprod(X * w, X)
  b <- crossprod(X * w, yy)
  as.numeric(solve(A, b)) }
  for (iter in seq_len(as.integer(max_iter))) {
    base <- beta[1] + if (length(cof)) as.numeric(cofmat %*% beta[-(1:2)]) else 0
    m0 <- base
    m1 <- base + beta[2]
    d0 <- exp(-((y - m0)^2) / (2 * s2))
    d1 <- exp(-((y - m1)^2) / (2 * s2))
    w0 <- G[, 1] * d0
    w1 <- G[, 2] * d1
    tot <- w0 + w1
    if (any(tot <= 0)) stop("mqtmpl: the mixture vanished at individual ", which(tot <= 0)[1])
    post <- w1 / tot
    ll <- sum(log(tot / sqrt(2 * pi * s2)))
    history <- c(history, ll)
    if (length(history) > 1 && abs(history[length(history)] - history[length(history) - 1]) < tol) break
    X <- rbind(cbind(1, 0, cofmat), cbind(1, 1, cofmat))
    Y <- c(y, y)
    W <- c(1 - post, post)
    beta <- wls(X, Y, W)
    s2 <- sum(W * (Y - as.numeric(X %*% beta))^2) / n
  }
  X0 <- cbind(1, cofmat)
  b0 <- wls(X0, y, rep(1, n))
  r0 <- y - as.numeric(X0 %*% b0)
  s0 <- sum(r0^2) / n
  ll0 <- -0.5 * n * (log(2 * pi * s0) + 1)
  lod <- (history[length(history)] - ll0) * (1 / log(10))
  list(lod = lod, b0 = beta[1], b = beta[2], cofactor_coefficients = beta[-(1:2)],
       sigma2 = s2, sigma2_null = s0, loglik = history[length(history)], loglik_null = ll0,
       iterations = length(history), posterior = post, rss = s2 * n, coef = beta)
}

# -- restored: morie-only definition kept through the rmorie sync --
mqtmpl_cim_one <- function(y, left, right, r_left, r_right, cofactors) {
  mqtmpl_cim_em(y, left, right, r_left, r_right, cofactors)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Qk left, qk+1 right
#'
#' A step of the mqtmpl_native implementation. Called by \code{mqtmpl_cim_one},
#' \code{mqtmpl_sample_genotypes}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s_left Accepted by the signature and not used anywhere in the body.
#' @param s_right Accepted by the signature and not used anywhere in the body.
#' @param r_left Numeric; combined arithmetically in the body.
#' @param r_right Numeric; combined arithmetically in the body.
#' @return A vector, from \code{c}.
#' @export
mqtmpl_genotype_probabilities <- function(s_left, s_right, r_left, r_right) {
  # Backcross coding, 0/1 at each flanking marker, no interference: the
  # QTL genotype given the flanks is the normalised product of the two
  # interval transmission probabilities (Broman et al. 2003). Returns
  # c(P(q = 1), P(q = 0)) for ONE individual -- the same closed form the
  # rmorie arm (.ghc_geno_prob) and the Python arm (rqtmpl) use.
  gL <- as.integer(s_left)
  gR <- as.integer(s_right)
  p1  <- if (gL == 1L) 1 - r_left  else r_left
  p2  <- if (gR == 1L) 1 - r_right else r_right
  p10 <- if (gL == 1L) r_left      else 1 - r_left
  p20 <- if (gR == 1L) r_right     else 1 - r_right
  tot <- p1 * p2 + p10 * p20
  if (tot <= 0) return(c(0, 0))
  c(p1 * p2 / tot, p10 * p20 / tot)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mqtmpl_haldane
#'
#' A step of the mqtmpl_native implementation. Called by
#' \code{mqtmpl_hmm_genotype_probabilities}, \code{mqtmpl_sample_genotypes},
#' \code{mqtmpl_scan_cim}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param d Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
mqtmpl_haldane <- function(d) {
  d <- as.numeric(d)
  0.5 * (1 - exp(-2 * d / 100))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mqtmpl_hmm_genotype_probabilities
#'
#' A step of the mqtmpl_native implementation. Called by \code{mqtmpl_sample_genotypes}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param genotypes Iterated over elementwise, with \code{sapply}.
#' @param positions A vector; its length is taken and its elements indexed.
#' @param error_rate Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @return The value of \code{out}, as built in the body.
#' @export
mqtmpl_hmm_genotype_probabilities <- function(genotypes, positions, error_rate = 0) {
  e <- as.numeric(error_rate)
  if (!(e >= 0 && e < 0.5)) {
    stop(sprintf("mqtmpl: the genotyping error rate must lie in [0, 0.5), got %g", e))
  }
  m <- length(positions)
  if (any(sapply(genotypes, length) != m)) {
    stop("mqtmpl: every individual needs one call per marker")
  }
  trans <- list()
  for (j in seq_len(m - 1L)) {
    d <- positions[j + 1L] - positions[j]
    if (d <= 0) stop("mqtmpl: marker positions must increase")
    trans[[j]] <- mqtmpl_haldane(d)
  }
  out <- list()
  for (row in genotypes) {
    emit <- function(j, state) {
      v <- row[j]
      if (is.null(v) || is.na(v)) return(1)
      if (as.integer(v) == state) 1 - e else e
    }
    f <- matrix(0, nrow = m, ncol = 2)
    f[1L, 1L] <- 0.5 * emit(1L, 0L)
    f[1L, 2L] <- 0.5 * emit(1L, 1L)
    for (j in seq.int(2L, m)) {
      r <- trans[[j - 1L]]
      for (s in 0:1) {
        f[j, s + 1L] <- emit(j, s) * (
          f[j - 1L, s + 1L] * (1 - r) + f[j - 1L, 2L - s] * r)
      }
      tot <- sum(f[j, ])
      if (tot <= 0) stop("mqtmpl: an individual's marker data have probability zero; raise the error rate")
      f[j, ] <- f[j, ] / tot
    }
    b <- matrix(0, nrow = m, ncol = 2)
    b[m, ] <- c(1, 1)
    for (j in seq.int(m - 1L, 1L)) {
      r <- trans[[j]]
      for (s in 0:1) {
        b[j, s + 1L] <- (b[j + 1L, s + 1L] * (1 - r) * emit(j + 1L, s) +
                         b[j + 1L, 2L - s] * r * emit(j + 1L, 1L - s))
      }
      tot <- sum(b[j, ])
      if (tot > 0) b[j, ] <- b[j, ] / tot
    }
    post <- matrix(0, nrow = m, ncol = 2)
    for (j in seq_len(m)) {
      p0 <- f[j, 1L] * b[j, 1L]
      p1 <- f[j, 2L] * b[j, 2L]
      s <- p0 + p1
      post[j, ] <- c(p0 / s, p1 / s)
    }
    out[[length(out) + 1L]] <- post
  }
  out
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mqtmpl_imputation_weights
#'
#' A step of the mqtmpl_native implementation. Called by \code{mqtmpl_scan_imp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken.
#' @param genotype_column A vector; its length is taken.
#' @param model_dimension Numeric; combined arithmetically in the body. Defaults to \code{2}.
#' @return A numeric value.
#' @export
mqtmpl_imputation_weights <- function(y, genotype_column, model_dimension = 2) {
  n <- length(y)
  if (n != length(genotype_column)) stop("mqtmpl: one genotype per phenotype")
  g <- as.numeric(genotype_column)
  my <- mean(y)
  mg <- mean(g)
  sgg <- sum((g - mg)^2)
  if (sgg <= 0) {
    rss <- sum((y - my)^2)
  } else {
    b <- sum((g - mg) * (y - my)) / sgg
    a <- my - b * mg
    rss <- sum((y - (a + b * g))^2)
  }
  rss <- max(rss, 1e-300)
  -0.5 * model_dimension * log(n) - 0.5 * n * log(rss)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mqtmpl_kw_n_imp
#'
#' A step of the mqtmpl_native implementation. Called by \code{mqtmpl_scanone}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param covariates A vector; its length is taken.
#' @return A numeric value.
#' @export
mqtmpl_kw_n_imp <- function(covariates) {
  if (length(covariates) > 0L) {
    stop("mqtmpl: covariates are not implemented for the imputation scan")
  }
  64L
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mqtmpl_lod_support_interval
#'
#' A step of the mqtmpl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param scan_result A list; the body reads \code{$lod}, \code{$position} from it.
#' @param drop Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1.5}.
#' @return A list with \code{peak}, \code{lower}, \code{upper}, \code{drop}, \code{peak_lod}.
#' @export
mqtmpl_lod_support_interval <- function(scan_result, drop = 1.5) {
  lod <- scan_result$lod
  pos <- scan_result$position
  k <- which.max(lod)
  cut <- lod[k] - as.numeric(drop)
  lo <- k
  while (lo > 1L && lod[lo - 1L] >= cut) lo <- lo - 1L
  hi <- k
  while (hi < length(lod) && lod[hi + 1L] >= cut) hi <- hi + 1L
  list(peak = pos[k], lower = pos[lo], upper = pos[hi],
       drop = as.numeric(drop), peak_lod = lod[k])
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mqtmpl_method_status
#'
#' A step of the mqtmpl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param method Optional; may be \code{NULL}. Carried through into a list the body builds.
#' @return A list with \code{method}, \code{available}, \code{reason}.
#' @export
mqtmpl_method_status <- function(method = NULL) {
  if (is.null(method)) {
    return(list(methods = mqtmpl_METHODS, available = mqtmpl_AVAILABLE,
                unavailable = mqtmpl_UNSOURCED))
  }
  if (!(method %in% mqtmpl_METHODS)) {
    stop(sprintf("mqtmpl: method must be one of %s, got %s",
                 paste(mqtmpl_METHODS, collapse = ", "), method))
  }
  list(method = method, available = method %in% mqtmpl_AVAILABLE,
       reason = if (method %in% names(mqtmpl_UNSOURCED)) mqtmpl_UNSOURCED[[method]] else "")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mqtmpl_permutation_threshold
#'
#' A step of the mqtmpl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param markers Passed to \code{mqtmpl_scanone}.
#' @param positions Passed to \code{mqtmpl_scanone}.
#' @param n_perm Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.
#' @param method Passed to \code{mqtmpl_scanone}. Defaults to \code{"em"}.
#' @param step Passed to \code{mqtmpl_scanone}. Defaults to \code{0.05}.
#' @param seed Passed to \code{set.seed}. Defaults to \code{0}.
#' @param ... Passed through.
#' @return A list with \code{estimate}, \code{threshold}, \code{alpha}, \code{n_perm},
#' \code{null_maxima}, \code{median_null}, \code{method}.
#' @export
mqtmpl_permutation_threshold <- function(y, markers, positions, n_perm = 100,
                                         alpha = 0.05, method = "em",
                                         step = 0.05, seed = 0, ...) {
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("mqtmpl: alpha must lie in (0, 1)")
  .rmorie_local_seed(seed)
  maxima <- c()
  ys <- as.numeric(y)
  for (k in seq_len(as.integer(n_perm))) {
    perm <- sample(ys, length(ys))
    maxima <- c(maxima, mqtmpl_scanone(perm, markers, positions, method, step)$peak_lod)
  }
  maxima <- sort(maxima)
  idx <- min(length(maxima) - 1L,
             max(0L, as.integer(ceiling((1 - a) * length(maxima))) - 1L))
  list(estimate = maxima[idx + 1L], threshold = maxima[idx + 1L],
       alpha = a, n_perm = as.integer(n_perm), null_maxima = maxima,
       median_null = maxima[length(maxima) %/% 2L + 1L],
       method = "permutation threshold; Churchill & Doerge (1994) via Broman et al. (2003)")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mqtmpl_sample_genotypes
#'
#' A step of the mqtmpl_native implementation. Called by \code{mqtmpl_scan_imp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param genotypes A vector; its length is taken.
#' @param positions A vector; its length is taken and its elements indexed.
#' @param grid A vector; its length is taken and its elements indexed.
#' @param n_imp Coerced to integer by the body, with \code{as.integer}. Defaults to \code{16}.
#' @param error_rate Passed to \code{mqtmpl_hmm_genotype_probabilities}. Defaults to \code{0}.
#' @param seed Passed to \code{set.seed}. Defaults to \code{0}.
#' @return The value of \code{out}, as built in the body.
#' @export
mqtmpl_sample_genotypes <- function(genotypes, positions, grid, n_imp = 16,
                                    error_rate = 0, seed = 0) {
  m <- length(positions)
  n <- length(genotypes)
  .rmorie_local_seed(seed)
  post <- mqtmpl_hmm_genotype_probabilities(genotypes, positions, error_rate)
  out <- list()
  for (imp in seq_len(as.integer(n_imp))) {
    draw <- list()
    for (i in seq_len(n)) {
      states <- integer(m)
      states[m] <- if (runif(1) < post[[i]][m, 2L]) 1L else 0L
      for (j in seq.int(m - 1L, 1L)) {
        r <- mqtmpl_haldane(positions[j + 1L] - positions[j])
        w <- c(post[[i]][j, 1L] * (1 - r + r * (states[j + 1L] == 0L)),
               post[[i]][j, 2L] * (1 - r + r * (states[j + 1L] == 1L)))
        tot <- sum(w)
        states[j] <- if (runif(1) < w[2L] / tot) 1L else 0L
      }
      row <- numeric(length(grid))
      for (gi in seq_along(grid)) {
        g <- grid[gi]
        js <- which(positions <= g + 1e-12)
        j <- max(js)
        if (j == m) { row[gi] <- states[j]
        next }
        d1 <- max(g - positions[j], 0)
        d2 <- max(positions[j + 1L] - g, 0)
        pr <- mqtmpl_genotype_probabilities(states[j], states[j + 1L],
                                            mqtmpl_haldane(d1),
                                            mqtmpl_haldane(d2))
        row[gi] <- if (runif(1) < pr[2L]) 1 else 0
      }
      draw[[i]] <- row
    }
    out[[length(out) + 1L]] <- draw
  }
  out
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mqtmpl_scan_cim
#'
#' A step of the mqtmpl_native implementation. Called by \code{mqtmpl_scanone}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Passed to \code{mqtmpl_cim_one}.
#' @param markers A vector; its length is taken and its elements indexed.
#' @param positions A vector; indexed elementwise.
#' @param cofactors A vector; its length is taken. Defaults to \code{list()}.
#' @param window Accepted by the signature and not used anywhere in the body. Defaults to \code{0}.
#' @param step Numeric; combined arithmetically in the body. Defaults to \code{0.02}.
#' @return A list with \code{estimate}, \code{peak_lod}, \code{peak_position},
#' \code{position}, \code{lod}, \code{fit}.
#' @export
mqtmpl_scan_cim <- function(y, markers, positions, cofactors = list(),
                            window = 0, step = 0.02) {
  m <- length(markers)
  out_pos <- c()
  out_lod <- c()
  fits <- list()
  if (length(cofactors) == 0L) {
    for (j in seq_len(m - 1L)) {
      span <- positions[j + 1L] - positions[j]
      d <- 0
      while (d <= span + 1e-12) {
        r_left <- mqtmpl_haldane(min(d, span))
        r_right <- mqtmpl_haldane(max(span - d, 0))
        f <- mqtmpl_cim_one(y, markers[[j]], markers[[j + 1L]],
                            r_left, r_right, cofactors)
        out_pos <- c(out_pos, positions[j] + d)
        out_lod <- c(out_lod, f$lod)
        fits[[length(fits) + 1L]] <- f
        d <- d + step
      }
    }
  } else {
    for (j in seq_len(m - 1L)) {
      span <- positions[j + 1L] - positions[j]
      d <- 0
      while (d <= span + 1e-12) {
        r_left <- mqtmpl_haldane(min(d, span))
        r_right <- mqtmpl_haldane(max(span - d, 0))
        f <- mqtmpl_cim_one(y, markers[[j]], markers[[j + 1L]],
                            r_left, r_right, cofactors)
        out_pos <- c(out_pos, positions[j] + d)
        out_lod <- c(out_lod, f$lod)
        fits[[length(fits) + 1L]] <- f
        d <- d + step
      }
    }
  }
  k <- which.max(out_lod)
  list(estimate = out_lod[k], peak_lod = out_lod[k],
       peak_position = out_pos[k], position = out_pos, lod = out_lod,
       fit = fits[[k]])
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mqtmpl_scan_imp
#'
#' A step of the mqtmpl_native implementation. Called by \code{mqtmpl_scanone}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken.
#' @param markers A vector; its length is taken and its elements indexed.
#' @param positions A vector; its length is taken and its elements indexed.
#' @param step Numeric; combined arithmetically in the body.
#' @param n_imp Coerced to integer by the body, with \code{as.integer}.
#' @param error_rate Passed to \code{mqtmpl_sample_genotypes}.
#' @param seed Passed to \code{mqtmpl_sample_genotypes}.
#' @return A list with \code{estimate}, \code{peak_lod}, \code{peak_position},
#' \code{position}, \code{lod}, \code{method_used}, \code{n_imputations}, \code{note},
#' \code{method}.
#' @export
mqtmpl_scan_imp <- function(y, markers, positions, step, n_imp,
                            error_rate, seed) {
  n <- length(y)
  grid <- c()
  g <- positions[1L]
  end <- positions[length(positions)]
  while (g <= end + 1e-12) { grid <- c(grid, g)
  g <- g + step }
  geno <- lapply(seq_len(n), function(i) {
    sapply(seq_along(markers), function(j) markers[[j]][i])
  })
  draws <- mqtmpl_sample_genotypes(geno, positions, grid, n_imp, error_rate, seed)
  null <- mqtmpl_imputation_weights(y, rep(0, n), model_dimension = 1)
  lods <- numeric(length(grid))
  for (gi in seq_along(grid)) {
    ws <- sapply(draws, function(d) {
      mqtmpl_imputation_weights(y, sapply(seq_len(n), function(i) d[[i]][gi]))
    })
    top <- max(ws)
    avg <- top + log(sum(exp(ws - top)) / length(ws))
    lods[gi] <- (avg - null) * mqtmpl_LOG10E
  }
  k <- which.max(lods)
  list(estimate = lods[k], peak_lod = lods[k], peak_position = grid[k],
       position = grid, lod = lods, method_used = "imp",
       n_imputations = as.integer(n_imp),
       note = "weights are n^(-v/2) RSS^(-n/2) on the log scale; the draws depend on the markers only, so a new model reuses them and only the weights change",
       method = "multiple-imputation scan; Sen & Churchill (2001) eqs (3)-(4)")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mqtmpl_scanone
#'
#' A step of the mqtmpl_native implementation. Called by \code{mqtmpl_permutation_threshold}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken and its elements indexed.
#' @param markers A vector; its length is taken and its elements indexed.
#' @param positions A vector; indexed elementwise.
#' @param method One of \code{"imp"}, \code{"mr"}. Defaults to \code{"em"}.
#' @param step Passed to \code{mqtmpl_scan_imp}. Defaults to \code{0.02}.
#' @param covariates A vector; its length is taken. Defaults to \code{list()}.
#' @param error_rate Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @return A list with \code{estimate}, \code{peak_lod}, \code{peak_position},
#' \code{position}, \code{lod}, \code{method_used}, \code{n_covariates},
#' \code{error_rate}, \code{method}.
#' @export
mqtmpl_scanone <- function(y, markers, positions, method = "em", step = 0.02,
                           covariates = list(), error_rate = 0) {
  mqtmpl_check_method(method)
  n <- length(y)
  if (any(sapply(markers, length) != n)) {
    stop(sprintf("mqtmpl: every marker must be typed on all %d individuals", n))
  }
  if (method == "imp") {
    return(mqtmpl_scan_imp(y, markers, positions, step,
                           mqtmpl_kw_n_imp(covariates), error_rate, 0))
  }
  if (method == "mr") {
    out_pos <- c()
    out_lod <- c()
    for (j in seq_along(markers)) {
      typed <- which(!sapply(markers[[j]], is.null))
      if (length(typed) < 3L) next
      sm <- mqtmpl_single_marker(y[typed], unlist(markers[[j]])[typed])
      out_pos <- c(out_pos, positions[j])
      out_lod <- c(out_lod, sm$lod)
    }
    if (length(out_lod) == 0L) {
      stop("mqtmpl: no marker has enough typed individuals")
    }
    k <- which.max(out_lod)
    return(list(estimate = out_lod[k], peak_lod = out_lod[k],
                peak_position = out_pos[k], position = out_pos,
                lod = out_lod, method_used = "mr",
                note = "marker regression reports LOD at markers only, and drops individuals not typed there",
                method = "marker regression scan; Broman et al. (2003)"))
  }
  # em
  res <- mqtmpl_scan_cim(y, markers, positions, covariates, 0, step)
  list(estimate = res$peak_lod, peak_lod = res$peak_lod,
       peak_position = res$peak_position, position = res$position,
       lod = res$lod, method_used = "em", n_covariates = length(covariates),
       error_rate = as.numeric(error_rate),
       method = "EM genome scan; Lander & Botstein (1989) via Broman et al. (2003)")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mqtmpl_single_marker
#'
#' A step of the mqtmpl_native implementation. Called by \code{mqtmpl_scanone}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken.
#' @param g Numeric; passed to \code{mean}.
#' @return A list with \code{lod}, \code{rss}, \code{rss0}.
#' @export
mqtmpl_single_marker <- function(y, g) {
  n <- length(y)
  my <- mean(y)
  mg <- mean(g)
  sgg <- sum((g - mg)^2)
  if (sgg <= 0) {
    rss <- sum((y - my)^2)
  } else {
    b <- sum((g - mg) * (y - my)) / sgg
    a <- my - b * mg
    rss <- sum((y - (a + b * g))^2)
  }
  rss <- max(rss, 1e-300)
  rss0 <- sum((y - my)^2)
  rss0 <- max(rss0, 1e-300)
  lod <- 0.5 * (n * log(rss0) - n * log(rss) - log(n)) * mqtmpl_LOG10E
  list(lod = lod, rss = rss, rss0 = rss0)
}

# -- restored: morie-only objects kept through the rmorie sync --
mqtmpl_AVAILABLE <- c("em", "mr", "imp")

mqtmpl_LOG10E <- log10(exp(1))

mqtmpl_METHODS <- c("em", "mr", "hk", "imp")

mqtmpl_UNSOURCED <- list(
  hk = paste0(
    "Haley-Knott regression is named but not defined in Broman et",
    " al. (2003); the primary source, Haley, C. S. & Knott, S. A.",
    " (1992) 'A simple regression method for mapping quantitative",
    " trait loci in line crosses using flanking markers', Heredit",
    "y 69(4), 315-324, doi:10.1038/hdy.1992.131, is not in the co",
    "rpus"
  )
)
