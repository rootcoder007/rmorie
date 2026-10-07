.sp_sum <- function(v) {
  s <- 0
  for (q in v) s <- s + q
  s
}

#' Polarization measures
#'
#' Esteban-Ray polarization over distinct positions, Foster-Wolfson
#' bipolarization W = 2 (2T - G) mu / m, and with a binary party the
#' Levendusky sorting correlation, the separation of party means (pooled-SD in
#' one dimension, Mahalanobis in two) and the between-party share of each
#' dimension's sum of squares; with feeling thermometers the in-party minus
#' out-party gap (affective polarization).
#'
#' @param x Positions (vector) or points (two-column matrix).
#' @param party Optional 0/1 party indicator.
#' @param alpha Esteban-Ray sensitivity.
#' @param weights Optional person weights for Esteban-Ray.
#' @param in_rating,out_rating Optional thermometer ratings.
#' @return list(esteban_ray, wolfson, gini, sorting, separation, between_share,
#'   affective) as applicable.
#' @references Esteban, J.-M. and Ray, D. (1994). On the measurement of
#'   polarization. Econometrica 62, 819-851. Foster, J. E. and Wolfson, M. C.
#'   (2010). Polarization and the decline of the middle class. Journal of Economic
#'   Inequality 8, 247-273. Iyengar, S., Sood, G. and Lelkes, Y. (2012). Affect,
#'   not ideology. Public Opinion Quarterly 76, 405-431.
#' @examples
#' PolarizationMeasures(c(0, 0, 1, 1))$esteban_ray
#' @export
PolarizationMeasures <- function(x, party = NULL, alpha = 1, weights = NULL, in_rating = NULL, out_rating = NULL) {
  pts <- is.matrix(x)
  out <- list()
  if (!pts) {
    v <- as.numeric(x)
    n <- length(v)
    w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
    sh <- tapply(w / .sp_sum(w), v, .sp_sum)
    ys <- as.numeric(names(sh))
    p <- as.numeric(sh)
    er <- 0
    for (a in seq_along(ys)) for (b in seq_along(ys)) er <- er + p[a]^(1 + alpha) * p[b] * abs(ys[a] - ys[b])
    out$esteban_ray <- er
    if (min(v) > 0) {
      sv <- sort(v)
      mu <- .sp_sum(sv) / n
      med <- if (n %% 2) sv[n %/% 2 + 1] else 0.5 * (sv[n / 2] + sv[n / 2 + 1])
      low <- .sp_sum(sv[seq_len(n %/% 2)]) + (if (n %% 2) 0.5 * sv[n %/% 2 + 1] else 0)
      G <- .sp_sum(abs(outer(sv, sv, "-"))) / (2 * n^2 * mu)
      out$wolfson <- 2 * (2 * (0.5 - low / (mu * n)) - G) * mu / med
      out$gini <- G
    }
  }
  if (!is.null(party)) {
    P <- if (pts) x * 1 else matrix(as.numeric(x), ncol = 1)
    g <- as.integer(party)
    n <- nrow(P)
    d <- ncol(P)
    m0 <- colMeans(P[g == 0, , drop = FALSE])
    m1 <- colMeans(P[g == 1, , drop = FALSE])
    R <- P
    R[g == 0, ] <- sweep(P[g == 0, , drop = FALSE], 2, m0)
    R[g == 1, ] <- sweep(P[g == 1, , drop = FALSE], 2, m1)
    S <- crossprod(R) / (n - 2)
    diff <- m1 - m0
    if (d == 1) {
      out$separation <- abs(diff) / sqrt(S[1, 1])
      out$sorting <- stats::cor(P[, 1], g)
    } else {
      if (d != 2) stop("dimensional separation is implemented for one or two dimensions", call. = FALSE)
      out$separation <- sqrt(as.numeric(t(diff) %*% solve(S, diff)))
    }
    grand <- colMeans(P)
    tss <- colSums(sweep(P, 2, grand)^2)
    bss <- sum(g == 0) * (m0 - grand)^2 + sum(g == 1) * (m1 - grand)^2
    out$between_share <- ifelse(tss > 0, bss / tss, 0)
  }
  if (!is.null(in_rating) && !is.null(out_rating)) out$affective <- mean(in_rating) - mean(out_rating)
  out
}

#' Spatial bargaining
#'
#' Nash bargaining solution for two players with quadratic losses (maximising the
#' product of gains over the status quo along the segment between their ideals,
#' by bisection on its derivative) and Rubinstein's alternating-offers share
#' (1 - delta_2) / (1 - delta_1 delta_2).
#'
#' @param ideal1,ideal2 Ideal points.
#' @param status_quo Optional disagreement policy.
#' @param delta1,delta2 Optional discount factors.
#' @return list(nash, nash_t, nash_product, rubinstein_share, rubinstein).
#' @references Nash, J. F. (1950). The bargaining problem. Econometrica 18,
#'   155-162. Rubinstein, A. (1982). Perfect equilibrium in a bargaining model.
#'   Econometrica 50, 97-109.
#' @examples
#' SpatialBargaining(0, 1)$nash
#' @export
SpatialBargaining <- function(ideal1, ideal2, status_quo = NULL, delta1 = NULL, delta2 = NULL) {
  a <- as.numeric(ideal1)
  b <- as.numeric(ideal2)
  u <- function(x, i) -.sp_sum((x - (if (i == 1) a else b))^2)
  d1 <- if (is.null(status_quo)) u(b, 1) else u(status_quo, 1)
  d2 <- if (is.null(status_quo)) u(a, 2) else u(status_quo, 2)
  L <- .sp_sum((b - a)^2)
  lo <- if (L > 0) max(0, 1 - sqrt(-d2 / L)) else 0
  hi <- if (L > 0) min(1, sqrt(-d1 / L)) else 0
  out <- list()
  if (hi >= lo) {
    fp <- function(t) -2 * t * L * (-(1 - t)^2 * L - d2) + 2 * (1 - t) * L * (-t^2 * L - d1)
    x0 <- lo
    x1 <- hi
    for (it in 1:200) {
      mid <- 0.5 * (x0 + x1)
      if (fp(mid) > 0) x0 <- mid else x1 <- mid
    }
    t <- 0.5 * (x0 + x1)
    out <- list(nash = a + t * (b - a), nash_t = t, nash_product = (-t^2 * L - d1) * (-(1 - t)^2 * L - d2))
  }
  if (!is.null(delta1) && !is.null(delta2)) {
    s1 <- (1 - delta2) / (1 - delta1 * delta2)
    out$rubinstein_share <- s1
    out$rubinstein <- b + s1 * (a - b)
  }
  out
}

#' Ranked probability score
#'
#' Mean squared difference of cumulative forecast and observed indicators over
#' K ordered categories, divided by K - 1.
#'
#' @param probs Matrix of forecast probabilities (rows) over ordered categories.
#' @param outcomes Observed categories (1-based).
#' @return list(rps, mean).
#' @references Epstein, E. S. (1969). A scoring system for probability forecasts
#'   of ranked categories. Journal of Applied Meteorology 8, 985-987.
#' @examples
#' RankedProbabilityScore(rbind(c(0.2, 0.5, 0.3)), 2)$rps
#' @export
RankedProbabilityScore <- function(probs, outcomes) {
  Pm <- matrix(as.numeric(as.matrix(probs)), ncol = ncol(as.matrix(probs)))
  K <- ncol(Pm)
  r <- vapply(seq_len(nrow(Pm)), function(i) {
    Fc <- cumsum(Pm[i, ])[-K]
    O <- as.numeric(outcomes[i] <= seq_len(K - 1))
    .sp_sum((Fc - O)^2) / (K - 1)
  }, 0)
  list(rps = r, mean = mean(r))
}

#' Party positions
#'
#' Mean (expert placements) or median (member ideal points) position of each
#' party per dimension with bootstrap standard errors (Philox resampling within
#' party) and inter-party distances.
#'
#' @param points Positions (vector) or points (matrix rows).
#' @param party Party labels.
#' @param statistic "mean" or "median".
#' @param n_boot Bootstrap replicates.
#' @param seed Philox seed.
#' @return list(parties, position, se, n, distance).
#' @references Laver, M. and Hunt, W. B. (1992). Policy and Party Competition.
#'   Routledge.
#' @examples
#' PartyPositions(c(1, 2, 3, 7, 8, 9), c("D", "D", "D", "R", "R", "R"), n_boot = 0)$position
#' @export
PartyPositions <- function(points, party, statistic = c("mean", "median"), n_boot = 200, seed = 0) {
  statistic <- match.arg(statistic)
  P <- if (is.matrix(points)) points * 1 else matrix(as.numeric(points), ncol = 1)
  labs <- sort(unique(as.character(party)))
  st <- if (statistic == "mean") function(v) .sp_sum(v) / length(v) else stats::median
  e <- .mh_rand(seed)
  pos <- list()
  se <- list()
  cnt <- integer(0)
  for (lab in labs) {
    mem <- P[as.character(party) == lab, , drop = FALSE]
    cnt <- c(cnt, nrow(mem))
    pos[[lab]] <- apply(mem, 2, st)
    if (n_boot > 0) {
      reps <- t(vapply(seq_len(n_boot), function(b) {
        idx <- vapply(seq_len(nrow(mem)), function(i) .mh_idx(e, nrow(mem)), 0)
        apply(mem[idx, , drop = FALSE], 2, st)
      }, numeric(ncol(P))))
      if (ncol(P) == 1) reps <- t(reps)
      reps <- matrix(reps, nrow = n_boot)
      se[[lab]] <- apply(reps, 2, stats::sd)
    }
  }
  M <- do.call(rbind, pos)
  list(parties = labs, position = unname(M), se = if (n_boot > 0) unname(do.call(rbind, se)) else NULL, n = cnt,
       distance = as.matrix(stats::dist(M)))
}

.dim_eig <- function(X) {
  C <- stats::cov(X)
  sort(eigen(C, symmetric = TRUE, only.values = TRUE)$values, decreasing = TRUE)
}

#' Dimensionality of spatial data
#'
#' Eigenvalues of the column-centred covariance (scree), the elbow (largest gap
#' below the chord from first to last eigenvalue) and Horn's parallel analysis
#' against Philox column permutations, with a Monte Carlo p-value for the second
#' dimension.
#'
#' @param X Data matrix (rows are units).
#' @param n_sim Number of permuted matrices.
#' @param seed Philox seed.
#' @param quantile Threshold quantile.
#' @return list(eigenvalues, share, elbow, parallel, threshold, p_second).
#' @references Horn, J. L. (1965). A rationale and test for the number of factors
#'   in factor analysis. Psychometrika 30, 179-185. Cattell, R. B. (1966). The
#'   scree test for the number of factors. Multivariate Behavioral Research 1,
#'   245-276.
#' @examples
#' Dimensionality(cbind(1:4, 2 * (1:4)), n_sim = 0)$share
#' @export
Dimensionality <- function(X, n_sim = 100, seed = 0, quantile = 0.95) {
  .morie_arg(X, "m")
  X <- as.matrix(X) * 1
  n <- nrow(X)
  m <- ncol(X)
  ev <- pmax(.dim_eig(X), 0)
  k <- length(ev)
  elbow <- if (k >= 3) {
    gap <- (ev[1] + (ev[k] - ev[1]) * (0:(k - 1)) / (k - 1)) - ev
    which(gap == max(gap))[1] - 1
  } else {
    1
  }
  out <- list(eigenvalues = ev, share = ev / .sp_sum(ev), elbow = elbow)
  if (n_sim > 0) {
    e <- .mh_rand(seed)
    sims <- matrix(0, n_sim, k)
    for (s in seq_len(n_sim)) {
      Y <- X
      for (j in seq_len(m)) {
        col <- X[, j]
        for (i in n:2) {
          r <- .mh_idx(e, i)
          tmp <- col[i]
          col[i] <- col[r]
          col[r] <- tmp
        }
        Y[, j] <- col
      }
      sims[s, ] <- .dim_eig(Y)
    }
    thr <- vapply(seq_len(k), function(d) sort(sims[, d])[min(floor(quantile * n_sim) + 1, n_sim)], 0)
    keep <- 0
    while (keep < k && ev[keep + 1] > thr[keep + 1]) keep <- keep + 1
    out$parallel <- keep
    out$threshold <- thr
    out$p_second <- if (k > 1) (1 + sum(sims[, 2] >= ev[2])) / (1 + n_sim) else NULL
  }
  out
}

.rc_best_cut <- function(proj, votes) {
  ok <- !is.na(votes)
  pv <- proj[ok]
  vv <- votes[ok]
  o <- order(pv)
  pv <- pv[o]
  vv <- vv[o]
  ya <- sum(vv)
  na <- length(vv) - ya
  u <- unique(pv)
  cands <- c(pv[1] - 1, if (length(u) > 1) (u[-length(u)] + u[-1]) / 2, pv[length(pv)] + 1)
  best <- c(length(vv) + 1, 0, 1)
  for (cc in cands) {
    by <- sum(pv < cc & vv == 1)
    bn <- sum(pv < cc & vv == 0)
    eu <- (na - bn) + by
    ed <- (ya - by) + bn
    if (eu < best[1]) best <- c(eu, cc, 1)
    if (ed < best[1]) best <- c(ed, cc, -1)
  }
  best
}

#' Roll-call analysis
#'
#' Simulates roll calls from a spatial probit model (normal vector uniform on the
#' sphere, cutting point U(-0.5, 0.5), P(yea) = Phi(beta (x.n - c)), Philox
#' draws) or takes observed votes, and finds each roll call's optimal cutting
#' line or plane exactly (candidate normals perpendicular to pairs in two
#' dimensions, through triples in three), reporting errors and APRE.
#'
#' @param ideals Ideal points (vector or matrix with 1 to 3 columns).
#' @param votes Optional 0/1/NA vote matrix (legislators by roll calls).
#' @param n_votes Number of roll calls to simulate.
#' @param beta Signal-to-noise of the simulated votes.
#' @param seed Philox seed.
#' @return list(votes, normals, cuts, errors, classification, apre, and
#'   true_normals, true_cuts when simulated).
#' @references Poole, K. T. (2000). Nonparametric unfolding of binary choice data.
#'   Political Analysis 8, 211-237. Poole, K. T. (2005). Spatial Models of
#'   Parliamentary Voting. Cambridge University Press.
#' @examples
#' OptimalCuttingLines(c(-1, -0.5, 0.4, 1), votes = matrix(c(0, 0, 1, 1)))$apre
#' @export
OptimalCuttingLines <- function(ideals, votes = NULL, n_votes = 0, beta = 5, seed = 0) {
  P <- if (is.matrix(ideals)) ideals * 1 else matrix(as.numeric(ideals), ncol = 1)
  n <- nrow(P)
  d <- ncol(P)
  if (d > 3) stop("cutting lines are enumerated exactly for 1 to 3 dimensions", call. = FALSE)
  out <- list()
  if (is.null(votes)) {
    e <- .mh_rand(seed)
    V <- matrix(0L, n, n_votes)
    tn <- list()
    tc <- numeric(0)
    for (j in seq_len(n_votes)) {
      nv <- vapply(seq_len(d), function(q) .mh_n(e), 0)
      nv <- nv / sqrt(.sp_sum(nv^2))
      cc <- .mh_u(e) - 0.5
      tn[[j]] <- nv
      tc <- c(tc, cc)
      for (i in seq_len(n)) {
        z <- beta * (.sp_sum(P[i, ] * nv) - cc)
        V[i, j] <- as.integer(.mh_u(e) < stats::pnorm(z))
      }
    }
    out$true_normals <- tn
    out$true_cuts <- tc
  } else {
    V <- matrix(as.integer(as.matrix(votes)), n)
  }
  dirs <- list()
  if (d == 1) {
    dirs <- list(1)
  } else if (d == 2) {
    for (i in 1:(n - 1)) for (k in (i + 1):n) {
      dx <- P[k, 1] - P[i, 1]
      dy <- P[k, 2] - P[i, 2]
      if (dx != 0 || dy != 0) {
        a <- atan2(dx, -dy)
        for (eps in c(-1e-7, 1e-7)) dirs[[length(dirs) + 1]] <- c(cos(a + eps), sin(a + eps))
      }
    }
  } else {
    for (cmb in utils::combn(n, 3, simplify = FALSE)) {
      u <- P[cmb[2], ] - P[cmb[1], ]
      v <- P[cmb[3], ] - P[cmb[1], ]
      nv <- c(u[2] * v[3] - u[3] * v[2], u[3] * v[1] - u[1] * v[3], u[1] * v[2] - u[2] * v[1])
      s <- sqrt(.sp_sum(nv^2))
      if (s > 1e-12) {
        base <- nv / s
        dirs[[length(dirs) + 1]] <- base
        for (q in 1:3) for (eps in c(-1e-6, 1e-6)) {
          w <- base
          w[q] <- w[q] + eps
          dirs[[length(dirs) + 1]] <- w / sqrt(.sp_sum(w^2))
        }
      }
    }
  }
  m <- ncol(V)
  normals <- list()
  cuts <- numeric(m)
  errs <- integer(m)
  minority <- integer(m)
  for (j in seq_len(m)) {
    col <- V[, j]
    obs <- col[!is.na(col)]
    minority[j] <- min(sum(obs), length(obs) - sum(obs))
    best <- NULL
    for (dv in dirs) {
      b <- .rc_best_cut(as.vector(P %*% dv), col)
      if (is.null(best) || b[1] < best$e) best <- list(e = b[1], n = b[3] * dv, c = b[3] * b[2])
    }
    errs[j] <- best$e
    normals[[j]] <- best$n
    cuts[j] <- best$c
  }
  tot <- sum(!is.na(V))
  c(out, list(votes = V, normals = normals, cuts = cuts, errors = errs, classification = 1 - sum(errs) / tot,
              apre = if (sum(minority)) (sum(minority) - sum(errs)) / sum(minority) else 1))
}

#' Wordscores
#'
#' Word scores from reference texts with known positions and the scores,
#' standard errors and LBG-rescaled scores of virgin texts.
#'
#' @param ref_counts Reference texts by words count matrix.
#' @param ref_scores Reference positions.
#' @param virgin_counts Virgin texts by words count matrix.
#' @param rescale Apply the LBG rescaling (needs two or more virgin texts).
#' @return list(word_scores, raw, se, rescaled).
#' @references Laver, M., Benoit, K. and Garry, J. (2003). Extracting policy
#'   positions from political texts using words as data. American Political
#'   Science Review 97, 311-331.
#' @examples
#' Wordscores(rbind(c(2, 0, 2), c(0, 2, 2)), c(-1, 1), rbind(c(1, 1, 2)), rescale = FALSE)$raw
#' @export
Wordscores <- function(ref_counts, ref_scores, virgin_counts, rescale = TRUE) {
  R <- as.matrix(ref_counts) * 1
  A <- as.numeric(ref_scores)
  V <- as.matrix(virgin_counts) * 1
  Fr <- R / rowSums(R)
  tot <- colSums(Fr)
  S <- ifelse(tot > 0, colSums(Fr / rep(ifelse(tot > 0, tot, 1), each = nrow(Fr)) * A), NA)
  raw <- se <- numeric(nrow(V))
  for (v in seq_len(nrow(V))) {
    seen <- which(!is.na(S) & V[v, ] > 0)
    N <- .sp_sum(V[v, seen])
    fv <- V[v, seen] / N
    raw[v] <- .sp_sum(fv * S[seen])
    se[v] <- sqrt(.sp_sum(fv * (S[seen] - raw[v])^2) / N)
  }
  out <- list(word_scores = S, raw = raw, se = se)
  if (rescale && length(raw) > 1) out$rescaled <- (raw - mean(raw)) * stats::sd(A) / stats::sd(raw) + mean(raw)
  out
}
