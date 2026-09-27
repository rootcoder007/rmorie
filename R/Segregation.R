.sg_counts <- function(x) {
  X <- as.matrix(x)
  storage.mode(X) <- "double"
  if (ncol(X) < 2 || any(X < 0)) stop("counts must be a non-negative units x groups matrix with at least two groups", call. = FALSE)
  keep <- rowSums(X) > 0
  list(X = X[keep, , drop = FALSE], keep = keep)
}

.sg_sum <- function(v) {
  s <- 0
  for (q in v) s <- s + q
  s
}

#' Dissimilarity and segregation indices
#'
#' Pairwise Duncan dissimilarity D_kl = sum |x_ik/X_k - x_il/X_l| / 2, the
#' one-against-the-rest segregation index IS_k, and with a contiguity matrix
#' Morrill's adjusted D minus the mean absolute difference of group shares across
#' adjacent units.
#'
#' @param x Units x groups count matrix.
#' @param contiguity Optional 0/1 contiguity matrix (zero diagonal).
#' @return list(D, IS, D_morrill).
#' @references Duncan, O. D. and Duncan, B. (1955). A methodological analysis of
#'   segregation indexes. American Sociological Review 20, 210-217. Morrill, R. L.
#'   (1991). On the measure of geographic segregation. Geography Research Forum 11,
#'   25-36.
#' @examples
#' DissimilarityIndex(rbind(c(10, 0), c(0, 10)))$D
#' @export
DissimilarityIndex <- function(x, contiguity = NULL) {
  cl <- .sg_counts(x)
  X <- cl$X
  n <- nrow(X)
  g <- ncol(X)
  tot <- colSums(X)
  D <- matrix(0, g, g)
  for (a in 1:(g - 1)) for (b in (a + 1):g) D[a, b] <- D[b, a] <- 0.5 * .sg_sum(abs(X[, a] / tot[a] - X[, b] / tot[b]))
  rows <- rowSums(X)
  IS <- vapply(seq_len(g), function(k) 0.5 * .sg_sum(abs(X[, k] / tot[k] - (rows - X[, k]) / (sum(rows) - tot[k]))), 0)
  out <- list(D = D, IS = IS)
  if (!is.null(contiguity)) {
    cm <- as.matrix(contiguity)[cl$keep, cl$keep, drop = FALSE]
    Dm <- matrix(0, g, g)
    for (a in 1:(g - 1)) for (b in (a + 1):g) {
      z <- ifelse(X[, a] + X[, b] > 0, X[, a] / (X[, a] + X[, b]), 0)
      Dm[a, b] <- Dm[b, a] <- D[a, b] - sum(cm * abs(outer(z, z, "-"))) / sum(cm)
    }
    out$D_morrill <- Dm
  }
  out
}

#' Exposure (interaction) index
#'
#' xPy_kl = sum_i (x_ik / X_k)(x_il / t_i) (exact: without replacement), and with a
#' distance matrix Morgan's distance-decay interaction with kernel
#' K_ij = t_j exp(-beta d_ij) / sum_j t_j exp(-beta d_ij).
#'
#' @param x Units x groups count matrix.
#' @param exact Sample without replacement.
#' @param distance Optional distance matrix.
#' @param beta Decay rate.
#' @return list(xPy, DPxy).
#' @references Massey, D. S. and Denton, N. A. (1988). The dimensions of
#'   residential segregation. Social Forces 67, 281-315. Morgan, B. S. (1983). An
#'   alternate approach to the development of a distance-based measure of racial
#'   segregation. American Journal of Sociology 88, 1237-1249.
#' @examples
#' ExposureIndex(rbind(c(5, 5), c(5, 5)))$xPy
#' @export
ExposureIndex <- function(x, exact = FALSE, distance = NULL, beta = 1) {
  cl <- .sg_counts(x)
  X <- cl$X
  g <- ncol(X)
  tot <- colSums(X)
  t <- rowSums(X)
  P <- matrix(0, g, g)
  for (a in seq_len(g)) for (b in seq_len(g)) {
    P[a, b] <- if (exact) .sg_sum(X[, a] / tot[a] * (X[, b] - (a == b)) / (t - 1)) else .sg_sum(X[, a] / tot[a] * X[, b] / t)
  }
  out <- list(xPy = P)
  if (!is.null(distance)) {
    E <- exp(-beta * as.matrix(distance)[cl$keep, cl$keep, drop = FALSE])
    K <- sweep(E, 2, t, "*")
    K <- K / rowSums(K)
    DP <- matrix(0, g, g)
    for (a in seq_len(g)) for (b in seq_len(g)) DP[a, b] <- .sg_sum(X[, a] / tot[a] * as.vector(K %*% (X[, b] / t)))
    out$DPxy <- DP
  }
  out
}

#' Isolation index
#'
#' Isolation xPx_k, the correlation ratio eta2_k = (xPx_k - P_k) / (1 - P_k), and
#' with a distance matrix Morgan's distance-decay isolation.
#'
#' @inheritParams ExposureIndex
#' @return list(xPx, eta2, DPxx).
#' @references Massey, D. S. and Denton, N. A. (1988). Social Forces 67, 281-315.
#'   White, M. J. (1986). Segregation and diversity measures in population
#'   distribution. Population Index 52, 198-221.
#' @examples
#' IsolationIndex(rbind(c(10, 0), c(0, 10)))$xPx
#' @export
IsolationIndex <- function(x, exact = FALSE, distance = NULL, beta = 1) {
  e <- ExposureIndex(x, exact, distance, beta)
  X <- .sg_counts(x)$X
  P <- colSums(X) / sum(X)
  xpx <- diag(e$xPy)
  out <- list(xPx = xpx, eta2 = (xpx - P) / (1 - P))
  if (!is.null(distance)) out$DPxx <- diag(e$DPxy)
  out
}

#' Spatial concentration indices
#'
#' Hoover's delta DEL, the absolute concentration ACO and the relative
#' concentration RCO of Massey and Denton (1988), with units ordered by area.
#'
#' @param x Units x groups count matrix.
#' @param area Unit areas.
#' @return list(DEL, ACO, RCO).
#' @references Massey, D. S. and Denton, N. A. (1988). Social Forces 67, 281-315.
#' @examples
#' SpatialConcentration(rbind(c(10, 0), c(0, 10)), c(1, 3))$DEL
#' @export
SpatialConcentration <- function(x, area) {
  cl <- .sg_counts(x)
  X <- cl$X
  a <- as.numeric(area)[cl$keep]
  n <- nrow(X)
  g <- ncol(X)
  tot <- colSums(X)
  t <- rowSums(X)
  DEL <- vapply(seq_len(g), function(k) 0.5 * .sg_sum(abs(X[, k] / tot[k] - a / sum(a))), 0)
  o <- order(a, seq_len(n))
  ts <- t[o]
  as_ <- a[o]
  xs <- X[o, , drop = FALSE]
  low <- lapply(seq_len(g), function(k) {
    s <- 0
    i <- 0
    while (s < tot[k]) {
      i <- i + 1
      s <- s + ts[i]
    }
    c(i, s)
  })
  high <- lapply(seq_len(g), function(k) {
    s <- 0
    i <- n + 1
    while (s < tot[k]) {
      i <- i - 1
      s <- s + ts[i]
    }
    c(i, s)
  })
  v1 <- vapply(seq_len(g), function(k) .sg_sum(xs[, k] * as_ / tot[k]), 0)
  v2 <- vapply(seq_len(g), function(k) .sg_sum(ts[1:low[[k]][1]] * as_[1:low[[k]][1]] / low[[k]][2]), 0)
  v3 <- vapply(seq_len(g), function(k) .sg_sum(ts[high[[k]][1]:n] * as_[high[[k]][1]:n] / high[[k]][2]), 0)
  RCO <- outer(seq_len(g), seq_len(g), Vectorize(function(p, q) (v1[p] / v1[q] - 1) / (v2[p] / v3[q] - 1)))
  list(DEL = DEL, ACO = 1 - (v1 - v2) / (v3 - v2), RCO = RCO)
}

#' Spatial clustering indices
#'
#' Absolute clustering ACL (contiguity with unit diagonal, or exp(-beta d)), and
#' with distances the relative clustering RCL and White's spatial proximity SP.
#'
#' @param x Units x groups count matrix.
#' @param contiguity Optional 0/1 contiguity matrix.
#' @param distance Optional distance matrix.
#' @param beta Decay rate.
#' @return list(ACL, RCL, SP).
#' @references Massey, D. S. and Denton, N. A. (1988). Social Forces 67, 281-315.
#'   White, M. J. (1983). The measurement of spatial segregation. American
#'   Journal of Sociology 88, 1008-1018.
#' @examples
#' ClusteringIndex(rbind(c(4, 1), c(3, 2), c(1, 4), c(0, 5)),
#'   contiguity = rbind(c(0, 1, 0, 0), c(1, 0, 1, 0), c(0, 1, 0, 1), c(0, 0, 1, 0)))$ACL
#' @export
ClusteringIndex <- function(x, contiguity = NULL, distance = NULL, beta = 1) {
  if (is.null(contiguity) && is.null(distance)) stop("give contiguity or distance", call. = FALSE)
  cl <- .sg_counts(x)
  X <- cl$X
  n <- nrow(X)
  g <- ncol(X)
  tot <- colSums(X)
  t <- rowSums(X)
  if (!is.null(contiguity)) {
    cm <- as.matrix(contiguity)[cl$keep, cl$keep, drop = FALSE]
    diag(cm) <- 1
  } else {
    cm <- exp(-beta * as.matrix(distance)[cl$keep, cl$keep, drop = FALSE])
  }
  ct <- as.vector(cm %*% t)
  ACL <- vapply(seq_len(g), function(k) {
    v1 <- .sg_sum(as.vector(cm %*% X[, k]) * X[, k] / tot[k])
    v2 <- sum(cm) * tot[k] / n^2
    v3 <- .sg_sum(ct * X[, k] / tot[k])
    (v1 - v2) / (v3 - v2)
  }, 0)
  out <- list(ACL = ACL)
  if (!is.null(distance)) {
    E <- exp(-beta * as.matrix(distance)[cl$keep, cl$keep, drop = FALSE])
    P <- vapply(seq_len(g), function(k) .sg_sum(as.vector(E %*% X[, k]) * X[, k]) / tot[k]^2, 0)
    Ptt <- .sg_sum(as.vector(E %*% t) * t) / sum(t)^2
    out$RCL <- outer(P, P, "/") - 1
    out$SP <- .sg_sum(P * tot) / (sum(t) * Ptt)
  }
  out
}

#' Centralization indices
#'
#' Units ordered by distance to the centre: ACE compares cumulative group shares
#' with cumulative area, RCE compares two groups' cumulative shares (units at equal
#' distance pooled).
#'
#' @param x Units x groups count matrix.
#' @param center_distance Distance of each unit to the centre.
#' @param area Optional unit areas (for ACE).
#' @return list(ACE, RCE).
#' @references Massey, D. S. and Denton, N. A. (1988). Social Forces 67, 281-315.
#' @examples
#' CentralizationIndex(rbind(c(10, 0), c(5, 5), c(0, 10)), c(0, 1, 2))$RCE
#' @export
CentralizationIndex <- function(x, center_distance, area = NULL) {
  cl <- .sg_counts(x)
  X <- cl$X
  n <- nrow(X)
  g <- ncol(X)
  tot <- colSums(X)
  dc <- as.numeric(center_distance)[cl$keep]
  o <- order(dc, seq_len(n))
  out <- list()
  if (!is.null(area)) {
    a <- as.numeric(area)[cl$keep]
    AI <- cumsum(a[o]) / sum(a)
    out$ACE <- vapply(seq_len(g), function(k) {
      XI <- cumsum(X[o, k]) / tot[k]
      .sg_sum(XI[-n] * AI[-1]) - .sg_sum(XI[-1] * AI[-n])
    }, 0)
  }
  lv <- sort(unique(dc))
  pooled <- t(vapply(lv, function(v) colSums(X[dc == v, , drop = FALSE]), numeric(g)))
  cum <- sweep(apply(pooled, 2, cumsum), 2, tot, "/")
  m <- length(lv)
  out$RCE <- outer(seq_len(g), seq_len(g), Vectorize(function(p, q) .sg_sum(cum[-m, p] * cum[-1, q]) - .sg_sum(cum[-1, p] * cum[-m, q])))
  out
}

#' Segregation evenness: Theil, Atkinson and Gini
#'
#' Each group against the rest: Theil's entropy index H, Atkinson's index with
#' shape \code{delta}, and the Gini segregation index.
#'
#' @param x Units x groups count matrix.
#' @param delta Atkinson shape in (0, 1).
#' @return list(H, atkinson, gini).
#' @references Massey, D. S. and Denton, N. A. (1988). Social Forces 67, 281-315.
#'   Atkinson, A. B. (1970). On the measurement of inequality. Journal of Economic
#'   Theory 2, 244-263.
#' @examples
#' SegregationEvenness(rbind(c(10, 0), c(0, 10)))$H
#' @export
SegregationEvenness <- function(x, delta = 0.5) {
  X <- .sg_counts(x)$X
  g <- ncol(X)
  t <- rowSums(X)
  T <- sum(t)
  ent <- function(p) ifelse(p > 0, p * log(1 / p), 0) + ifelse(p < 1, (1 - p) * log(1 / (1 - p)), 0)
  H <- AT <- G <- numeric(g)
  for (k in seq_len(g)) {
    P <- sum(X[, k]) / T
    p <- X[, k] / t
    H[k] <- .sg_sum(t * (ent(P) - ent(p))) / (ent(P) * T)
    AT[k] <- 1 - P / (1 - P) * .sg_sum((1 - p)^(1 - delta) * p^delta * t / (P * T))^(1 / (1 - delta))
    G[k] <- sum(outer(t, t) * abs(outer(p, p, "-"))) / (2 * T^2 * P * (1 - P))
  }
  list(H = H, atkinson = AT, gini = G)
}
