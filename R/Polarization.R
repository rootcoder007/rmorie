#' Polarization indices
#'
#' \code{PolarizationIndices}: variance, Gini mean difference, bias-corrected
#' skewness and excess kurtosis, and the bimodality coefficient.
#' \code{EstebanRayIndex}: Esteban and Ray (1994) polarization.
#' \code{EarthMoversDistance}: one-dimensional Wasserstein-1 distance.
#' \code{IssueConstraint}: mean absolute correlation between issues.
#' \code{PolarizationTrend}: OLS trend of a polarization series. Identical to
#' the Python arm \code{morie.fn.polarize}.
#'
#' @param positions Positions (ideal points, opinions, group locations).
#' @param weights Optional weights.
#' @param shares Group population shares.
#' @param alpha Esteban-Ray sensitivity.
#' @param k Esteban-Ray scale constant.
#' @param a,b Samples.
#' @param X Respondents x issues matrix.
#' @param time,values Time points and polarization values.
#' @return Numeric or list.
#' @references Esteban, J.-M. and Ray, D. (1994). On the measurement of
#'   polarization. Econometrica 62, 819-851.
#'
#'   Pfister, R., Schwarz, K. A., Janczyk, M., Dale, R. and Freeman, J. B.
#'   (2013). Good things peak in pairs: a note on the bimodality coefficient.
#'   Frontiers in Psychology 4, 700.
#' @examples
#' PolarizationIndices(c(-1, -1, 1, 1, 0))$gini_mean_difference
#' EstebanRayIndex(c(0, 1), c(0.5, 0.5))
#' @export
PolarizationIndices <- function(positions, weights = NULL) {
  x <- positions
  n <- length(x)
  w <- if (is.null(weights)) rep(1, n) else weights
  d <- x - mean(x)
  var <- sum(d^2) / (n - 1)
  gmd <- sum(outer(w, w) * abs(outer(x, x, `-`))) / sum(w)^2
  m2 <- mean(d^2)
  g1 <- mean(d^3) / m2^1.5
  g2 <- mean(d^4) / m2^2 - 3
  G1 <- if (n > 2) g1 * sqrt(n * (n - 1)) / (n - 2) else NaN
  G2 <- if (n > 3) (n - 1) / ((n - 2) * (n - 3)) * ((n + 1) * g2 + 6) else NaN
  bc <- if (n > 3) (G1^2 + 1) / (G2 + 3 * (n - 1)^2 / ((n - 2) * (n - 3))) else NaN
  list(variance = var, sd = sqrt(var), gini_mean_difference = gmd, skewness = G1, excess_kurtosis = G2,
       bimodality_coefficient = bc)
}

#' @rdname PolarizationIndices
#' @export
EstebanRayIndex <- function(positions, shares = NULL, alpha = 1, k = 1) {
  p <- if (is.null(shares)) rep(1, length(positions)) else shares
  p <- p / sum(p)
  k * sum(outer(p^(1 + alpha), p) * abs(outer(positions, positions, `-`)))
}

#' @rdname PolarizationIndices
#' @export
EarthMoversDistance <- function(a, b) {
  pts <- sort(unique(c(a, b)))
  if (length(pts) < 2) return(0)
  Fa <- stats::ecdf(a)(pts[-length(pts)])
  Fb <- stats::ecdf(b)(pts[-length(pts)])
  sum(abs(Fa - Fb) * diff(pts))
}

#' @rdname PolarizationIndices
#' @export
IssueConstraint <- function(X) {
  R <- unname(stats::cor(as.matrix(X)))
  list(correlation = R, constraint = mean(abs(R[upper.tri(R)])))
}

#' @rdname PolarizationIndices
#' @export
PolarizationTrend <- function(time, values) {
  n <- length(time)
  sxx <- sum((time - mean(time))^2)
  b <- sum((time - mean(time)) * (values - mean(values))) / sxx
  a0 <- mean(values) - b * mean(time)
  se <- if (n > 2) sqrt(sum((values - a0 - b * time)^2) / (n - 2) / sxx) else NaN
  list(slope = b, intercept = a0, se = se, t = if (se > 0) b / se else Inf)
}

#' Party systems and roll-call utilities
#'
#' \code{PartySystemIndices}: Rae fractionalization, Laakso-Taagepera and
#' Golosov effective numbers. \code{PartyDivergence}: differences of party
#' means and medians, overlap, and Rice cohesion. \code{RollcallMatrix}:
#' legislator x vote matrix with Voteview codes. \code{RollcallSummary}: yeas,
#' nays, minority shares, margins, participation. \code{RollcallFilter}:
#' \code{wnominate}-style lopsided-vote and participation filtering (indices
#' returned 1-based).
#'
#' @param shares Vote or seat shares.
#' @param positions Ideal points.
#' @param party Party labels.
#' @param left,right Labels of the two parties.
#' @param votes Legislators x roll calls matrix (1 yea, 0 nay, \code{NA}).
#' @param legislator,vote,choice Long-format records.
#' @param yea,nay Codes counted as yea and nay.
#' @param matrix Roll-call matrix.
#' @param lop Minimum minority share.
#' @param minvotes Minimum votes per legislator.
#' @return List.
#' @references Golosov, G. V. (2010). The effective number of parties: a new
#'   approach. Party Politics 16, 171-192.
#'
#'   Poole, K., Lewis, J., Lo, J. and Carroll, R. (2011). Scaling roll call
#'   votes with wnominate in R. Journal of Statistical Software 42(14).
#' @examples
#' PartySystemIndices(c(50, 30, 20))$golosov
#' RollcallMatrix(c("a", "a", "b", "b"), c(1, 2, 1, 2), c(1, 6, 4, 9))$matrix
#' @export
PartySystemIndices <- function(shares) {
  s <- shares / sum(shares)
  h <- sum(s^2)
  list(fractionalization = 1 - h, effective_number = 1 / h, golosov = sum(s / (s + max(s)^2 - s^2)))
}

#' @rdname PartySystemIndices
#' @export
PartyDivergence <- function(positions, party, left, right, votes = NULL) {
  L <- positions[party == left]
  R <- positions[party == right]
  lo <- min(R)
  hi <- max(L)
  ov <- if (lo <= hi) sum(positions >= lo & positions <= hi) else 0
  out <- list(mean_difference = mean(R) - mean(L), median_difference = stats::median(R) - stats::median(L),
              overlap = ov, overlap_share = ov / length(positions))
  if (!is.null(votes)) {
    V <- as.matrix(votes)
    rice <- function(nm) {
      rows <- V[party == nm, , drop = FALSE]
      rc <- apply(rows, 2, function(col) {
        col <- col[!is.na(col)]
        if (length(col)) abs(2 * sum(col == 1) - length(col)) / length(col) else NA_real_
      })
      mean(rc[!is.na(rc)])
    }
    out$rice_left <- rice(left)
    out$rice_right <- rice(right)
  }
  out
}

#' @rdname PartySystemIndices
#' @export
RollcallMatrix <- function(legislator, vote, choice, yea = 1:3, nay = 4:6) {
  legs <- sort(unique(legislator))
  vs <- sort(unique(vote))
  M <- matrix(NA_real_, length(legs), length(vs))
  code <- ifelse(choice %in% yea, 1, ifelse(choice %in% nay, 0, NA_real_))
  M[cbind(match(legislator, legs), match(vote, vs))] <- code
  list(matrix = M, legislators = legs, votes = vs)
}

#' @rdname PartySystemIndices
#' @export
RollcallSummary <- function(matrix) {
  M <- as.matrix(matrix)
  ye <- colSums(M == 1, na.rm = TRUE)
  na <- colSums(M == 0, na.rm = TRUE)
  list(yeas = unname(ye), nays = unname(na), minority_share = unname(ifelse(ye + na > 0, pmin(ye, na) / (ye + na), NaN)),
       margin = unname(abs(ye - na)), participation = unname(rowSums(!is.na(M))))
}

#' @rdname PartySystemIndices
#' @export
RollcallFilter <- function(matrix, lop = 0.025, minvotes = 20) {
  M <- as.matrix(matrix)
  s <- RollcallSummary(M)
  kv <- which(!is.na(s$minority_share) & s$minority_share >= lop)
  kl <- which(rowSums(!is.na(M[, kv, drop = FALSE])) >= minvotes)
  list(votes = kv, legislators = kl, matrix = M[kl, kv, drop = FALSE])
}
