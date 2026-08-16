# LTMLE with clustering.
# Sources: van der Laan, M. J. & Rose, S. (2018) *Targeted Learning
# in Data Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 15
# (Schnitzer, van der Laan, Moodie & Platt): the PROBIT
# cluster-randomized trial of a breastfeeding promotion programme,
# used because breastfeeding cannot be directly allocated; the
# estimation of the effect of different durations of breastfeeding
# on the number of periods of hospitalization in the first year;
# hospitalizations treated as a TIME-VARYING CONFOUNDER because they
# may also affect continuation of breastfeeding; the two
# parametrizations of the g-formula; and an LTMLE implementation
# accounting for an outcome partially determined by time-varying
# confounders and for the clustering arising from the study
# design. Kramer, M. S. et al. (2001) "Promotion of Breastfeeding
# Intervention Trial (PROBIT): a randomized trial in the Republic
# of Belarus", JAMA 285(4), 413-420, doi:10.1001/jama.285.4.413.
# The trial. Schnitzer, M. E., Moodie, E. E. M., van der Laan, M.
# J., Platt, R. W. & Klein, M. B. (2014) "Modeling the impact of
# hepatitis C viral clearance on end-stage liver disease in an HIV
# co-infected cohort with targeted maximum likelihood estimation",
# Biometrics 70(1), 144-152, doi:10.1111/biom.12105.
#
# Native implementation mirroring Python morie.fn.tlclust exactly:
# the same naive and cluster-level variances, the same design
# effect, the same two g-formula parametrizations with the same
# consistency check, and the same LTMLE-with-clustering entry point.

#' The LTMLE-with-clustering entry point
#'
#' Part of the tlclust_native implementation; see the file header for
#' the source it follows.
#'
#' @param Q_seq See Usage.
#' @param H_seq See Usage.
#' @param Y See Usage.
#' @param cluster See Usage.
#' @param ic Defaults to \code{NULL}.
#' @return A list, whose contents depend on the branch taken; across the branches its names are \code{estimate}, \code{psi}, \code{se_clustered}, \code{se_naive}, \code{ci}, \code{n_clusters}, \code{design_effect}, \code{method}, \code{note}, \code{naive}, \code{pooled}, \code{sequential}.
#' @export
morie_tlclust <- function(Q_seq, H_seq, Y, cluster, ic = NULL) {
  # the LTMLE-with-clustering entry point
  if (is.list(Q_seq) && !is.null(Y) && !is.null(cluster)) {
    r <- tlltmle::ltmle(Q_seq, H_seq, Y)
    q <- r$Q_star[[length(r$Q_star)]]
    psi <- r$psi
    if (is.null(ic))
      ic <- as.numeric(q) - psi
    cv <- cluster_variance(ic, cluster)
    nv <- naive_variance(ic)
    list(estimate = psi, psi = psi,
         se_clustered = cv$se, se_naive = nv,
         ci = c(psi - 1.96 * cv$se, psi + 1.96 * cv$se),
         n_clusters = cv$n_clusters,
         design_effect = if (nv > 0) cv$se / nv else NaN,
         method = "LTMLE with cluster-level influence-curve inference; van der Laan & Rose (2018) Chap. 15",
         note = "clustering changes the VARIANCE, not the point estimate")
  } else {
    # mirror the three primitives when called directly
    list(naive = naive_variance(Q_seq),
         pooled = g_formula_pooled(Q_seq),
         sequential = g_formula_sequential(Q_seq))
  }
}

#' naive_variance
#'
#' Part of the tlclust_native implementation; see the file header for
#' the source it follows.
#'
#' @param ic See Usage.
#' @return A numeric value.
#' @export
naive_variance <- function(ic) {
  v <- as.numeric(ic)
  n <- length(v)
  if (n < 2L)
    stop("tlclust: at least 2 observations are needed")
  m <- mean(v)
  sqrt(sum((v - m)^2) / (n - 1) / n)
}

#' cluster_variance
#'
#' Part of the tlclust_native implementation; see the file header for
#' the source it follows.
#'
#' @param ic See Usage.
#' @param cluster See Usage.
#' @return A list with \code{se}, \code{n_clusters}, \code{cluster_sums}, \code{note}.
#' @export
cluster_variance <- function(ic, cluster) {
  v <- as.numeric(ic)
  c <- as.character(cluster)
  if (length(v) != length(c))
    stop(sprintf("tlclust: %d influence values for %d cluster labels",
                 length(v), length(c)))
  agg <- tapply(v, c, sum)
  J <- length(agg)
  if (J < 2L)
    stop("tlclust: at least 2 clusters are needed")
  sums <- as.numeric(agg)
  m <- mean(sums)
  var <- sum((sums - m)^2) / (J - 1)
  list(se = sqrt(var / J) / (length(v) / J),
       n_clusters = J, cluster_sums = sums,
       note = "the CLUSTER is independent, not the individual")
}

#' design_effect
#'
#' Part of the tlclust_native implementation; see the file header for
#' the source it follows.
#'
#' @param ic See Usage.
#' @param cluster See Usage.
#' @return A list with \code{se_naive}, \code{se_clustered}, \code{ratio}, \code{note}.
#' @export
design_effect <- function(ic, cluster) {
  a <- naive_variance(ic)
  b <- cluster_variance(ic, cluster)$se
  list(se_naive = a, se_clustered = b,
       ratio = if (a > 0) b / a else NaN,
       note = "a ratio above 1 is the understatement caused by treating within-cluster observations as independent")
}

#' g_formula_pooled
#'
#' Part of the tlclust_native implementation; see the file header for
#' the source it follows.
#'
#' @param Q_final See Usage.
#' @param weights Defaults to \code{NULL}.
#' @return A list with \code{psi}, \code{parametrization}, \code{note}.
#' @export
g_formula_pooled <- function(Q_final, weights = NULL) {
  q <- as.numeric(Q_final)
  w <- if (is.null(weights)) rep(1, length(q)) else as.numeric(weights)
  t <- sum(w)
  if (t <= 1e-12)
    stop("tlclust: the weights sum to zero")
  list(psi = sum(w * q) / t,
       parametrization = "pooled",
       note = "one regression on the full history")
}

#' g_formula_sequential
#'
#' Part of the tlclust_native implementation; see the file header for
#' the source it follows.
#'
#' @param Q_seq See Usage.
#' @return A list with \code{psi}, \code{parametrization}, \code{T}, \code{note}.
#' @export
g_formula_sequential <- function(Q_seq) {
  if (length(Q_seq) == 0L)
    stop("tlclust: the sequence is empty")
  cur <- as.numeric(Q_seq[[length(Q_seq)]])
  for (t in (length(Q_seq) - 1L):1L) {
    nxt <- as.numeric(Q_seq[[t]])
    if (length(nxt) != length(cur))
      stop(sprintf("tlclust: the regressions differ in length at time %d",
                   t - 1L))
    cur <- nxt
  }
  list(psi = mean(cur),
       parametrization = "sequential",
       T = length(Q_seq),
       note = "identifies the same estimand; fails differently under misspecification")
}

.tlclust_cheatsheet <- function() {
  paste("tlclust: PROBIT randomised HOSPITALS because breastfeeding ",
        "cannot be allocated. Hospitalisation is both part of the ",
        "outcome and a TIME-VARYING CONFOUNDER affected by prior ",
        "exposure -- condition on it and you block the effect, ",
        "ignore it and confounding stays; sequential ",
        "g-computation is what handles it. TWO parametrizations of ",
        "the g-formula identify the same estimand and misspecify ",
        "differently, so implement both. Clustering changes the ",
        "VARIANCE only: aggregate the influence curve to the ",
        "cluster before taking its variance, or the standard error ",
        "is understated.", sep = "")
}
