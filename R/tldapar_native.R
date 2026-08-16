# Data-adaptive target parameters and CV-TMLE.
# Sources: van der Laan, M. J. & Rose, S. (2018) *Targeted Learning
# in Data Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 9
# (data-adaptive target parameters; the example of defining
# treatment or exposure levels from the data; methodology for
# data-adaptive parameters; the TMLE of the v-specific data-adaptive
# parameter and the combination of v-specific TMLEs across
# estimation samples; CV-TMLE and CV-TMLE for data-adaptive
# parameters; CV-TMLE for variable importance measures; and the
# varImpact software with the Framingham Heart Study analysis).
# Hubbard, A. E., Kherad-Pajouh, S. & van der Laan, M. J. (2016)
# "Statistical Inference for Data Adaptive Target Parameters",
# International Journal of Biostatistics 12(1), 3-19,
# doi:10.1515/ijb-2015-0013. Zheng, W. & van der Laan, M. J.
# (2011) "Cross-Validated Targeted Minimum-Loss-Based Estimation",
# in *Targeted Learning*, Springer, 459-474,
# doi:10.1007/978-1-4419-9782-1_27.
#
# Native implementation mirroring Python morie.fn.tldapar exactly:
# the same V-fold split, the same data-adaptive parameter with
# pooled influence curve, the same combine step, the same
# variable-importance construction, and the same naive-reuse
# diagnostic.

#' morie_tldapar
#'
#' Part of the tldapar_native implementation; see the file header for
#' the source it follows.
#'
#' @param n See Usage.
#' @param V Defaults to \code{10L}.
#' @param seed Defaults to \code{0L}.
#' @param define_on_training Defaults to \code{NULL}.
#' @param estimate_on_holdout Defaults to \code{NULL}.
#' @param fold_estimates Defaults to \code{NULL}.
#' @param fold_ics Defaults to \code{NULL}.
#' @param effect Defaults to \code{NULL}.
#' @param screen Defaults to \code{NULL}.
#' @param reuse_fn Defaults to \code{NULL}.
#' @param mode Defaults to \code{c("split", "combine", "vimp", "reuse")}.
#' @return The value of \code{variable_importance}.
#' @export
morie_tldapar <- function(n, V = 10L, seed = 0L,
                          define_on_training = NULL,
                          estimate_on_holdout = NULL,
                          fold_estimates = NULL, fold_ics = NULL,
                          effect = NULL, screen = NULL,
                          reuse_fn = NULL,
                          mode = c("split", "combine", "vimp",
                                   "reuse")) {
  mode <- match.arg(mode)
  if (mode == "split")
    return(split_sample(n, V, seed))
  if (mode == "combine")
    return(cv_tmle(fold_estimates, fold_ics, n))
  if (mode == "reuse")
    return(naive_reuse(reuse_fn, n, seed))
  variable_importance(NULL, NULL, screen, effect, V = V, seed = seed)
}

#' split_sample
#'
#' Part of the tldapar_native implementation; see the file header for
#' the source it follows.
#'
#' @param n See Usage.
#' @param V Defaults to \code{10L}.
#' @param seed Defaults to \code{0L}.
#' @return A list with \code{estimation}, \code{training}, \code{V}.
#' @export
split_sample <- function(n, V = 10L, seed = 0L) {
  n <- as.integer(n)
  V <- as.integer(V)
  if (V < 2L || V > n)
    stop(sprintf("tldapar: V must lie in 2..%d, got %d", n, V))
  e_rng <- .ghc_rng(as.numeric(seed))
  idx <- seq_len(n)
  for (i in n:2) {
    j <- as.integer(.ghc_unif(e_rng, 1L) * (i + 1)) %% (i + 1)
    if (j == 0L) j <- 1L
    if (j == i) j <- i - 1L
    tmp <- idx[i]; idx[i] <- idx[j]; idx[j] <- tmp
  }
  est <- lapply(seq_len(V), function(v)
    sort(idx[seq(v, length(idx), by = V)]))
  list(estimation = est,
       training = lapply(est, function(f)
         sort(setdiff(seq_len(n), f))),
       V = V)
}

#' data_adaptive_parameter
#'
#' Part of the tldapar_native implementation; see the file header for
#' the source it follows.
#'
#' @param define_on_training See Usage.
#' @param estimate_on_holdout See Usage.
#' @param n See Usage.
#' @param V Defaults to \code{10L}.
#' @param seed Defaults to \code{0L}.
#' @return A list with \code{estimate}, \code{psi}, \code{fold_estimates}, \code{fold_parameters}, \code{se}, \code{ci}, \code{V}, \code{method}, \code{note}.
#' @export
data_adaptive_parameter <- function(define_on_training,
                                   estimate_on_holdout,
                                   n, V = 10L, seed = 0L) {
  sp <- split_sample(n, V, seed)
  ests <- numeric(V)
  ics <- rep(0, n)
  params <- vector("list", V)
  for (v in seq_len(V)) {
    p <- define_on_training(sp$training[[v]])
    params[[v]] <- p
    r <- estimate_on_holdout(p, sp$estimation[[v]])
    ests[v] <- as.numeric(r$estimate)
    for (a in seq_along(sp$estimation[[v]])) {
      ics[sp$estimation[[v]][a]] <- as.numeric(r$ic[a])
    }
  }
  psi <- mean(ests)
  m <- mean(ics)
  se <- sqrt(sum((ics - m)^2) / (n - 1) / n)
  list(estimate = psi, psi = psi, fold_estimates = ests,
       fold_parameters = params, se = se,
       ci = c(psi - 1.96 * se, psi + 1.96 * se),
       V = V,
       method = "data-adaptive target parameter with CV-TMLE; van der Laan & Rose (2018) Chap. 9",
       note = "the parameter is FIXED conditional on the training split, so the reported quantity is the one that was estimated")
}

#' cv_tmle
#'
#' Part of the tldapar_native implementation; see the file header for
#' the source it follows.
#'
#' @param fold_estimates See Usage.
#' @param fold_ics See Usage.
#' @param n See Usage.
#' @return A list with \code{psi}, \code{se}, \code{ci}, \code{mean_ic}, \code{note}.
#' @export
cv_tmle <- function(fold_estimates, fold_ics, n) {
  e <- as.numeric(fold_estimates)
  if (length(e) == 0L)
    stop("tldapar: no fold estimates given")
  psi <- mean(e)
  ic <- unlist(lapply(fold_ics, as.numeric))
  if (length(ic) != as.integer(n))
    stop(sprintf("tldapar: %d influence-curve values for %d observations",
                 length(ic), as.integer(n)))
  m <- mean(ic)
  se <- sqrt(sum((ic - m)^2) / (length(ic) - 1) / length(ic))
  list(psi = psi, se = se,
       ci = c(psi - 1.96 * se, psi + 1.96 * se),
       mean_ic = m,
       note = "each fold's fit is independent of the data it is evaluated on, which is what removes the Donsker condition")
}

#' variable_importance
#'
#' Part of the tldapar_native implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param Y See Usage.
#' @param screen See Usage.
#' @param effect See Usage.
#' @param V Defaults to \code{5L}.
#' @param seed Defaults to \code{0L}.
#' @return A list with \code{estimate}, \code{importance}, \code{V}, \code{method}.
#' @export
variable_importance <- function(X, Y, screen, effect, V = 5L,
                                seed = 0L) {
  rows <- as.matrix(X)
  if (is.null(dim(rows))) rows <- matrix(as.numeric(X), ncol = 1)
  n <- nrow(rows)
  sp <- split_sample(n, V, seed)
  per <- list()
  for (v in seq_len(sp$V)) {
    sel <- screen(sp$training[[v]])
    for (j in sel) {
      r <- effect(j, sp$estimation[[v]])
      key <- as.character(j)
      if (is.null(per[[key]])) per[[key]] <- list(est = numeric(0),
                                                  ic = numeric(0))
      per[[key]]$est <- c(per[[key]]$est, as.numeric(r$estimate))
      per[[key]]$ic <- c(per[[key]]$ic, as.numeric(r$ic))
    }
  }
  out <- list()
  for (key in names(per)) {
    d <- per[[key]]
    psi <- mean(d$est)
    ic <- d$ic
    m <- mean(ic)
    se <- sqrt(sum((ic - m)^2) / max(length(ic) - 1, 1) / length(ic))
    out[[key]] <- list(psi = psi, se = se,
                       ci = c(psi - 1.96 * se, psi + 1.96 * se),
                       folds_selected = length(d$est))
  }
  list(estimate = out, importance = out, V = sp$V,
       method = "CV-TMLE variable importance; van der Laan & Rose (2018) Chap. 9, as in varImpact")
}

#' naive_reuse
#'
#' Part of the tldapar_native implementation; see the file header for
#' the source it follows.
#'
#' @param define_and_estimate See Usage.
#' @param n See Usage.
#' @param seed Defaults to \code{0L}.
#' @return A list with \code{estimate}, \code{warning}.
#' @export
naive_reuse <- function(define_and_estimate, n, seed = 0L) {
  r <- define_and_estimate(seq_len(as.integer(n)))
  list(estimate = as.numeric(r$estimate),
       warning = "the parameter was selected and estimated on the same sample; the reported inference is not valid for the selected parameter")
}

#' .tldapar_cheatsheet
#'
#' Part of the tldapar_native implementation; see the file header for
#' the source it follows.
#'
#' @return A character value.
#' @export
.tldapar_cheatsheet <- function() {
  paste("tldapar: when the QUESTION depends on the data -- which ",
        "levels to contrast, which variable to report -- defining ",
        "and estimating on the same sample invalidates the ",
        "interval, and no estimator fixes that. Split: DEFINE the ",
        "parameter on the training split, ESTIMATE it on the ",
        "held-out one, so conditional on training it is fixed. ",
        "Then CV-TMLE averages the v-specific TMLEs and pools the ",
        "influence curve -- which also removes the Donsker ",
        "condition, since each fit is independent of the data it ",
        "is scored on.", sep = "")
}
