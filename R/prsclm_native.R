# R arm of prsclm -- PLINK-style LD clumping followed by p-value
# thresholding into polygenic scores.
# Purcell, S. et al. (2007) Am. J. Hum. Genet. 81(3), 559-575;
# International Schizophrenia Consortium (2009) Nature 460, 748-752;
# Choi, S. W. & O'Reilly, P. F. (2019) GigaScience 8(7), giz082.
# Mirrors src/morie/fn/prsclm.py.

.prsclm_EPS <- 1e-12
.prsclm_DEFAULT_THRESHOLDS <- c(5e-8, 1e-6, 1e-4, 1e-3, 0.01, 0.05, 0.1,
                                0.5, 1.0)

#' .prsclm_rows
#'
#' Part of the prsclm_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @return The value of \code{m}, as built in the body.
#' @export
.prsclm_rows <- function(x) {
  if (is.matrix(x)) m <- x
  else if (is.data.frame(x)) m <- as.matrix(x)
  else if (is.list(x)) m <- do.call(rbind, lapply(x, as.numeric))
  else m <- matrix(as.numeric(x), ncol = 1L)
  storage.mode(m) <- "double"
  m
}

#' morie_prsclm_prs_cs_clump
#'
#' Part of the prsclm_native implementation; see the file header for the
#' source it follows.
#'
#' @param sumstats See Usage.
#' @param ld_ref See Usage.
#' @param p_threshold Defaults to \code{NULL}.
#' @param r2 Defaults to \code{0.1}.
#' @param window Defaults to \code{250000}.
#' @param genotypes Defaults to \code{NULL}.
#' @param standardize Defaults to \code{FALSE}.
#' @return A list with \code{estimate}, \code{n_retained}, \code{thresholds}, \code{retained}, \code{score}, \code{score_threshold}, \code{scores_by_threshold}, \code{index_variants}, \code{index_variant_names}, \code{clump_of}, \code{clump_members}, \code{clump_sizes}, \code{index_is_most_significant}, \code{n_clumps}, \code{n_variants}, \code{n_individuals}, \code{r2}, \code{window}, \code{standardized}, \code{weights}, \code{method}, \code{note}.
#' @export
morie_prsclm_prs_cs_clump <- function(sumstats, ld_ref, p_threshold = NULL,
                                      r2 = 0.1, window = 250000.0,
                                      genotypes = NULL,
                                      standardize = FALSE) {
  if (!is.list(sumstats))
    stop("prsclm: sumstats must be a mapping with 'beta' and 'p'")
  for (key in c("beta", "p"))
    if (!(key %in% names(sumstats)))
      stop(sprintf("prsclm: sumstats is missing '%s'", key))
  beta <- as.numeric(sumstats$beta)
  pv <- as.numeric(sumstats$p)
  m <- length(beta)
  if (m == 0L) stop("prsclm: no variants")
  if (length(pv) != m)
    stop(sprintf("prsclm: %d effect sizes but %d p-values", m, length(pv)))
  if (any(pv < 0.0 | pv > 1.0)) stop("prsclm: a p-value outside [0, 1]")
  pos <- if (is.null(sumstats$position)) as.numeric(seq_len(m) - 1L) else
    as.numeric(sumstats$position)
  if (length(pos) != m)
    stop(sprintf("prsclm: %d variants but %d positions", m, length(pos)))
  names_ <- if (is.null(sumstats$snp)) sprintf("v%d", seq_len(m) - 1L) else
    as.character(sumstats$snp)
  if (length(names_) != m)
    stop(sprintf("prsclm: %d variants but %d names", m, length(names_)))

  R <- .prsclm_rows(ld_ref)
  if (nrow(R) != m || ncol(R) != m)
    stop(sprintf("prsclm: ld_ref must be %d by %d", m, m))
  if (any(R < -1e-9))
    stop(paste0("prsclm: ld_ref holds a negative entry -- it must be ",
                "squared correlations, not correlations; squaring them ",
                "here would change the answer without saying so"))
  asym <- max(abs(R - t(R)))
  if (asym > 1e-8)
    stop(sprintf("prsclm: ld_ref is not symmetric (largest asymmetry %.3g)",
                 asym))
  r2t <- as.numeric(r2)
  if (!(r2t >= 0.0 && r2t <= 1.0)) stop("prsclm: r2 must be in [0, 1]")
  win <- as.numeric(window)
  if (win < 0.0) stop("prsclm: the window cannot be negative")

  thr <- if (is.null(p_threshold)) .prsclm_DEFAULT_THRESHOLDS else
    as.numeric(p_threshold)
  if (any(thr < 0.0 | thr > 1.0))
    stop("prsclm: a threshold outside [0, 1]")
  thr <- sort(unique(thr))

  # ---- PLINK clumping: most significant first, ties by position then index
  ord <- order(pv, pos, seq_len(m))
  clump_of <- rep(-1L, m)
  idxs <- integer(0)
  members <- list()
  for (i in ord) {
    if (clump_of[i] != -1L) next
    clump_of[i] <- i
    grp <- i
    for (j in seq_len(m)) {
      if (clump_of[j] != -1L || j == i) next
      if (abs(pos[j] - pos[i]) <= win && R[i, j] > r2t) {
        clump_of[j] <- i
        grp <- c(grp, j)
      }
    }
    idxs <- c(idxs, i)
    members[[length(members) + 1L]] <- sort(grp)
  }
  o <- order(idxs)
  idxs <- idxs[o]
  members <- members[o]

  # every index variant must be the most significant in its own clump -- if
  # it is not, the greedy order was applied wrongly
  index_is_top <- all(vapply(seq_along(idxs), function(u)
    pv[idxs[u]] <= min(pv[members[[u]]]) + 1e-15, TRUE))

  G <- NULL; n <- 0L
  if (!is.null(genotypes)) {
    G <- .prsclm_rows(genotypes)
    n <- nrow(G)
    if (ncol(G) != m)
      stop(sprintf("prsclm: genotypes must have %d columns", m))
    if (isTRUE(standardize)) {
      for (a in seq_len(m)) {
        mu <- sum(G[, a]) / n
        sd_ <- sqrt(sum((G[, a] - mu) ^ 2) / max(n - 1L, 1L))
        G[, a] <- if (sd_ > .prsclm_EPS) (G[, a] - mu) / sd_ else 0.0
      }
    }
  }

  retained <- list(); scores <- list(); counts <- integer(0)
  for (t in thr) {
    keep <- idxs[pv[idxs] < t]
    retained[[length(retained) + 1L]] <- as.numeric(keep - 1L)
    counts <- c(counts, length(keep))
    if (is.null(G)) {
      scores[[length(scores) + 1L]] <- NULL
    } else if (length(keep) == 0L) {
      scores[[length(scores) + 1L]] <- rep(0.0, n)
    } else {
      scores[[length(scores) + 1L]] <-
        as.numeric(G[, keep, drop = FALSE] %*% beta[keep])
    }
  }

  best <- NULL
  for (u in seq_along(thr)) if (counts[u] > 0L) { best <- u; break }

  list(estimate = as.numeric(counts), n_retained = as.integer(counts),
       thresholds = thr, retained = retained,
       score = if (!is.null(best) && !is.null(G)) scores[[best]] else NULL,
       score_threshold = if (!is.null(best)) thr[best] else NULL,
       scores_by_threshold = scores,
       # REPORTED indices follow the Python spec and are 0-based
       index_variants = as.numeric(idxs - 1L),
       index_variant_names = names_[idxs],
       clump_of = as.numeric(clump_of - 1L),
       clump_members = lapply(members, function(g) as.numeric(g - 1L)),
       clump_sizes = vapply(members, length, 0L),
       index_is_most_significant = index_is_top,
       n_clumps = as.integer(length(members)), n_variants = as.integer(m),
       n_individuals = as.integer(n),
       r2 = r2t, window = win, standardized = isTRUE(standardize),
       weights = beta,
       method = paste0("PLINK-style LD clumping (most significant index ",
                       "variant first, correlated neighbours within the ",
                       "window removed) followed by p-value thresholding ",
                       "(Purcell et al. 2007; International Schizophrenia ",
                       "Consortium 2009; Choi & O'Reilly 2019)"),
       note = paste0("index_variants and clump_of are 0-based; several ",
                     "thresholds are always scored because the best one is ",
                     "a property of the target sample, and reporting one ",
                     "number would hide that a choice was made"))
}

#' .prsclm_cheatsheet
#'
#' Part of the prsclm_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
.prsclm_cheatsheet <- function() {
  paste0("prsclm: morie_prsclm_prs_cs_clump(sumstats, ld_ref, p_threshold) ",
         "-> LD clumping plus thresholded polygenic scores (Purcell et al. ",
         "2007 PLINK; Choi & O'Reilly 2019 PRSice-2)")
}

morie_prsclm <- morie_prsclm_prs_cs_clump
