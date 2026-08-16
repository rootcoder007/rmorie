# MetaBAT 2: adaptive binning of metagenome assemblies.
# Sources: Kang, D. D., Li, F., Kirton, E., Thomas, A., Egan, R., An,
# H. & Wang, Z. (2019) "MetaBAT 2: an adaptive binning algorithm for
# robust and efficient genome reconstruction from metagenome
# assemblies", *PeerJ* 7, e7359, doi:10.7717/peerj.7359. The abstract:
# existing binning performance suffers, especially on assemblies of
# poor quality; MetaBAT 2 using a new ADAPTIVE binning algorithm to
# eliminate manual parameter tuning; extensive software engineering
# optimisation for computational and memory efficiency; comparison
# against alternative tools on over 100 real-world metagenome assemblies
# showing superior accuracy and speed; and binning a typical assembly
# in a few minutes on a single commodity workstation.
#
# Kang, D. D., Froula, J., Egan, R. & Wang, Z. (2015) "MetaBAT, an
# efficient tool for accurately reconstructing single genomes from
# complex microbial communities", *PeerJ* 3, e1165,
# doi:10.7717/peerj.1165. The predecessor whose parameters this
# removes.

.METABD_EPS <- 1e-12
.METABD_BASES <- strsplit("ACGT", "")[[1L]]

#' .metabd_mat
#'
#' A step of the metabd_native implementation. Called by \code{bin_contigs}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @return One of two values, depending on the branch taken.
#' @export
.metabd_mat <- function(X) {
  if (is.matrix(X)) {
    out <- vector("list", nrow(X))
    for (i in seq_len(nrow(X))) out[[i]] <- as.numeric(X[i, ])
    out
  } else {
    lapply(X, function(r) as.numeric(unlist(r)))
  }
}
#' .metabd_vec
#'
#' A step of the metabd_native implementation. Called by \code{abundance_correlation}, \code{bin_contigs}, \code{composite_distance}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Passed to \code{unlist}.
#' @return A vector, from \code{as.numeric}.
#' @export
.metabd_vec <- function(v) as.numeric(unlist(v))

#' tetranucleotide_frequency
#'
#' A step of the metabd_native implementation. Called by \code{morie_metabd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param seq Coerced to character by the body, with \code{as.character}.
#' @param kk Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4L}.
#' @param canonical A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{frequency}, \code{vector}, \code{kmers}, \code{n_kmers}, \code{canonical}.
#' @export
tetranucleotide_frequency <- function(seq, kk = 4L, canonical = TRUE) {
  s <- toupper(as.character(seq))
  K <- as.integer(kk)
  if (K < 1L) stop("metabd: k must be at least 1")
  if (nchar(s) < K) stop("metabd: the sequence is shorter than k")
  comp <- c(A = "T", C = "G", G = "C", T = "A")
  counts <- list(); tot <- 0L
  chars <- strsplit(s, "")[[1L]]
  for (i in seq_len(nchar(s) - K + 1L)) {
    m <- paste0(chars[i:(i + K - 1L)], collapse = "")
    if (any(!(strsplit(m, "")[[1L]] %in% names(comp)))) next
    if (isTRUE(canonical)) {
      rc_chars <- vapply(rev(strsplit(m, "")[[1L]]), function(c) comp[[c]], character(1))
      rc <- paste0(rc_chars, collapse = "")
      m <- if (m < rc) m else rc
    }
    counts[[m]] <- if (is.null(counts[[m]])) 1L else counts[[m]] + 1L
    tot <- tot + 1L
  }
  if (tot == 0L) stop("metabd: no valid k-mers in the sequence")
  keys <- sort(names(counts))
  freq <- setNames(vapply(keys, function(k) counts[[k]] / tot, numeric(1)), keys)
  list(frequency = as.list(freq),
       vector = vapply(keys, function(k) counts[[k]] / tot, numeric(1)),
       kmers = keys, n_kmers = as.integer(tot),
       canonical = isTRUE(canonical))
}

#' abundance_correlation
#'
#' A step of the metabd_native implementation. Called by \code{composite_distance}, \code{morie_metabd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param cov_a Passed to \code{.metabd_vec}.
#' @param cov_b Passed to \code{.metabd_vec}.
#' @return A list with \code{correlation}, \code{n_samples}.
#' @export
abundance_correlation <- function(cov_a, cov_b) {
  a <- .metabd_vec(cov_a); b <- .metabd_vec(cov_b)
  if (length(a) != length(b))
    stop("metabd: the coverage vectors differ in length")
  if (length(a) < 2L)
    stop("metabd: abundance covariance needs at least 2 samples; with one sample only composition is informative")
  ma <- sum(a) / length(a); mb <- sum(b) / length(b)
  num <- sum((a - ma) * (b - mb))
  den <- sqrt(sum((a - ma)^2) * sum((b - mb)^2))
  list(correlation = if (den > .METABD_EPS) num / den else 0.0,
       n_samples = length(a))
}

#' length_weight
#'
#' A step of the metabd_native implementation. Called by \code{composite_distance}, \code{morie_metabd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param length Coerced to numeric by the body, with \code{as.numeric}.
#' @param l_min Numeric; combined arithmetically in the body. Defaults to \code{2500}.
#' @param l_ref Numeric; combined arithmetically in the body. Defaults to \code{1e+05}.
#' @return A list with \code{weight}, \code{length}, \code{below_minimum}.
#' @export
length_weight <- function(length, l_min = 2500.0, l_ref = 100000.0) {
  L <- as.numeric(length)
  if (L <= 0) stop("metabd: the contig length must be positive")
  if (L < l_min)
    return(list(weight = 0.0, length = L, below_minimum = TRUE,
                note = "too short for a usable composition estimate"))
  w <- log(L / l_min) / log(l_ref / l_min)
  list(weight = min(max(w, 0), 1), length = L, below_minimum = FALSE)
}

#' composite_distance
#'
#' A step of the metabd_native implementation. Called by \code{bin_contigs}, \code{morie_metabd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tnf_a Passed to \code{.metabd_vec}.
#' @param tnf_b Passed to \code{.metabd_vec}.
#' @param cov_a Optional; may be \code{NULL}. Passed to \code{.metabd_vec}.
#' @param cov_b Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param len_a Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param len_b Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param w_abundance Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.5}.
#' @return A list with \code{distance}, \code{composition}, \code{abundance}, \code{abundance_usable}, \code{confidence}, \code{effective_weight}, \code{note}.
#' @export
composite_distance <- function(tnf_a, tnf_b, cov_a = NULL, cov_b = NULL,
                               len_a = NULL, len_b = NULL,
                               w_abundance = 0.5) {
  a <- .metabd_vec(tnf_a); b <- .metabd_vec(tnf_b)
  if (length(a) != length(b))
    stop("metabd: the composition vectors differ in length")
  d_tnf <- sqrt(sum((a - b)^2))
  wa <- as.numeric(w_abundance)
  d_abd <- 0.0; usable <- FALSE
  if (!is.null(cov_a) && !is.null(cov_b) && length(.metabd_vec(cov_a)) >= 2L) {
    r <- abundance_correlation(cov_a, cov_b)$correlation
    d_abd <- 1.0 - r
    usable <- TRUE
  }
  if (!usable) wa <- 0.0
  conf <- 1.0
  if (!is.null(len_a) && !is.null(len_b))
    conf <- min(length_weight(len_a)$weight, length_weight(len_b)$weight)
  d <- (1.0 - wa) * d_tnf + wa * d_abd
  list(distance = d, composition = d_tnf,
       abundance = if (usable) d_abd else NULL,
       abundance_usable = usable, confidence = conf,
       effective_weight = wa,
       note = "with a single sample the abundance term drops out automatically")
}

#' bin_contigs
#'
#' A step of the metabd_native implementation. Called by \code{morie_metabd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tnfs Passed to \code{.metabd_mat}.
#' @param coverages Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param lengths Optional; may be \code{NULL}. Passed to \code{.metabd_vec}.
#' @param threshold Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.15}.
#' @param min_bin_size Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{2e+05}.
#' @return A list with \code{estimate}, \code{bins}, \code{unbinned}, \code{n_bins}, \code{n_unbinned}, \code{method}, \code{note}.
#' @export
bin_contigs <- function(tnfs, coverages = NULL, lengths = NULL,
                        threshold = 0.15, min_bin_size = 200000.0) {
  T <- .metabd_mat(tnfs)
  n <- length(T)
  L <- if (is.null(lengths)) rep(1e5, n) else .metabd_vec(lengths)
  bins <- list(); assigned <- rep(FALSE, n)
  order <- order(-L)
  for (i in order) {
    if (assigned[i]) next
    cur <- c(i); assigned[i] <- TRUE
    for (j in order) {
      if (assigned[j]) next
      d <- composite_distance(
        T[[i]], T[[j]],
        if (is.null(coverages)) NULL else coverages[[i]],
        if (is.null(coverages)) NULL else coverages[[j]],
        L[i], L[j])$distance
      if (d < as.numeric(threshold)) {
        cur <- c(cur, j); assigned[j] <- TRUE
      }
    }
    bins[[length(bins) + 1L]] <- cur
  }
  bin_sizes <- vapply(bins, function(b) sum(L[b]), numeric(1))
  big_idx <- which(bin_sizes >= as.numeric(min_bin_size))
  small_idx <- which(bin_sizes < as.numeric(min_bin_size))
  big <- bins[big_idx]
  small <- unlist(bins[small_idx])
  list(estimate = big, bins = big, unbinned = sort(small),
       n_bins = length(big), n_unbinned = length(small),
       method = "adaptive composite binning; Kang et al. (2019)",
       note = "sub-threshold groups are UNBINNED, not reported as draft genomes")
}

#' purity_completeness
#'
#' A step of the metabd_native implementation. Called by \code{morie_metabd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param bins See Usage.
#' @param truth Passed to \code{unlist}.
#' @return A list with \code{per_bin}, \code{mean_purity}, \code{mean_completeness}, \code{note}.
#' @export
purity_completeness <- function(bins, truth) {
  t <- as.list(unlist(truth))
  per_bin <- list()
  for (b in bins) {
    labs <- t[b]
    if (length(labs) == 0L) next
    counts <- table(unlist(labs))
    dom <- names(which.max(counts))
    purity <- counts[[dom]] / length(labs)
    total <- sum(vapply(t, function(x) identical(x, dom), logical(1)))
    per_bin[[length(per_bin) + 1L]] <- list(
      dominant = dom, purity = purity,
      completeness = if (total > 0) counts[[dom]] / total else 0.0,
      size = length(labs))
  }
  mp <- if (length(per_bin) > 0L) mean(vapply(per_bin, `[[`, numeric(1), "purity")) else 0
  mc <- if (length(per_bin) > 0L) mean(vapply(per_bin, `[[`, numeric(1), "completeness")) else 0
  list(per_bin = per_bin, mean_purity = mp, mean_completeness = mc,
       note = "contamination and fragmentation are different failures")
}

metabat2 <- bin_contigs
metagenome_binning <- bin_contigs

#' .metabd_cheatsheet
#'
#' A step of the metabd_native implementation. Called by \code{morie_metabd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.metabd_cheatsheet <- function() {
  paste("metabd: bin contigs into draft genomes from TWO signals ",
        "-- tetranucleotide composition (available always, noisy ",
        "on short contigs) and abundance covariance ACROSS SAMPLES ",
        "(strong, but undefined with one sample). Earlier tools ",
        "needed manual parameter tuning and degraded on poor ",
        "assemblies; the contribution is an ADAPTIVE algorithm ",
        "that removes the tuning. Confidence must scale with ",
        "contig LENGTH, since discarding short contigs discards ",
        "most of the assembly. Purity and completeness are ",
        "separate failures and are reported separately.", sep = "")
}

#' morie_metabd
#'
#' A step of the metabd_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param op A vector; its length is taken.
#' @param ... Passed through.
#' @return The value of \code{switch}.
#' @export
morie_metabd <- function(op, ...) {
  if (missing(op) || length(op) != 1L)
    stop("metabd: op must be one of tetranucleotide_frequency, abundance_correlation, length_weight, composite_distance, bin_contigs, purity_completeness, cheatsheet")
  op <- as.character(op)
  switch(op,
    "tetranucleotide_frequency" = tetranucleotide_frequency(...),
    "abundance_correlation" = abundance_correlation(...),
    "length_weight" = length_weight(...),
    "composite_distance" = composite_distance(...),
    "bin_contigs" = bin_contigs(...),
    "metabat2" = bin_contigs(...),
    "metagenome_binning" = bin_contigs(...),
    "purity_completeness" = purity_completeness(...),
    "cheatsheet" = list(cheatsheet = .metabd_cheatsheet()),
    stop("metabd: unknown op ", shQuote(op))
  )
}
