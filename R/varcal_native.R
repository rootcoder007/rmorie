# morie.fn -- function file (rootcoder007/morie)
# Variant calling as image classification, DeepVariant style.
#
# **The pipeline.** Find candidate variants in aligned reads with **high
# sensitivity and low specificity**; encode the reference and read data
# around each candidate as a pileup image; hand the image to a
# classifier that emits probabilities for the three diploid genotypes
# (homozygous reference, heterozygous, homozygous alternate).
#
# The asymmetry in step one is deliberate and is the part most often
# got wrong. The candidate stage is *supposed* to over-call: for Ion
# Torrent data the paper's candidates have a positive predictive value
# of **8.1%**, which the classifier then lifts to **99.7%**. Filtering
# hard at the candidate stage would throw away true variants that the
# classifier could have rescued, and sensitivity lost there is lost for
# good -- across datasets the final calls give up a mean of only
# **2.3%** of candidate sensitivity.
#
# **What the pileup image is for.** Presenting every read at a locus in
# one image lets a convolutional network account for the dependence
# *between* reads, rather than treating each as independent evidence the
# way a hand-built likelihood does. The paper's argument is that the
# network approximates the true but unknown interdependent likelihood
# function, and its calibration curve is the evidence.
#
# **What this module implements, and what it does not.** Candidate
# generation, the pileup encoding, the genotype posterior from a
# supplied scorer, and the evaluation arithmetic are all here and
# anchored. The Inception-v2 network itself is **not** reimplemented:
# the preprint gives the architecture by reference and does not specify
# the per-channel image encoding beyond "reference and read bases,
# quality scores, and other read features ... encoded into an RGB pileup
# image". ``encode_pileup`` therefore takes the channel set as an
# argument and names its default in the result rather than pretending a
# specific encoding came from the paper. ``genotype_posterior`` accepts
# any scorer, so a trained model can be dropped in.
#
# References
# ----------
# Poplin, R., Newburger, D., Dijamco, J., Nguyen, N., Loy, D., Gross,
# S. S., McLean, C. Y. & DePristo, M. A. (2016) "Creating a universal
# SNP and small indel variant caller with deep neural networks",
# bioRxiv 092890, doi:10.1101/092890 (published as Poplin et al. (2018)
# *Nature Biotechnology* 36(10), 983-987,
# doi:10.1038/nbt.4235). The high-sensitivity/low-specificity candidate
# stage, the pileup image of reference and read data around each
# candidate, the Inception-v2 classifier emitting the three diploid
# genotype probabilities, the calibration argument for why an image of
# all reads captures inter-read dependence, and the printed accounting:
# candidate PPV 8.1% raised to 99.7% on Ion Torrent, a mean loss of 2.3%
# of candidate sensitivity, and the SOLiD figures of 13.9% PPV at 96.2%
# sensitivity.
#
# Li, H. (2011) "A statistical framework for SNP calling, mutation
# discovery, association mapping and population genetical parameter
# estimation from sequencing data", *Bioinformatics* 27(21), 2987-2993,
# doi:10.1093/bioinformatics/btr509, for the conventional genotype
# likelihood this replaces.

varcal_GENOTYPES <- c("hom_ref", "het", "hom_alt")
varcal_CHANNEL_SETS <- c("base_quality_strand")

# Private helpers
#' Private helpers
#'
#' A step of the varcal_native implementation. Called by \code{varcal_encode_pileup}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param b One of \code{"A"}, \code{"C"}, \code{"G"}, \code{"T"}.
#' @return A numeric value.
#' @export
.varcal_base_code <- function(b) {
  b <- toupper(as.character(b))
  if (b == "A") return(0.25)
  if (b == "C") return(0.5)
  if (b == "G") return(0.75)
  if (b == "T") return(1.0)
  return(0.0)
}

#' .varcal_phred
#'
#' A step of the varcal_native implementation. Called by \code{varcal_genotype_posterior}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
.varcal_phred <- function(p) {
  p <- max(min(as.numeric(p), 1.0), 1e-12)
  return(-10.0 * log10(p))
}

#' .varcal_chars
#'
#' A step of the varcal_native implementation. Called by \code{.varcal_norm_reads}, \code{morie_varcal}, \code{varcal_encode_pileup} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken.
#' @return One of two values, depending on the branch taken.
#' @export
.varcal_chars <- function(x) {
  if (is.character(x) && length(x) == 1L) strsplit(x, "")[[1]] else x
}

#' .varcal_norm_reads
#'
#' A step of the varcal_native implementation. Called by \code{morie_varcal}, \code{varcal_encode_pileup}, \code{varcal_find_candidates} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param reads Iterated over elementwise, with \code{lapply}.
#' @return The value of \code{lapply}.
#' @export
.varcal_norm_reads <- function(reads) {
  lapply(reads, function(r) { r$seq <- .varcal_chars(r$seq); r })
}

#' varcal_pileup_column
#'
#' A step of the varcal_native implementation. Called by \code{varcal_find_candidates}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param reads Passed to \code{.varcal_norm_reads}.
#' @param position Numeric; combined arithmetically in the body.
#' @param reference A vector; its length is taken and its elements indexed.
#' @return A list with \code{observations}, \code{reference}, \code{depth}.
#' @export
varcal_pileup_column <- function(reads, position, reference) {
  reads <- .varcal_norm_reads(reads)
  reference <- .varcal_chars(reference)
  obs <- list()
  for (r in reads) {
    start <- as.integer(r$pos)
    seq <- r$seq
    if (start <= position && position < start + length(seq)) {
      i <- position - start
      i_r <- i + 1
      bq_val <- if ("bq" %in% names(r)) as.integer(r$bq[i_r]) else 30L
      mq_val <- as.integer(if ("mq" %in% names(r)) r$mq else 60L)
      rev_val <- as.logical(if ("reverse" %in% names(r)) r$reverse else FALSE)
      obs[[length(obs) + 1]] <- list(
        base = seq[i_r],
        bq = bq_val,
        mq = mq_val,
        reverse = rev_val
      )
    }
  }
  ref <- if (position < length(reference)) reference[position + 1] else "N"
  list(observations = obs, reference = ref, depth = length(obs))
}

#' varcal_find_candidates
#'
#' A step of the varcal_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param reads Passed to \code{.varcal_norm_reads}.
#' @param reference A vector; its length is taken.
#' @param min_alt_count The body requires: varcal: min_alt_count must be at least 1. Defaults to \code{2}.
#' @param min_alt_fraction The body requires: varcal: min_alt_fraction must lie in [0, 1]. Defaults to \code{0.05}.
#' @param min_bq Passed to \code{>=}. Defaults to \code{10}.
#' @return The value of \code{out}, as built in the body.
#' @export
varcal_find_candidates <- function(reads, reference, min_alt_count = 2,
                                   min_alt_fraction = 0.05, min_bq = 10) {
  reads <- .varcal_norm_reads(reads)
  reference <- .varcal_chars(reference)
  if (min_alt_fraction < 0.0 || min_alt_fraction > 1.0) {
    stop("varcal: min_alt_fraction must lie in [0, 1]")
  }
  if (min_alt_count < 1) {
    stop("varcal: min_alt_count must be at least 1")
  }
  out <- list()
  for (pos in seq_along(reference)) {
    pos_py <- pos - 1
    col <- varcal_pileup_column(reads, pos_py, reference)
    kept <- list()
    for (o in col$observations) {
      if (o$bq >= min_bq) {
        kept[[length(kept) + 1]] <- o
      }
    }
    if (length(kept) == 0) next
    counts <- list()
    for (o in kept) {
      b <- o$base
      if (b %in% names(counts)) {
        counts[[b]] <- counts[[b]] + 1
      } else {
        counts[[b]] <- 1
      }
    }
    ref <- col$reference
    sorted_bases <- sort(names(counts))
    for (base in sorted_bases) {
      if (base == ref) next
      n <- counts[[base]]
      frac <- n / as.numeric(length(kept))
      if (n >= min_alt_count && frac >= min_alt_fraction) {
        out[[length(out) + 1]] <- list(
          position = pos_py,
          reference = ref,
          alternate = base,
          alt_count = n,
          depth = length(kept),
          alt_fraction = frac
        )
      }
    }
  }
  out
}

#' varcal_encode_pileup
#'
#' A step of the varcal_native implementation. Called by \code{morie_varcal}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param reads Passed to \code{.varcal_norm_reads}.
#' @param reference A vector; its length is taken and its elements indexed.
#' @param candidate A list; the body reads \code{$position} from it.
#' @param width Numeric; combined arithmetically in the body. Defaults to \code{21}.
#' @param height Passed to \code{>=}. Defaults to \code{100}.
#' @param channels Coerced to character by the body, with \code{as.character}. Defaults to \code{"base_quality_strand"}.
#' @return A list with \code{reference_row}, \code{read_rows}, \code{n_reads}, \code{width}, \code{centre}, \code{channels}, \code{channel_set}, \code{note}.
#' @export
varcal_encode_pileup <- function(reads, reference, candidate, width = 21,
                                 height = 100,
                                 channels = "base_quality_strand") {
  reads <- .varcal_norm_reads(reads)
  reference <- .varcal_chars(reference)
  if (!(channels %in% varcal_CHANNEL_SETS)) {
    stop(sprintf("varcal: channels must be one of %s, got %s",
                 paste(varcal_CHANNEL_SETS, collapse = ", "),
                 as.character(channels)))
  }
  if (width %% 2 == 0) {
    stop("varcal: width must be odd so the candidate sits in the middle column")
  }
  half <- width %/% 2
  centre <- as.integer(candidate$position)
  lo <- centre - half
  hi <- centre + half
  rows <- list()
  for (r in reads) {
    start <- as.integer(r$pos)
    stop_p <- start + length(r$seq)
    if (stop_p <= lo || start > hi) next
    row <- list()
    for (p in lo:hi) {
      if (start <= p && p < stop_p && p >= 0 && p < length(reference)) {
        i <- p - start
        i_r <- i + 1
        b <- r$seq[i_r]
        bq_val <- if ("bq" %in% names(r)) as.numeric(r$bq[i_r]) else 30
        rev_val <- if ("reverse" %in% names(r)) r$reverse else FALSE
        row[[length(row) + 1]] <- c(
          .varcal_base_code(b),
          min(bq_val / 60.0, 1.0),
          if (isTRUE(rev_val)) 0.0 else 1.0,
          if (b == reference[p + 1]) 1.0 else 0.0
        )
      } else {
        row[[length(row) + 1]] <- c(0.0, 0.0, 0.0, 0.0)
      }
    }
    rows[[length(rows) + 1]] <- row
    if (length(rows) >= height) break
  }
  ref_row <- list()
  for (p in lo:hi) {
    if (p >= 0 && p < length(reference)) {
      ref_row[[length(ref_row) + 1]] <- c(
        .varcal_base_code(reference[p + 1]),
        1.0, 1.0, 1.0
      )
    } else {
      ref_row[[length(ref_row) + 1]] <- c(0.0, 1.0, 1.0, 1.0)
    }
  }
  list(
    reference_row = ref_row,
    read_rows = rows,
    n_reads = length(rows),
    width = width,
    centre = centre,
    channels = c("base", "base_quality", "strand", "matches_reference"),
    channel_set = channels,
    note = "the preprint specifies an RGB pileup image of bases, qualities and read features but not the per-channel mapping; this set is named here rather than attributed to the paper"
  )
}

#' varcal_genotype_posterior
#'
#' A step of the varcal_native implementation. Called by \code{morie_varcal}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param image A list; the body reads \code{$read_rows}, \code{$width} from it.
#' @param scorer The body requires: varcal: the scorer must return three non-negative scores.
#' @param prior Optional; may be \code{NULL}. A vector; its length is taken.
#' @return A list with \code{posterior}, \code{call}, \code{quality}, \code{scores}, \code{source}.
#' @export
varcal_genotype_posterior <- function(image, scorer = NULL, prior = NULL) {
  if (is.null(prior)) {
    prior <- c(0.9985, 0.001, 0.0005)
  }
  if (length(prior) != 3 || abs(sum(prior) - 1.0) > 1e-9) {
    stop("varcal: the prior must be three probabilities summing to 1")
  }
  if (is.null(scorer)) {
    alt <- 0.0
    tot <- 0.0
    mid <- image$width %/% 2
    for (row in image$read_rows) {
      cell <- row[[mid + 1]]
      if (cell[1] > 0.0) {
        tot <- tot + 1.0
        if (cell[4] <= 0.5) {
          alt <- alt + 1.0
        }
      }
    }
    f <- if (tot > 0.0) alt / tot else 0.0
    scores <- c(
      max(1.0 - 2.0 * f, 0.0),
      1.0 - abs(2.0 * f - 1.0),
      max(2.0 * f - 1.0, 0.0)
    )
    source_str <- "pileup-fraction fallback, NOT a trained network"
  } else {
    scores <- as.numeric(scorer(image))
    if (length(scores) != 3 || any(scores < 0.0)) {
      stop("varcal: the scorer must return three non-negative scores")
    }
    source_str <- "supplied scorer"
  }
  post <- scores * prior
  tot <- sum(post)
  if (tot <= 0.0) {
    post <- as.numeric(prior)
    tot <- 1.0
  } else {
    post <- post / tot
  }
  k <- which.max(post)
  list(
    posterior = setNames(as.list(post), varcal_GENOTYPES),
    call = varcal_GENOTYPES[k],
    quality = .varcal_phred(1.0 - post[k]),
    scores = scores,
    source = source_str
  )
}

#' morie_varcal
#'
#' A step of the varcal_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param reads Passed to \code{.varcal_norm_reads}.
#' @param reference Passed to \code{.varcal_chars}.
#' @param scorer Passed to \code{varcal_genotype_posterior}.
#' @param min_quality Passed to \code{>=}. Defaults to \code{10}.
#' @param ... Passed through.
#' @return A list with \code{estimate}, \code{candidates}, \code{n_candidates}, \code{calls}, \code{n_called}, \code{method}.
#' @export
morie_varcal <- function(reads, reference, scorer = NULL, min_quality = 10.0,
                         ...) {
  reads <- .varcal_norm_reads(reads)
  reference <- .varcal_chars(reference)
  kw <- list(...)
  cands <- do.call(varcal_find_candidates,
                   c(list(reads = reads, reference = reference), kw))
  calls <- list()
  for (c in cands) {
    img <- varcal_encode_pileup(reads, reference, c)
    g <- varcal_genotype_posterior(img, scorer)
    call_item <- c(c, g)
    call_item$passes <- (g$call != "hom_ref" && g$quality >= min_quality)
    calls[[length(calls) + 1]] <- call_item
  }
  n_called <- 0
  for (cc in calls) {
    if (isTRUE(cc$passes)) n_called <- n_called + 1
  }
  list(
    estimate = n_called,
    candidates = cands,
    n_candidates = length(cands),
    calls = calls,
    n_called = n_called,
    method = "candidate generation, pileup encoding and genotype classification; Poplin et al. (2016)"
  )
}

#' varcal_evaluate
#'
#' A step of the varcal_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param called See Usage.
#' @param truth See Usage.
#' @param candidates Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return The value of \code{out}, as built in the body.
#' @export
varcal_evaluate <- function(called, truth, candidates = NULL) {
  tset <- character(0)
  for (t in truth) {
    key <- paste(as.character(t$position), as.character(t$alternate),
                 sep = ",")
    if (!(key %in% tset)) tset <- c(tset, key)
  }
  cset <- character(0)
  for (cc in called) {
    key <- paste(as.character(cc$position), as.character(cc$alternate),
                 sep = ",")
    if (!(key %in% cset)) cset <- c(cset, key)
  }
  tp <- 0
  for (k in cset) {
    if (k %in% tset) tp <- tp + 1
  }
  ppv <- if (length(cset) > 0) tp / length(cset) else 0.0
  sens <- if (length(tset) > 0) tp / length(tset) else 0.0
  out <- list(
    true_positives = tp,
    called = length(cset),
    truth = length(tset),
    ppv = ppv,
    sensitivity = sens
  )
  if (!is.null(candidates)) {
    aset <- character(0)
    for (ac in candidates) {
      key <- paste(as.character(ac$position), as.character(ac$alternate),
                   sep = ",")
      if (!(key %in% aset)) aset <- c(aset, key)
    }
    atp <- 0
    for (k in aset) {
      if (k %in% tset) atp <- atp + 1
    }
    out$candidate_ppv <- if (length(aset) > 0) atp / length(aset) else 0.0
    out$candidate_sensitivity <- if (length(tset) > 0) atp / length(tset) else 0.0
    out$ppv_gain <- ppv - out$candidate_ppv
    out$sensitivity_loss <- out$candidate_sensitivity - sens
  }
  out
}

#' varcal_cheatsheet
#'
#' A step of the varcal_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
varcal_cheatsheet <- function() {
  "varcal: candidates are generated with HIGH sensitivity and low specificity on purpose -- 8.1% PPV on Ion Torrent, which the classifier lifts to 99.7% while giving up a mean 2.3% of candidate sensitivity. The pileup image puts every read at the locus in one picture so the network can use the dependence between reads. The Inception-v2 network itself is not reimplemented here; genotype_posterior takes any scorer, and the default is a labelled fallback, not a trained model."
}

# Compact alias per ledger/NAMING.md
deep_variant_call <- morie_varcal
