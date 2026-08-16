# morie.fn -- function file (rootcoder007/morie)
# The Ensembl Variant Effect Predictor: consequences of a variant.
#
# McLaren, W., Gil, L., Hunt, S. E., Riat, H. S., Ritchie, G. R. S.,
# Thormann, A., Flicek, P., & Cunningham, F. (2016) "The Ensembl
# Variant Effect Predictor", Genome Biology 17:122.
# doi:10.1186/s13059-016-0974-4
#
# The VEP answers one question per (variant, transcript) pair: what
# does this allele do to this transcript? For each overlap the API
# builds an object per allele and evaluates consequence types using a
# set of predicate functions, so one allele can carry several terms at
# once.
#
# The terms come from a standardized Sequence Ontology set with stable
# identifiers. The severity ordering used here is Ensembl's published
# consequence table (reference [67]): 41 terms, rank 1
# (transcript_ablation) to rank 41 (sequence_variant), each with a
# HIGH/MODERATE/LOW/MODIFIER impact. It is reproduced in
# CONSEQUENCE_RANK with the SO accessions.
#
# Picking one line per variant (Table 7): --pick gives priority to the
# canonical transcript, then protein coding transcripts, then more
# severe consequence types -- an ordered list, not a tie-break on
# severity. --per_gene is the same rule within each gene.
#
# HGVS c. and p. notations are produced from the transcript
# coordinates, with an indel in repetitive sequence shifted to its
# most 3' position.
#
# Coordinates are VCF-style: pos is 1-based and ref/alt share a leading
# anchor base for indels.

# Ensembl's published consequence table: rank, SO term, impact, SO
# accession. The rank IS the severity order.
.vepan_TERMS <- c(
  "transcript_ablation", "splice_acceptor_variant", "splice_donor_variant",
  "stop_gained", "frameshift_variant", "stop_lost", "start_lost",
  "transcript_amplification", "feature_elongation", "feature_truncation",
  "inframe_insertion", "inframe_deletion", "missense_variant",
  "protein_altering_variant", "splice_donor_5th_base_variant",
  "splice_region_variant", "splice_donor_region_variant",
  "splice_polypyrimidine_tract_variant", "incomplete_terminal_codon_variant",
  "start_retained_variant", "stop_retained_variant", "synonymous_variant",
  "coding_sequence_variant", "mature_miRNA_variant", "5_prime_UTR_variant",
  "3_prime_UTR_variant", "non_coding_transcript_exon_variant",
  "intron_variant", "NMD_transcript_variant",
  "non_coding_transcript_variant", "coding_transcript_variant",
  "upstream_gene_variant", "downstream_gene_variant", "TFBS_ablation",
  "TFBS_amplification", "TF_binding_site_variant",
  "regulatory_region_ablation", "regulatory_region_amplification",
  "regulatory_region_variant", "intergenic_variant", "sequence_variant")

.vepan_IMPACTS <- c(
  "HIGH", "HIGH", "HIGH", "HIGH", "HIGH", "HIGH", "HIGH", "HIGH", "HIGH",
  "HIGH", "MODERATE", "MODERATE", "MODERATE", "MODERATE", "LOW", "LOW",
  "LOW", "LOW", "LOW", "LOW", "LOW", "LOW", "MODIFIER", "MODIFIER",
  "MODIFIER", "MODIFIER", "MODIFIER", "MODIFIER", "MODIFIER", "MODIFIER",
  "MODIFIER", "MODIFIER", "MODIFIER", "MODERATE", "MODIFIER", "MODIFIER",
  "MODIFIER", "MODIFIER", "MODIFIER", "MODIFIER", "MODIFIER")

.vepan_SO <- c(
  "SO:0001893", "SO:0001574", "SO:0001575", "SO:0001587", "SO:0001589",
  "SO:0001578", "SO:0002012", "SO:0001889", "SO:0001907", "SO:0001906",
  "SO:0001821", "SO:0001822", "SO:0001583", "SO:0001818", "SO:0001787",
  "SO:0001630", "SO:0002170", "SO:0002169", "SO:0001626", "SO:0002019",
  "SO:0001567", "SO:0001819", "SO:0001580", "SO:0001620", "SO:0001623",
  "SO:0001624", "SO:0001792", "SO:0001627", "SO:0001621", "SO:0001619",
  "SO:0001968", "SO:0001631", "SO:0001632", "SO:0001895", "SO:0001892",
  "SO:0001782", "SO:0001894", "SO:0001891", "SO:0001566", "SO:0001628",
  "SO:0001060")

morie_vepan_CONSEQUENCE_RANK <- stats::setNames(seq_along(.vepan_TERMS),
                                                .vepan_TERMS)
morie_vepan_CONSEQUENCE_IMPACT <- stats::setNames(.vepan_IMPACTS,
                                                  .vepan_TERMS)
morie_vepan_CONSEQUENCE_SO <- stats::setNames(.vepan_SO, .vepan_TERMS)

# Table 7's --pick order, applied in sequence. Canonical first.
morie_vepan_PICK_ORDER <- c("canonical", "protein_coding",
                            "consequence_rank", "transcript_id")

.vepan_COMPLEMENT <- c(A="T", C="G", G="C", T="A", N="N")
.vepan_AA3 <- c(A="Ala", R="Arg", N="Asn", D="Asp", C="Cys", Q="Gln",
                E="Glu", G="Gly", H="His", I="Ile", L="Leu", K="Lys",
                M="Met", F="Phe", P="Pro", S="Ser", T="Thr", W="Trp",
                Y="Tyr", V="Val", "*"="Ter", X="Xaa")

#' Python range(lo, hi+1); empty when hi < lo
#'
#' A step of the vepan_native implementation. Called by \code{.vepan_apply}, \code{.vepan_hgvs_c}, \code{.vepan_splice_terms} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param lo See Usage.
#' @param hi See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.vepan_seqrange <- function(lo, hi) {
  # Python range(lo, hi+1); empty when hi < lo.
  if (hi >= lo) lo:hi else integer(0)
}

#' 0-based index of ch in s, or -1 (like Python str.find)
#'
#' A step of the vepan_native implementation. Called by \code{.vepan_coding_terms}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s See Usage.
#' @param ch See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.vepan_find <- function(s, ch) {
  # 0-based index of ch in s, or -1 (like Python str.find).
  p <- regexpr(ch, s, fixed=TRUE)
  if (p < 0) -1L else as.integer(p - 1L)
}

#' 0-based character access
#'
#' A step of the vepan_native implementation. Called by \code{.vepan_cds_frame}, \code{.vepan_coding_terms}, \code{.vepan_hgvs_c}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s Character; passed to \code{substr}.
#' @param k Numeric; combined arithmetically in the body.
#' @return The value of \code{substr}.
#' @export
.vepan_char <- function(s, k) {
  # 0-based character access.
  substr(s, k + 1L, k + 1L)
}

#' Severity rank of an SO term; 1 is worst. Unknown terms sort last
#'
#' A step of the vepan_native implementation. Called by \code{.vepan_pick_key}, \code{morie_vepan_annotate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param term See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
morie_vepan_consequence_rank <- function(term) {
  # Severity rank of an SO term; 1 is worst. Unknown terms sort last.
  r <- morie_vepan_CONSEQUENCE_RANK[[term]]
  if (is.null(r)) length(.vepan_TERMS) + 1L else r
}

#' HIGH/MODERATE/LOW/MODIFIER for an SO term
#'
#' A step of the vepan_native implementation. Called by \code{morie_vepan_annotate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param term See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
morie_vepan_consequence_impact <- function(term) {
  # HIGH/MODERATE/LOW/MODIFIER for an SO term.
  i <- morie_vepan_CONSEQUENCE_IMPACT[[term]]
  if (is.null(i)) "MODIFIER" else i
}

#' The lowest-ranked (worst) term
#'
#' A step of the vepan_native implementation. Called by \code{morie_vepan_annotate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param terms See Usage.
#' @return The value of \code{[}.
#' @export
morie_vepan_most_severe_consequence <- function(terms) {
  # The lowest-ranked (worst) term.
  ts <- as.character(terms)
  if (length(ts) == 0L) {
    stop("vepan: no consequence terms given")
  }
  ranks <- vapply(ts, morie_vepan_consequence_rank, integer(1))
  ord <- order(ranks, ts)
  ts[ord[1L]]
}

#' .vepan_revcomp
#'
#' A step of the vepan_native implementation. Called by \code{.vepan_apply}, \code{.vepan_hgvs_c}, \code{morie_vepan_transcript_sequence}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s Character; passed to \code{toupper}.
#' @return A character value.
#' @export
.vepan_revcomp <- function(s) {
  chars <- rev(strsplit(toupper(s), "")[[1L]])
  comp <- ifelse(chars %in% names(.vepan_COMPLEMENT),
                 .vepan_COMPLEMENT[chars], "N")
  paste(comp, collapse="")
}

# ------------------------------------------------------ transcript model

#' .vepan_exons
#'
#' A step of the vepan_native implementation. Called by \code{.vepan_transcript}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ex See Usage.
#' @return The value of \code{[}.
#' @export
.vepan_exons <- function(ex) {
  if (is.matrix(ex)) {
    m <- ex
  } else {
    m <- do.call(rbind, lapply(ex, function(p) as.integer(unlist(p))))
  }
  storage.mode(m) <- "integer"
  m[order(m[, 1L]), , drop=FALSE]
}

#' .vepan_transcript
#'
#' A step of the vepan_native implementation. Called by \code{morie_vepan_transcript_sequence}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tr A list; the body reads \code{$biotype}, \code{$canonical}, \code{$cds_end}, \code{$cds_start}, \code{$chrom}, \code{$exons}, \code{$gene}, \code{$id}, \code{$strand} from it.
#' @return A list with \code{id}, \code{gene}, \code{chrom}, \code{strand}, \code{exons}, \code{cds_start}, \code{cds_end}, \code{biotype}, \code{canonical}, \code{start}, \code{end}.
#' @export
.vepan_transcript <- function(tr) {
  if (is.null(tr[["exons"]])) {
    stop("vepan: a transcript needs exons as (start, end)")
  }
  exons <- .vepan_exons(tr[["exons"]])
  if (nrow(exons) == 0L) {
    stop("vepan: a transcript needs at least one exon")
  }
  for (r in seq_len(nrow(exons))) {
    if (exons[r, 1L] > exons[r, 2L] || exons[r, 1L] < 1L) {
      stop("vepan: exon coordinates must be 1-based and ascending")
    }
  }
  if (nrow(exons) > 1L) {
    for (k in seq_len(nrow(exons) - 1L)) {
      if (exons[k, 2L] >= exons[k + 1L, 1L]) {
        stop("vepan: exons must not overlap")
      }
    }
  }
  strand <- if (is.null(tr[["strand"]])) "+" else tr[["strand"]]
  if (!(strand %in% c("+", "-"))) {
    stop("vepan: strand must be '+' or '-'")
  }
  cs <- tr[["cds_start"]]
  ce <- tr[["cds_end"]]
  biotype <- if (!is.null(tr[["biotype"]])) {
    tr[["biotype"]]
  } else if (!is.null(cs)) {
    "protein_coding"
  } else {
    "lncRNA"
  }
  if (biotype == "protein_coding" && (is.null(cs) || is.null(ce))) {
    stop(paste0("vepan: a protein_coding transcript needs cds_start and ",
                "cds_end"))
  }
  if (!is.null(cs) && !is.null(ce) && as.integer(cs) > as.integer(ce)) {
    stop("vepan: cds_start must not exceed cds_end")
  }
  list(id=if (is.null(tr[["id"]])) "TR" else tr[["id"]],
       gene=if (is.null(tr[["gene"]])) "GENE" else tr[["gene"]],
       chrom=if (is.null(tr[["chrom"]])) "chr1" else tr[["chrom"]],
       strand=strand, exons=exons,
       cds_start=if (is.null(cs)) NULL else as.integer(cs),
       cds_end=if (is.null(ce)) NULL else as.integer(ce),
       biotype=biotype,
       canonical=isTRUE(tr[["canonical"]]),
       start=exons[1L, 1L], end=exons[nrow(exons), 2L])
}

#' The spliced transcript, 5\' to 3\', and the genomic position of each
#'
#' of its bases (returns list(seq, gpos)).
#'
#' @param tr Passed to \code{.vepan_transcript}.
#' @param genome See Usage.
#' @return A list with \code{seq}, \code{gpos}.
#' @export
morie_vepan_transcript_sequence <- function(tr, genome) {
  # The spliced transcript, 5' to 3', and the genomic position of each
  # of its bases (returns list(seq, gpos)).
  t <- .vepan_transcript(tr)
  g <- toupper(as.character(genome))
  seq_parts <- character(0)
  gpos <- integer(0)
  for (r in seq_len(nrow(t$exons))) {
    a <- t$exons[r, 1L]
    b <- t$exons[r, 2L]
    seq_parts <- c(seq_parts, substr(g, a, b))
    gpos <- c(gpos, a:b)
  }
  s <- toupper(paste(seq_parts, collapse=""))
  if (t$strand == "-") {
    s <- .vepan_revcomp(s)
    gpos <- rev(gpos)
  }
  list(seq=s, gpos=gpos)
}

#' CDNA index (0-based) of each coding base, and the coding sequence
#'
#' A step of the vepan_native implementation. Called by \code{morie_vepan_annotate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A list; the body reads \code{$cds_end}, \code{$cds_start} from it.
#' @param genome Passed to \code{morie_vepan_transcript_sequence}.
#' @return A list with \code{cds}, \code{coding}, \code{sg}.
#' @export
.vepan_cds_frame <- function(t, genome) {
  # cDNA index (0-based) of each coding base, and the coding sequence.
  if (is.null(t$cds_start)) {
    return(list(cds=NULL, coding=NULL, sg=NULL))
  }
  sg <- morie_vepan_transcript_sequence(t, genome)
  seq <- sg$seq
  gpos <- sg$gpos
  coding <- which(gpos >= t$cds_start & gpos <= t$cds_end) - 1L  # 0-based
  if (length(coding) == 0L) {
    stop("vepan: the CDS does not overlap any exon")
  }
  cds <- paste(vapply(coding, function(k) .vepan_char(seq, k), character(1)),
               collapse="")
  list(cds=cds, coding=coding, sg=sg)
}

#' .vepan_introns
#'
#' A step of the vepan_native implementation. Called by \code{.vepan_splice_terms}, \code{morie_vepan_annotate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A list; the body reads \code{$exons} from it.
#' @return The value of \code{out}, as built in the body.
#' @export
.vepan_introns <- function(t) {
  ex <- t$exons
  out <- list()
  if (nrow(ex) > 1L) {
    for (k in seq_len(nrow(ex) - 1L)) {
      if (ex[k + 1L, 1L] - ex[k, 2L] > 1L) {
        out <- c(out, list(c(ex[k, 2L] + 1L, ex[k + 1L, 1L] - 1L)))
      }
    }
  }
  out
}

#' .vepan_variant
#'
#' A step of the vepan_native implementation. Called by \code{morie_vepan_annotate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A list; the body reads \code{$alt}, \code{$chrom}, \code{$id}, \code{$pos}, \code{$ref} from it.
#' @return A list with \code{chrom}, \code{pos}, \code{ref}, \code{alt}, \code{id}.
#' @export
.vepan_variant <- function(v) {
  if (is.null(v[["pos"]]) || is.null(v[["ref"]]) || is.null(v[["alt"]])) {
    stop("vepan: a variant needs pos, ref and alt")
  }
  pos <- as.integer(v[["pos"]])
  ref <- toupper(as.character(v[["ref"]]))
  alt <- toupper(as.character(v[["alt"]]))
  if (pos < 1L) {
    stop("vepan: pos is 1-based and must be positive")
  }
  if (nchar(ref) == 0L || nchar(alt) == 0L) {
    stop("vepan: ref and alt must be non-empty")
  }
  bases <- strsplit(paste0(ref, alt), "")[[1L]]
  if (any(!(bases %in% c("A", "C", "G", "T", "N")))) {
    stop("vepan: ref and alt must be ACGTN")
  }
  if (nchar(ref) > 1L && nchar(alt) > 1L) {
    stop("vepan: only SNVs and anchored indels are handled")
  }
  list(chrom=if (is.null(v[["chrom"]])) "chr1" else v[["chrom"]],
       pos=pos, ref=ref, alt=alt, id=v[["id"]])
}

#' .vepan_kind
#'
#' A step of the vepan_native implementation. Called by \code{.vepan_affected}, \code{.vepan_apply}, \code{.vepan_coding_terms} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A list; the body reads \code{$alt}, \code{$ref} from it.
#' @return One of two values, depending on the branch taken.
#' @export
.vepan_kind <- function(v) {
  if (nchar(v$ref) == 1L && nchar(v$alt) == 1L) {
    return("SNV")
  }
  if (nchar(v$alt) > nchar(v$ref)) "insertion" else "deletion"
}

#' Genomic bases the variant changes (1-based, inclusive). Insertion
#'
#' yields lo > hi (an empty span between pos and pos+1).
#'
#' @param v A list; the body reads \code{$pos}, \code{$ref} from it.
#' @return A vector, from \code{c}.
#' @export
.vepan_affected <- function(v) {
  # Genomic bases the variant changes (1-based, inclusive). Insertion
  # yields lo > hi (an empty span between pos and pos+1).
  k <- .vepan_kind(v)
  if (k == "SNV") {
    return(c(v$pos, v$pos))
  }
  if (k == "insertion") {
    return(c(v$pos + 1L, v$pos))
  }
  c(v$pos + 1L, v$pos + nchar(v$ref) - 1L)
}

# ------------------------------------------------------- the predicates

#' Splice consequences from the distance to each exon boundary
#'
#' A step of the vepan_native implementation. Called by \code{morie_vepan_annotate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A list; the body reads \code{$end}, \code{$exons}, \code{$start}, \code{$strand} from it.
#' @param v See Usage.
#' @param lo Passed to \code{.vepan_seqrange}.
#' @param hi Passed to \code{.vepan_seqrange}.
#' @return The value of \code{unique}.
#' @export
.vepan_splice_terms <- function(t, v, lo, hi) {
  # Splice consequences from the distance to each exon boundary.
  terms <- character(0)
  fwd <- t$strand == "+"
  for (intr in .vepan_introns(t)) {
    ia <- intr[1L]
    ib <- intr[2L]
    if (fwd) {
      d_start <- ia
      a_end <- ib
    } else {
      d_start <- ib
      a_end <- ia
    }
    step <- if (fwd) 1L else -1L
    for (p in .vepan_seqrange(lo, hi)) {
      if (!(min(ia, ib) <= p && p <= max(ia, ib))) {
        next
      }
      d <- (p - d_start) * step + 1L
      a <- (a_end - p) * step + 1L
      if (d <= 2L) {
        terms <- c(terms, "splice_donor_variant")
      } else if (d == 5L) {
        terms <- c(terms, "splice_donor_5th_base_variant")
      }
      if (a <= 2L) {
        terms <- c(terms, "splice_acceptor_variant")
      }
      if (3L <= d && d <= 8L) {
        terms <- c(terms, "splice_region_variant")
      }
      if (3L <= a && a <= 8L) {
        terms <- c(terms, "splice_region_variant")
      }
      if (3L <= a && a <= 17L) {
        terms <- c(terms, "splice_polypyrimidine_tract_variant")
      }
    }
  }
  for (r in seq_len(nrow(t$exons))) {
    a <- t$exons[r, 1L]
    b <- t$exons[r, 2L]
    for (p in .vepan_seqrange(lo, hi)) {
      if (a <= p && p <= b) {
        near <- min(p - a + 1L, b - p + 1L)
        touches <- ((a != t$start && p - a + 1L <= 3L) ||
                    (b != t$end && b - p + 1L <= 3L))
        if (near <= 3L && touches) {
          terms <- c(terms, "splice_region_variant")
        }
      }
    }
  }
  unique(terms)
}

#' The coding sequence after the variant, and where it changed
#'
#' (0-based offset). Returns list(alt_cds, off) or list(NULL, NULL).
#'
#' @param cds A vector; its length is taken.
#' @param v A list; the body reads \code{$alt}, \code{$pos} from it.
#' @param coding Numeric; combined arithmetically in the body.
#' @param gpos A vector; indexed elementwise.
#' @param strand One of \code{"-"}, \code{"+"}.
#' @return A list with \code{alt_cds}, \code{off}.
#' @export
.vepan_apply <- function(cds, v, coding, gpos, strand) {
  # The coding sequence after the variant, and where it changed
  # (0-based offset). Returns list(alt_cds, off) or list(NULL, NULL).
  pos_map <- stats::setNames(coding, as.character(gpos[coding + 1L]))
  aff <- .vepan_affected(v)
  lo <- aff[1L]
  hi <- aff[2L]
  kind <- .vepan_kind(v)
  if (kind == "insertion") {
    anchor <- pos_map[[as.character(v$pos)]]
    if (is.null(anchor)) {
      return(list(alt_cds=NULL, off=NULL))
    }
    ins <- substr(v$alt, 2L, nchar(v$alt))
    if (strand == "-") {
      ins <- .vepan_revcomp(ins)
      cut <- anchor
    } else {
      cut <- anchor + 1L
    }
    alt <- paste0(substr(cds, 1L, cut), ins,
                  substr(cds, cut + 1L, nchar(cds)))
    return(list(alt_cds=alt, off=cut))
  }
  offs <- sort(unlist(lapply(.vepan_seqrange(lo, hi), function(p) {
    val <- pos_map[[as.character(p)]]
    if (is.null(val)) NULL else val
  })))
  if (length(offs) == 0L) {
    return(list(alt_cds=NULL, off=NULL))
  }
  if (kind == "deletion") {
    chars <- strsplit(cds, "")[[1L]]
    keep <- chars[-(offs + 1L)]
    return(list(alt_cds=paste(keep, collapse=""), off=offs[1L]))
  }
  base <- if (strand == "+") v$alt else .vepan_COMPLEMENT[[v$alt]]
  o <- offs[1L]
  list(alt_cds=paste0(substr(cds, 1L, o), base,
                      substr(cds, o + 2L, nchar(cds))), off=o)
}

#' Everything that depends on the protein: the predicate set. Returns
#'
#' list(terms, info).
#'
#' @param t A list; the body reads \code{$strand} from it.
#' @param v Passed to \code{.vepan_apply}.
#' @param cds A vector; its length is taken.
#' @param coding Passed to \code{.vepan_apply}.
#' @param gpos Passed to \code{.vepan_apply}.
#' @return A list with \code{terms}, \code{info}.
#' @export
.vepan_coding_terms <- function(t, v, cds, coding, gpos) {
  # Everything that depends on the protein: the predicate set. Returns
  # list(terms, info).
  terms <- character(0)
  ap <- .vepan_apply(cds, v, coding, gpos, t$strand)
  if (is.null(ap$alt_cds)) {
    return(list(terms=terms, info=list()))
  }
  alt_cds <- ap$alt_cds
  off <- ap$off
  ref_prot <- translate(cds)
  alt_prot <- translate(alt_cds)
  kind <- .vepan_kind(v)
  delta <- nchar(alt_cds) - nchar(cds)
  codon <- off %/% 3L
  info <- list(cds_position=off + 1L, protein_position=codon + 1L,
               ref_codon=substr(cds, codon * 3L + 1L, codon * 3L + 3L),
               ref_aa=if (codon < nchar(ref_prot)) {
                 .vepan_char(ref_prot, codon)
               } else {
                 ""
               },
               alt_aa=if (codon < nchar(alt_prot)) {
                 .vepan_char(alt_prot, codon)
               } else {
                 ""
               },
               cds_length=nchar(cds), alt_cds_length=nchar(alt_cds))
  if (kind != "SNV" && (delta %% 3L) != 0L) {
    terms <- c(terms, "frameshift_variant")
    if (grepl("*", substr(alt_prot, 1L, codon + 1L), fixed=TRUE)) {
      terms <- c(terms, "stop_gained")
    }
    return(list(terms=unique(terms), info=info))
  }
  if (kind == "insertion") {
    terms <- c(terms, "inframe_insertion")
  } else if (kind == "deletion") {
    terms <- c(terms, "inframe_deletion")
  }
  ref_stop <- .vepan_find(ref_prot, "*")
  alt_stop <- .vepan_find(alt_prot, "*")
  if (codon == 0L) {
    if (info$alt_aa == "M" && info$ref_aa == "M") {
      terms <- c(terms, "start_retained_variant")
    } else if (info$ref_aa == "M") {
      terms <- c(terms, "start_lost")
    }
  }
  if (ref_stop >= 0L && codon == ref_stop) {
    if (info$alt_aa == "*") {
      terms <- c(terms, "stop_retained_variant")
    } else {
      terms <- c(terms, "stop_lost")
    }
  }
  expected_stop <- if (ref_stop >= 0L) ref_stop + delta %/% 3L else -1L
  if (alt_stop >= 0L &&
      (expected_stop < 0L || alt_stop < expected_stop) &&
      !("stop_lost" %in% terms)) {
    terms <- c(terms, "stop_gained")
  }
  if (kind == "SNV" && length(terms) == 0L) {
    if (info$ref_aa == info$alt_aa) {
      terms <- c(terms, "synonymous_variant")
    } else {
      terms <- c(terms, "missense_variant")
    }
  }
  if (length(terms) == 0L) {
    terms <- c(terms, "coding_sequence_variant")
  }
  list(terms=unique(terms), info=info)
}

#' C. notation, with an indel shifted to its most 3\' position
#'
#' A step of the vepan_native implementation. Called by \code{morie_vepan_annotate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A list; the body reads \code{$strand} from it.
#' @param v A list; the body reads \code{$alt}, \code{$pos} from it.
#' @param seq A vector; its length is taken.
#' @param gpos A vector; its length is taken.
#' @param cds_first Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @return A character value.
#' @export
.vepan_hgvs_c <- function(t, v, seq, gpos, cds_first) {
  # c. notation, with an indel shifted to its most 3' position.
  idx <- stats::setNames(seq_along(gpos) - 1L, as.character(gpos))
  aff <- .vepan_affected(v)
  lo <- aff[1L]
  hi <- aff[2L]
  anchor <- idx[[as.character(v$pos)]]
  if (is.null(anchor)) {
    return(NULL)
  }
  kind <- .vepan_kind(v)
  c_of <- function(k) {
    if (!is.null(cds_first)) k - cds_first + 1L else k + 1L
  }
  if (kind == "SNV") {
    k <- idx[[as.character(v$pos)]]
    refb <- .vepan_char(seq, k)
    altb <- if (t$strand == "+") v$alt else .vepan_COMPLEMENT[[v$alt]]
    return(sprintf("c.%d%s>%s", c_of(k), refb, altb))
  }
  if (kind == "insertion") {
    ins <- substr(v$alt, 2L, nchar(v$alt))
    if (t$strand == "-") {
      ins <- .vepan_revcomp(ins)
    }
    k <- if (t$strand == "-") anchor else anchor + 1L
    li <- nchar(ins)
    while (k + li <= nchar(seq) && substr(seq, k + 1L, k + li) == ins) {
      k <- k + li
    }
    return(sprintf("c.%d_%dins%s", c_of(k - 1L), c_of(k), ins))
  }
  ks <- sort(unlist(lapply(.vepan_seqrange(lo, hi), function(p) {
    val <- idx[[as.character(p)]]
    if (is.null(val)) NULL else val
  })))
  if (length(ks) == 0L) {
    return(NULL)
  }
  a <- ks[1L]
  b <- ks[length(ks)]
  n <- b - a + 1L
  while (b + n < nchar(seq) &&
         substr(seq, a + 1L, b + 1L) == substr(seq, a + n + 1L, b + n + 1L)) {
    a <- a + n
    b <- b + n
  }
  dele <- substr(seq, a + 1L, b + 1L)
  if (a == b) {
    return(sprintf("c.%ddel%s", c_of(a), dele))
  }
  sprintf("c.%d_%ddel%s", c_of(a), c_of(b), dele)
}

#' .vepan_hgvs_p
#'
#' A step of the vepan_native implementation. Called by \code{morie_vepan_annotate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param info A list; the body reads \code{$alt_aa}, \code{$protein_position}, \code{$ref_aa} from it.
#' @param terms One of \code{"frameshift_variant"}, \code{"start_retained_variant"}, \code{"stop_retained_variant"}, \code{"synonymous_variant"}.
#' @return A character value.
#' @export
.vepan_hgvs_p <- function(info, terms) {
  if (length(info) == 0L || is.null(info$protein_position)) {
    return(NULL)
  }
  ref <- info$ref_aa
  alt <- info$alt_aa
  pos <- info$protein_position
  aa3 <- function(x) if (!is.null(x) && x %in% names(.vepan_AA3)) {
    .vepan_AA3[[x]]
  } else {
    "Xaa"
  }
  if ("frameshift_variant" %in% terms) {
    return(sprintf("p.%s%dfs", aa3(ref), pos))
  }
  if (is.null(ref) || ref == "") {
    return(NULL)
  }
  if (any(c("synonymous_variant", "stop_retained_variant",
            "start_retained_variant") %in% terms)) {
    return(sprintf("p.%s%d=", aa3(ref), pos))
  }
  if (is.null(alt) || alt == "") {
    return(NULL)
  }
  sprintf("p.%s%d%s", aa3(ref), pos, aa3(alt))
}

# ------------------------------------------------------------- annotate

#' morie_vepan_annotate
#'
#' A step of the vepan_native implementation. Called by \code{morie_vepan_vep_annotation}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param variant Passed to \code{.vepan_variant}.
#' @param transcripts See Usage.
#' @param genome See Usage.
#' @param upstream Defaults to \code{5000}.
#' @param downstream Defaults to \code{5000}.
#' @return The value of \code{[}.
#' @export
morie_vepan_annotate <- function(variant, transcripts, genome,
                                 upstream=5000, downstream=5000) {
  # One record per (variant, transcript) overlap, as the VEP emits. A
  # variant beyond every transcript's flank gets a single
  # intergenic_variant record with no transcript attached.
  v <- .vepan_variant(variant)
  trs <- lapply(transcripts, .vepan_transcript)
  if (upstream < 0 || downstream < 0) {
    stop("vepan: flank sizes must be non-negative")
  }
  g <- toupper(as.character(genome))
  aff <- .vepan_affected(v)
  lo <- aff[1L]
  hi <- aff[2L]
  span_lo <- min(lo, v$pos)
  span_hi <- max(hi, v$pos)
  vlabel <- if (!is.null(v$id)) {
    v$id
  } else {
    sprintf("%s:%d%s>%s", v$chrom, v$pos, v$ref, v$alt)
  }
  out <- list()
  for (t in trs) {
    if (t$chrom != v$chrom) {
      next
    }
    if (t$strand == "+") {
      five <- upstream
      three <- downstream
    } else {
      five <- downstream
      three <- upstream
    }
    if (span_hi < t$start - five || span_lo > t$end + three) {
      next
    }
    terms <- character(0)
    if (span_hi < t$start) {
      terms <- c(terms, if (t$strand == "+") "upstream_gene_variant"
                        else "downstream_gene_variant")
    } else if (span_lo > t$end) {
      terms <- c(terms, if (t$strand == "+") "downstream_gene_variant"
                        else "upstream_gene_variant")
    }
    info <- list()
    hgvs_c <- NULL
    hgvs_p <- NULL
    if (length(terms) == 0L) {
      sg <- morie_vepan_transcript_sequence(t, g)
      seq <- sg$seq
      gpos <- sg$gpos
      in_exon <- FALSE
      for (r in seq_len(nrow(t$exons))) {
        for (p in .vepan_seqrange(lo, hi)) {
          if (t$exons[r, 1L] <= p && p <= t$exons[r, 2L]) {
            in_exon <- TRUE
          }
        }
      }
      if (!in_exon && .vepan_kind(v) == "insertion") {
        for (r in seq_len(nrow(t$exons))) {
          if (t$exons[r, 1L] <= v$pos && v$pos <= t$exons[r, 2L]) {
            in_exon <- TRUE
          }
        }
      }
      in_intron <- FALSE
      for (intr in .vepan_introns(t)) {
        for (p in .vepan_seqrange(lo, hi)) {
          if (intr[1L] <= p && p <= intr[2L]) {
            in_intron <- TRUE
          }
        }
      }
      terms <- unique(c(terms,
                        .vepan_splice_terms(t, v, min(lo, v$pos),
                                            max(hi, v$pos))))
      if (in_intron) {
        terms <- c(terms, "intron_variant")
      }
      if (t$biotype != "protein_coding") {
        if (in_exon) {
          terms <- c(terms, "non_coding_transcript_exon_variant")
        }
        terms <- c(terms, "non_coding_transcript_variant")
      } else if (in_exon) {
        cf <- .vepan_cds_frame(t, g)
        cds <- cf$cds
        coding <- cf$coding
        cds_first <- coding[1L]
        all_in_cds <- TRUE
        rng <- .vepan_seqrange(lo, hi)
        if (length(rng) > 0L) {
          all_in_cds <- all(rng >= t$cds_start & rng <= t$cds_end)
        }
        ins_in_cds <- (.vepan_kind(v) == "insertion" &&
                       t$cds_start <= v$pos && v$pos <= t$cds_end)
        if ((length(rng) > 0L && all_in_cds) || ins_in_cds) {
          ct <- .vepan_coding_terms(t, v, cds, coding, gpos)
          terms <- unique(c(terms, ct$terms))
          info <- ct$info
        } else {
          utr5 <- (v$pos < t$cds_start) == (t$strand == "+")
          terms <- c(terms, if (utr5) "5_prime_UTR_variant"
                            else "3_prime_UTR_variant")
        }
        hgvs_c <- .vepan_hgvs_c(t, v, seq, gpos, cds_first)
        hgvs_p <- .vepan_hgvs_p(info, terms)
      }
      if (length(terms) == 0L) {
        terms <- c(terms, "intron_variant")
      }
    }
    terms <- unique(terms)
    ms <- morie_vepan_most_severe_consequence(terms)
    ranks <- vapply(terms, morie_vepan_consequence_rank, integer(1))
    record <- list(
      variant=vlabel, transcript=t$id, gene=t$gene, biotype=t$biotype,
      canonical=t$canonical, strand=t$strand,
      consequences=terms[order(ranks)],
      most_severe=ms,
      impact=morie_vepan_consequence_impact(ms),
      hgvs_c=hgvs_c, hgvs_p=hgvs_p)
    for (nm in names(info)) {
      record[[nm]] <- info[[nm]]
    }
    out <- c(out, list(record))
  }
  if (length(out) == 0L) {
    out <- list(list(
      variant=vlabel, transcript=NULL, gene=NULL, biotype=NULL,
      canonical=FALSE, strand=NULL,
      consequences=c("intergenic_variant"),
      most_severe="intergenic_variant", impact="MODIFIER",
      hgvs_c=NULL, hgvs_p=NULL))
  }
  keys_rank <- vapply(out,
                      function(r) morie_vepan_consequence_rank(r$most_severe),
                      integer(1))
  keys_tr <- vapply(out,
                    function(r) if (is.null(r$transcript)) "" else r$transcript,
                    character(1))
  out[order(keys_rank, keys_tr)]
}

#' Table 7\'s order: canonical, then protein coding, then severity
#'
#' A step of the vepan_native implementation. Called by \code{morie_vepan_pick}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param r A list; the body reads \code{$biotype}, \code{$canonical}, \code{$most_severe}, \code{$transcript} from it.
#' @return The value of \code{list}.
#' @export
.vepan_pick_key <- function(r) {
  # Table 7's order: canonical, then protein coding, then severity.
  list(if (isTRUE(r$canonical)) 0L else 1L,
       if (!is.null(r$biotype) && r$biotype == "protein_coding") 0L else 1L,
       morie_vepan_consequence_rank(r$most_severe),
       if (is.null(r$transcript)) "" else r$transcript)
}

#' Lexicographic comparison of two pick keys
#'
#' A step of the vepan_native implementation. Called by \code{morie_vepan_pick}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param k1 A vector; its length is taken and its elements indexed.
#' @param k2 A vector; indexed elementwise.
#' @return A logical value.
#' @export
.vepan_key_less <- function(k1, k2) {
  # Lexicographic comparison of two pick keys.
  for (i in seq_along(k1)) {
    if (identical(k1[[i]], k2[[i]])) {
      next
    }
    if (is.character(k1[[i]]) || is.character(k2[[i]])) {
      return(as.character(k1[[i]]) < as.character(k2[[i]]))
    }
    return(k1[[i]] < k2[[i]])
  }
  FALSE
}

#' Pick (one record) or --per_gene (one per gene)
#'
#' A step of the vepan_native implementation. Called by \code{morie_vepan_vep_annotation}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param records See Usage.
#' @param per_gene A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return The value of \code{unname}.
#' @export
morie_vepan_pick <- function(records, per_gene=FALSE) {
  # --pick (one record) or --per_gene (one per gene).
  rs <- records
  if (length(rs) == 0L) {
    stop("vepan: nothing to pick from")
  }
  if (!isTRUE(per_gene)) {
    best <- rs[[1L]]
    bkey <- .vepan_pick_key(best)
    for (r in rs[-1L]) {
      k <- .vepan_pick_key(r)
      if (.vepan_key_less(k, bkey)) {
        best <- r
        bkey <- k
      }
    }
    return(list(best))
  }
  best <- list()
  bkeys <- list()
  for (r in rs) {
    key <- if (is.null(r$gene)) "NA" else r$gene
    k <- .vepan_pick_key(r)
    if (is.null(best[[key]]) || .vepan_key_less(k, bkeys[[key]])) {
      best[[key]] <- r
      bkeys[[key]] <- k
    }
  }
  ord <- order(names(best))
  unname(best[ord])
}

#' morie_vepan_vep_annotation
#'
#' A step of the vepan_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param variants See Usage.
#' @param transcripts See Usage.
#' @param genome Passed to \code{morie_vepan_annotate}.
#' @param upstream Passed to \code{morie_vepan_annotate}. Defaults to \code{5000}.
#' @param downstream Passed to \code{morie_vepan_annotate}. Defaults to \code{5000}.
#' @param mode One of \code{"all"}, \code{"per_gene"}, \code{"pick"}. Defaults to \code{"all"}.
#' @param no_intergenic A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return A list with \code{estimate}, \code{annotations}, \code{n_variants}, \code{n_annotations}, \code{consequence_counts}, \code{mode}, \code{method}, \code{note}.
#' @export
morie_vepan_vep_annotation <- function(variants, transcripts, genome,
                                       upstream=5000, downstream=5000,
                                       mode="all", no_intergenic=FALSE) {
  # Annotate every variant against every transcript. mode is "all"
  # (every overlap), "pick" (Table 7's one line per variant) or
  # "per_gene".
  if (!(mode %in% c("all", "pick", "per_gene"))) {
    stop("vepan: mode must be 'all', 'pick' or 'per_gene'")
  }
  vs <- lapply(variants, .vepan_variant)
  if (length(vs) == 0L) {
    stop("vepan: no variants given")
  }
  trs <- transcripts
  rows <- list()
  for (v in vs) {
    recs <- morie_vepan_annotate(v, trs, genome, upstream, downstream)
    if (isTRUE(no_intergenic)) {
      recs <- Filter(function(r) !is.null(r$transcript), recs)
      if (length(recs) == 0L) {
        next
      }
    }
    if (mode == "pick") {
      recs <- morie_vepan_pick(recs)
    } else if (mode == "per_gene") {
      recs <- morie_vepan_pick(recs, per_gene=TRUE)
    }
    rows <- c(rows, recs)
  }
  by_term <- list()
  for (r in rows) {
    for (tm in r$consequences) {
      by_term[[tm]] <- (if (is.null(by_term[[tm]])) 0L else by_term[[tm]]) + 1L
    }
  }
  list(
    estimate=rows, annotations=rows, n_variants=length(vs),
    n_annotations=length(rows), consequence_counts=by_term, mode=mode,
    method=paste0("Ensembl Variant Effect Predictor (McLaren et al. ",
                  "2016): per-transcript consequence predicates with ",
                  "Sequence Ontology terms, severity from Ensembl's ",
                  "published consequence table"),
    note=paste0("regulatory, TFBS, NMD, miRNA and structural-variant ",
                "terms are in the rank table for severity comparison ",
                "but are not predicted here, since they need a ",
                "regulatory build or transcript flags a gene model ",
                "does not carry"))
}

#' morie_vepan_cheatsheet
#'
#' A step of the vepan_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_vepan_cheatsheet <- function() {
  paste0(
    "vepan: Ensembl VEP (McLaren et al. 2016). One record per ",
    "(variant, transcript): predicate functions assign Sequence ",
    "Ontology consequence terms, ranked 1-41 by Ensembl's ",
    "published severity table, so most_severe_consequence and ",
    "impact mean something. mode='pick' applies Table 7's order ",
    "-- canonical transcript first, then protein coding, then ",
    "severity -- and mode='per_gene' does the same per gene. ",
    "HGVS c. and p. are emitted, with indels shifted to their ",
    "most 3' position."
  )
}

# compact alias per ledger/NAMING.md
morie_vepan_vepannotation <- morie_vepan_vep_annotation

#' @export
morie_vepan <- morie_vepan_vep_annotation
