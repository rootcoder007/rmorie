# morie.fn -- function file (rootcoder007/morie)
# R arm of snpeff (translate, codon_table, annotate_variant, snpeff).
# Source:
#   Cingolani, P., et al. (2012) "A program for annotating and
#   predicting the effects of single nucleotide polymorphisms,
#   SnpEff", Fly 6(2), 80-92. The classification by where a variant
#   falls in a transcript and, for coding variants, by what the codon
#   becomes. Impact grades are the paper's own four: HIGH, MODERATE,
#   LOW and MODIFIER. Translation uses NCBI genetic code table 1.

.BASES <- "TCAG"
.AA <- paste0("FFLLSSSSYY**CC*W",
              "LLLLPPPPHHQQRRRR",
              "IIIMTTTTNNKKSSRR",
              "VVVVAAAADDEEGGGG")

.SNPEFF_CODONS <- local({
  tbl <- list()
  for (i in 1:4) {
    for (j in 1:4) {
      for (k in 1:4) {
        b1 <- substr(.BASES, i, i)
        b2 <- substr(.BASES, j, j)
        b3 <- substr(.BASES, k, k)
        tbl[[paste0(b1, b2, b3)]] <-
          substr(.AA, (i - 1L) * 16L + (j - 1L) * 4L + k,
                 (i - 1L) * 16L + (j - 1L) * 4L + k)
      }
    }
  }
  tbl
})

.SNPEFF_HIGH <- c("stop_gained", "stop_lost", "start_lost",
                  "frameshift_variant")
.SNPEFF_MODERATE <- c("missense_variant", "inframe_insertion",
                      "inframe_deletion")
.SNPEFF_LOW <- c("synonymous_variant", "stop_retained_variant")

codon_table <- function() .SNPEFF_CODONS

translate <- function(seq, to_stop = FALSE) {
  s <- toupper(as.character(seq))
  s <- gsub("U", "T", s, fixed = TRUE)
  if (any(!strsplit(s, "")[[1L]] %in% c("A", "C", "G", "T", "N")))
    stop(sprintf("snpeff: %r is not a nucleotide", seq))
  chars <- strsplit(s, "")[[1L]]
  if (any(!(chars %in% c("A", "C", "G", "T", "N"))))
    stop(sprintf("snpeff: %r is not a nucleotide", seq))
  out <- character(0L)
  i <- 1L
  while (i + 2L <= length(chars)) {
    codon <- paste0(chars[i], chars[i + 1L], chars[i + 2L],
                    collapse = "")
    aa <- if (codon %in% names(.SNPEFF_CODONS))
      .SNPEFF_CODONS[[codon]] else "X"
    if (isTRUE(to_stop) && aa == "*") break
    out <- c(out, aa)
    i <- i + 3L
  }
  paste0(out, collapse = "")
}

.snpeff_impact <- function(effect) {
  if (effect %in% .SNPEFF_HIGH) return("HIGH")
  if (effect %in% .SNPEFF_MODERATE) return("MODERATE")
  if (effect %in% .SNPEFF_LOW) return("LOW")
  "MODIFIER"
}

.snpeff_pack <- function(effect, ref_codon, alt_codon, ref_aa, alt_aa,
                         ref, alt, pos, codon_index = NULL,
                         hgvs_p = NULL) {
  list(effect = effect, impact = .snpeff_impact(effect),
       ref_codon = ref_codon, alt_codon = alt_codon,
       ref_aa = ref_aa, alt_aa = alt_aa,
       codon_index = codon_index,
       hgvs_p = hgvs_p,
       hgvs_c = sprintf("c.%d%s>%s", pos + 1L, ref, alt),
       pos = as.integer(pos), ref = ref, alt = alt)
}

annotate_variant <- function(cds, pos, ref, alt, cds_start = 0,
                             upstream = 5000, downstream = 5000,
                             transcript_len = NULL) {
  seq <- toupper(as.character(cds))
  seq <- gsub("U", "T", seq, fixed = TRUE)
  ref <- toupper(as.character(ref))
  alt <- toupper(as.character(alt))
  pos <- as.integer(pos)
  if (nchar(seq) == 0L)
    stop("snpeff: the sequence is empty")
  if (nchar(ref) == 0L || nchar(alt) == 0L)
    stop("snpeff: ref and alt must be non-empty")
  if (pos < 0L || pos >= nchar(seq))
    stop(sprintf("snpeff: position %d is outside the sequence", pos))
  ref_at <- substr(seq, pos + 1L, pos + nchar(ref))
  if (ref_at != ref)
    stop(sprintf(paste0("snpeff: the reference allele %r does not match ",
                        "the sequence at position %d (%r)"),
                 ref, pos, ref_at))
  end <- if (is.null(transcript_len)) nchar(seq)
  else cds_start + as.integer(transcript_len)
  if (pos < cds_start) {
    d <- cds_start - pos
    eff <- if (d <= upstream) "upstream_gene_variant"
    else "intergenic_variant"
    return(.snpeff_pack(eff, NULL, NULL, NULL, NULL, ref, alt, pos))
  }
  if (pos >= end) {
    d <- pos - end + 1L
    eff <- if (d <= downstream) "downstream_gene_variant"
    else "intergenic_variant"
    return(.snpeff_pack(eff, NULL, NULL, NULL, NULL, ref, alt, pos))
  }
  coding <- substr(seq, cds_start + 1L, end)
  off <- pos - cds_start
  mutated <- paste0(substr(coding, 1L, off),
                    alt,
                    substr(coding, off + nchar(ref) + 1L,
                           nchar(coding)),
                    collapse = "")
  if (nchar(ref) != nchar(alt)) {
    shift <- (nchar(alt) - nchar(ref)) %% 3L
    if (shift) {
      eff <- "frameshift_variant"
    } else {
      eff <- if (nchar(alt) > nchar(ref)) "inframe_insertion"
      else "inframe_deletion"
    }
    return(.snpeff_pack(eff, NULL, NULL, translate(coding),
                        translate(mutated), ref, alt, pos,
                        codon_index = off %/% 3L))
  }
  ci <- off %/% 3L
  rc1 <- ci * 3L + 1L
  ref_codon <- substr(coding, rc1, rc1 + 2L)
  alt_codon <- substr(mutated, rc1, rc1 + 2L)
  if (nchar(ref_codon) < 3L || nchar(alt_codon) < 3L)
    stop(sprintf("snpeff: the coding sequence is not a whole number of codons at position %d",
                 pos))
  ra <- if (ref_codon %in% names(.SNPEFF_CODONS))
    .SNPEFF_CODONS[[ref_codon]] else "X"
  aa <- if (alt_codon %in% names(.SNPEFF_CODONS))
    .SNPEFF_CODONS[[alt_codon]] else "X"
  if (ci == 0L && ra == "M" && aa != "M") {
    eff <- "start_lost"
  } else if (ra == "*" && aa != "*") {
    eff <- "stop_lost"
  } else if (ra != "*" && aa == "*") {
    eff <- "stop_gained"
  } else if (ra == aa) {
    eff <- if (ra == "*") "stop_retained_variant"
    else "synonymous_variant"
  } else {
    eff <- "missense_variant"
  }
  hgvs_p <- if (ra != aa) sprintf("p.%s%d%s", ra, ci + 1L, aa)
  else sprintf("p.%s%d=", ra, ci + 1L)
  .snpeff_pack(eff, ref_codon, alt_codon, ra, aa, ref, alt, pos,
               codon_index = ci, hgvs_p = hgvs_p)
}

snpeff <- function(cds, variants, cds_start = 0, upstream = 5000,
                   downstream = 5000, transcript_len = NULL) {
  if (length(variants) == 0L)
    return(list(estimate = list(), annotations = list(),
                effect_counts = list(), impact_counts = list(),
                n_variants = 0L, protein = "",
                method = paste0("variant effect annotation (Cingolani ",
                                "et al. 2012, SnpEff), standard genetic ",
                                "code"),
                note = paste0("impact grades are the paper's HIGH / ",
                              "MODERATE / LOW / MODIFIER; positions are ",
                              "0-based and hgvs_c is 1-based, as the ",
                              "notation requires")))
  out <- vector("list", length(variants))
  for (i in seq_along(variants)) {
    v <- variants[[i]]
    if (length(v) != 3L)
      stop("snpeff: each variant must be (pos, ref, alt)")
    out[[i]] <- annotate_variant(cds, v[[1L]], v[[2L]], v[[3L]],
                                 cds_start, upstream, downstream,
                                 transcript_len)
  }
  counts <- list()
  impacts <- list()
  for (a in out) {
    eff <- a$effect
    counts[[eff]] <- if (is.null(counts[[eff]])) 1L
    else counts[[eff]] + 1L
    imp <- a$impact
    impacts[[imp]] <- if (is.null(impacts[[imp]])) 1L
    else impacts[[imp]] + 1L
  }
  seq <- toupper(as.character(cds))
  seq <- gsub("U", "T", seq, fixed = TRUE)
  if (is.null(transcript_len))
    coding_seq <- substr(seq, cds_start + 1L, nchar(seq))
  else
    coding_seq <- substr(seq, cds_start + 1L,
                         cds_start + as.integer(transcript_len))
  list(estimate = out, annotations = out,
       effect_counts = counts, impact_counts = impacts,
       n_variants = length(out),
       protein = translate(coding_seq),
       method = paste0("variant effect annotation (Cingolani et al. ",
                       "2012, SnpEff), standard genetic code"),
       note = paste0("impact grades are the paper's HIGH / MODERATE / ",
                     "LOW / MODIFIER; positions are 0-based and hgvs_c ",
                     "is 1-based, as the notation requires"))
}

.snpeff_cheatsheet <- function() {
  paste0("snpeff: variant annotation (Cingolani et al. 2012). ",
         "Classify by codon change: synonymous, missense, ",
         "stop_gained, stop_lost, start_lost; by indel length mod 3: ",
         "frameshift against inframe; by position: upstream, ",
         "downstream, intergenic. Impact HIGH for the four that ",
         "break the protein, MODERATE for missense and inframe ",
         "indels, LOW for synonymous, MODIFIER for the rest.")
}

# public names resolved by fn/_lazy_map.json
variant_effect <- codon_table
varianteffect <- codon_table

morie_snpeff <- list(snpeff = snpeff,
                     translate = translate,
                     codon_table = codon_table,
                     annotate_variant = annotate_variant,
                     cheatsheet = cheatsheet,
                     variant_effect = variant_effect,
                     varianteffect = varianteffect)
