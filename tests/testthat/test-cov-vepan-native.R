# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/vepan_native.R (Ensembl VEP, McLaren et al. 2016).
# A hand-built gene: exon 5..30, CDS 8..22 = ATG GCT TGG AAA TAA
# (Met Ala Trp Lys Ter). Each variant's codon change is worked out by
# hand, so the consequence terms and HGVS strings are derived, not
# copied.

.ve_g <- paste0("CCCC", "GGG", "ATGGCTTGGAAATAA", "CCCCCCCC", "GGGGGGGGGG")
.ve_t <- list(id = "T1", gene = "G1", exons = list(c(5, 30)), cds_start = 8, cds_end = 22,
              canonical = TRUE)
.ve_rc <- function(s) paste(rev(strsplit(chartr("ACGT", "TGCA", s), "")[[1]]), collapse = "")
.ve_one <- function(pos, ref, alt, tr = list(.ve_t)) {
  morie_vepan_annotate(list(pos = pos, ref = ref, alt = alt), tr, .ve_g)[[1]]
}

test_that("rank, impact and most-severe follow Ensembl's severity table", {
  expect_identical(morie_vepan_consequence_rank("transcript_ablation"), 1L)
  expect_identical(morie_vepan_consequence_rank("stop_gained"), 4L)
  expect_identical(morie_vepan_consequence_rank("sequence_variant"), 41L)
  expect_identical(morie_vepan_consequence_rank("made_up"), 42L)
  expect_identical(morie_vepan_consequence_impact("missense_variant"), "MODERATE")
  expect_identical(morie_vepan_consequence_impact("synonymous_variant"), "LOW")
  expect_identical(morie_vepan_consequence_impact("made_up"), "MODIFIER")
  expect_identical(morie_vepan_most_severe_consequence(c("intron_variant", "missense_variant",
                                                         "splice_region_variant")), "missense_variant")
  expect_error(morie_vepan_most_severe_consequence(character(0)), "no consequence")
})

test_that("transcript_sequence splices exons and reverse-complements on the minus strand", {
  tr <- list(exons = list(c(20, 30), c(5, 12)), strand = "+", biotype = "lncRNA")
  s <- morie_vepan_transcript_sequence(tr, .ve_g)
  expect_identical(s$seq, paste0(substr(.ve_g, 5, 12), substr(.ve_g, 20, 30)))
  expect_identical(s$gpos, c(5:12, 20:30))
  tr$strand <- "-"
  m <- morie_vepan_transcript_sequence(tr, tolower(.ve_g))
  expect_identical(m$seq, .ve_rc(s$seq))
  expect_identical(m$gpos, rev(s$gpos))
  expect_error(morie_vepan_transcript_sequence(list(strand = "+"), .ve_g), "needs exons")
  expect_error(morie_vepan_transcript_sequence(list(exons = list(c(5, 12), c(10, 20)), biotype = "x"), .ve_g),
               "overlap")
  expect_error(morie_vepan_transcript_sequence(list(exons = list(c(5, 12)), strand = "*", biotype = "x"), .ve_g),
               "strand")
  expect_error(morie_vepan_transcript_sequence(list(exons = list(c(5, 12)), biotype = "protein_coding"), .ve_g),
               "cds_start and cds_end")
})

test_that("annotate derives coding consequences and HGVS from the codon change", {
  r <- .ve_one(14, "T", "C")                      # TGG -> CGG, Trp3Arg
  expect_identical(r$most_severe, "missense_variant")
  expect_identical(r$impact, "MODERATE")
  expect_identical(r$hgvs_c, "c.7T>C")
  expect_identical(r$hgvs_p, "p.Trp3Arg")
  expect_identical(r$protein_position, 3L)
  expect_identical(r$ref_codon, "TGG")
  r <- .ve_one(16, "G", "A")                      # TGG -> TGA
  expect_identical(r$most_severe, "stop_gained")
  expect_identical(r$impact, "HIGH")
  expect_identical(r$hgvs_p, "p.Trp3Ter")
  r <- .ve_one(13, "T", "C")                      # GCT -> GCC, Ala
  expect_identical(r$consequences, "synonymous_variant")
  expect_identical(r$hgvs_p, "p.Ala2=")
  expect_identical(.ve_one(8, "A", "G")$most_severe, "start_lost")
  expect_identical(.ve_one(22, "A", "C")$most_severe, "stop_lost")        # TAA -> TAC
  expect_identical(.ve_one(21, "A", "G")$most_severe, "stop_retained_variant")  # TAA -> TGA
  # deleting one A of AAA: frameshift, shifted to the 3'-most A (c.12)
  r <- .ve_one(16, "GA", "G")
  expect_true("frameshift_variant" %in% r$consequences)
  expect_identical(r$hgvs_c, "c.12delA")
  expect_identical(r$hgvs_p, "p.Lys4fs")
  expect_identical(.ve_one(16, "GAAA", "G")$most_severe, "inframe_deletion")
  expect_identical(.ve_one(16, "G", "GCCC")$most_severe, "inframe_insertion")
  expect_identical(.ve_one(6, "G", "A")$consequences, "5_prime_UTR_variant")
  expect_identical(.ve_one(25, "C", "A")$consequences, "3_prime_UTR_variant")
  expect_identical(.ve_one(2, "C", "A")$consequences, "upstream_gene_variant")
  expect_identical(.ve_one(35, "G", "A")$consequences, "downstream_gene_variant")
  # outside every flank: a single intergenic record
  r <- morie_vepan_annotate(list(pos = 35, ref = "G", alt = "A"), list(.ve_t), .ve_g, upstream = 1, downstream = 1)
  expect_identical(r[[1]]$most_severe, "intergenic_variant")
  expect_null(r[[1]]$transcript)
  expect_error(.ve_one(0, "A", "C"), "positive")
  expect_error(.ve_one(14, "TG", "CA"), "only SNVs")
  expect_error(.ve_one(14, "Q", "C"), "ACGTN")
  expect_error(morie_vepan_annotate(list(pos = 14, ref = "T"), list(.ve_t), .ve_g), "needs pos, ref and alt")
  expect_error(morie_vepan_annotate(list(pos = 14, ref = "T", alt = "C"), list(.ve_t), .ve_g, upstream = -1),
               "non-negative")
})

test_that("splice terms come from the distance to the exon boundary", {
  tr <- list(id = "S", exons = list(c(5, 12), c(20, 30)), biotype = "lncRNA")
  d <- .ve_one(13, "C", "A", list(tr))            # first intron base: donor
  expect_true("splice_donor_variant" %in% d$consequences)
  expect_true("intron_variant" %in% d$consequences)
  a <- .ve_one(19, "C", "A", list(tr))            # last intron base: acceptor
  expect_true("splice_acceptor_variant" %in% a$consequences)
  expect_true("non_coding_transcript_variant" %in% a$consequences)
  e <- .ve_one(25, "C", "A", list(tr))
  expect_identical(e$consequences, c("non_coding_transcript_exon_variant", "non_coding_transcript_variant"))
  # minus strand swaps donor and acceptor
  tr$strand <- "-"
  expect_true("splice_acceptor_variant" %in% .ve_one(13, "C", "A", list(tr))$consequences)
})

test_that("pick and per_gene apply Table 7's order; vep_annotation counts terms", {
  t2 <- list(id = "T2", gene = "G1", exons = list(c(5, 30)), cds_start = 8, cds_end = 22)
  t3 <- list(id = "T3", gene = "G2", exons = list(c(5, 30)), biotype = "lncRNA")
  recs <- morie_vepan_annotate(list(pos = 14, ref = "T", alt = "C"), list(t3, t2, .ve_t), .ve_g)
  expect_length(recs, 3L)
  expect_identical(morie_vepan_pick(recs)[[1]]$transcript, "T1")    # canonical wins
  pg <- morie_vepan_pick(recs, per_gene = TRUE)
  expect_identical(vapply(pg, function(r) r$transcript, ""), c("T1", "T3"))
  expect_error(morie_vepan_pick(list()), "nothing to pick")
  vs <- list(list(pos = 14, ref = "T", alt = "C"), list(pos = 16, ref = "G", alt = "A"),
             list(pos = 35, ref = "G", alt = "A", chrom = "chr2"))
  for (fn in list(morie_vepan_vep_annotation, morie_vepan)) {
    a <- fn(vs, list(t2, .ve_t, t3), .ve_g)
    expect_equal(a$n_variants, 3L)
    expect_equal(a$n_annotations, 3 + 3 + 1)
    expect_equal(a$consequence_counts$missense_variant, 2L)
    expect_equal(a$consequence_counts$intergenic_variant, 1L)
    p <- fn(vs, list(t2, .ve_t, t3), .ve_g, mode = "pick", no_intergenic = TRUE)
    expect_equal(p$n_annotations, 2L)
    expect_identical(vapply(p$annotations, function(r) r$most_severe, ""), c("missense_variant", "stop_gained"))
    g <- fn(vs, list(t2, .ve_t, t3), .ve_g, mode = "per_gene")
    expect_equal(g$n_annotations, 5L)
    expect_error(fn(vs, list(.ve_t), .ve_g, mode = "best"), "mode must be")
    expect_error(fn(list(), list(.ve_t), .ve_g), "no variants")
  }
})

test_that("morie_vepan_cheatsheet names the pick order", {
  s <- morie_vepan_cheatsheet()
  expect_length(s, 1L)
  expect_match(s, "canonical transcript first")
})
