# R arm of alfmpv -- MSA pairing for a protein complex.
#
# A complex is predicted from one alignment covering every chain. Rows
# that are PAIRED, one sequence per chain and all from the same organism,
# carry the inter-chain co-evolutionary signal; everything else is stacked
# block diagonally for the intra-chain signal and gapped elsewhere. How
# rows get paired is the whole method, and three papers specify it three
# different ways, so all three are here and the caller picks.
#
#   multimer   Evans et al. (2022). Group hits by UniProt OX, rank within
#              a species by similarity to that chain's query, join
#              same-rank rows across chains.
#   colabfold  Mirdita et al. (2022). Keep only species covering every
#              chain, require the alignment to cover at least half the
#              query, and pair ONLY the best hit by E-value, so a species
#              contributes exactly one row.
#   folddock   Bryant et al. (2022). Drop hits more than 90% gaps, then
#              pair the highest-ranked hit from one organism with the
#              highest-ranked hit of the interacting chain from the same
#              organism. One row per organism again.
#
# AlphaFold-Multimer states the same-rank rule but not what to do when a
# species has a different number of hits for each chain. ColabFold and
# FoldDock sidestep it: one hit per species means the counts cannot
# disagree. Only the multimer route has to decide, and it pairs up to the
# smallest count and leaves the surplus block diagonal. That is this
# implementation's reading, not a quotation, and it is reported as
# pairing_rule.
#
# Reported indices are 0-based, which is the package convention for
# reported fields whatever the language of the arm.
#
# References
#   Evans, R. et al. (2022) "Protein complex prediction with
#     AlphaFold-Multimer." bioRxiv 2021.10.04.463034v2, section 2.1,
#     doi:10.1101/2021.10.04.463034
#   Mirdita, M. et al. (2022) "ColabFold: making protein folding
#     accessible to all." Nature Methods 19(6), 679-682,
#     doi:10.1038/s41592-022-01488-1
#   Bryant, P., Pozzati, G. & Elofsson, A. (2022) "Improved prediction of
#     protein-protein interactions using AlphaFold2." Nature
#     Communications 13, 1265, doi:10.1038/s41467-022-28865-w

.ALFMPV_MODES <- c("multimer", "colabfold", "folddock")

# One hit attribute as a vector of length n, defaulted when absent: a
# caller with no E-values should still be able to run the multimer route,
# which never looks at them.
.alfmpv_field <- function(chain, name, n, default) {
  v <- if (is.list(chain)) chain[[name]] else NULL
  if (is.null(v)) return(rep(default, n))
  v <- as.numeric(v)
  if (length(v) != n)
    stop(sprintf("alfmpv: chain field %s has %d entries but %d hits",
                 name, length(v), n), call. = FALSE)
  v
}

.alfmpv_chain_table <- function(chain, idx) {
  if (is.list(chain) && !is.null(chain$species)) {
    species <- as.character(chain$species)
  } else if (is.atomic(chain)) {
    species <- as.character(chain)
    chain <- list()
  } else {
    stop(sprintf("alfmpv: chain %d has no species field", idx),
         call. = FALSE)
  }
  n <- length(species)
  list(species = species,
       evalue = .alfmpv_field(chain, "evalue", n, 0),
       identity = .alfmpv_field(chain, "identity", n, 0),
       gaps = .alfmpv_field(chain, "gaps", n, 0),
       coverage = .alfmpv_field(chain, "coverage", n, 1),
       n = n)
}

# Which hits survive the route's own filter, in order.
.alfmpv_keep <- function(tab, mode, min_coverage, max_gap) {
  if (!tab$n) return(integer(0))
  ok <- rep(TRUE, tab$n)
  if (mode == "colabfold") ok <- tab$coverage >= min_coverage
  if (mode == "folddock") ok <- tab$gaps <= max_gap
  which(ok)
}

# Lower sorts first. Ties break on the hit's position in the alignment,
# so the order is total and neither arm can wander off on a tie.
.alfmpv_order <- function(tab, mode, idx, pos) {
  if (mode == "multimer") return(order(-tab$identity[idx], pos))
  if (mode == "colabfold") return(order(tab$evalue[idx], pos))
  order(pos)
}

#' MSA pairing for a protein complex
#'
#' @param msas one entry per chain: either a plain vector of species
#'   identifiers, or a list with a species element and any of evalue,
#'   identity, gaps and coverage as parallel vectors.
#' @param mode multimer, colabfold or folddock.
#' @param min_coverage colabfold only: least fraction of the query the
#'   alignment must cover. 0.5 is the published value.
#' @param max_gap folddock only: most gaps a hit may be, as a fraction.
#'   0.9 is the published value.
#' @param copies copy count per chain for a homo-oligomer. ColabFold
#'   copies the alignment per component rather than searching again, so
#'   the copies share one chain's hits.
#' @param max_pairs stop after this many paired rows.
#' @return a list with paired, species_paired, unpaired, n_paired,
#'   n_unpaired, n_rows, n_chains, chain_source, n_filtered, mode,
#'   pairing_rule and method.
#' @export
morie_alfmpv_msa_pairing <- function(msas, mode = "multimer",
                                     min_coverage = 0.5, max_gap = 0.9,
                                     copies = NULL, max_pairs = NULL) {
  if (!(length(mode) == 1L && mode %in% .ALFMPV_MODES))
    stop(sprintf("alfmpv: mode = %s; expected one of %s", mode,
                 paste(.ALFMPV_MODES, collapse = ", ")), call. = FALSE)
  if (!length(msas)) stop("alfmpv: no chains given", call. = FALSE)

  tabs <- lapply(seq_along(msas), function(i)
    .alfmpv_chain_table(msas[[i]], i))

  # A homo-oligomer reuses one chain's alignment for each copy rather
  # than searching again, so the copies are the same table.
  if (!is.null(copies)) {
    if (length(copies) != length(tabs))
      stop(sprintf("alfmpv: copies has %d entries for %d chains",
                   length(copies), length(tabs)), call. = FALSE)
    expanded <- list()
    source <- integer(0)
    for (j in seq_along(copies)) {
      k <- as.integer(copies[j])
      if (is.na(k) || k < 1L)
        stop(sprintf("alfmpv: copies[%d] = %s; need at least 1", j,
                     as.character(copies[j])), call. = FALSE)
      for (r in seq_len(k)) {
        expanded[[length(expanded) + 1L]] <- tabs[[j]]
        source <- c(source, j - 1L)
      }
    }
    tabs <- expanded
  } else {
    source <- seq_along(tabs) - 1L
  }

  nc <- length(tabs)
  kept <- lapply(tabs, .alfmpv_keep, mode, min_coverage, max_gap)

  # Species order follows the first chain, so the paired block has a
  # defined order without sorting strings -- which would put the two arms
  # at the mercy of their collation locales.
  ord <- character(0)
  for (c0 in seq_len(nc))
    for (i in kept[[c0]]) {
      s <- tabs[[c0]]$species[i]
      if (!(s %in% ord)) ord <- c(ord, s)
    }

  by_species <- vector("list", nc)
  for (c0 in seq_len(nc)) {
    d <- list()
    idx <- kept[[c0]]
    if (length(idx)) {
      sp <- tabs[[c0]]$species[idx]
      for (s in unique(sp)) {
        w <- which(sp == s)
        sel <- idx[w]
        d[[s]] <- sel[.alfmpv_order(tabs[[c0]], mode, sel, w)]
      }
    }
    by_species[[c0]] <- d
  }

  paired <- list()
  species_paired <- character(0)
  used <- vector("list", nc)
  for (c0 in seq_len(nc)) used[[c0]] <- integer(0)

  for (s in ord) {
    lists <- lapply(seq_len(nc), function(c0) {
      v <- by_species[[c0]][[s]]
      if (is.null(v)) integer(0) else v
    })
    if (any(vapply(lists, length, integer(1)) == 0L)) next
    depth <- if (mode %in% c("colabfold", "folddock")) 1L
             else min(vapply(lists, length, integer(1)))
    hit_cap <- FALSE
    for (k in seq_len(depth)) {
      if (!is.null(max_pairs) && length(paired) >= as.integer(max_pairs)) {
        hit_cap <- TRUE
        break
      }
      row <- vapply(lists, function(v) v[k], integer(1))
      paired[[length(paired) + 1L]] <- as.integer(row - 1L)
      species_paired <- c(species_paired, s)
      for (c0 in seq_len(nc)) used[[c0]] <- c(used[[c0]], row[c0])
    }
    if (hit_cap ||
        (!is.null(max_pairs) && length(paired) >= as.integer(max_pairs)))
      break
  }

  unpaired <- lapply(seq_len(nc), function(c0)
    as.integer(setdiff(kept[[c0]], used[[c0]]) - 1L))

  rule <- if (mode == "multimer")
    paste("pair up to the smallest per-species hit count; the surplus",
          "goes block diagonal (this implementation's reading -- Evans",
          "et al. state the same-rank rule but not the unequal case)")
  else "one hit per species, so counts cannot disagree"

  method <- switch(mode,
    multimer = paste("AlphaFold-Multimer species pairing (Evans et al.",
                     "2022, section 2.1)"),
    colabfold = sprintf(paste("ColabFold best-hit-per-species pairing",
                              "(Mirdita et al. 2022), coverage >= %g"),
                        min_coverage),
    folddock = sprintf(paste("FoldDock top-ranked-per-organism pairing",
                             "(Bryant et al. 2022), gaps <= %g"), max_gap))

  list(paired = paired,
       species_paired = species_paired,
       unpaired = unpaired,
       n_paired = length(paired),
       n_unpaired = vapply(unpaired, length, integer(1)),
       n_rows = length(paired) + sum(vapply(unpaired, length, integer(1))),
       n_chains = nc,
       chain_source = as.integer(source),
       n_filtered = vapply(seq_len(nc), function(c0)
         as.integer(tabs[[c0]]$n - length(kept[[c0]])), integer(1)),
       mode = mode,
       pairing_rule = rule,
       method = method)
}

#' AlphaFold-Multimer chain pairing
#'
#' @param chains alias for msas, for the older call shape.
#' @param msas one entry per chain; see morie_alfmpv_msa_pairing.
#' @param mode multimer, colabfold or folddock.
#' @param ... passed to morie_alfmpv_msa_pairing.
#' @return see morie_alfmpv_msa_pairing.
#' @export
morie_alfmpv <- function(chains = NULL, msas = NULL, mode = "multimer",
                         ...) {
  if (is.null(msas)) msas <- chains
  if (is.null(msas)) stop("alfmpv: give the per-chain MSAs", call. = FALSE)
  morie_alfmpv_msa_pairing(msas, mode = mode, ...)
}
