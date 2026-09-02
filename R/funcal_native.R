# morie.fn -- function file (rootcoder007/morie)
# eggNOG-mapper v2: functional annotation through orthology.
#
# Cantalapiedra, C. P., Hernandez-Plaza, A., Letunic, I., Bork, P., &
# Huerta-Cepas, J. (2021) "eggNOG-mapper v2: Functional Annotation,
# Orthology Assignments, and Domain Prediction at the Metagenomic Scale",
# *Molecular Biology and Evolution* 38(12), 5825-5829.
# doi:10.1093/molbev/msab293
#
# The premise is the first sentence of the paper: infer function "via
# orthology, rather than by homology" -- a best BLAST hit is a homologue,
# which may be a paralogue that has drifted in function, whereas an
# orthologue is the gene that speciation separated and is far more likely
# to have kept it. So the pipeline never transfers a term from a hit
# directly. It runs in four stages (Figure 1):
#
# 1. **Seed orthologs.** Search each query against the reference and keep
#    the best hit that clears the e-value, bit-score and coverage cut-offs.
#    The search tool (DIAMOND, MMseqs2, HMMER) changes speed and
#    sensitivity, not the logic downstream, so it is a label here.
# 2. **Orthology assignment.** The seed hit places the query in a
#    precomputed orthologous group, and the members of that group are the
#    query's orthologues. Each relationship is typed by how many genes sit
#    on each side: ``one2one``, ``one2many``, ``many2one``, ``many2many``.
# 3. **Taxonomic scope.** Terms may only be transferred from orthologues
#    inside the requested lineage, "preventing transferring functional
#    terms from orthologs of unwanted lineages".
# 4. **Transfer.** Terms held by the surviving orthologues become the
#    query's annotation, per source: protein name, KEGG pathways and
#    modules, GO labels, EC numbers, BiGG reactions, CAZy, COG category,
#    the OG itself, and free-text description.
#
# **What is and is not here.** The eggNOG v5 database (5,090 organisms,
# 4.4 M OGs) is not vendored, and no sequence search is run: the alignment
# tools are external programs. Everything that decides *what gets
# annotated and with what* is here and is exact -- the hit filters, the
# orthology typing, the scope restriction, and the support rule for
# accepting a term. Feed it hits and an orthologous-group table (from a
# real eggNOG download, or from anything else with the same shape).
#
# ``min_support`` has no counterpart in the paper's defaults, where a term
# held by any in-scope orthologue is transferred; it is exposed because a
# single divergent orthologue is exactly how a wrong term propagates, and
# ``min_support=1`` reproduces the paper's behaviour.

#' The four ways an orthology relationship can be shaped.
#' @noRd
ORTHOLOGY_TYPES <- c("one2one", "one2many", "many2one", "many2many")

#' The annotation sources the paper lists.
#' @noRd
ANNOTATION_SOURCES <- c("name", "kegg_pathway", "kegg_module", "go", "ec",
                        "bigg", "cazy", "cog_category", "og", "description")

.funcal_SEARCHERS <- c("diamond", "mmseqs", "hmmer")

#' .funcal_first
#'
#' A step of the funcal_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @return The value of \code{[}.
#' @export
.funcal_first <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  if (is.list(x)) return(x[[1]])
  return(x[1])
}

#' .funcal_hit
#'
#' A step of the funcal_native implementation. Called by \code{morie_funcal}, \code{morie_funcal_seed_orthologs}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param h Optional; may be \code{NULL}. A list; the body reads \code{$evalue}, \code{$query}, \code{$query_cov}, \code{$score}, \code{$target}, \code{$target_cov} from it.
#' @return A list with \code{query}, \code{target}, \code{evalue}, \code{score}, \code{query_cov}, \code{target_cov}.
#' @export
.funcal_hit <- function(h) {
  if (is.null(h) || is.null(h[["query"]]) || is.null(h[["target"]])) {
    stop("funcal: a hit needs 'query' and 'target'")
  }
  evalue <- if (is.null(h[["evalue"]])) 0.0 else as.numeric(h[["evalue"]])
  score <- if (is.null(h[["score"]])) 0.0 else as.numeric(h[["score"]])
  query_cov <- if (is.null(h[["query_cov"]])) 1.0 else as.numeric(h[["query_cov"]])
  target_cov <- if (is.null(h[["target_cov"]])) 1.0 else as.numeric(h[["target_cov"]])

  if (evalue < 0) {
    stop("funcal: an e-value cannot be negative")
  }
  if (query_cov < 0 || query_cov > 1) {
    stop("funcal: query_cov must be a fraction in [0, 1]")
  }
  if (target_cov < 0 || target_cov > 1) {
    stop("funcal: target_cov must be a fraction in [0, 1]")
  }

  list(query = h[["query"]], target = h[["target"]], evalue = evalue,
       score = score, query_cov = query_cov, target_cov = target_cov)
}

#' morie_funcal_seed_orthologs
#'
#' A step of the funcal_native implementation. Called by \code{morie_funcal}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param hits See Usage.
#' @param evalue The body requires: funcal: evalue must be positive and score non-negative. Defaults to \code{0.001}.
#' @param score The body requires: funcal: evalue must be positive and score non-negative. Defaults to \code{60}.
#' @param query_cov Passed to \code{<}. Defaults to \code{0.2}.
#' @param target_cov Passed to \code{<}. Defaults to \code{0.2}.
#' @param searcher Passed to \code{\%in\%}. Defaults to \code{"diamond"}.
#' @return The value of \code{result}, as built in the body.
#' @export
morie_funcal_seed_orthologs <- function(hits, evalue = 1e-3, score = 60.0,
                                       query_cov = 0.2, target_cov = 0.2,
                                       searcher = "diamond") {
  if (!(searcher %in% .funcal_SEARCHERS)) {
    stop(sprintf("funcal: searcher must be one of %s",
                 paste(.funcal_SEARCHERS, collapse = ", ")))
  }
  if (evalue <= 0 || score < 0) {
    stop("funcal: evalue must be positive and score non-negative")
  }
  if (query_cov < 0 || query_cov > 1 || target_cov < 0 || target_cov > 1) {
    stop("funcal: coverage cut-offs are fractions")
  }

  kept <- list()
  for (raw in hits) {
    h <- .funcal_hit(raw)
    if (h$evalue > evalue || h$score < score) next
    if (h$query_cov < query_cov || h$target_cov < target_cov) next

    best <- kept[[h$query]]
    better <- FALSE
    if (is.null(best)) {
      better <- TRUE
    } else if (h$evalue < best$evalue) {
      better <- TRUE
    } else if (h$evalue == best$evalue && h$score > best$score) {
      better <- TRUE
    }
    if (better) {
      kept[[h$query]] <- h
    }
  }

  result <- list()
  for (q in names(kept)) {
    h <- kept[[q]]
    h$searcher <- searcher
    result[[q]] <- h
  }
  return(result)
}

#' .funcal_type_of
#'
#' A step of the funcal_native implementation. Called by \code{morie_funcal_assign_orthologs}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_query_side Passed to \code{<=}.
#' @param n_target_side Passed to \code{<=}.
#' @return A character value.
#' @export
.funcal_type_of <- function(n_query_side, n_target_side) {
  left <- if (n_query_side <= 1) "one" else "many"
  right <- if (n_target_side <= 1) "one" else "many"
  paste0(left, "2", right)
}

#' morie_funcal_assign_orthologs
#'
#' A step of the funcal_native implementation. Called by \code{morie_funcal}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param seeds A vector; indexed elementwise.
#' @param groups A vector; indexed elementwise.
#' @param taxa Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param target_taxa Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param target_types Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_funcal_assign_orthologs <- function(seeds, groups, taxa = NULL,
                                         target_taxa = NULL,
                                         target_types = NULL) {
  if (!is.null(target_types)) {
    bad <- target_types[!target_types %in% ORTHOLOGY_TYPES]
    if (length(bad) > 0) {
      stop(sprintf("funcal: unknown orthology type '%s'", bad[1]))
    }
  }
  if (is.null(taxa)) taxa <- list()

  out <- list()
  for (q in names(seeds)) {
    seed <- seeds[[q]]
    g <- groups[[seed$target]]
    if (is.null(g) || length(g) == 0) {
      out[[q]] <- list(og = NULL, orthologs = list(), seed = seed$target,
                       dropped_by_scope = 0)
      next
    }

    members_all <- g[["members"]]
    if (is.null(members_all)) members_all <- character(0)
    if (!is.character(members_all)) members_all <- as.character(members_all)
    members <- members_all[members_all != seed$target]

    seed_og <- g[["og"]]
    same_group <- character(0)
    for (p in names(seeds)) {
      s <- seeds[[p]]
      g2 <- groups[[s$target]]
      if (!is.null(g2) && identical(g2[["og"]], seed_og)) {
        same_group <- c(same_group, p)
      }
    }

    rels <- list()
    dropped <- 0
    for (m in members) {
      lineage <- taxa[[m]]
      if (is.null(lineage)) lineage <- list()

      if (!is.null(target_taxa)) {
        in_scope <- FALSE
        for (t in target_taxa) {
          if (t %in% lineage) { in_scope <- TRUE; break }
        }
        if (!in_scope) { dropped <- dropped + 1; next }
      }

      n_target <- 1
      if (length(lineage) > 0) {
        lineage_first <- lineage[[1]]
        n_target <- sum(sapply(members, function(x) {
          x_taxa <- taxa[[x]]
          if (is.null(x_taxa) || length(x_taxa) == 0) return(FALSE)
          identical(x_taxa[[1]], lineage_first)
        }))
      }

      kind <- .funcal_type_of(length(same_group), n_target)
      if (!is.null(target_types) && !(kind %in% target_types)) next

      rels[[length(rels) + 1]] <- list(ortholog = m, type = kind,
                                       lineage = lineage)
    }

    out[[q]] <- list(og = g[["og"]], orthologs = rels, seed = seed$target,
                     dropped_by_scope = dropped)
  }
  return(out)
}

#' morie_funcal_transfer_terms
#'
#' A step of the funcal_native implementation. Called by \code{morie_funcal}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param assignments A vector; indexed elementwise.
#' @param annotations A vector; indexed elementwise.
#' @param sources Optional; may be \code{NULL}. Coerced to character by the body, with \code{as.character}.
#' @param min_support The body requires: funcal: min_support must be at least 1. Defaults to \code{1}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_funcal_transfer_terms <- function(assignments, annotations, sources = NULL,
                                       min_support = 1) {
  if (min_support < 1) {
    stop("funcal: min_support must be at least 1")
  }
  srcs <- if (is.null(sources)) ANNOTATION_SOURCES else as.character(sources)
  bad <- srcs[!srcs %in% ANNOTATION_SOURCES]
  if (length(bad) > 0) {
    stop(sprintf("funcal: unknown annotation source '%s'", bad[1]))
  }

  out <- list()
  for (q in names(assignments)) {
    a <- assignments[[q]]
    counts <- list()
    for (rel in a$orthologs) {
      ann <- annotations[[rel$ortholog]]
      if (is.null(ann)) ann <- list()
      for (s in srcs) {
        terms <- ann[[s]]
        if (is.null(terms)) next
        if (is.null(counts[[s]])) counts[[s]] <- list()
        for (term in terms) {
          key <- as.character(term)
          if (is.null(counts[[s]][[key]])) {
            counts[[s]][[key]] <- 1
          } else {
            counts[[s]][[key]] <- counts[[s]][[key]] + 1
          }
        }
      }
    }

    kept <- list()
    for (s in srcs) {
      if (is.null(counts[[s]])) {
        kept[[s]] <- character(0)
      } else {
        terms_counts <- counts[[s]]
        kept[[s]] <- sort(names(terms_counts)[
          sapply(terms_counts, function(c) c >= min_support)
        ])
      }
    }

    out[[q]] <- list(og = a$og, seed = a$seed,
                     n_orthologs = length(a$orthologs),
                     terms = kept, support = counts)
  }
  return(out)
}

#' morie_funcal
#'
#' A step of the funcal_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param hits Passed to \code{morie_funcal_seed_orthologs}.
#' @param groups Passed to \code{morie_funcal_assign_orthologs}.
#' @param annotations Passed to \code{morie_funcal_transfer_terms}.
#' @param taxa Passed to \code{morie_funcal_assign_orthologs}.
#' @param target_taxa Optional; may be \code{NULL}. Passed to \code{morie_funcal_assign_orthologs}.
#' @param target_types Optional; may be \code{NULL}. Passed to \code{morie_funcal_assign_orthologs}.
#' @param sources Passed to \code{morie_funcal_transfer_terms}.
#' @param evalue Passed to \code{morie_funcal_seed_orthologs}. Defaults to \code{0.001}.
#' @param score Passed to \code{morie_funcal_seed_orthologs}. Defaults to \code{60}.
#' @param query_cov Passed to \code{morie_funcal_seed_orthologs}. Defaults to \code{0.2}.
#' @param target_cov Passed to \code{morie_funcal_seed_orthologs}. Defaults to \code{0.2}.
#' @param min_support Passed to \code{morie_funcal_transfer_terms}. Defaults to \code{1}.
#' @param searcher Passed to \code{morie_funcal_seed_orthologs}. Defaults to \code{"diamond"}.
#' @return A list with \code{estimate}, \code{annotations}, \code{seeds}, \code{orthologs}, \code{n_queries}, \code{n_with_seed}, \code{n_annotated}, \code{searcher}, \code{target_taxa}, \code{target_types}, \code{min_support}, \code{method}, \code{note}.
#' @export
morie_funcal <- function(hits, groups, annotations, taxa = NULL,
                        target_taxa = NULL, target_types = NULL,
                        sources = NULL, evalue = 1e-3, score = 60.0,
                        query_cov = 0.2, target_cov = 0.2,
                        min_support = 1, searcher = "diamond") {
  seeds <- morie_funcal_seed_orthologs(hits, evalue, score, query_cov,
                                       target_cov, searcher)
  assigned <- morie_funcal_assign_orthologs(seeds, groups, taxa,
                                            target_taxa, target_types)
  annotated <- morie_funcal_transfer_terms(assigned, annotations, sources,
                                           min_support)

  queries <- character(0)
  for (h in hits) {
    norm <- .funcal_hit(h)
    if (!(norm$query %in% queries)) {
      queries <- c(queries, norm$query)
    }
  }
  queries <- sort(queries)

  n_ann <- 0
  for (q in names(annotated)) {
    terms <- annotated[[q]]$terms
    has_any <- FALSE
    for (s in names(terms)) {
      if (length(terms[[s]]) > 0) { has_any <- TRUE; break }
    }
    if (has_any) n_ann <- n_ann + 1
  }

  list(
    estimate = annotated,
    annotations = annotated,
    seeds = seeds,
    orthologs = assigned,
    n_queries = length(queries),
    n_with_seed = length(seeds),
    n_annotated = n_ann,
    searcher = searcher,
    target_taxa = if (is.null(target_taxa)) NULL else as.list(target_taxa),
    target_types = if (is.null(target_types)) NULL else as.list(target_types),
    min_support = as.integer(min_support),
    method = paste("eggNOG-mapper v2 (Cantalapiedra et al. 2021): seed",
                   "orthologs, orthologous-group assignment, taxonomic",
                   "scoping, then functional transfer from orthologs"),
    note = paste("no sequence search and no eggNOG v5 database are",
                 "bundled -- hits and the orthologous-group table are",
                 "supplied; everything that decides what is annotated",
                 "and with what is computed here. min_support=1 is the",
                 "paper's behaviour")
  )
}

morie_funcal_functional_annotation <- morie_funcal

#' morie_funcal_cheatsheet
#'
#' A step of the funcal_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_funcal_cheatsheet <- function() {
  paste("funcal: eggNOG-mapper v2 (Cantalapiedra et al. 2021).",
        "Function is transferred from ORTHOLOGS, not from the best",
        "hit: filter hits to a seed ortholog per query, take the",
        "members of its orthologous group, type each relationship",
        "one2one/one2many/many2one/many2many, drop orthologs outside",
        "the requested taxonomic scope, then transfer terms (name,",
        "KEGG, GO, EC, BiGG, CAZy, COG category, OG, description)",
        "held by the survivors.")
}
