# Anchors for eggNOG-mapper v2 style functional annotation
# (Cantalapiedra et al. 2021).
#
# The pipeline is deterministic: a seed is the best hit per query, the
# orthologs are the rest of the seed's orthologous group, and a term is
# transferred when enough of those orthologs carry it. Each of those is
# exactly checkable, and nothing was checking them: 11.5% coverage with no
# test naming any function in the file.

test_that("the orthology type is a two-by-two on the two side counts", {
  expect_identical(.funcal_type_of(1, 1), "one2one")
  expect_identical(.funcal_type_of(1, 2), "one2many")
  expect_identical(.funcal_type_of(3, 1), "many2one")
  expect_identical(.funcal_type_of(3, 5), "many2many")
  # a side count of zero counts as one, not as many
  expect_identical(.funcal_type_of(0, 0), "one2one")
  # every label it can produce is one the package declares
  for (nq in 0:4) {
    for (nt in 0:4) {
      expect_true(.funcal_type_of(nq, nt) %in% ORTHOLOGY_TYPES)
    }
  }
  expect_setequal(ORTHOLOGY_TYPES,
                  c("one2one", "one2many", "many2one", "many2many"))
})

test_that("a hit needs a query and a target, and its fields are fractions", {
  expect_error(.funcal_hit(list(target = "t")), "needs 'query' and 'target'")
  expect_error(.funcal_hit(list(query = "q")), "needs 'query' and 'target'")
  expect_error(.funcal_hit(NULL), "needs 'query' and 'target'")
  expect_error(.funcal_hit(list(query = "q", target = "t", evalue = -1)),
               "e-value cannot be negative")
  expect_error(.funcal_hit(list(query = "q", target = "t", query_cov = 1.5)),
               "query_cov must be a fraction")
  expect_error(.funcal_hit(list(query = "q", target = "t", target_cov = -0.1)),
               "target_cov must be a fraction")
  # the defaults are a perfect hit with no score
  h <- .funcal_hit(list(query = "q", target = "t"))
  expect_identical(h$evalue, 0)
  expect_identical(h$score, 0)
  expect_identical(h$query_cov, 1)
  expect_identical(h$target_cov, 1)
})

hit <- function(q, t, e, s, qc = 0.9, tc = 0.9)
  list(query = q, target = t, evalue = e, score = s,
       query_cov = qc, target_cov = tc)

test_that("the seed is the best hit per query, e-value first then score", {
  hits <- list(
    hit("q1", "t1", 1e-20, 200),
    hit("q1", "t2", 1e-30, 150),   # a better e-value beats a better score
    hit("q1", "t3", 1e-30, 300))   # tied e-value, so the higher score wins
  s <- morie_funcal_seed_orthologs(hits)
  expect_named(s, "q1")
  expect_identical(s$q1$target, "t3")
  expect_identical(s$q1$evalue, 1e-30)
  expect_identical(s$q1$score, 300)
  # order of arrival does not matter
  expect_identical(morie_funcal_seed_orthologs(rev(hits))$q1$target, "t3")
})

test_that("each cut-off removes the hit it is meant to remove", {
  # above the e-value cut-off
  expect_length(morie_funcal_seed_orthologs(list(hit("q", "t", 1e-1, 200))), 0L)
  # below the score cut-off
  expect_length(morie_funcal_seed_orthologs(list(hit("q", "t", 1e-40, 10))), 0L)
  # below the query coverage cut-off
  expect_length(
    morie_funcal_seed_orthologs(list(hit("q", "t", 1e-40, 200, qc = 0.05))), 0L)
  # below the target coverage cut-off
  expect_length(
    morie_funcal_seed_orthologs(list(hit("q", "t", 1e-40, 200, tc = 0.05))), 0L)
  # and a hit that clears all four survives
  expect_length(morie_funcal_seed_orthologs(list(hit("q", "t", 1e-40, 200))), 1L)
  # the cut-offs are arguments, so loosening them lets the hit through
  expect_length(
    morie_funcal_seed_orthologs(list(hit("q", "t", 1e-1, 200)), evalue = 1),
    1L)
  expect_length(
    morie_funcal_seed_orthologs(list(hit("q", "t", 1e-40, 10)), score = 0),
    1L)
})

test_that("the seed search validates its own arguments", {
  hits <- list(hit("q", "t", 1e-40, 200))
  expect_error(morie_funcal_seed_orthologs(hits, searcher = "nope"),
               "searcher must be one of")
  expect_error(morie_funcal_seed_orthologs(hits, evalue = 0),
               "evalue must be positive")
  expect_error(morie_funcal_seed_orthologs(hits, score = -1),
               "score non-negative")
  expect_error(morie_funcal_seed_orthologs(hits, query_cov = 2),
               "coverage cut-offs are fractions")
  expect_error(morie_funcal_seed_orthologs(hits, target_cov = -1),
               "coverage cut-offs are fractions")
  # every declared searcher is accepted
  for (sr in .funcal_SEARCHERS) {
    expect_length(morie_funcal_seed_orthologs(hits, searcher = sr), 1L)
  }
})

setup_one <- function() {
  hits <- list(hit("q1", "t1", 1e-30, 200))
  groups <- list(t1 = list(og = "OG1", members = c("t1", "m2", "m3", "m4")))
  list(hits = hits, groups = groups,
       seeds = morie_funcal_seed_orthologs(hits))
}

test_that("the orthologs are the group without the seed itself", {
  e <- setup_one()
  a <- morie_funcal_assign_orthologs(e$seeds, e$groups)
  expect_identical(a$q1$og, "OG1")
  expect_identical(a$q1$seed, "t1")
  # three of the four members, the seed excluded
  expect_length(a$q1$orthologs, 3L)
  expect_false("t1" %in% names(a$q1$orthologs))
})

test_that("a seed in no known group yields no orthologs", {
  e <- setup_one()
  a <- morie_funcal_assign_orthologs(e$seeds, list())
  expect_null(a$q1$og)
  expect_length(a$q1$orthologs, 0L)
  expect_identical(a$q1$seed, "t1")
  expect_identical(a$q1$dropped_by_scope, 0)
  # an empty group is the same as an absent one
  a2 <- morie_funcal_assign_orthologs(
    e$seeds, list(t1 = list(og = "OG1", members = character(0))))
  expect_length(a2$q1$orthologs, 0L)
})

test_that("an unknown orthology type is refused", {
  e <- setup_one()
  expect_error(
    morie_funcal_assign_orthologs(e$seeds, e$groups,
                                  target_types = c("one2one", "nope")),
    "unknown orthology type 'nope'")
  # the declared ones are accepted
  expect_silent(
    morie_funcal_assign_orthologs(e$seeds, e$groups,
                                  target_types = ORTHOLOGY_TYPES))
})

test_that("a term is transferred with the count of orthologs carrying it", {
  e <- setup_one()
  a <- morie_funcal_assign_orthologs(e$seeds, e$groups)
  annotations <- list(
    m2 = list(go = c("GO:0001", "GO:0002")),
    m3 = list(go = "GO:0002"),
    m4 = list(go = character(0)))
  tt <- morie_funcal_transfer_terms(a, annotations)
  expect_identical(tt$q1$og, "OG1")
  expect_identical(tt$q1$seed, "t1")
  expect_equal(tt$q1$n_orthologs, 3)
  expect_setequal(tt$q1$terms$go, c("GO:0001", "GO:0002"))
  # the support is a count of orthologs, not a fraction
  expect_equal(tt$q1$support$go[["GO:0001"]], 1)
  expect_equal(tt$q1$support$go[["GO:0002"]], 2)
})

test_that("min_support is a count and filters on it", {
  e <- setup_one()
  a <- morie_funcal_assign_orthologs(e$seeds, e$groups)
  annotations <- list(m2 = list(go = c("GO:0001", "GO:0002")),
                      m3 = list(go = "GO:0002"))
  expect_setequal(
    morie_funcal_transfer_terms(a, annotations, min_support = 1)$q1$terms$go,
    c("GO:0001", "GO:0002"))
  # two orthologs carry GO:0002, only one carries GO:0001
  expect_identical(
    morie_funcal_transfer_terms(a, annotations, min_support = 2)$q1$terms$go,
    "GO:0002")
  expect_length(
    morie_funcal_transfer_terms(a, annotations, min_support = 3)$q1$terms$go, 0L)
  expect_error(morie_funcal_transfer_terms(a, annotations, min_support = 0),
               "min_support must be at least 1")
})

test_that("the seed's own annotation is not transferred to the query", {
  # the premise of the paper: a term comes from the orthologs, and the
  # seed is the query's own best hit rather than an ortholog of it
  e <- setup_one()
  a <- morie_funcal_assign_orthologs(e$seeds, e$groups)
  annotations <- list(m2 = list(go = "GO:0001"),
                      t1 = list(go = "GO:0999"))
  tt <- morie_funcal_transfer_terms(a, annotations)
  expect_true("GO:0001" %in% tt$q1$terms$go)
  expect_false("GO:0999" %in% tt$q1$terms$go)
})

test_that("a query with no annotated orthologs gets no terms", {
  e <- setup_one()
  a <- morie_funcal_assign_orthologs(e$seeds, e$groups)
  tt <- morie_funcal_transfer_terms(a, list())
  expect_length(tt$q1$terms$go, 0L)
  expect_equal(tt$q1$n_orthologs, 3)
})

test_that("the pipeline reports what it annotated", {
  e <- setup_one()
  annotations <- list(m2 = list(go = c("GO:0001", "GO:0002")),
                      m3 = list(go = "GO:0002"))
  full <- morie_funcal(e$hits, e$groups, annotations)
  expect_equal(full$n_queries, 1)
  expect_equal(full$n_with_seed, 1)
  expect_equal(full$n_annotated, 1)
  expect_identical(full$searcher, "diamond")
  expect_match(full$method, "ortholog|eggNOG|Cantalapiedra")
  expect_identical(full$estimate, full$annotations)
  # a hit that fails the cut-offs leaves nothing to annotate
  none <- morie_funcal(list(hit("q", "t", 1e-1, 5)), e$groups, annotations)
  expect_equal(none$n_with_seed, 0)
  expect_equal(none$n_annotated, 0)
})
