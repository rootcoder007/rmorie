# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Pins the per-variable taxonomy invariants on the R side (mirror of
# tests/test_variable_taxonomy.py on the Python side).

# ── Identifier detection ───────────────────────────────────────────

test_that("identifier-shaped names are classified as identifier", {
  for (n in c("_id", "_Id", "RecordID", "UniqueIndividual_ID",
              "BatchFileName", "Indiv_Index")) {
    t <- morie_classify_variable(n, "string", NULL, "any")
    expect_equal(t$level, "identifier", info = paste("name=", n))
    expect_equal(t$role,  "identifier", info = paste("name=", n))
  }
})


# ── Boolean detection ──────────────────────────────────────────────

test_that("dtype=bool -> boolean level", {
  t <- morie_classify_variable("MentalHealth_Alert", "bool", NULL, "b01")
  expect_equal(t$level, "boolean")
  expect_equal(t$cardinality, "binary")
})

test_that("Yes/No valid_values -> boolean", {
  t <- morie_classify_variable("IndivInjuries_PhysicalInjuries",
                                 "string", c("Yes", "No"),
                                 "uof_individual_records")
  expect_equal(t$level, "boolean")
})


# ── Ordinal vs nominal ─────────────────────────────────────────────

test_that("Age_Category is ordinal", {
  t <- morie_classify_variable("Age_Category", "string",
                                 c("18 to 24", "25 to 49", "50+"), "b01")
  expect_equal(t$level, "ordinal")
})

test_that("Race without order is nominal", {
  t <- morie_classify_variable("Race", "string",
                                 c("White", "Black", "Asian"),
                                 "uof_individual_records")
  expect_equal(t$level, "nominal")
})


# ── Ratio (counts) ─────────────────────────────────────────────────

test_that("Number*Days int names are ratio", {
  t <- morie_classify_variable("NumberConsecutiveDays_Segregation",
                                 "int", NULL, "any")
  expect_equal(t$level, "ratio")
})

test_that("float dtype is ratio", {
  t <- morie_classify_variable("some_score", "float", NULL, "any")
  expect_equal(t$level, "ratio")
})


# ── Cross-year-safety overrides (CRITICAL INVARIANTS) ──────────────

test_that("OTIS UniqueIndividual_ID is cross_year_safe=FALSE", {
  for (ds in c("b01", "a01")) {
    t <- morie_classify_variable("UniqueIndividual_ID", "string",
                                   NULL, ds)
    expect_false(t$cross_year_safe,
                 info = paste("dataset=", ds,
                              "; OTIS dict states random per-fiscal-year ID"))
    expect_equal(t$role, "identifier")
    notes_lc <- tolower(t$notes %||% "")
    expect_true(grepl("fiscal year|fiscal-year", notes_lc),
                info = paste("notes lacking fiscal-year ref:", t$notes))
  }
})

test_that("ARSAU BatchFileName is identifier across all 4 record types", {
  for (ds in c("uof_main_records", "uof_individual_records",
                "uof_weapon_records", "uof_probe_cycle_records")) {
    t <- morie_classify_variable("BatchFileName", "string", NULL, ds)
    expect_equal(t$role, "identifier", info = paste("dataset=", ds))
  }
})

test_that("ARSAU PhysicalInjuries marked as outcome", {
  t <- morie_classify_variable("IndivInjuries_PhysicalInjuries",
                                 "bool", c("Yes", "No"),
                                 "uof_individual_records")
  expect_equal(t$role, "outcome")
})


# ── Recommended summary per level ──────────────────────────────────

test_that("recommended_summary mentions the right test per level", {
  cases <- list(
    list(level = "boolean",   expect = "Wilson"),
    list(level = "nominal",   expect = "chi-square"),
    list(level = "ordinal",   expect = "median"),
    list(level = "ratio",     expect = "Pareto"),
    list(level = "date",      expect = "histogram"),
    list(level = "identifier", expect = "identifier")
  )
  for (c in cases) {
    tax <- list(dataset_name = "X", column_name = "X",
                 level = c$level, cardinality = "unknown",
                 role = "covariate", cross_year_safe = TRUE)
    class(tax) <- c("morie_variable_taxonomy", "list")
    msg <- morie_recommended_summary(tax)
    expect_true(grepl(c$expect, msg, ignore.case = TRUE),
                info = paste("level=", c$level, "msg=", msg))
  }
})


# ── Recommended pair test (Stevens dispatcher) ─────────────────────

.tax <- function(level) {
  out <- list(dataset_name = "X", column_name = "X",
               level = level, cardinality = "unknown",
               role = "covariate", cross_year_safe = TRUE)
  class(out) <- c("morie_variable_taxonomy", "list")
  out
}

test_that("nominal x nominal -> chi-square", {
  expect_true(grepl("chi-square",
                     morie_recommended_pair_test(.tax("nominal"), .tax("nominal")),
                     ignore.case = TRUE))
})

test_that("ordinal x ordinal -> Spearman", {
  expect_true(grepl("Spearman",
                     morie_recommended_pair_test(.tax("ordinal"), .tax("ordinal")),
                     ignore.case = TRUE))
})

test_that("nominal x ratio -> ANOVA or Kruskal", {
  msg <- morie_recommended_pair_test(.tax("nominal"), .tax("ratio"))
  expect_true(grepl("anova|kruskal", msg, ignore.case = TRUE))
})

test_that("interval x ratio -> Pearson", {
  msg <- morie_recommended_pair_test(.tax("interval"), .tax("ratio"))
  expect_true(grepl("Pearson|Spearman", msg, ignore.case = TRUE))
})

test_that("identifier pair refuses test", {
  msg <- morie_recommended_pair_test(.tax("identifier"), .tax("ratio"))
  expect_true(grepl("identifier", msg, ignore.case = TRUE))
})


# ── Override registry integrity ────────────────────────────────────

test_that("INVARIANT_OVERRIDES registry is not empty", {
  expect_true(length(morie:::.MORIE_INVARIANT_OVERRIDES) > 0)
})

test_that("each override has valid field names", {
  valid <- c("level", "cardinality", "role", "cross_year_safe",
              "dictionary_described", "valid_values", "nullable",
              "raw_dtype", "notes", "source")
  for (entry in morie:::.MORIE_INVARIANT_OVERRIDES) {
    unknown <- setdiff(names(entry$patch), valid)
    if (length(unknown) > 0L) {
      stop(sprintf("override (%s, %s) has unknown fields: %s",
                    entry$ds_prefix, entry$col,
                    paste(unknown, collapse = ",")))
    }
    expect_length(unknown, 0L)
  }
})


# ── Print method ───────────────────────────────────────────────────

test_that("print.morie_variable_taxonomy emits expected lines", {
  t <- morie_classify_variable("UniqueIndividual_ID", "string", NULL, "b01")
  out <- capture.output(print(t))
  expect_true(any(grepl("UniqueIndividual_ID", out)))
  expect_true(any(grepl("cross_year_safe", out)))
})
