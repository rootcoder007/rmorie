# SPDX-License-Identifier: AGPL-3.0-or-later
# A2AJ Canadian Legal Data client: pure helpers, then network-gated calls.

set.seed(1)

test_that("a2aj parquet URL follows the Hugging Face layout", {
  set.seed(1)
  expect_equal(
    morie_ingest_a2aj_url("SCC"),
    "https://huggingface.co/datasets/a2aj/canadian-case-law/resolve/main/SCC/train.parquet"
  )
  expect_match(
    morie_ingest_a2aj_url("REGULATIONS-ON", doc_type = "laws"),
    "canadian-laws/resolve/main/REGULATIONS-ON/train.parquet$"
  )
  expect_error(morie_ingest_a2aj_url("scc"), "upper-case")
  expect_error(morie_ingest_a2aj_url("SCC", doc_type = "statutes"))
})

test_that("records_df unions keys and keeps JSON arrays as list columns", {
  set.seed(1)
  df <- rmorie:::.morie_a2aj_records_df(list(
    list(citation_en = "2020 SCC 1", cases_cited_en = list("2019 SCC 9", NULL)),
    list(citation_en = "2021 ONCA 2", score = "0.5")
  ))
  expect_equal(names(df), c("citation_en", "cases_cited_en", "score"))
  expect_equal(nrow(df), 2L)
  expect_equal(df$cases_cited_en[[1]], c("2019 SCC 9", NA_character_))
  expect_null(df$cases_cited_en[[2]])
  expect_equal(df$score, c(NA_character_, "0.5"))
  expect_equal(nrow(rmorie:::.morie_a2aj_records_df(list())), 0L)
})

test_that("search and fetch validate their arguments before any request", {
  set.seed(1)
  expect_error(morie_ingest_a2aj_search(""), "non-empty")
  expect_error(morie_ingest_a2aj_search("x", size = 0), "between 1 and 50")
  expect_error(morie_ingest_a2aj_search("x", size = 51), "between 1 and 50")
  expect_error(morie_ingest_a2aj_search("x", search_type = "title"))
  expect_error(morie_ingest_a2aj_fetch(""), "non-empty")
  expect_error(morie_ingest_a2aj_fetch("2023 SCC 17", output_language = "de"))
})

test_that("citation_edges expands the cited list column into (from, to) pairs", {
  set.seed(1)
  x <- data.frame(
    citation_en = c("2020 SCC 1", "2021 ONCA 2", "2022 FC 3"),
    stringsAsFactors = FALSE
  )
  x$cases_cited_en <- I(list(c("2019 SCC 9", "2018 FCA 3"), character(0), NULL))
  edges <- morie_ingest_a2aj_citation_edges(x)
  expect_equal(edges$from, c("2020 SCC 1", "2020 SCC 1"))
  expect_equal(edges$to, c("2019 SCC 9", "2018 FCA 3"))
  empty <- morie_ingest_a2aj_citation_edges(x[2, , drop = FALSE])
  expect_equal(nrow(empty), 0L)
  expect_equal(names(empty), c("from", "to"))
  expect_error(morie_ingest_a2aj_citation_edges(x[, "citation_en", drop = FALSE]),
    "needs columns"
  )
})

test_that("gaps table names the courts the upstream issues ask for", {
  set.seed(1)
  g <- morie_ingest_a2aj_gaps()
  expect_equal(nrow(g), 24L)
  expect_equal(g$canlii_database_id[g$code == "HRTO"], "onhrt")
  expect_equal(g$canlii_database_id[g$code == "BCHRT"], "bchrt")
  expect_equal(g$canlii_database_id[g$code == "ONSC"], "onsc")
  expect_equal(g$issue[g$code == "HRTO"], 4L)
  expect_equal(g$issue[g$code == "ABKB"], 3L)
  expect_equal(g$issue[g$code == "QCCA"], 1L)
  expect_false(anyDuplicated(g$canlii_database_id) > 0)
})

test_that("coverage lists the Supreme Court with a date range (network)", {
  set.seed(1)
  skip_on_cran()
  skip_if_no_network("api.a2aj.ca", 443)
  cov <- .skip_on_upstream_error(morie_ingest_a2aj_coverage("cases"))
  expect_true("SCC" %in% cov$dataset)
  expect_s3_class(cov$earliest_document_date, "Date")
  expect_true(cov$number_of_documents[cov$dataset == "SCC"] > 10000L)
})

test_that("name search finds Roncarelli and fetch returns text (network)", {
  set.seed(1)
  skip_on_cran()
  skip_if_no_network("api.a2aj.ca", 443)
  hits <- .skip_on_upstream_error(morie_ingest_a2aj_search("roncarelli",
    search_type = "name", dataset = "SCC", size = 5
  ))
  expect_true(any(grepl("Roncarelli", hits$name_en)))
  doc <- .skip_on_upstream_error(morie_ingest_a2aj_fetch("2023 SCC 17",
    end_char = 200
  ))
  expect_s3_class(doc, "data.frame")
  expect_true(nchar(doc$unofficial_text_en) > 50L)
  # Issue 2 of the A2AJ tracker: this decision is not in the corpus.
  expect_null(.skip_on_upstream_error(morie_ingest_a2aj_fetch("2007 BCSC 1700")))
})
