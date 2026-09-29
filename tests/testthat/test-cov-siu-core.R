# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/siu_core.R exports: the report URL, the boilerplate
# stripper (the privacy paragraph and the witness-officer glossary
# note, with non-breaking spaces normalised) and the LLM model
# listing. The two entry points that need a network are skipped.

test_that("morie_siu_report_url builds the directors-report link", {
  expect_identical(morie_siu_report_url(4567),
                   "https://www.siu.on.ca/en/directors_report_details.php?drid=4567")
  expect_identical(morie_siu_report_url(c(1, 22)),
                   paste0("https://www.siu.on.ca/en/directors_report_details.php?drid=", c(1, 22)))
  expect_error(morie_siu_report_url(0))
  expect_error(morie_siu_report_url(NA_integer_))
})

test_that("morie_siu_strip_boilerplate removes the privacy paragraph", {
  keep_a <- "The Director reviewed the file."
  keep_b <- "The complainant was transported to hospital."
  txt <- paste0(keep_a, " This information may include the names of parties, ",
                "the nature of the injuries sustained by the affected person. ", keep_b)
  out <- morie_siu_strip_boilerplate(txt)
  expect_false(grepl("information may include", out, fixed = TRUE))
  expect_true(grepl(keep_a, out, fixed = TRUE))
  expect_true(grepl(keep_b, out, fixed = TRUE))
  # it also stops at "evidence", and every occurrence goes
  two <- paste("A. This information may include names and the evidence.",
               "B. This information may include more and the evidence.", "C.")
  o2 <- morie_siu_strip_boilerplate(two)
  expect_false(grepl("may include", o2, fixed = TRUE))
  expect_true(grepl("A.", o2, fixed = TRUE))
  expect_true(grepl("C.", o2, fixed = TRUE))
  # a report with no boilerplate is returned unchanged
  plain <- "Nothing to remove here."
  expect_identical(morie_siu_strip_boilerplate(plain), plain)
  expect_error(morie_siu_strip_boilerplate(c("a", "b")))
  expect_error(morie_siu_strip_boilerplate(NA_character_))
})

test_that("morie_siu_strip_boilerplate removes the witness-officer glossary note", {
  # both eras' phrasings, straight and curly apostrophe
  for (mid in c("who, in the opinion of the SIU Director,",
                "who, in the SIU Director's opinion,",
                paste0("who, in the SIU Director", "’", "s opinion,"))) {
    txt <- paste("WO #1 was interviewed. A witness official is a police officer", mid,
                 "is involved in the incident but is not a subject official.",
                 "The investigation continued.")
    out <- morie_siu_strip_boilerplate(txt)
    expect_false(grepl("not a subject official", out, fixed = TRUE))
    expect_true(grepl("WO #1 was interviewed", out, fixed = TRUE))
    expect_true(grepl("The investigation continued", out, fixed = TRUE))
  }
  # a non-breaking space is normalised, so the ordinal survives as plain text
  nb <- paste0("SO", " ", "#1 was designated.")
  expect_true(grepl("SO #1", morie_siu_strip_boilerplate(nb), fixed = TRUE))
  # the glossary note must not take the surrounding narrative with it
  keep <- "SO #1 declined an interview."
  txt <- paste("A witness officer is an officer who, in the opinion of the SIU Director,",
               "is not a subject officer.", keep)
  expect_true(grepl(keep, morie_siu_strip_boilerplate(txt), fixed = TRUE))
})

test_that("morie_llm_models lists the configured server's models", {
  # with no server configured the listing is empty, not an error
  m <- morie_llm_models()
  expect_type(m, "character")
  if (!nzchar(Sys.getenv("MORIE_LLM_BASE"))) {
    expect_length(m, 0L)
  }
  # an unreachable base returns an empty listing rather than stopping
  expect_length(morie_llm_models(base = "http://127.0.0.1:1"), 0L)
})

test_that("morie_siu_fetch_report needs the SIU website", {
  skip("network: fetches https://www.siu.on.ca")
  morie_siu_fetch_report(1)
})
