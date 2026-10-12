# R/siu.R, the review side: recorded corrections, the anomaly check with and without a model,
# the per-column audit, and the field-by-field translation, all on a temporary cache.

.sr_pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"
.sr <- function(name) get(name, envir = environment(morie_siu_llm_extract))

.sr_cache <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  df <- data.frame(
    case_number = c("24-TST-001", "24-TST-002", "24-TST-003"),
    drid = c(7L, 8L, 9L), nrid = c(9L, NA, 10L),
    police_service = c("Toronto Police Service", "Ontario Provincial Police", ""),
    location_of_call = c("Queen Street", "", "Rue Principale"),
    narrative_summary = c("An officer attended.", "", "Un agent est intervenu."),
    news_release_summary = c("", "", "Communiqué"),
    news_release_title = c("", "", ""),
    relevant_legislation = c("", "", ""),
    `_language` = c("en", "en", "fr"),
    check.names = FALSE, stringsAsFactors = FALSE)
  utils::write.csv(df, file.path(dir, "SIU.csv"), row.names = FALSE, na = "")
  dir.create(file.path(dir, "html"))
  .sr(".siu_write_html_cache")(file.path(dir, "html"), "drid_7.html.gz",
                               "<html><p>Toronto Police Service officers attended Queen Street.</p></html>")
  dir
}

test_that("a recorded correction is one row per case and field, the last write winning", {
  dir <- withr::local_tempdir()
  expect_error(morie_siu_record_correction("24-TST-001", "not_a_field", "x", cache_dir = dir),
               "'not_a_field' is not an SIU field")
  p <- morie_siu_record_correction("24-TST-001", "police_service", "TPS", note = "seen", cache_dir = dir)
  expect_equal(p, file.path(dir, "canonical_overrides.csv"))
  morie_siu_record_correction("24-TST-001", "location_of_call", "Queen St", cache_dir = dir)
  morie_siu_record_correction("24-TST-001", "police_service", "Toronto Police Service", cache_dir = dir)
  got <- utils::read.csv(p, colClasses = "character")
  expect_equal(names(got), c("case_number", "field", "verified_value", "note", "recorded_at_utc"))
  expect_equal(got$field, c("location_of_call", "police_service"))
  expect_equal(got$verified_value, c("Queen St", "Toronto Police Service"))
  expect_match(got$recorded_at_utc, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T")
  # the loader merges the shipped table with the user's, the user's row winning
  testthat::local_mocked_bindings(.siu_corrections_dir = function() file.path(dir, "none"), .package = .sr_pkg)
  ov <- .sr(".siu_load_canonical_overrides")(user_cache_dir = dir)
  mine <- ov[ov$case_number == "24-TST-001", ]
  expect_equal(nrow(mine), 2L)
  expect_equal(mine$verified_value[mine$field == "police_service"], "Toronto Police Service")
  applied <- .sr(".siu_apply_canonical_overrides")(
    data.frame(case_number = c("24-TST-001", "x"), police_service = c("", ""), stringsAsFactors = FALSE), mine)
  expect_equal(applied$police_service, c("Toronto Police Service", ""))
})

test_that("the anomaly check judges each populated field by the model, or verbatim when no model answers", {
  dir <- .sr_cache()
  withr::local_envvar("_R_CHECK_PACKAGE_NAME_" = "rmorie")
  withr::local_options(morie.siu.allow_fetch = NULL)
  seen <- NULL
  testthat::local_mocked_bindings(
    .siu_llm_call = function(model, prompt, timeout_s = 600L, mock_response_text = NULL, expect_json = FALSE) {
      seen <<- prompt
      if (!is.null(mock_response_text)) return(mock_response_text)
      stop("no provider")
    },
    .package = .sr_pkg)
  # no model: verbatim containment decides
  d <- morie_siu_anomaly_check("24-TST-001", cache_dir = dir)
  expect_equal(d$field, c("case_number", "police_service", "location_of_call", "narrative_summary"))
  expect_equal(d$verdict, c("unclear", "agree", "agree", "unclear"))
  expect_match(d$reason[2], "deterministic mode")
  expect_match(seen, "- police_service: Toronto Police Service")
  expect_match(seen, "REPORT HTML:\n<html>")
  # a model's verdicts, with the parser's values merged back
  reply <- paste0("```json\n[{\"field\":\"police_service\",\"verdict\":\"agree\",\"reason\":\"named\"},",
                  "{\"field\":\"location_of_call\",\"verdict\":\"disagree\",\"reason\":\"not there\"}]\n```")
  d2 <- morie_siu_anomaly_check("24-TST-001", cache_dir = dir, mock_response_text = reply)
  expect_equal(names(d2), c("field", "parser_value", "verdict", "reason"))
  expect_equal(d2$parser_value, c("Toronto Police Service", "Queen Street"))
  expect_equal(d2$verdict, c("agree", "disagree"))
  # prose instead of JSON falls back; an empty array is an empty table; a reply without fields is an error
  d3 <- morie_siu_anomaly_check("24-TST-001", cache_dir = dir, mock_response_text = "I cannot help")
  expect_equal(d3$verdict, c("unclear", "agree", "agree", "unclear"))
  expect_equal(nrow(morie_siu_anomaly_check("24-TST-001", cache_dir = dir, mock_response_text = "[]")), 0L)
  expect_error(morie_siu_anomaly_check("24-TST-001", cache_dir = dir, mock_response_text = "[{\"verdict\":\"agree\"}]"),
               "missing 'field' column")
})

test_that("the column audit aggregates verdicts per field and keeps the disagreements as examples", {
  dir <- withr::local_tempdir()
  verdict <- function(field, verdict, parser_value = "v") {
    data.frame(field = field, parser_value = parser_value, verdict = verdict, reason = paste("because", verdict),
               stringsAsFactors = FALSE)
  }
  testthat::local_mocked_bindings(
    morie_siu_anomaly_check = function(case_number, model, cache_dir, max_html_chars) {
      switch(case_number,
        "A" = verdict(c("police_service", "location_of_call"), c("agree", "disagree")),
        "B" = stop("page missing"),
        "C" = verdict("police_service", "unclear"),
        "D" = verdict(c("police_service", "location_of_call"), c("agree", "disagree")))
    },
    .package = .sr_pkg)
  expect_error(morie_siu_audit_columns(c(NA, " ", "")), "must be non-empty")
  msgs <- testthat::capture_messages(out <- morie_siu_audit_columns(c("A", "B", "C", "D", "D"), cache_dir = dir,
                                                                    max_examples_per_field = 1L))
  expect_match(msgs, "auditing B \\[2/4\\]", all = FALSE)
  expect_match(msgs, "skipped: page missing", all = FALSE)
  expect_equal(out$field, c("location_of_call", "police_service"))
  expect_equal(out$n_audited, c(2L, 3L))
  expect_equal(out$n_agree, c(0L, 2L))
  expect_equal(out$n_disagree, c(2L, 0L))
  expect_equal(out$n_unclear, c(0L, 1L))
  expect_equal(out$agree_rate, c(0, 1))
  ex <- attr(out, "examples")
  expect_equal(names(ex), c("location_of_call", "police_service"))
  expect_equal(nrow(ex$location_of_call), 1L)
  expect_equal(ex$location_of_call$case_number, "A")
  expect_equal(ex$location_of_call$verdict, "disagree")
  expect_equal(ex$police_service$verdict, "unclear")
  # every verdict unclear: the rate is unknown and the audit says a model may be missing
  testthat::local_mocked_bindings(
    morie_siu_anomaly_check = function(case_number, ...) verdict("police_service", "unclear"), .package = .sr_pkg)
  expect_message(out2 <- morie_siu_audit_columns("A", cache_dir = dir, progress = FALSE), "no field was judged")
  expect_true(is.na(out2$agree_rate))
  expect_null(attr(out2, "examples")$police_service$nothing)
  testthat::local_mocked_bindings(morie_siu_anomaly_check = function(case_number, ...) stop("down"), .package = .sr_pkg)
  expect_error(morie_siu_audit_columns("A", cache_dir = dir, progress = FALSE), "All audit attempts failed")
})

test_that("translation picks its rows by language, asks for every non-empty field at once and records the answers", {
  dir <- .sr_cache()
  testthat::local_mocked_bindings(.siu_corrections_dir = function() file.path(dir, "user-data"), .package = .sr_pkg)
  seen <- list()
  answer <- function(prompt) {
    "```json\n{\"narrative_summary\":\"An agent intervened.\",\"news_release_summary\":\"Release\",\"location_of_call\":\"\"}\n```"
  }
  testthat::local_mocked_bindings(
    .siu_llm_call = function(model, prompt, timeout_s = 600L, ...) {
      seen[[length(seen) + 1L]] <<- list(model = model, prompt = prompt, timeout = timeout_s)
      answer(prompt)
    },
    .package = .sr_pkg)
  # the default target comes from MORIE_USER_LANG, then LANG, then English
  withr::local_envvar(MORIE_USER_LANG = NA, LANG = "fr_CA.UTF-8")
  msgs <- testthat::capture_messages(out <- morie_siu_translate(cache_dir = dir))
  # the two English rows are candidates; 002 has every field empty, so one request is made
  expect_match(msgs, "Translating 2 case\\(s\\) to .fr. via ollama", all = FALSE)
  expect_match(msgs, "1/2 24-TST-001 done", all = FALSE)
  expect_match(msgs, "Translated 1 \\(case, field\\) cells", all = FALSE)
  expect_length(seen, 1)
  expect_equal(seen[[1]]$model, "ollama")
  expect_match(seen[[1]]$prompt, "fields into clear professional French\\.")
  expect_match(seen[[1]]$prompt, "\\[narrative_summary\\]\nAn officer attended\\.\n\\[/narrative_summary\\]")
  expect_false(grepl("[news_release_summary]", seen[[1]]$prompt, fixed = TRUE))
  expect_equal(seen[[1]]$timeout, 120L)
  # only the fields that were asked for are recorded, whatever else the model returned
  expect_equal(out, data.frame(case_number = "24-TST-001", field = "narrative_summary",
                               verified_value = "An agent intervened.", stringsAsFactors = FALSE))
  rec <- utils::read.csv(file.path(dir, "canonical_overrides.csv"), colClasses = "character")
  expect_equal(rec$verified_value[rec$field == "narrative_summary"], "An agent intervened.")
  expect_match(rec$note[1], "auto-translated to fr via ollama")
  # an already translated case is left alone
  expect_message(again <- morie_siu_translate(target_lang = "fr", case_numbers = "24-TST-001", cache_dir = dir),
                 "Nothing to translate")
  expect_equal(nrow(again), 0L)
  # the French-to-English wrapper: the source language filter, a full-name target
  seen <- list()
  withr::local_envvar(MORIE_USER_LANG = "HI")
  dir2 <- .sr_cache()
  testthat::local_mocked_bindings(.siu_corrections_dir = function() file.path(dir2, "user-data"), .package = .sr_pkg)
  out2 <- morie_siu_translate_fr_to_en(cache_dir = dir2, progress = FALSE, model = c("ollama", "hosted"))
  expect_equal(unique(out2$case_number), "24-TST-003")
  expect_match(seen[[1]]$prompt, "fields into clear professional English\\.")
  expect_match(seen[[1]]$prompt, "\\[news_release_summary\\]\nCommuniqué\n")
  out3 <- morie_siu_translate(case_numbers = c("24-TST-001", "nope"), cache_dir = dir2, progress = FALSE)
  expect_match(seen[[length(seen)]]$prompt, "fields into clear professional Hindi\\.")
  expect_equal(nrow(out3), 1L)
  # a model that fails or answers prose skips the case and says so
  testthat::local_mocked_bindings(.siu_llm_call = function(model, prompt, ...) stop("offline"), .package = .sr_pkg)
  dir3 <- .sr_cache()
  testthat::local_mocked_bindings(.siu_corrections_dir = function() file.path(dir3, "user-data"), .package = .sr_pkg)
  msgs <- testthat::capture_messages(out4 <- morie_siu_translate(target_lang = "es", cache_dir = dir3))
  expect_match(msgs, "skip 24-TST-001: offline", all = FALSE)
  expect_equal(nrow(out4), 0L)
  testthat::local_mocked_bindings(.siu_llm_call = function(model, prompt, ...) "not json at all", .package = .sr_pkg)
  expect_equal(nrow(morie_siu_translate(target_lang = "es", cache_dir = dir3, progress = FALSE)), 0L)
})
