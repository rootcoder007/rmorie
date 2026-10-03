# SPDX-License-Identifier: AGPL-3.0-or-later
# Fixes for the 1.3.9 stress-test findings (2026-10-02): each test is one finding.

.pkg <- utils::packageName(environment(morie_cli))   # "rmorie", or "morie" in the morie r-package
.cap <- function(...) {
  txt <- character()
  status <- morie_cli(c(...), out = function(s) txt <<- c(txt, s))
  list(status = status, text = paste(txt, collapse = ""))
}

test_that("inspect/verify --module matches the hyphenated module name against the tables it wrote", {
  d <- withr::local_tempdir()
  utils::write.csv(data.frame(a = 1:2, b = c("x", "")), file.path(d, "power_summary.csv"), row.names = FALSE)
  utils::write.csv(data.frame(a = 1:2), file.path(d, "ebac_core.csv"), row.names = FALSE)
  expect_equal(basename(.cli_csv_files(d, "power-design")), "power_summary.csv")
  expect_equal(basename(.cli_csv_files(d, "power_design")), "power_summary.csv")
  expect_equal(basename(.cli_csv_files(d, "ebac-core")), "ebac_core.csv")
  msgs <- character()
  all_files <- .cli_csv_files(d, "logistic-models", out = function(s) msgs <<- c(msgs, s))
  expect_length(all_files, 2L)
  expect_match(msgs, "no table in .* names the module logistic-models")
  r <- .cap("inspect", d, "--module", "power-design")
  expect_equal(r$status, 0L)
  expect_match(r$text, "power_summary.csv")
  expect_false(grepl("ebac_core", r$text))
  expect_equal(.cap("verify", d, "--module", "power-design")$status, 0L)
})

test_that("verify: a column of blank cells is not an empty column; an all-NA column still is", {
  d <- withr::local_tempdir()
  f <- file.path(d, "power_ebac_endpoint_anchors.csv")
  utils::write.csv(data.frame(endpoint = c("a", "b"), missing_formula_inputs = c("", "")), f, row.names = FALSE)
  rep <- .cli_verify_csv(f)
  expect_true(rep$checks$no_empty_column)
  expect_true(rep$passed)
  g <- file.path(d, "bad.csv")
  utils::write.csv(data.frame(endpoint = c("a", "b"), v = c(NA, NA)), g, row.names = FALSE)
  expect_false(.cli_verify_csv(g)$checks$no_empty_column)
  expect_equal(.cap("verify", f)$status, 0L)
})

test_that("verify-pollution: --exposure-mean without --exposure-prevalence is a usage error, not prevalence 0", {
  r <- .cap("verify-pollution", "--pollutant", "no2", "--exposure-mean", "25")
  expect_equal(r$status, 2L)
  expect_match(r$text, "go together")
  ok <- .cap("verify-pollution", "--pollutant", "no2", "--exposure-mean", "25", "--exposure-prevalence", "0.5")
  expect_equal(ok$status, 0L)
  expect_match(ok$text, "STATUS: ok")
})

test_that("emissions --seconds must be a positive number; the sampler never outlives the verb", {
  r <- .cap("emissions", "--seconds", "abc")
  expect_equal(r$status, 2L)
  expect_match(r$text, "positive number, not 'abc'")
  expect_equal(.cap("emissions", "--seconds", "-5")$status, 2L)
  expect_equal(.cap("emissions", "--seconds", "0")$status, 2L)
})

test_that("sample: --n is the total for stratified draws, --per-stratum for N each, .weight announced and droppable", {
  d <- withr::local_tempdir()
  f <- file.path(d, "pop.csv")
  utils::write.csv(data.frame(region = rep(c("A", "B", "C", "D"), each = 50), x = 1:200), f, row.names = FALSE)
  o <- file.path(d, "s.csv")
  r <- .cap("sample", f, "--n", "20", "--method", "stratified", "--strata-col", "region", "--output", o)
  expect_equal(r$status, 0L)
  expect_match(r$text, "Sampled 20 rows using stratified")
  expect_match(r$text, "design weight")
  expect_equal(nrow(utils::read.csv(o)), 20L)
  r2 <- .cap("sample", f, "--n", "5", "--method", "stratified", "--strata-col", "region", "--per-stratum", "--output", o)
  expect_match(r2$text, "Sampled 20 rows")
  r3 <- .cap("sample", f, "--n", "10", "--output", o, "--no-weight")
  expect_false(grepl("design weight", r3$text))
  expect_equal(names(utils::read.csv(o)), c("region", "x"))
  r4 <- .cap("sample", f, "--n", "10", "--output", o)
  expect_true(".weight" %in% names(utils::read.csv(o)))
  expect_equal(.cap("sample", f, "--n", "500", "--method", "stratified", "--strata-col", "region")$status, 1L)
})

test_that("run-module rejects an unknown name before loading any data", {
  r <- .cap("run-module", "bogus-module")
  expect_equal(r$status, 1L)
  expect_match(r$text, "Unknown module: bogus-module")
  expect_false(grepl("Loaded", r$text))
})

test_that("usage exit codes agree: exec, explain, analyze all exit 2 without arguments; analyze errors exit 1", {
  expect_equal(.cap("exec")$status, 2L)
  expect_equal(.cap("explain")$status, 2L)
  expect_equal(.cap("analyze")$status, 2L)
  r <- .cap("analyze", "bogus_subject")
  expect_equal(r$status, 1L)
})

test_that("exec prints what the code printed, with a final newline", {
  r <- .cap("exec", "cat(1+1)")
  expect_equal(r$status, 0L)
  expect_equal(r$text, "2\n")
  expect_equal(.cap("exec", "6 * 7")$text, "[1] 42\n")
})

test_that("agent --help and perseus --help describe the percy verb", {
  for (v in c("agent", "perseus", "percy")) {
    r <- .cap(v, "--help")
    expect_equal(r$status, 0L, info = v)
    expect_match(r$text, "usage: rmorie percy", info = v)
  }
})

test_that("login --to-email needs --email", {
  r <- .cap("login", "--to-email")
  expect_equal(r$status, 2L)
  expect_match(r$text, "--to-email needs --email")
})

test_that("download-bootstrap without --survey lists the keys instead of downloading", {
  r <- .cap("download-bootstrap")
  expect_equal(r$status, 2L)
  expect_match(r$text, "usage: rmorie download-bootstrap --survey KEY\\|all")
  expect_match(r$text, "ocs22bt")
})

test_that("the tutorial stops with a message when stdin is closed; chat ends at EOF", {
  testthat::local_mocked_bindings(.package = .pkg, .cli_readline = function(prompt) NA_character_)
  r <- .cap("tutorial")
  expect_equal(r$status, 2L)
  expect_match(r$text, "interactive terminal")
  testthat::local_mocked_bindings(.package = .pkg, morie_llm_detect_provider = function() "hosted")
  expect_equal(.cap("chat")$status, 0L)
})

test_that("the local fallback text is marked, and ask/percy name the cause and exit 1", {
  fb <- .morie_llm_local_fallback("q")
  expect_true(isTRUE(attr(fb, "fallback")))
  expect_match(as.character(fb), "local-only mode")
  testthat::local_mocked_bindings(.package = .pkg,
    morie_llm_ask = function(prompt, context = NULL, model = NULL, ...) .morie_llm_local_fallback(prompt),
    morie_llm_detect_provider = function() "hosted",
    .morie_llm_api_base = function() NULL,
    .morie_llm_hosted_key = function() "k",
    .morie_llm_hosted_rejected = function() TRUE,
    .morie_llm_hosted_base = function() "https://llm.example")
  r <- .cap("percy", "what", "is", "a", "PAF")
  expect_equal(r$status, 1L)
  expect_match(r$text, "hosted key was rejected by https://llm.example")
  a <- .cap("ask", "hello")
  expect_equal(a$status, 1L)
  expect_match(a$text, "hosted key was rejected")
  testthat::local_mocked_bindings(.package = .pkg,
    .morie_llm_api_base = function() "http://127.0.0.1:9", .morie_llm_probe_api = function() FALSE)
  expect_match(.cap("ask", "hello")$text, "could not connect to your endpoint http://127.0.0.1:9")
})

test_that("morie_load_dataset() and morie_spillover_ht() validate their inputs in words", {
  expect_error(morie_load_dataset(NULL), "key must be a single dataset key")
  expect_error(morie_load_dataset(c("a", "b")), "single dataset key")
  pr <- matrix(0.3, 20, 3)
  expect_error(morie_spillover_ht(rnorm(19), rep(0:2, length.out = 20), pr), "y has 19, exposure 20")
})

test_that("morie_run_pipeline() without a project says what it needs", {
  withr::local_dir(withr::local_tempdir())
  expect_error(morie_run_pipeline(steps = NULL, verbose = FALSE), "no workflow scripts found")
})

test_that("morie_otis_causal_grid() skips a pair whose columns the frame lacks and runs the others", {
  df <- morie_otis_load()
  expect_false("NumberConsecutiveDays_Segregation" %in% names(df))
  expect_error(morie_otis_make_pair_c(df), "NumberConsecutiveDays_Segregation")
  expect_warning(g <- morie_otis_causal_grid(df, seed = 1L), "skipped")
  expect_true(is.data.frame(g))
  expect_true(all(grepl("^\\(a\\)|^\\(b\\)", g[[1L]])))
})

test_that("morie_install_extras() looks packages up without loading them", {
  expect_true(.morie_pkg_installed("stats"))
  expect_false(.morie_pkg_installed("no.such.package.xyz"))
  expect_false("no.such.package.xyz" %in% loadedNamespaces())
})

test_that("morie_emissions_track() writes nothing unless an output_dir is given", {
  withr::local_envvar(MORIE_EMISSIONS_OFFLINE = "1")
  d <- withr::local_tempdir()
  withr::local_dir(d)
  r <- morie_emissions_track(sum(sqrt(1:1000)), measure_power_secs = 0.2, country_iso_code = "CAN")
  expect_true(r$emissions_kg >= 0)
  expect_length(list.files(d), 0L)
  o <- file.path(d, "out")
  r2 <- morie_emissions_track(sum(sqrt(1:1000)), output_dir = o, measure_power_secs = 0.2, country_iso_code = "CAN")
  expect_true(file.exists(file.path(o, "emissions.csv")))
})

test_that("install_cli() writes a launcher that pins the library it was installed from", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  p <- install_cli(dir = d)
  lines <- readLines(p)
  lib <- normalizePath(dirname(system.file(package = .pkg)), winslash = "/")
  expect_true(any(grepl(lib, lines, fixed = TRUE)))
  expect_true(any(grepl("^R_LIBS=", lines)))
  expect_false(any(grepl("--vanilla", lines)))
  expect_true(any(grepl(paste0(.pkg, "::morie_cli()"), lines, fixed = TRUE)))
})

test_that("morie_siu_sanity_check() accepts the corpus's yes/no, true/false and lower-case gender values", {
  cols <- c("case_number", "date_of_incident_iso", "date_of_injury_iso", "date_siu_notified_iso",
            "date_of_director_decision_iso", "number_of_affected_persons", "number_of_civilian_witnesses",
            "number_of_subject_officials", "number_of_witness_officials", "age_affected",
            "number_of_officers_involved", "charges_recommended", "directors_decision_reasonable",
            "sex_gender_affected", "police_service", "narrative_summary", "supplemental_materials",
            "mental_health_or_race_indications")
  row <- as.list(setNames(rep("", length(cols)), cols))
  row$case_number <- "17-OVI-201"; row$date_of_incident_iso <- "2017-05-01"
  row$charges_recommended <- "False"; row$directors_decision_reasonable <- "no"
  row$sex_gender_affected <- "female (Complainant #1) and male (Complainant #2)"
  row$police_service <- "Toronto Police Service"
  row$narrative_summary <- paste(rep("The narrative of the incident as reviewed.", 4), collapse = " ")
  row$supplemental_materials <- "https://twitter.com/SIUOntario; sitemap"
  good <- as.data.frame(row, stringsAsFactors = FALSE)
  s <- morie_siu_sanity_check(good)
  expect_equal(s$issues_count, 0L)
  bad <- good
  bad$charges_recommended <- "I am satisfied that there are no reasonable grounds"
  bad$sex_gender_affected <- "unknown"
  b <- morie_siu_sanity_check(bad)
  expect_match(b$issues, "charges_recommended:not-Yes/No")
  expect_match(b$issues, "sex_gender_affected:bad-value")
})

test_that("the SIU LLM chain reads GEMINI_API_KEY and knows the hosted tier", {
  withr::local_envvar(GEMINI_API_KEY = "", GOOGLE_API_KEY = "")
  expect_error(.siu_llm_call_one("gemini", "p"), "GEMINI_API_KEY")
  expect_true("hosted" %in% names(.siu_llm_providers()))
  testthat::local_mocked_bindings(.package = .pkg, .morie_llm_hosted_key = function() NULL)
  expect_error(.siu_llm_call_one("hosted", "p"), "not logged in")
  expect_equal(formals(morie_siu_llm_extract)$model, quote(c("ollama", "hosted", "gemini")))
})

test_that("selftest names the reason for a skip", {
  skip_if_not(exists(".cli_selftest"))
  src <- deparse(body(.cli_selftest))
  expect_true(any(grepl("rmorie was built without liboqs/libsodium", src, fixed = TRUE)))
  expect_true(any(grepl("no CPADS CSV", src, fixed = TRUE)))
})

test_that("the examples with recycled inputs now use equal lengths", {
  for (t in c("Anc", "LmsFilt", "Msm109", "Msm115")) {
    rd <- file.path("..", "..", "man", paste0(t, ".Rd"))
    if (!file.exists(rd)) rd <- system.file("help", package = .pkg)
    skip_if(!file.exists(file.path("..", "..", "man", paste0(t, ".Rd"))))
    tf <- tempfile(fileext = ".R")
    tools::Rd2ex(file.path("..", "..", "man", paste0(t, ".Rd")), tf)
    expect_no_warning(source(tf, local = new.env()), message = "longer object length|not a multiple")
  }
})

test_that("multi-treatment matching skips the reference level whatever the column's type", {
  set.seed(1)
  df <- data.frame(treat3 = sample(rep(0:2, c(90, 55, 55))), x1 = rnorm(200), x2 = rnorm(200))
  expect_true(is.integer(df$treat3))
  expect_no_warning(r <- morie_matching_multi_treatment(df, "treat3", c("x1", "x2")))
  expect_equal(names(r), c("1", "2"))
  expect_equal(r[["1"]]$details$reference_group, 0)
  dfc <- df
  dfc$treat3 <- as.character(df$treat3)
  expect_equal(names(morie_matching_multi_treatment(dfc, "treat3", c("x1", "x2"))), c("1", "2"))
})

test_that("the launcher passes no explicit --args and morie_cli() tolerates one", {
  lines <- readLines(system.file("bin", "rmorie", package = .pkg))
  expect_false(any(grepl("--args", lines[!grepl("^#", lines)], fixed = TRUE)))
  txt <- character()
  expect_equal(morie_cli(c("--args", "version"), out = function(s) txt <<- c(txt, s)), 0L)
  expect_match(paste(txt, collapse = ""), .pkg)
})
