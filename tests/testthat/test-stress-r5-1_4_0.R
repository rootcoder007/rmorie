# SPDX-License-Identifier: AGPL-3.0-or-later
# Fixes for the round-5 fresh-user findings (2026-10-03): each test is one finding.

.cap <- function(...) {
  txt <- character()
  status <- morie_cli(c(...), out = function(s) txt <<- c(txt, s))
  list(status = status, text = paste(txt, collapse = ""))
}

test_that("survey GLM: rows dropped for a missing covariate keep their own weights (survey's SEs)", {
  skip_if_not_installed("survey")
  set.seed(3)
  n <- 200
  d <- data.frame(y = rbinom(n, 1, .4), x = rnorm(n), z = rnorm(n), w = runif(n, 1, 300))
  d$x[sample(n, 60)] <- NA
  f <- expect_silent(.morie_svyglm_native(y ~ x + z, d, d$w, stats::quasibinomial()))
  g <- survey::svyglm(y ~ x + z, survey::svydesign(ids = ~1, weights = ~w, data = d), family = quasibinomial())
  # the two IRLS stop at their own convergence tolerances: equal to that precision
  expect_equal(unname(sqrt(diag(f$vcov))), unname(sqrt(diag(stats::vcov(g)))), tolerance = 1e-6)
})

test_that("verify: a declared not_available row passes, a broken quote fails, a negative SE fails, totals print", {
  d <- withr::local_tempdir()
  ok <- file.path(d, "ebac_smote_status.csv")
  writeLines(c("smote_package,package_available,method,note,class_ratio_after",
               "\"smotefamily\",FALSE,\"not_available\",\"smotefamily not installed.\",NA"), ok)
  expect_true(.cli_verify_csv(ok)$passed)
  broken <- file.path(d, "broken.csv")
  writeLines(c("a,b", "1,\"unterminated"), broken)
  expect_false(.cli_verify_csv(broken)$checks$csv_parses)
  se <- file.path(d, "alt.csv")
  writeLines(c("x,se", "1,-1"), se)
  expect_false(.cli_verify_csv(se)$checks$se_nonnegative)
  r <- .cap("verify", d)
  expect_equal(r$status, 1L)
  expect_match(r$text, "3 tables: 1 PASS, 2 FAIL")
})

test_that("SMOTE sensitivity runs natively; a balanced outcome is skipped and says so", {
  set.seed(1)
  imb <- data.frame(y = c(rep(1L, 40), rep(0L, 160)), x = rnorm(200), g = sample(c("a", "b"), 200, TRUE))
  s <- .smote_sensitivity(imb, "y", c("x", "g"))
  expect_true(s$status$run_completed)
  expect_equal(s$status$class_ratio_after, 1)
  expect_true(all(c("term", "OR", "p_value") %in% names(s$or)) && nrow(s$or) == 3L)
  bal <- data.frame(y = rep(0:1, 100), x = rnorm(200), g = "a")
  expect_message(b <- .smote_sensitivity(bal, "y", "x"), "classes already balanced")
  expect_false(b$status$run_completed)
  expect_equal(nrow(b$or), 0L)
})

test_that("profile-dataset: an unknown column and a valueless flag are usage errors (rc 2)", {
  d <- withr::local_tempdir()
  f <- file.path(d, "p.csv")
  utils::write.csv(data.frame(`Prevalence (%)` = c(1.5, 2, 3), Subgroup = c("a", "b", "c"), empty = NA, one = 1,
                              check.names = FALSE), f, row.names = FALSE)
  r <- .cap("profile-dataset", f, "--treatment", "nosuchcol")
  expect_equal(r$status, 2L)
  expect_match(r$text, "not a column of")
  expect_equal(.cap("profile-dataset", f, "--outcome")$status, 2L)
  r <- .cap("profile-dataset", f, "--outcome", "Prevalence (%)", "--treatment", "Subgroup", "--suggest")
  expect_equal(r$status, 0L)
  expect_match(r$text, "Prevalence (%)", fixed = TRUE)
  expect_match(r$text, "group_comparison")
  p <- morie_dataset_profile(utils::read.csv(f, check.names = FALSE))
  expect_equal(p$columns$empty$suggested_role, "empty")
  expect_equal(p$columns$one$suggested_role, "constant")
})

test_that("sample: usage errors exit 2 in words; proportional strata add up to --n exactly", {
  d <- withr::local_tempdir()
  f <- file.path(d, "s.csv")
  utils::write.csv(data.frame(id = 1:100, g = rep(c("E", "N", "S", "W", "X", "Y", "Z"), length.out = 100),
                              school = rep(1:20, each = 5), size = 1:100), f, row.names = FALSE)
  r <- .cap("sample", f, "--n", "20", "--method", "nosuch")
  expect_equal(r$status, 2L)
  expect_match(r$text, "methods: srs, stratified, cluster, pps")
  expect_equal(.cap("sample", f, "--n", "20", "--method", "stratified")$status, 2L)
  r <- .cap("sample", f, "--n", "25", "--method", "cluster", "--cluster-col", "school")
  expect_equal(r$status, 1L)
  expect_match(r$text, "school has 20 clusters")
  expect_equal(.cap("sample", f, "--n", "101", "--method", "pps", "--size-col", "size")$status, 1L)
  s <- suppressMessages(morie_stratified_sample(utils::read.csv(f), "g", 34L, proportional = TRUE))
  expect_equal(nrow(s), 34L)
})

test_that("emissions: --country takes ISO-2, an unwritable directory is named, --country without a value is rc 2", {
  expect_equal(.cap("emissions", "--country")$status, 2L)
  # a directory inside a regular file cannot be created on any OS (/proc/... is creatable on Windows)
  blocker <- withr::local_tempfile()
  writeLines("x", blocker)
  bad <- file.path(blocker, "out")
  r <- .cap("emissions", "--seconds", "1", "--output-dir", bad, "--country", "FRA")
  expect_equal(r$status, 1L)
  expect_match(r$text, paste("cannot write to", bad), fixed = TRUE)
  expect_equal(.emissions_iso2_to_iso3("FR"), "FRA")
  expect_match(.cap("help")$text, "--country ISO3")
})

test_that("exec, ask, provider and login: wording and exit codes", {
  r <- .cap("exec", "--file", "nosuch-file.R")
  expect_equal(r$status, 1L)
  expect_match(r$text, "nosuch-file.R: no such file")
  expect_equal(.cap("ask")$status, 2L)
  expect_equal(.cap("provider", "set", "--key", "k")$status, 2L)
  local_mocked_bindings(.cli_readline = function(prompt) "", .package = utils::packageName(environment(morie_cli)))
  r <- .cap("login", "--token")
  expect_equal(r$status, 2L)
  expect_match(r$text, "--token needs a value")
  local_mocked_bindings(.cli_readline = function(prompt) NA_character_, .package = utils::packageName(environment(morie_cli)))
  expect_equal(.cap("login", "--token")$status, 2L)  # stdin closed
  expect_error(morie_llm_login(email = "notanemail"), "'notanemail' is not an email address")
})

test_that("a missing file named in a failed write/read is reported by its path, without a leaked warning", {
  r <- .cap("exec", "x <- readLines('/no/such/morie-file')")
  expect_equal(r$status, 1L)
  expect_match(r$text, "cannot open /no/such/morie-file")
})

test_that("list-modules describes each module and --outputs lists its files", {
  r <- .cap("list-modules", "--outputs")
  expect_match(r$text, "power-design .*power")
  expect_match(r$text, "power_summary.csv")
})

test_that("explain covers every module table and reads Windows paths", {
  miss <- character()
  for (m in names(.morie_module_outputs)) for (f in .morie_module_outputs[[m]]) {
    if (grepl("\\.csv$", f) && grepl("^No registered", explain_file(f))) miss <- c(miss, f)
  }
  expect_equal(miss, character())
  d <- withr::local_tempdir()
  p <- file.path(d, "logistic_odds_ratios.csv")
  writeLines(c("term,OR,p_value,weird_col", "x,1.2,0.04,3"), p)
  txt <- explain_file(p)
  expect_match(txt, "odds ratio = exp(coefficient)", fixed = TRUE)
  expect_match(txt, "weird_col")
  expect_false(grepl("^No registered", explain_file("C:\\Users\\me\\out\\power_summary.csv")))
})

test_that("analyze: OTIS columns arrive in snake_case; SIU ages, services and years are cleaned", {
  x <- .otis_snake_names(data.frame(EndFiscalYear = 1, UniqueIndividual_ID = 1, Region_AtTimeOfPlacement = 1,
                                    MentalHealth_Alert = 1, AgeCategory = 1))
  expect_equal(names(x), c("end_fiscal_year", "unique_individual_id", "region_at_time_of_placement",
                           "mental_health_alert", "age_category"))
  s <- .siu_an_clean(data.frame(police_service = c("Ontario Provincial Police (OPP)", "Ontario Provincial Police"),
                                date_of_incident_iso = c("1982-09-05", "July 18, 2021")))
  expect_equal(unique(s$police_service), "Ontario Provincial Police")
  expect_equal(s$date_of_incident_iso, c("", "2021-07-18"))
  d <- morie_siu_demographics(data.frame(sex_gender_affected = "male", age_affected = c(30, 40, 1985)))
  lines <- vapply(d$summary_lines, function(l) paste(unlist(l), collapse = "="), "")
  expect_true("Ages outside 0-110 (left out)=1" %in% lines)
  expect_true("Age range=30-40" %in% lines)
  expect_equal(.siu_an_charge_flag(c("no criminal charges warranted", "True", "", "No Charges to Issue")),
               c(FALSE, TRUE, NA, FALSE))
})

test_that("ingest: an unknown SIU report id and an unknown citation fail in words", {
  pkg <- utils::packageName(environment(morie_cli))
  local_mocked_bindings(morie_siu_fetch_report = function(id) "<title>Director&#039;s Report Details, Case Number: </title>",
                        morie_ingest_a2aj_fetch = function(...) NULL, .package = pkg)
  r <- .cap("ingest", "siu", "--report-id", "99999999", "--out", withr::local_tempdir())
  expect_equal(r$status, 1L)
  expect_match(r$text, "no SIU report with id 99999999")
  r <- .cap("ingest", "a2aj", "fetch", "9999 SCC 999")
  expect_equal(r$status, 1L)
  expect_match(r$text, "no decision found for '9999 SCC 999'")
})

test_that("the SIU parser reads the report body, not the table of contents, and ordinal dates", {
  html <- paste0(
    "<ul><li><a>The Investigation</a></li><li><a>Incident Narrative</a></li><li><a>Evidence</a></li></ul>",
    "<h2>The Investigation</h2><p>At approximately 11:46 a.m. on August 3rd, 2017, the Guelph Police ",
    "Service (GPS) notified the SIU of an injury.</p>",
    "<h2>Incident Narrative</h2><p>Just prior to 10:00 a.m. on August 2nd, 2017, three men attempted a robbery.</p>",
    "<h2>Evidence</h2><p>none</p>")
  r <- morie_siu_parse_report(html, engine = "native")
  expect_equal(r[["police_service"]], "Guelph Police Service")
  expect_equal(r[["date_siu_notified_iso"]], "2017-08-03")
  expect_equal(r[["date_of_incident_iso"]], "2017-08-02")
  expect_equal(.siu_core_to_iso_date("Sept. 5, 2019"), "2019-09-05")
  expect_equal(.siu_core_to_iso_date("3 August 2017"), "2017-08-03")
})

test_that("SIU corrections persist in the user data directory and win over the shipped table", {
  withr::local_envvar(R_USER_DATA_DIR = withr::local_tempdir())
  p <- morie_siu_record_correction("20-OFD-082", "number_of_subject_officials", "3 SO")
  expect_true(startsWith(normalizePath(p), normalizePath(Sys.getenv("R_USER_DATA_DIR"))))
  ov <- .siu_load_canonical_overrides()
  hit <- ov[ov$case_number == "20-OFD-082" & ov$field == "number_of_subject_officials", ]
  expect_equal(hit$verified_value, "3 SO")
  expect_error(morie_siu_record_correction("20-OFD-082", "nosuchfield", "x"), "is not an SIU field")
})

test_that("eBAC uses 0.6 fl oz of ethanol per standard drink", {
  expect_equal(morie_calculate_ebac(4, 180, 2, 0.73), (2.4 * 5.14) / (180 * 0.73) - 0.03, tolerance = 1e-12)
})

test_that("verify-pollution reads a NAPS pull: NO2 in ppb is converted to ug/m3", {
  d <- withr::local_tempdir()
  f <- file.path(d, "naps.csv")
  utils::write.csv(data.frame(station_id = 1, value = c(5, 10), unit = "ppb"), f, row.names = FALSE)
  expect_message(r <- morie_verify_pollution("no2", exposure_csv = f, reference = 10), "x 1.88")
  expect_equal(r$inputs$exposure_mean, 7.5 * 1.88)
  writeLines(c("station_id,value,unit", "1,0.5,ppm"), f)
  expect_equal(morie_verify_pollution("no2", exposure_csv = f)$status, "error")
})

test_that("omega scores reverse-keyed items the other way round", {
  set.seed(7)
  f <- rnorm(400)
  X <- sapply(1:6, function(j) 0.8 * f + 0.6 * rnorm(400))
  colnames(X) <- paste0("i", 1:6)
  Y <- X
  Y[, c(2, 5)] <- -Y[, c(2, 5)]
  a <- morie_psymet_omega(X)
  expect_message(b <- morie_psymet_omega(Y), "i2, i5")
  expect_equal(b$total, a$total, tolerance = 1e-9)
  expect_equal(b$hier, a$hier, tolerance = 1e-9)
})

test_that("multi-treatment matching refuses a continuous column", {
  set.seed(1)
  d <- data.frame(t = sample(0:2, 300, TRUE), x = rnorm(300))
  d$y <- d$t + d$x + rnorm(300)
  expect_error(morie_matching_multi_treatment(d, "y", "x"), "300 distinct values")
})

test_that("ML-KEM, ML-DSA and SLH-DSA work without liboqs (rmoriebricklayer's FIPS code)", {
  k <- morie_crypto_mlkem768_keygen()
  e <- morie_crypto_mlkem768_encaps(k$pk)
  expect_identical(morie_crypto_mlkem768_decaps(k$sk, e$ct), e$shared_secret)
  s <- morie_crypto_mldsa65_keygen()
  sig <- morie_crypto_mldsa65_sign(s$sk, charToRaw("m"))
  expect_true(morie_crypto_mldsa65_verify(s$pk, charToRaw("m"), sig))
  expect_false(morie_crypto_mldsa65_verify(s$pk, charToRaw("n"), sig))
  h <- morie_crypto_slhdsa_keygen()
  expect_true(morie_crypto_slhdsa_verify(h$pk, "m", morie_crypto_slhdsa_sign(h$sk, "m")))
  expect_true(all(morie_crypto_pqc_inventory()$available[1:3]))
})

test_that("install_extras(ask = TRUE) off a terminal does not pretend the user declined", {
  skip_if(interactive())
  expect_message(r <- morie_install_extras(which = "smotefamily_not_a_pkg", ask = TRUE), "no terminal to ask on")
  expect_length(r$installed, 0L)
})

test_that("ask with a model the hosted tier does not list says so in one line", {
  pkg <- utils::packageName(environment(morie_cli))
  local_mocked_bindings(.morie_llm_hosted_key = function() "k", .morie_llm_hosted_base = function() "https://h.invalid",
                        morie_llm_hosted_models = function(...) c("a", "b"),
                        morie_llm_request_completion = function(...) stop("404"),
                        .morie_llm_gemini_key = function() NULL, .morie_llm_api_base = function() NULL,
                        .morie_llm_openai_key = function() NULL, .package = pkg)
  expect_error(morie_llm_ask("hi", model = "nosuch", provider = "hosted"), "no model 'nosuch'")
})

test_that("a large workbook is streamed to its first sheet; a small one picks the data sheet over a cover sheet", {
  skip_if(!nzchar(Sys.which("zip")))
  d <- withr::local_tempdir()
  dir.create(file.path(d, "x", "xl", "worksheets"), recursive = TRUE)
  dir.create(file.path(d, "x", "xl", "_rels"))
  writeLines('<workbook xmlns:r="r"><sheets><sheet name="Data" sheetId="1" r:id="rId1"/></sheets></workbook>',
             file.path(d, "x", "xl", "workbook.xml"))
  writeLines('<Relationships><Relationship Id="rId1" Target="worksheets/sheet1.xml"/></Relationships>',
             file.path(d, "x", "xl", "_rels", "workbook.xml.rels"))
  writeLines("<sst><si><t>name</t></si><si><t>caf&#233;</t></si></sst>", file.path(d, "x", "xl", "sharedStrings.xml"))
  writeLines(paste0('<worksheet><sheetData><row r="1"><c r="A1" t="s"><v>0</v></c><c r="B1" t="inlineStr"><is><t>n</t></is></c></row>',
                    '<row r="2"><c r="A2" t="s"><v>1</v></c><c r="B2"><v>3</v></c></row><row r="3"/></sheetData></worksheet>'),
             file.path(d, "x", "xl", "worksheets", "sheet1.xml"))
  wb <- file.path(d, "w.xlsx")
  withr::with_dir(file.path(d, "x"), utils::zip(wb, c("xl/workbook.xml", "xl/_rels/workbook.xml.rels",
                                                    "xl/sharedStrings.xml", "xl/worksheets/sheet1.xml"), flags = "-q"))
  withr::local_envvar(MORIE_CACHE_DIR = withr::local_tempdir())
  got <- .morie_xlsx_stream(wb)
  expect_equal(names(got), c("name", "n"))
  expect_equal(got$n, 3L)
  expect_equal(got$name, "caf\u00e9")
})

test_that("SIU dates must exist; the analyses count one row per case; sex has a few clean categories", {
  expect_equal(.siu_core_to_iso_date("February 30, 2019"), "")
  expect_equal(.siu_core_to_iso_date("February 29, 2020"), "2020-02-29")
  d <- data.frame(case_number = c("17-OVI-201", "17-OVI-201", "", "18-TCI-001"), `_language` = c("fr", "en", "unknown", "en"),
                  police_service = "Toronto Police Service", check.names = FALSE)
  s <- .siu_an_clean(d)
  expect_equal(nrow(s), 2L)
  expect_equal(s[["_language"]][s$case_number == "17-OVI-201"], "en")
  expect_equal(.siu_an_sex(c("Male", "boy", "female (Complainant #1) and male (Complainant #2)", "trans female",
                             strrep("ual assault complaint text ", 4), NA)),
               c("male", "male", "multiple persons", "transgender", "unknown", "unknown"))
})

test_that("R-backed run-modules prints one line per module and where the tables went", {
  pkg <- utils::packageName(environment(morie_cli))
  local_mocked_bindings(morie_run_morie_module = function(module_name, cpads_csv, output_dir = NULL) {
    utils::write.csv(data.frame(x = 1), file.path(output_dir, paste0(gsub("-", "_", module_name), ".csv")), row.names = FALSE)
    list(t = data.frame(x = 1))
  }, .package = pkg)
  d <- withr::local_tempdir()
  expect_message(r <- .cap("run-modules", "--modules", "power-design,data-wrangling", "--output-dir", d), "\\[2/2\\] data-wrangling done")
  expect_match(r$text, "2 new files")
})

test_that("eBAC refuses an impossible weight or a negative count", {
  expect_error(morie_calculate_ebac(5, -150, 2, 0.73), "weight_lbs must be > 0")
  expect_error(morie_calculate_ebac(-5, 150, 2, 0.73), "cannot be negative")
})
