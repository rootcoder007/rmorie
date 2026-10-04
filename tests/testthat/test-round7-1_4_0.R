# Fixes for the round-7 fresh-user findings (2026-10-03): one test per finding.

test_that("workbook headers wrapped inside a cell become one-line names", {
  df <- data.frame(a = 1, b = 2)
  names(df) <- c("Number of \nhospital stays", "  Percentage \r\n(total)  ")
  attr(df, "morie_sheet") <- "Data"
  got <- .morie_xlsx_one_line_names(df)
  expect_equal(names(got), c("Number of hospital stays", "Percentage (total)"))
  expect_equal(attr(got, "morie_sheet"), "Data")
  skip_if_not_installed("readxl")
  d <- withr::local_tempdir()
  dir.create(file.path(d, "x", "xl", "worksheets"), recursive = TRUE)
  dir.create(file.path(d, "x", "_rels"))
  dir.create(file.path(d, "x", "xl", "_rels"))
  main <- "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
  writeLines(paste0('<?xml version="1.0" encoding="UTF-8"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
                    '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
                    '<Default Extension="xml" ContentType="application/xml"/>',
                    '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
                    '<Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>',
                    '</Types>'), file.path(d, "x", "[Content_Types].xml"))
  writeLines(paste0('<?xml version="1.0" encoding="UTF-8"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
                    '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>',
                    '</Relationships>'), file.path(d, "x", "_rels", ".rels"))
  writeLines(paste0('<?xml version="1.0" encoding="UTF-8"?><workbook xmlns="', main, '" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
                    '<sheets><sheet name="Data" sheetId="1" r:id="rId1"/></sheets></workbook>'),
             file.path(d, "x", "xl", "workbook.xml"))
  writeLines(paste0('<?xml version="1.0" encoding="UTF-8"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
                    '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>',
                    '</Relationships>'), file.path(d, "x", "xl", "_rels", "workbook.xml.rels"))
  writeLines(paste0('<?xml version="1.0" encoding="UTF-8"?><worksheet xmlns="', main, '"><sheetData>',
                    '<row r="1"><c r="A1" t="inlineStr"><is><t>Region</t></is></c>',
                    '<c r="B1" t="inlineStr"><is><t xml:space="preserve">Number of &#10;sites (total)</t></is></c></row>',
                    '<row r="2"><c r="A2" t="inlineStr"><is><t>East</t></is></c><c r="B2"><v>3</v></c></row>',
                    '</sheetData></worksheet>'), file.path(d, "x", "xl", "worksheets", "sheet1.xml"))
  wb <- file.path(d, "w.xlsx")
  withr::with_dir(file.path(d, "x"), utils::zip(wb, c("[Content_Types].xml", "_rels/.rels", "xl/workbook.xml",
                                                    "xl/_rels/workbook.xml.rels", "xl/worksheets/sheet1.xml"),
                                                flags = "-q"))
  got <- .morie_xlsx_data_sheet(wb)
  expect_equal(names(got), c("Region", "Number of sites (total)"))
  expect_equal(got[[2]], 3)
})

.cap7 <- function(...) {
  txt <- character()
  status <- morie_cli(c(...), out = function(s) txt <<- c(txt, s))
  list(status = status, text = paste(txt, collapse = ""))
}

test_that("the tutorial refuses before the welcome when nobody can type", {
  local_mocked_bindings(.cli_stdin_closed = function() TRUE, .package = utils::packageName(environment(morie_cli)))
  r <- .cap7("tutorial")
  expect_equal(r$status, 2L)
  expect_match(r$text, "^the tutorial needs an interactive terminal")
  expect_false(grepl("Welcome|STEP 1", r$text))
})

test_that("the verify-pollution banner names the R command", {
  txt <- .envhealth_report_text(morie_verify_pollution("no2", demo = TRUE))
  expect_match(txt, "  rmorie verify-pollution -- NO2 -> all_cause_mortality", fixed = TRUE)
})

test_that("the key store no longer asks for jsonlite (the JSON is read natively)", {
  expect_false(any(grepl("jsonlite", deparse(.morie_keystore_require))))
})

test_that("a numeric matrix is clustered by rows, like the same data frame", {
  m <- as.matrix(iris[1:4])
  rownames(m) <- paste0("r", seq_len(nrow(m)))
  expect_no_warning(rm <- morie_cluster(m, k = 3))
  expect_no_warning(rd <- morie_cluster(iris[1:4], k = 3))
  expect_equal(rm$n_obs, 150L)
  expect_length(rm$assignments, 150L)
  expect_equal(rm$feature_names, colnames(m))
  expect_equal(unname(rm$assignments), unname(rd$assignments))
  expect_warning(morie_cluster(unname(m), k = 3), "no row names")
})

test_that("the native JSON reader is linear and decodes escapes and surrogate pairs", {
  big <- paste(rep("a \\\"q\\\" \\u00e9\\n", 5000), collapse = "")
  txt <- paste0('{"t":"', big, '","e":"\\ud83d\\ude00","n":[1,2.5,-3e2],"z":null,"ok":true}')
  r <- morie_fetch_json(txt, simplify = FALSE)
  expect_equal(r$t, strrep("a \"q\" é\n", 5000))
  expect_equal(r$e, "\U0001F600")
  expect_equal(unlist(r$n), c(1, 2.5, -300))
  expect_null(r$z)
  expect_true(r$ok)
  expect_error(morie_fetch_json('{"a":"open'), "unterminated string")
})

test_that("the native JSON writer escapes every control character", {
  js <- morie_json_stringify(list(s = "a\x1fb\x01c\td"))
  expect_equal(js, "{\"s\":\"a\\u001fb\\u0001c\\td\"}")
  expect_equal(morie_fetch_json(js)$s, "a\x1fb\x01c\td")
})

test_that("a weighted binomial survey fit is quiet and matches survey::svyglm on the same design", {
  set.seed(1)
  df <- data.frame(y = rbinom(40, 1, .5), x = rnorm(40), w = runif(40, .5, 2))
  df$x[1:10] <- NA
  expect_no_warning(nat <- .morie_svyglm_native(y ~ x, data = df, weights = df$w, family = stats::binomial()))
  skip_if_not_installed("survey")
  ref <- survey::svyglm(y ~ x, survey::svydesign(ids = ~1, weights = ~w, data = df), family = quasibinomial())
  expect_equal(unname(sqrt(diag(nat$vcov))), unname(sqrt(diag(stats::vcov(ref)))), tolerance = 1e-8)
  expect_equal(unname(nat$coefficients[, 1]), unname(stats::coef(ref)), tolerance = 1e-8)
})

test_that("omega follows psych::omega (Promax, fm = pa) and ignores which way items are keyed", {
  skip_if_not_installed("psych")
  skip_if_not_installed("GPArotation")
  set.seed(2)
  x <- as.data.frame(psych::sim.hierarchical(n = 5000, raw = TRUE)$observed)
  ps <- suppressWarnings(psych::omega(x, nfactors = 3, fm = "pa", rotate = "Promax", plot = FALSE))
  me <- morie_psymet_omega(x, nf = 3)
  expect_equal(me$hier, ps$omega_h, tolerance = 1e-3)
  expect_equal(me$total, ps$omega.tot, tolerance = 1e-3)
  xr <- x
  for (j in c(2, 5, 8)) xr[[j]] <- -xr[[j]]
  mr <- suppressMessages(morie_psymet_omega(xr, nf = 3))
  expect_equal(mr$hier, me$hier, tolerance = 1e-10)
  expect_equal(mr$total, me$total, tolerance = 1e-10)
})

test_that("pull of a missing own file says where it goes and writes nothing", {
  d <- withr::local_tempdir()
  withr::local_dir(d)
  withr::local_envvar(MORIE_DATA_DIR = file.path(d, "data"))
  txt <- character()
  st <- morie_cli(c("pull", "mapq"), out = function(s) txt <<- c(txt, s))
  expect_equal(st, 1L)
  expect_match(paste(txt, collapse = ""), "mapq is your own research file and it is not at .*TKARONTOMAPQ.xlsx: put it there")
  expect_false(file.exists(file.path(d, "mapq.csv")))
})

test_that("pull --all needs -y where nobody can confirm", {
  local_mocked_bindings(.cli_stdin_closed = function() TRUE)
  txt <- character()
  st <- morie_cli(c("pull", "--all"), out = function(s) txt <<- c(txt, s))
  expect_equal(st, 2L)
  expect_match(paste(txt, collapse = ""), "pass -y")
})

test_that("the SIU sex column maps to the same categories as the Python arm", {
  x <- c("Male", strrep("ual assault. the unit's jurisdiction covers ", 3),
         "female (Complainant #1) and male (Complainant #2)", "Transgender woman", "Female")
  expect_equal(.siu_an_sex(x), c("male", "unknown", "multiple persons", "transgender", "female"))
})


test_that("a title row above the header is dropped and stacked tables are cut", {
  df <- data.frame(
    "Table 1 Hospital stays" = c("Jurisdiction", "Canada", "Ontario", NA, "Table 1b", "Jurisdiction"),
    "...2" = c("Number of\nstays", "196717", "70000", NA, NA, "Number"),
    "...3" = c("Rate", "1.5", "2.0", NA, NA, "Rate"),
    check.names = FALSE, stringsAsFactors = FALSE)
  attr(df, "morie_sheet") <- "Table 1"
  got <- suppressMessages(.morie_xlsx_promote_header(df))
  expect_equal(names(got), c("Jurisdiction", "Number of stays", "Rate"))
  expect_equal(nrow(got), 2L)
  expect_equal(got[["Number of stays"]], c(196717L, 70000L))
  expect_equal(got$Rate, c(1.5, 2.0))
  expect_message(.morie_xlsx_promote_header(df), "left out")
  plain <- data.frame(a = 1:2, b = 3:4)
  expect_identical(.morie_xlsx_promote_header(plain), plain)
})

test_that("cached row counts come from a note, not from reading the table", {
  d <- withr::local_tempdir()
  f <- file.path(d, "t.rds")
  saveRDS(data.frame(x = 1:7), f)
  expect_equal(.morie_cache_rows(f), 7L)          # read once, noted
  expect_true(file.exists(paste0(f, ".n")))
  writeLines("7", paste0(f, ".n"))
  unlink(f); saveRDS(data.frame(x = 1:3), f)       # the table changed after the note
  Sys.setFileTime(paste0(f, ".n"), Sys.time() - 60)
  expect_equal(.morie_cache_rows(f), 3L)          # a stale note is not trusted
})

test_that("analyze siu fails when no analysis can run, names the file, and refuses bad JSON", {
  d <- withr::local_tempdir()
  wrong <- file.path(d, "wrong.csv")
  writeLines(c("a,b", "1,2"), wrong)
  json <- utils::capture.output(r <- suppressMessages(.cap7("analyze", "siu", sprintf('{"data":"%s"}', wrong))))
  expect_equal(r$status, 1L)
  expect_match(r$text, "every analysis in this subject failed")
  expect_match(paste(json, collapse = ""), "wrong.csv", fixed = TRUE)  # the per-analysis warnings name the file
  expect_false(grepl("SIU_by_case.csv", paste(json, collapse = ""), fixed = TRUE))
  b <- .cap7("analyze", "otis", "not json")
  expect_equal(b$status, 2L)
  expect_match(b$text, "must be a JSON object")
})

test_that("an empty causal grid keeps the documented columns", {
  g <- suppressMessages(suppressWarnings(morie_otis_causal_grid(data.frame(a = 1:3))))
  expect_equal(nrow(g), 0L)
  expect_equal(names(g), c("pair", "estimator", "n", "p_treat", "ate", "ate_se", "ate_pval",
                           "ci95_lo", "ci95_hi", "notes"))
})

test_that("a request that gets no response names the host and curl's reason", {
  expect_error(.morie_dataset_http_text("http://127.0.0.1:9/x"), "could not reach 127\\.0\\.0\\.1 \\(.+\\)")
})

test_that("models tells a rejected key from an unreachable gateway", {
  pkg <- utils::packageName(environment(morie_cli))
  withr::local_envvar(MORIE_HOSTED_KEY = "sk-test")
  local_mocked_bindings(morie_llm_hosted_models = function(...) structure(character(), default = NULL),
                        .morie_llm_hosted_rejected = function() TRUE, .package = pkg)
  r <- .cap7("models")
  expect_match(r$text, "rejected your key")
  expect_equal(r$status, 1L)
  local_mocked_bindings(.morie_llm_hosted_rejected = function() FALSE, .package = pkg)
  r <- .cap7("models")
  expect_match(r$text, "not reachable")
  expect_false(grepl("login again", r$text))
})

test_that("with a stored hosted key the fallback does not claim no provider is configured", {
  pkg <- utils::packageName(environment(morie_cli))
  local_mocked_bindings(.morie_llm_hosted_key = function() "sk-test", .package = pkg)
  txt <- .morie_llm_local_fallback("hi")
  expect_false(grepl("no LLM provider detected", txt, fixed = TRUE))
  expect_match(txt, "one is configured")
})


test_that("records with NULL and nested fields become one row each", {
  df <- .morie_dataset_records_to_df(list(
    list(name = "a", notes = NULL, resources = list(list(url = "u1"), list(url = "u2"))),
    list(name = "b", extra = "x")))
  expect_equal(nrow(df), 2L)
  expect_equal(names(df), c("name", "notes", "resources", "extra"))
  expect_true(is.na(df$notes[1]))
  expect_match(df$resources[1], "u2", fixed = TRUE)
  expect_equal(df$extra, c(NA, "x"))
})

test_that("a JSON service error is an error, not data", {
  pkg <- utils::packageName(environment(morie_cli))
  local_mocked_bindings(.morie_http_get_with_status = function(url, ...) {
    list(body = '{"message":"Service unavailable","errorCode":"service-unavailable"}', status_code = 200L, error = "")
  }, .package = pkg)
  expect_error(.morie_dataset_http_json("https://data.example.org/resource/x.json"),
               "data.example.org: Service unavailable \\(service-unavailable\\)")
  local_mocked_bindings(.morie_http_get_with_status = function(url, ...) {
    list(body = '{"message":"no such dataset"}', status_code = 404L, error = "")
  }, .package = pkg)
  expect_error(.morie_dataset_http_json("https://data.example.org/x"), "HTTP 404 from data.example.org: no such dataset")
  n <- 0L
  local_mocked_bindings(.morie_http_get_with_status = function(url, ...) {
    n <<- n + 1L
    if (n == 1L) list(body = "", status_code = 503L, error = "") else list(body = "[{\"a\":1}]", status_code = 200L, error = "")
  }, .package = pkg)
  local_mocked_bindings(Sys.sleep = function(...) NULL, .package = "base")
  expect_equal(.morie_dataset_http_json("https://data.example.org/y")$a, 1)  # one retry after a 503
})

test_that("validate_schema takes column_rule() lists and the named shorthand, and words a bad rule", {
  d <- data.frame(age = c(20, 30, -1))
  a <- validate_schema(d, list(column_rule("age", dtype = "numeric", min_val = 0)))
  b <- validate_schema(d, list(age = list(dtype = "numeric", min_val = 0)))
  expect_false(a$passed)
  expect_equal(a$errors, b$errors)
  expect_error(validate_schema(d, list(list(dtype = "numeric"))), "has no column name")
  expect_error(validate_schema(d, list(age = list(type = "numeric"))), "unused argument")
})

test_that("a missing Toronto resource id is refused in words", {
  expect_error(morie_datasets_toronto_open_ckan_resource(character()), "one CKAN resource id")
})

test_that("the ARSAU dictionary reads without readxl", {
  skip_if_not_installed("writexl")
  f <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(data.frame(Variable = c("year", "force_used"), `Data type` = c("integer", "character"),
                                 Notes = c("fiscal year", "type of force"), check.names = FALSE), f)
  local_mocked_bindings(requireNamespace = function(package, ...) package != "readxl", .package = "base")
  d <- morie_arsau_read_xlsx_dictionary(f)
  expect_equal(d$name, c("year", "force_used"))
  expect_equal(d$type, c("integer", "character"))
  expect_error(morie_arsau_read_xlsx_dictionary(f, sheet = 3), "sheet 3 is not in")
})


test_that("an ArcGIS layer reads one row per feature with either JSON reader", {
  pkg <- utils::packageName(environment(morie_cli))
  page <- paste0('{"features":[{"attributes":{"OBJECTID":1,"EVENT":"a"}},',
                 '{"attributes":{"OBJECTID":2,"EVENT":"b"}},{"attributes":{"OBJECTID":3,"EVENT":null}}],',
                 '"exceededTransferLimit":false}')
  local_mocked_bindings(.morie_read_text = function(url) if (grepl("returnCountOnly", url)) '{"count":3}' else page,
                        .package = pkg)
  withr::local_options(morie.quiet = TRUE)
  a <- suppressMessages(morie_fetch_arcgis("https://x.test/arcgis/rest/services/H/FeatureServer/0"))
  local_mocked_bindings(requireNamespace = function(package, ...) package != "jsonlite", .package = "base")
  b <- suppressMessages(morie_fetch_arcgis("https://x.test/arcgis/rest/services/H/FeatureServer/0"))
  for (d in list(a, b)) {
    expect_equal(dim(d), c(3L, 2L))
    expect_equal(d$OBJECTID, c(1, 2, 3))
    expect_equal(d$EVENT, c("a", "b", NA))
  }
  expect_equal(length(.morie_arcgis_feature_list(jsonlite::fromJSON(page)$features)), 3L)
})


test_that("the hypothesis-test table adjusts p-values across the whole family of tests", {
  out <- suppressWarnings(.run_frequentist_module_internal(make_canonical_cpads()))
  tab <- out$frequentist_hypothesis_tests
  p <- tab$p_value
  m <- sum(!is.na(p))
  expect_equal(m, 2L)
  expect_equal(tab$p_bonferroni, pmin(1, p * m))  # Bonferroni: every p times the family size
  o <- order(p)
  bh <- numeric(m)
  bh[o] <- rev(cummin(rev(pmin(1, p[o] * m / seq_len(m)))))  # BH step-up, written out
  expect_equal(tab$p_fdr_bh, bh)
  expect_equal(tab$sig_bonf, tab$p_bonferroni < 0.05)
  expect_equal(tab$sig_fdr, tab$p_fdr_bh < 0.05)
})


test_that("the effect-size table reports Cohen's h, the risk difference and the odds ratio", {
  es <- suppressWarnings(.run_frequentist_module_internal(make_canonical_cpads()))$frequentist_effect_sizes
  expect_true(all(c("cohens_h", "risk_difference", "odds_ratio") %in% names(es)))
  expect_equal(es$cohens_h, 2 * asin(sqrt(es$p1)) - 2 * asin(sqrt(es$p2)))
  expect_equal(es$risk_difference, es$p1 - es$p2)
  expect_equal(es$odds_ratio, (es$p1 / (1 - es$p1)) / (es$p2 / (1 - es$p2)))
})


test_that("verify and inspect of a module that writes no tables check nothing", {
  d <- withr::local_tempdir()
  utils::write.csv(data.frame(a = 1, b = 2), file.path(d, "power_summary.csv"), row.names = FALSE)
  for (verb in c("verify", "inspect")) {
    r <- .cap7(verb, d, "--module", "figures")
    expect_equal(r$status, 0L)
    expect_match(r$text, "figures writes no tables")
    r <- .cap7(verb, d, "--module", "descriptive-statistics")
    expect_equal(r$status, 1L)
    expect_match(r$text, "no table of descriptive-statistics")
  }
})

test_that("bad input to the six hanging functions stops at once with a worded error", {
  expect_error(BayesOutbreak(NULL), "`observed` must be a numeric vector")
  expect_error(BayesOutbreak(NA), "`observed` must be a numeric vector")
  expect_error(BayesOutbreak(c(1, 2, 3)), "the reference window needs at least 7")
  expect_error(BayesOutbreak(rep(NA_real_, 8)), "no observed counts")
  expect_error(DlaAggregate("abc"), "`n_particles` must be one whole number")
  expect_error(morie_dsp_ruler_fd(NA), "at least 4 values")
  expect_error(.bt_primes("abc"), "`m` must be one number")
  expect_error(.rfkprimes("abc"), "`k` must be one number")
  # valid calls keep their values
  expect_identical(.bt_primes(5), c(2L, 3L, 5L, 7L, 11L))
  expect_identical(.rfkprimes(3L), c(2L, 3L, 5L))
  expect_true(is.logical(BayesOutbreak(c(3, 5, 2, 4, 6, 3, 4, 12), w = 6, time_points = 8)$alarm))
})

test_that("DiffEnt with no grid and no callable pdf never opens a pdf device file", {
  d <- tempfile("diffent"); dir.create(d); old <- setwd(d); on.exit(setwd(old), add = TRUE)
  for (bad in list(NA, NULL, "abc", data.frame())) {
    expect_error(DiffEnt(bad), "give either a grid")
  }
  expect_length(list.files(d, all.files = TRUE, no.. = TRUE), 0L)
  xg <- seq(-6, 6, by = 0.01)
  h <- DiffEnt(x = xg, p = dnorm(xg))$entropy
  expect_equal(h, 0.5 * log2(2 * pi * exp(1)), tolerance = 1e-6)
})

test_that("morie_siu_refresh_manifest rejects a bad out_path before any request", {
  testthat::local_mocked_bindings(
    .siu_discover_max_drid = function(...) stop("network touched"),
    .siu_http_get_many_with_status = function(...) stop("network touched")
  )
  for (bad in list(NA, "abc", data.frame(), 1, file.path(tempfile(), "x.csv.gz"))) {
    expect_error(morie_siu_refresh_manifest(bad), "`out_path` must be NULL or one .csv.gz")
  }
})
