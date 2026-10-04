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
