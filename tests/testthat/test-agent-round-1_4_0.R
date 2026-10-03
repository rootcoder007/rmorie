# The findings of the 1.4.0 fresh-user test agents (l14, 2026-10-03), each with its fix.
.pkg <- utils::packageName(environment(morie_cli))

.cap <- function(...) {
  buf <- character()
  status <- withCallingHandlers(
    morie_cli(c(...), out = function(x) buf <<- c(buf, x)),
    message = function(m) invokeRestart("muffleMessage")
  )
  list(status = status, text = paste(buf, collapse = ""))
}

test_that("the dataset catalog has the 71 keys of the Python package, NAPS and CCHS included", {
  cat_df <- morie_dataset_catalog()
  expect_equal(nrow(cat_df), 71L)
  expect_true(all(c("naps-no2-on-2023", "naps-pm25-ca-2023", "naps-co-on-2023", "cchs22", "siumanifest") %in% cat_df$key))
  naps <- cat_df[cat_df$source == "naps", ]
  expect_equal(nrow(naps), 24L)
  expect_true(all(naps$fetcher == "morie_fetch_naps"))
  args <- .morie_parse_fetcher_args(naps$fetcher_args[naps$key == "naps-no2-on-2023"])
  expect_equal(args, list(pollutant = "no2", year = 2023L, province = "ON"))
  expect_equal(nrow(morie_list_datasets()), 71L + sum(morie_list_datasets()$type == "hosted"))
})

test_that("a NAPS hourly file parses to long rows and -999 hours are dropped", {
  lines <- c(
    "Hourly data // Donnees horaires",
    "",
    "Pollutant//Polluant,NAPS ID//Identifiant SNPA,City//Ville,Province/Territory//Province/Territoire,Latitude//Latitude,Longitude//Longitude,Date//Date,H01//H01,H02//H02,H03//H03",
    "NO2,60101,Ottawa,ON,45.4,-75.7,2023-01-01,12,-999,14.5",
    "NO2,70119,Montreal,QC,45.5,-73.6,2023-01-01,-999,3,"
  )
  out <- .morie_parse_naps_hourly(lines, "no2")
  expect_equal(nrow(out), 3L)
  expect_equal(out$value[out$station_id == "60101"], c(12, 14.5))
  expect_equal(out$datetime_local[out$station_id == "60101"], c("2023-01-01 01:00", "2023-01-01 03:00"))
  expect_equal(unique(out$unit), "ppb")
  expect_equal(out$province[out$station_id == "70119"], "QC")
  expect_error(.morie_parse_naps_hourly(c("x", "y"), "no2"), "Pollutant//Polluant")
})

test_that("morie_fetch_naps() downloads one file, parses it and filters a province", {
  lines <- c(
    "Pollutant//Polluant,NAPS ID//Identifiant SNPA,City//Ville,Province/Territory//Province/Territoire,Latitude//Latitude,Longitude//Longitude,Date//Date,H01//H01",
    "PM25,60101,Ottawa,ON,45.4,-75.7,2023-06-01,8",
    "PM25,70119,Montreal,QC,45.5,-73.6,2023-06-01,9"
  )
  testthat::local_mocked_bindings(
    .morie_dl = function(url, dest, headers = NULL, label = basename(dest), ...) {
      expect_match(url, "PM25_2023.csv")
      expect_true(nzchar(headers[["User-Agent"]]))
      writeLines(lines, dest)
      dest
    },
    .package = .pkg
  )
  on <- morie_fetch_naps(2023, "pm25", province = "ON")
  expect_equal(on$station_id, "60101")
  expect_equal(on$unit, "ug/m3")
  all_prov <- morie_fetch_naps(2023, "pm25")
  expect_equal(nrow(all_prov), 2L)
  expect_error(morie_fetch_naps(2023, "lead"), "pollutant must be one of")
})

test_that("rmorie list-datasets names a route for every key and ends with the footer", {
  r <- .cap("list-datasets")
  expect_equal(r$status, 0L)
  expect_match(r$text, "naps-no2-on-2023 .*ECCC NAPS")
  expect_match(r$text, "cchs22 .*Statistics Canada")
  expect_match(r$text, "mapq .*own file: data/datasets/vsr/TKARONTOMAPQ.xlsx")
  expect_match(r$text, "siumanifest .*rmoriedata \\(CRAN\\)")
  expect_match(r$text, "hibsa .*health-infobase.canada.ca \\(or data.rmorie.com\\)")
  expect_match(r$text, "71 keys: 70 download from their portal, rmoriedata or data.rmorie.com on first use; 1 is your own research file")
  expect_match(r$text, "Curated tables at data.rmorie.com")
})

test_that("inspect, verify and pull without a path print usage and exit 2", {
  for (verb in c("inspect", "verify", "pull")) {
    r <- .cap(verb)
    expect_equal(r$status, 2L, info = verb)
    expect_match(r$text, paste0("usage: rmorie ", verb))
  }
})

test_that("emissions refuses Inf and verify-pollution refuses non-numeric flags", {
  r <- .cap("emissions", "--seconds", "Inf")
  expect_equal(r$status, 2L)
  expect_match(r$text, "positive number, not 'Inf'")
  r <- .cap("verify-pollution", "--pollutant", "no2", "--exposure-mean", "abc", "--exposure-prevalence", "0.5")
  expect_equal(r$status, 2L)
  expect_match(r$text, "--exposure-mean must be a number")
})

test_that("verify-pollution uses one reference concentration and counts avoided deaths over the exposed share", {
  r <- morie_verify_pollution(pollutant = "no2", exposure_mean = 20, exposure_prevalence = 0.5,
                              reference = 5.8, baseline_rate = 500, population = 1e6)
  rr <- exp(log(1.02) * (20 - 5.8) / 10)
  paf <- 0.5 * (rr - 1) / (0.5 * (rr - 1) + 1)
  expect_equal(r$pipeline$burden$reference_conc, 5.8)
  expect_equal(r$pipeline$burden$attributable_cases, paf * 500 / 1e5 * 1e6, tolerance = 1e-9)
  expect_equal(r$pipeline$displaced$deaths_displaced, 500 / 1e5 * 1e6 * 0.5 * (1 - 1 / rr), tolerance = 1e-9)
})

test_that("generate-template takes a positional module, fills the description and never clobbers", {
  d <- withr::local_tempdir()
  r <- .cap("generate-template", "power-design", "--out", file.path(d, "p.md"))
  expect_equal(r$status, 0L)
  txt <- paste(readLines(file.path(d, "p.md")), collapse = "\n")
  expect_false(grepl("[REPLACE_WITH_MODULE_DESCRIPTION]", txt, fixed = TRUE))
  r2 <- .cap("generate-template", "power-design", "--out", file.path(d, "p.md"))
  expect_equal(r2$status, 1L)
  expect_match(r2$text, "already exists")
  r3 <- .cap("generate-template", "--module", "nosuchmodule", "--out", file.path(d, "q.md"))
  expect_equal(r3$status, 1L)
  expect_match(r3$text, "unknown module: nosuchmodule")
  expect_false(file.exists(file.path(d, "q.md")))
})

test_that("exec sees the package's functions without a prefix", {
  r <- .cap("exec", "nrow(morie_list_morie_modules())")
  expect_equal(r$status, 0L)
  expect_match(r$text, "23")
})

test_that("the launcher pins its library with .libPaths() so ~/.Renviron cannot replace it", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  target <- suppressMessages(install_cli(dir = dir))
  txt <- paste(readLines(target), collapse = "\n")
  expect_match(txt, ".libPaths(c(", fixed = TRUE)
  expect_false(grepl("export R_LIBS", txt, fixed = TRUE))
})

test_that("the synthetic-CPADS notice is given once per session", {
  skip_if_not_installed("rmoriedata")
  withr::local_options(morie.cpads_synthetic_noticed = NULL)
  m1 <- capture.output(invisible(morie_load_cpads_data()), type = "message")
  m2 <- capture.output(invisible(morie_load_cpads_data()), type = "message")
  expect_true(any(grepl("synthetic frame", m1)))
  expect_false(any(grepl("synthetic frame", m2)))
})

test_that("edit refuses a terminal editor when stdin is not a terminal", {
  skip_if(isatty(stdin()))
  withr::local_envvar(VISUAL = "", EDITOR = "nano")
  d <- withr::local_tempdir()
  r <- .cap("edit", file.path(d, "f.txt"))
  expect_equal(r$status, 1L)
  expect_match(r$text, "terminal editor")
})


# ---- round 3: the rmorie agent's remaining findings

test_that("verify fails impossible p-values and reversed intervals", {
  d <- withr::local_tempdir()
  f <- file.path(d, "bad.csv")
  utils::write.csv(data.frame(p_value = c(1.5, -0.1, 0.2), ci_lower = c(1, 2, 3), ci_upper = c(2, 1, 4)), f, row.names = FALSE)
  r <- .cli_verify_csv(f)
  expect_false(r$passed)
  expect_false(r$checks$p_values_in_unit_interval)
  expect_false(r$checks$ci_bounds_ordered)
  g <- file.path(d, "good.csv")
  utils::write.csv(data.frame(p_value = c(0.5, 0.01), ci_lower = c(1, 2), ci_upper = c(2, 3)), g, row.names = FALSE)
  expect_true(.cli_verify_csv(g)$passed)
})

test_that("--module selects the module's own tables and rejects an unknown module", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "power-design"))
  for (f in c("power-design/power_summary.csv", "descriptive_statistics_summary.csv", "ebac_final_weighted_descriptives.csv", "otis_descriptives.csv")) {
    utils::write.csv(data.frame(a = 1), file.path(d, f), row.names = FALSE)
  }
  expect_equal(basename(.cli_csv_files(d, "descriptive-statistics")), "descriptive_statistics_summary.csv")
  expect_equal(basename(.cli_csv_files(d, "power-design")), "power_summary.csv")
  # no ebac_core table here: the first word "ebac_" is the fallback prefix, never a substring (otis_descriptives stays out)
  expect_equal(basename(.cli_csv_files(d, "ebac-core")), "ebac_final_weighted_descriptives.csv")
  r <- .cap("verify", d, "--module", "nosuch")
  expect_equal(r$status, 1L)
  expect_match(r$text, "unknown module: nosuch")
})

test_that("a stratum shorter than its allocation carries weight 1 under --per-stratum", {
  s <- morie_stratified_sample(data.frame(g = rep(c("W", "E"), c(5, 50)), x = 1:55), "g", 6)
  expect_equal(unique(s$.weight[s$g == "W"]), 1)
  expect_equal(unique(s$.weight[s$g == "E"]), 50 / 6)
})

test_that("run-modules rejects an unknown module before loading data, and sample --seed must be a number", {
  r <- .cap("run-modules", "--modules", "power-design,ghost")
  expect_equal(r$status, 1L)
  expect_match(r$text, "Unknown module: ghost")
  d <- withr::local_tempdir()
  f <- file.path(d, "t.csv")
  utils::write.csv(data.frame(x = 1:10), f, row.names = FALSE)
  r <- .cap("sample", f, "--n", "2", "--seed", "abc")
  expect_equal(r$status, 2L)
  expect_match(r$text, "--seed must be an integer")
})

test_that("morie_run_pipeline() refuses a project_root that does not exist", {
  expect_error(morie_run_pipeline(project_root = file.path(tempdir(), "no-such-project")), "does not exist")
})

test_that("the family status reports the rmorie launcher, never a separate rmorie-cli", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  suppressMessages(install_cli(dir = d))
  withr::local_envvar(PATH = paste(d, Sys.getenv("PATH"), sep = .Platform$path.sep))
  txt <- paste(utils::capture.output(b <- morie_bricklayer(check = TRUE)), collapse = "\n")
  expect_true(isTRUE(b[["rmorie_launcher"]]))
  expect_false(grepl("rmorie-cli|proprietary", txt))
  expect_match(txt, "rmorie launcher")
})

test_that("SIU sanity checks accept the reviewed corpus's bare counts and reasoning text", {
  df <- data.frame(
    case_number = c("17-OVI-201", "18-OCI-001"),
    number_of_officers_involved = c("1", "2 SO 1 WO"),
    directors_decision_reasonable = c("On the evidence before me, I find reasonable grounds to believe the force was justified.", "no"),
    charges_recommended = c("no charges", "maybe"),
    sex_gender_affected = c("male", "female"),
    narrative_summary = c(strrep("x", 120), strrep("y", 120)),
    stringsAsFactors = FALSE
  )
  r <- morie_siu_sanity_check(df)
  expect_false(grepl("number_of_officers_involved", r$issues[r$case_number == "17-OVI-201"]))
  expect_false(grepl("directors_decision_reasonable", r$issues[r$case_number == "17-OVI-201"]))
  expect_false(grepl("charges_recommended", r$issues[r$case_number == "17-OVI-201"]))
  expect_match(r$issues[r$case_number == "18-OCI-001"], "charges_recommended:not-Yes/No")
  expect_false(any(grepl("police_service", r$issues)))
})

test_that("HTML entities in SIU text become characters", {
  expect_equal(.siu_html_to_text("Director&#039;s &eacute;t&#x00e9; <b>x</b>"), "Director's été x")
})

test_that("emissions --country names an unknown code and the OTIS grid says when nothing can be built", {
  r <- .cap("emissions", "--seconds", "0.2", "--country", "zz", "--no-capsule", "--output-dir", tempfile("em"))
  expect_match(r$text, "--country ZZ: not an ISO-3 code")
  expect_false(grepl("\\(NA\\)", r$text))
  expect_message(g <- suppressWarnings(morie_otis_causal_grid(data.frame(a = 1:3))), "no treatment-outcome pair")
  expect_equal(nrow(g), 0L)
})


test_that("hyphenated catalog keys resolve as written, emissions refuses a day-long run, templates read as a sentence", {
  expect_equal(.fuzzy_match_key("naps-co-on-2023"), "naps-co-on-2023")
  expect_equal(.fuzzy_match_key("NAPS-CO-ON-2023"), "naps-co-on-2023")
  expect_equal(.fuzzy_match_key("ocp21"), "ocp21")
  expect_equal(.cap("emissions", "--seconds", "1e9")$status, 2L)
  d <- withr::local_tempdir()
  expect_equal(.cap("generate-template", "power-design", "--out", file.path(d, "p.md"))$status, 0L)
  txt <- paste(readLines(file.path(d, "p.md")), collapse = "\n")
  expect_match(txt, "which provides [a-z]")
  expect_match(txt, "rmorie generate-template")
})

test_that("round 4: a small stratified total names the strata it leaves empty", {
  df <- data.frame(region = rep(c("N", "S", "E", "W"), c(50L, 30L, 15L, 5L)), x = seq_len(100L))
  expect_message(s <- morie_stratified_sample(df, "region", 3L, proportional = TRUE, seed = 1L),
                 "leaves E, W with no rows")
  expect_equal(sort(unique(s$region)), c("N", "S"))
  expect_silent(morie_stratified_sample(df, "region", 2L, seed = 1L))
})

test_that("round 4: both spellings of a NAPS key resolve and analyze reports a subject whose analyses all failed", {
  expect_equal(.fuzzy_match_key("naps_co_on_2023"), "naps-co-on-2023")
  expect_equal(.fuzzy_match_key("naps-co-on-2023"), "naps-co-on-2023")
  failed <- list(a = list(title = "x (failed)", warnings = "boom"),
                 b = list(title = "y", summary_lines = list(), tables = list(), payload = list(), warnings = "missing column(s): z"))
  expect_true(.cli_analyze_all_failed(failed))
  expect_true(.cli_analyze_all_failed(.morie_to_json(failed, auto_unbox = TRUE)))
  ok <- list(a = list(title = "x", tables = list(data.frame(n = 1))))
  expect_false(.cli_analyze_all_failed(ok))
  expect_false(.cli_analyze_all_failed(list(subject = "tps", status = "not_available")))
})

test_that("round 4: the fallback cause names the requested model", {
  withr::local_envvar(GEMINI_API_KEY = "", GOOGLE_API_KEY = "")
  local_mocked_bindings(.morie_llm_api_base = function() NULL, .morie_llm_hosted_key = function() NULL)
  expect_match(.cli_llm_fallback_cause("nosuchmodel"), "no provider answered for model 'nosuchmodel'")
  # a stale hosted key is still named, after the model
  local_mocked_bindings(.morie_llm_hosted_key = function() "k", .morie_llm_hosted_rejected = function() TRUE)
  expect_match(.cli_llm_fallback_cause("nosuchmodel"), "model 'nosuchmodel': your hosted key was rejected")
  # asking Gemini with a Gemini key set points at that key, whatever else is configured
  withr::local_envvar(GEMINI_API_KEY = "AIzaFAKE")
  expect_match(.cli_llm_fallback_cause("gemini"), "model 'gemini': your GEMINI_API_KEY")
})

test_that("round 4: omega hierarchical is below one on a two-factor scale", {
  set.seed(4)
  n <- 400L
  g <- rnorm(n)
  s1 <- rnorm(n)
  s2 <- rnorm(n)
  X <- cbind(sapply(1:4, function(j) 0.5 * g + 0.6 * s1 + rnorm(n, sd = 0.6)),
             sapply(1:4, function(j) 0.5 * g + 0.6 * s2 + rnorm(n, sd = 0.6)))
  o1 <- morie_psymet_omega(X, nf = 1)
  expect_equal(o1$hier, o1$total, tolerance = 0.01)  # one factor: general factor = the factor, up to model fit
  o2 <- morie_psymet_omega(X, nf = 2)
  expect_lt(o2$hier, 0.7)                         # the group factors carry real variance
  expect_lt(o2$hier, o2$total)
  # Schmid-Leiman by hand: promax, g loadings sqrt(phi_12) per factor, project the items
  R <- cor(X)
  L <- .morie_paf(R, 2L)
  pm <- stats::promax(L, m = 4)
  P <- unclass(pm$loadings)
  Phi <- solve(crossprod(pm$rotmat))
  s <- sign(colSums(P))
  P <- sweep(P, 2L, s, "*")
  g <- drop(P %*% rep(sqrt((Phi * outer(s, s))[1L, 2L]), 2L))
  expect_equal(o2$hier, sum(g)^2 / sum(R), tolerance = 1e-12)
})

test_that("round 4: verify reports a declared placeholder table and a header-only table instead of failing them", {
  d <- withr::local_tempdir()
  utils::write.csv(data.frame(model = "not computed", or = NA, p_value = NA,
                              significant = "SMOTE resampling is not part of the R workflow"),
                   file.path(d, "smote_or.csv"), row.names = FALSE)
  writeLines("\"estimand\",\"estimate\",\"se\"", file.path(d, "dml.csv"))
  rp <- .cli_verify_csv(file.path(d, "smote_or.csv"))
  expect_true(rp$passed)
  expect_match(.cli_verify_text(rp), "placeholder row")
  rh <- .cli_verify_csv(file.path(d, "dml.csv"))
  expect_true(rh$passed)
  expect_match(.cli_verify_text(rh), "header only")
  utils::write.csv(data.frame(a = 1:2, b = NA), file.path(d, "bad.csv"), row.names = FALSE)
  expect_false(.cli_verify_csv(file.path(d, "bad.csv"))$passed)
})

test_that("round 4: the OTIS grid names the columns a pair lacks", {
  df <- data.frame(UniqueIndividual_ID = 1:6, EndFiscalYear = 2023L, Gender = "M", Age_Category = "A",
                   Region_AtTimeOfPlacement = "R", Region_MostRecentPlacement = "R",
                   MentalHealth_Alert = c(0, 1, 0, 1, 0, 1), SuicideRisk_Alert = c(0, 0, 1, 1, 0, 1))
  w <- character()
  withCallingHandlers(suppressMessages(tryCatch(morie_otis_causal_grid(df), error = function(e) NULL)),
                      warning = function(x) {
                        w <<- c(w, conditionMessage(x))
                        invokeRestart("muffleWarning")
                      })
  expect_true(any(grepl("\\(b\\).*lacks SuicideWatch_Alert, Number_Of_Placements", w)))
  expect_true(any(grepl("\\(c\\).*NumberConsecutiveDays_Segregation", w)))
})

test_that("round 4: the two-proportion power table applies the design effect of the weights", {
  set.seed(11)
  n <- 300L
  d <- data.frame(gender = sample(1:2, n, TRUE), heavy_drinking_30d = rbinom(n, 1, 0.3),
                  weight = runif(n, 0.5, 3), alcohol_past12m = 1L, ebac_legal = rbinom(n, 1, 0.2),
                  ebac_tot = runif(n), age_group = 1L, province_region = 1L, mental_health = 1L,
                  physical_health = 1L, cannabis_any_use = rbinom(n, 1, 0.4))
  tabs <- suppressWarnings(suppressMessages(.run_power_design_module_extended(d)))
  pt <- tabs$power_two_proportion_gender
  r <- pt[pt$power_scope == "heavy_drinking_30d", ][1L, ]
  lab <- .cpads_labeled_data(d)
  w <- lab$weight[lab$gender_label %in% c(r$group1, r$group2)]
  deff <- length(w) * sum(w^2) / sum(w)^2
  se <- sqrt(1 / r$n1 + 1 / r$n2)
  z <- stats::qnorm(0.975)
  expect_equal(r$n_eq_eff, r$n_eq * deff, tolerance = 1e-12)
  expect_equal(r$power_srs, stats::pnorm(abs(r$h) / se - z), tolerance = 1e-12)
  expect_equal(r$power_deff, stats::pnorm(abs(r$h) / (se * sqrt(deff)) - z), tolerance = 1e-12)
  expect_gt(deff, 1)
})

test_that("round 4: REML tau2 is the converged optimum (metafor agrees once its threshold is tight)", {
  skip_if_not_installed("metafor")
  est <- c(-0.25, -0.10, -0.40, 0.05, -0.30)
  v <- c(0.010, 0.020, 0.015, 0.030, 0.012)
  ref <- metafor::rma(est, v, method = "REML", control = list(threshold = 1e-12, maxiter = 1000))$tau2
  expect_equal(morie_meta_hksj(est, v, tau2 = "REML")$tau2, ref, tolerance = 1e-6)
})

test_that("round 4: resolve_so takes a case number in place of the report text", {
  skip_if_not_installed("rmoriedata")
  r <- morie_siu_resolve_so("17-OVI-201")
  expect_equal(r$count, 1L)
  expect_match(r$reason, "case 17-OVI-201, drid 46")
  expect_error(morie_siu_resolve_so("99-ZZZ-999"), "is a case number")
})

test_that("round 4: SIU page text decodes in a C locale (unmarked UTF-8 and Latin-1 bytes)", {
  u <- rawToChar(as.raw(c(0x3c, 0x70, 0x3e, 0x51, 0x75, 0xc3, 0xa9, 0x62, 0x65, 0x63, 0x20, 0x26, 0x65, 0x61, 0x63,
                          0x75, 0x74, 0x65, 0x3b, 0x74, 0x26, 0x65, 0x61, 0x63, 0x75, 0x74, 0x65, 0x3b, 0x3c, 0x2f, 0x70, 0x3e)))
  l1 <- rawToChar(as.raw(c(0x51, 0x75, 0xe9, 0x62, 0x65, 0x63, 0x20, 0x26, 0x61, 0x6d, 0x70, 0x3b)))
  out <- withr::with_locale(c(LC_CTYPE = "C"), c(.siu_html_to_text(u), .siu_html_to_text(l1)))
  expect_equal(out, c("Qu\u00e9bec \u00e9t\u00e9", "Qu\u00e9bec &"))
})


test_that("round 4: the native SIU parser reads ordinal dates and the force that notified the SIU", {
  html <- paste0(
    "<html><body><h2>The Investigation</h2><h3>Notification of the SIU</h3>",
    "<p>At approximately 11:46 a.m. on August 3rd, 2017, the Guelph Police Service ( GPS ) notified the SIU ",
    "of the injury.</p><p>Under the Police Services Act, the Director decides. The Ontario Police Services Board ",
    "and the Police Services Act are named again here: Police Services Act.</p>",
    "<h2>Incident Narrative</h2><p>The GPS reported that at 10:30 a.m., on August 3rd, 2017, three masked men ",
    "attempted to rob a bank.</p></body></html>")
  f <- morie_siu_parse_report(html, engine = "native")
  expect_equal(f[["police_service"]], "Guelph Police Service")
  expect_equal(f[["date_siu_notified_iso"]], "2017-08-03")
})

test_that("round 4: only the expected design-weight warning is muffled in weighted binomial fits", {
  d <- data.frame(y = c(0, 1, 0, 1, 1, 0, 1, 0), x = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1.5, 2.2, 0.7, 1.1, 3.3, 0.9, 1.4, 2.6))
  expect_silent(.glm_design_weighted(stats::glm(y ~ x, data = d, family = stats::binomial(), weights = w)))
  sep <- data.frame(y = c(0, 0, 0, 1, 1, 1), x = 1:6, w = 1.5)
  expect_warning(.glm_design_weighted(stats::glm(y ~ x, data = sep, family = stats::binomial(), weights = w)),
                 "fitted probabilities")
})

test_that("round 4: the cause names the model first, then the real cause, and a Gemini model points at the Gemini key", {
  withr::local_envvar(GEMINI_API_KEY = "", GOOGLE_API_KEY = "")
  local_mocked_bindings(.morie_llm_api_base = function() NULL, .morie_llm_hosted_key = function() "k",
                        .morie_llm_hosted_rejected = function() TRUE)
  expect_match(.cli_llm_fallback_cause("nosuchmodel"), "model 'nosuchmodel': your hosted key was rejected")
  withr::local_envvar(GEMINI_API_KEY = "AIzaFAKE")
  expect_match(.cli_llm_fallback_cause("gemini"), "model 'gemini': your GEMINI_API_KEY")
})
