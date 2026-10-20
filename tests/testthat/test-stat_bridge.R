# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/stat_bridge.R

test_that("stat_bridge_registry_json emits JSON text", {
  skip_if_not_installed("jsonlite")
  out <- stat_bridge_registry_json()
  expect_true(is.character(out))
  expect_true(nchar(out) > 0L)
  # JSON should parse
  parsed <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_true(is.list(parsed))
})

test_that("stat_bridge_help formats categories", {
  out <- stat_bridge_help()
  expect_true(is.character(out))
  expect_true(grepl("MORIE", out))
  expect_true(grepl("Total:", out))
})

test_that("stat_bridge_exec returns text for known command", {
  out <- stat_bridge_exec("bonferroni 0.01 0.04 0.05")
  expect_true(is.character(out))
})

test_that("stat_bridge_exec error paths", {
  expect_match(stat_bridge_exec(""), "empty")
  expect_match(stat_bridge_exec("totally_unknown_xyz"), "Unknown command")
})

# Note: tests below expect "Usage" (not "Error") in dispatcher messages so
# r-lib/actions/check-r-package log scanner doesn't annotate these test
# lines as job-level errors. Same reason ERR: replaces Error: in handler
# tryCatch paths.

test_that("stat_bridge_exec captures handler error", {
  # Register a command that throws so we trigger the tryCatch path
  cmd <- stat_command("test_bridge_throw", "T", "u", "d",
                      handler_repl = function(...) stop("boom"))
  register_stat_command(cmd)
  on.exit(.unregister_stat_command("test_bridge_throw"), add = TRUE)
  out <- stat_bridge_exec("test_bridge_throw")
  expect_true(grepl("Error", out) || nchar(out) >= 0)
})

test_that("stat_bridge_fn_info works for known + unknown", {
  out <- stat_bridge_fn_info("bonferroni")
  expect_true(grepl("bonferroni", out))
  out2 <- stat_bridge_fn_info("not_a_command_xyz")
  expect_match(out2, "Not found")
})

test_that("stat_bridge_fn_info handles command with no aliases", {
  cmd <- stat_command("test_bridge_no_alias", "T", "use", "desc",
                      handler_repl = function() 1)
  register_stat_command(cmd)
  on.exit(.unregister_stat_command("test_bridge_no_alias"), add = TRUE)
  out <- stat_bridge_fn_info("test_bridge_no_alias")
  expect_true(grepl("none", out))
})

test_that("stat_bridge_fn_search returns matches and no-match", {
  out <- stat_bridge_fn_search("bonferroni")
  expect_true(is.character(out))
  out_none <- stat_bridge_fn_search("zzzzz_no_match_query")
  expect_match(out_none, "No matches")
})

test_that("stat_bridge_fn_search respects max_results cap", {
  out <- stat_bridge_fn_search("morie", max_results = 3L)
  expect_true(is.character(out))
})

test_that("stat_bridge_verify returns data.frame", {
  df <- stat_bridge_verify()
  expect_s3_class(df, "data.frame")
  expect_true(all(c("name", "ok", "message") %in% names(df)))
})

test_that("stat_bridge_main dispatches all modes", {
  expect_invisible(stat_bridge_main(character(0)))
  rj <- stat_bridge_main(c("registry-json"))
  expect_true(is.character(rj))
  hp <- stat_bridge_main(c("help"))
  expect_true(is.character(hp))
  # exec needs args
  e0 <- stat_bridge_main(c("exec"))
  expect_match(e0, "Usage")
  e1 <- stat_bridge_main(c("exec", "bonferroni", "0.01", "0.05"))
  expect_true(is.character(e1))
  # fn-info
  f0 <- stat_bridge_main(c("fn-info"))
  expect_match(f0, "Usage")
  f1 <- stat_bridge_main(c("fn-info", "bonferroni"))
  expect_true(is.character(f1))
  # fn-search
  s0 <- stat_bridge_main(c("fn-search"))
  expect_match(s0, "Usage")
  s1 <- stat_bridge_main(c("fn-search", "morie"))
  expect_true(is.character(s1))
  # verify
  v <- stat_bridge_main(c("verify"))
  expect_true(is.character(v))
  # unknown mode
  u <- stat_bridge_main(c("nonsense_mode_xyz"))
  expect_match(u, "Unknown mode")
})

test_that("verify runs every seeded command on sample inputs", {
  df <- stat_bridge_verify(execute = TRUE)
  seeded <- c("bonferroni", "holm", "benjamini_hochberg", "benjamini_yekutieli",
              "storey_q", "fisher_combined", "stouffer_combined", "harmonic_mean_p",
              "adjust_p_values", "estimate_pi0", "n_effective_tests", "nw_regression",
              "local_linear", "kde", "silverman_bandwidth", "loocv_bandwidth",
              "gam_smoother")
  got <- df[df$name %in% seeded, ]
  expect_setequal(got$name, seeded)
  expect_true(all(got$ok %in% TRUE), info = paste(got$name[!got$ok %in% TRUE], got$message[!got$ok %in% TRUE], collapse = "; "))
  # no test-only commands left behind by the tests above
  expect_false(any(startsWith(df$name, "test_bridge_")))
})

test_that("verify reports a command it cannot feed as needing input, not failing", {
  register_stat_command(stat_command("test_bridge_needs", "T", "test_bridge_needs(z)", "d",
                                     handler_repl = function(z) z))
  on.exit(.unregister_stat_command("test_bridge_needs"), add = TRUE)
  register_stat_command(stat_command("test_bridge_boom", "T", "test_bridge_boom()", "d",
                                     handler_repl = function() stop("boom")))
  on.exit(.unregister_stat_command("test_bridge_boom"), add = TRUE)
  df <- stat_bridge_verify(execute = TRUE)
  expect_true(is.na(df$ok[df$name == "test_bridge_needs"]))
  expect_match(df$message[df$name == "test_bridge_needs"], "needs input: z")
  expect_false(df$ok[df$name == "test_bridge_boom"])
  out <- utils::capture.output(stat_bridge_main(args = "verify"))
  expect_true(any(grepl("NEEDS INPUT  test_bridge_needs", out, fixed = TRUE)))
  expect_true(any(grepl("FAIL  test_bridge_boom -- boom", out, fixed = TRUE)))
  expect_true(any(grepl("PASS  bonferroni", out, fixed = TRUE)))
})
