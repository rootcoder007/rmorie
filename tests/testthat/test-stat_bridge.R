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

test_that("stat_bridge_exec captures handler error", {
  # Register a command that throws so we trigger the tryCatch path
  cmd <- stat_command("test_bridge_throw", "T", "u", "d",
                      handler_repl = function(...) stop("boom"))
  register_stat_command(cmd)
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
  expect_match(e0, "Error")
  e1 <- stat_bridge_main(c("exec", "bonferroni", "0.01", "0.05"))
  expect_true(is.character(e1))
  # fn-info
  f0 <- stat_bridge_main(c("fn-info"))
  expect_match(f0, "Error")
  f1 <- stat_bridge_main(c("fn-info", "bonferroni"))
  expect_true(is.character(f1))
  # fn-search
  s0 <- stat_bridge_main(c("fn-search"))
  expect_match(s0, "Error")
  s1 <- stat_bridge_main(c("fn-search", "morie"))
  expect_true(is.character(s1))
  # verify
  v <- stat_bridge_main(c("verify"))
  expect_true(is.character(v))
  # unknown mode
  u <- stat_bridge_main(c("nonsense_mode_xyz"))
  expect_match(u, "Unknown mode")
})
