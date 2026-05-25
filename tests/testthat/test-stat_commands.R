# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/stat_commands.R

test_that("stat_command constructs a valid command", {
  fn <- function(x) x + 1
  cmd <- stat_command(
    name = "test_cmd_alpha",
    category = "Testing",
    usage = "test_cmd_alpha(x)",
    description = "test alpha",
    handler_repl = fn,
    aliases = c("tca", "alpha")
  )
  expect_s3_class(cmd, "morie_stat_command")
  expect_equal(cmd$name, "test_cmd_alpha")
  expect_equal(cmd$category, "Testing")
  expect_true(is.function(cmd$handler_stat))
})

test_that("stat_command rejects invalid input", {
  expect_error(stat_command("", "C", "u", "d", function() 1))
  expect_error(stat_command("name", "C", "u", "d", "not_a_function"))
})

test_that("stat_command custom handler_stat preserved", {
  hs <- function(parts, log, store) "custom"
  cmd <- stat_command("test_cmd_beta", "T", "u", "d",
                       handler_repl = function() 1,
                       handler_stat = hs)
  expect_identical(cmd$handler_stat, hs)
})

test_that("register/resolve work", {
  cmd <- stat_command("test_cmd_reg", "TestCat", "u", "d",
                      handler_repl = function() 42,
                      aliases = c("tcr_alias"))
  register_stat_command(cmd)
  expect_equal(resolve_stat_command("test_cmd_reg")$name, "test_cmd_reg")
  expect_equal(resolve_stat_command("tcr_alias")$name, "test_cmd_reg")
})

test_that("register_stat_command rejects non-command", {
  expect_error(register_stat_command("not a cmd"), "morie_stat_command")
})

test_that("resolve_stat_command returns NULL for unknown", {
  expect_null(resolve_stat_command("nonexistent_command_xyz"))
  expect_null(resolve_stat_command(c("a", "b")))  # not length-1
  expect_null(resolve_stat_command(123))
})

test_that("all_stat_command_names returns sorted unique names", {
  v <- all_stat_command_names()
  expect_true(is.character(v))
  expect_true(length(v) >= 1L)
  expect_equal(v, sort(unique(v)))
})

test_that("commands_by_category returns grouped list", {
  cmd <- stat_command("test_cmd_cat", "GroupTest", "u", "d",
                      handler_repl = function() 1)
  register_stat_command(cmd)
  groups <- commands_by_category()
  expect_true(is.list(groups))
  expect_true("GroupTest" %in% names(groups))
  expect_true("test_cmd_cat" %in% groups[["GroupTest"]])
})

test_that("run_stat_command invokes handler", {
  cmd <- stat_command("test_cmd_run", "T", "u", "d",
                      handler_repl = function(x) x * 2)
  register_stat_command(cmd)
  expect_equal(run_stat_command("test_cmd_run", 5), 10)
})

test_that("run_stat_command errors on unknown", {
  expect_error(run_stat_command("not_there"), "Unknown")
})

test_that("n_stat_commands returns count >= 1", {
  expect_true(n_stat_commands() >= 1L)
})

test_that("print.morie_stat_command emits readable output", {
  cmd <- stat_command("test_cmd_print", "PrintCat", "usage_str",
                      "Print desc", function() 1,
                      aliases = c("a1", "a2"))
  out <- capture.output(print(cmd))
  expect_true(any(grepl("test_cmd_print", out)))
  expect_true(any(grepl("PrintCat", out)))
  expect_true(any(grepl("Print desc", out)))
})

test_that("print.morie_stat_command with no aliases", {
  cmd <- stat_command("test_cmd_no_alias", "NoAlias", "u", "Some desc",
                      function() 1)
  out <- capture.output(print(cmd))
  expect_true(any(grepl("test_cmd_no_alias", out)))
})

test_that("clear_stat_commands resets registry, then re-seed", {
  n_before <- n_stat_commands()
  n_cleared <- clear_stat_commands()
  expect_equal(n_cleared, n_before)
  expect_equal(n_stat_commands(), 0L)
  # Re-seed for downstream tests
  morie:::.morie_seed_stat_commands()
  morie:::.morie_auto_register_stat_commands()
  expect_true(n_stat_commands() >= 1L)
})

test_that(".morie_infer_category prefix lookup", {
  expect_equal(morie:::.morie_infer_category("morie_did_event_study"), "DiD")
  expect_equal(morie:::.morie_infer_category("morie_tps_compute"), "TPS Spatial")
  expect_equal(morie:::.morie_infer_category("morie_otis_load"), "OTIS")
  # Unknown prefix falls back
  out <- morie:::.morie_infer_category("totally_random_xyz_func")
  expect_true(is.character(out))
})

test_that("custom handler_stat default wraps errors", {
  cmd <- stat_command("test_cmd_err", "ErrCat", "u", "d",
                      handler_repl = function(x) stop("boom"))
  register_stat_command(cmd)
  log_entries <- character(0)
  log_list <- list(write = function(msg) log_entries <<- c(log_entries, msg))
  cmd$handler_stat(c("test_cmd_err", "1"), log_list, function(x) NULL)
  expect_true(any(grepl("Error", log_entries)))
})

test_that("default handler_stat with successful result calls store", {
  stored <- character(0)
  store_fn <- function(x) stored <<- c(stored, x)
  cmd <- stat_command("test_cmd_store", "StoreCat", "u", "d",
                      handler_repl = function() "value123")
  log_list <- list(write = function(msg) NULL)
  cmd$handler_stat(c("test_cmd_store"), log_list, store_fn)
  expect_true(any(grepl("value123", stored)))
})
