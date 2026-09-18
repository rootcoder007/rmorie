# agent() forwards to the CLI through system2(), which hands its
# arguments to a shell. A stub binary on PATH records what arrived and
# `f(log)` runs with the stub first on PATH.

with_stub_bin <- function(f) {
  testthat::skip_on_os("windows")
  dir <- tempfile("stubbin")
  dir.create(dir)
  log <- file.path(dir, "argv.txt")
  bin <- file.path(dir, "rmorie")
  writeLines(c("#!/bin/sh", paste0("printf '%s\\n' \"$@\" > '", log, "'"),
               "echo ok"), bin)
  Sys.chmod(bin, "0755")
  old <- Sys.getenv("PATH")
  on.exit(Sys.setenv(PATH = old), add = TRUE)
  Sys.setenv(PATH = paste(dir, old, sep = .Platform$path.sep))
  f(log)
}

test_that("agent() delivers the task as one argument, shell characters inert", {
  with_stub_bin(function(log) {
    marker <- tempfile("never")
    task <- sprintf("mean of (1:10); touch %s", marker)
    expect_identical(agent(task), "ok")
    argv <- readLines(log)
    expect_identical(argv[1:3], c("agent", "--backend", "auto"))
    expect_length(argv, 4L)
    expect_identical(argv[4], task)
    expect_false(file.exists(marker))
  })
})

test_that("agent() passes model, backend and --dry-run through quoted", {
  with_stub_bin(function(log) {
    agent("plain task", model = "gpt-4o mini", backend = "ollama",
          dry_run = TRUE)
    argv <- readLines(log)
    expect_identical(argv, c("agent", "--backend", "ollama", "-m",
                             "gpt-4o mini", "--dry-run", "plain task"))
  })
})

test_that("agent() rejects bad task, model and backend before the shell", {
  expect_error(agent("   "), "task")
  expect_error(agent(NA_character_), "task")
  expect_error(agent("q", backend = c("a", "b")), "backend")
  expect_error(agent("q", model = NA_character_), "model")
})
