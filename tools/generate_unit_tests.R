# SPDX-License-Identifier: AGPL-3.0-or-later
# tools/generate_unit_tests.R
#
# For every exported function whose runnable example call we know
# (from tools/runnable_examples.json + tools/manual_examples.R's
# hand-crafted set), generate a real testthat block that:
#   1. Calls the function with the crafted args
#   2. Asserts the return is non-NULL
#   3. Asserts class/type/dims/finite-ness as appropriate
#
# This converts "example execution incidentally exercising the
# function" into "real unit test asserting on the function's
# behaviour" -- raising type="tests" coverage substantially.
#
# Output: tests/testthat/test-cov-gen-A.R, -B.R, -C.R (split for
# parallel readability; ~50 tests per file).
#
# Usage:  Rscript tools/generate_unit_tests.R [--apply]

suppressMessages({
  library(morie)
  library(callr)
})

APPLY <- "--apply" %in% commandArgs(trailingOnly = TRUE)

# 1. Collect all known example calls
ex_json <- jsonlite::read_json("tools/runnable_examples.json")
ok_from_json <- Filter(function(v) v$status == "ok" && nzchar(v$call %||% ""),
                       ex_json)
calls <- list()
for (n in names(ok_from_json)) calls[[n]] <- ok_from_json[[n]]$call

# 2. Pull hand-crafted ones from manual_examples.R (re-eval its EX list)
source_env <- new.env()
sys.source("tools/manual_examples.R", envir = source_env, keep.source = FALSE)
if (exists("EX", envir = source_env)) {
  manual_ex <- get("EX", envir = source_env)
  for (n in names(manual_ex)) calls[[n]] <- manual_ex[[n]]
}
cat(sprintf("collected %d example calls\n", length(calls)))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# 3. Probe each: capture return value + classify
probe <- function(fname, code) {
  callr::r(
    function(src) {
      suppressMessages(library(morie))
      set.seed(1)
      out <- eval(parse(text = src), envir = new.env())
      list(
        class       = class(out),
        is_null     = is.null(out),
        is_df       = is.data.frame(out),
        is_list     = is.list(out) && !is.data.frame(out),
        is_numeric  = is.numeric(out),
        is_logical  = is.logical(out),
        is_char     = is.character(out),
        length      = if (!is.null(out)) length(out) else 0L,
        dim         = if (!is.null(dim(out))) dim(out) else integer(0),
        names_pre10 = if (!is.null(names(out))) head(names(out), 10) else character(0),
        all_finite  = if (is.numeric(out)) all(is.finite(out)) else NA
      )
    },
    args = list(src = code), timeout = 10
  )
}

results <- list()
for (i in seq_along(calls)) {
  fn <- names(calls)[i]
  code <- calls[[fn]]
  out <- tryCatch(probe(fn, code), error = function(e) list(error = conditionMessage(e)))
  results[[fn]] <- list(code = code, info = out)
  if (i %% 20 == 0) cat(sprintf("  probed %d/%d\n", i, length(calls)))
}

ok_n <- sum(vapply(results, function(r) is.null(r$info$error), logical(1)))
cat(sprintf("\nprobed OK: %d / %d\n", ok_n, length(results)))

# 4. Build test_that blocks
build_test <- function(fn, code, info) {
  if (!is.null(info$error)) return(NULL)
  lines <- character()
  # Wrap the call in a unique variable; assert on its properties.
  lines <- c(lines,
    sprintf("test_that(\"%s runs without error\", {", fn),
    "  set.seed(1)"
  )
  # The call itself: if multi-line, indent each
  code_lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  # Wrap the LAST line as assignment to .out
  if (length(code_lines) > 0) {
    code_lines[length(code_lines)] <- paste0(".out <- ", code_lines[length(code_lines)])
    lines <- c(lines, paste0("  ", code_lines))
  }
  # Assertions based on what probe captured
  lines <- c(lines, "  expect_false(is.null(.out))")
  if (info$is_df) {
    lines <- c(lines, "  expect_s3_class(.out, \"data.frame\")")
  } else if (info$is_list) {
    lines <- c(lines, "  expect_type(.out, \"list\")")
    if (length(info$names_pre10) > 0) {
      nms_quoted <- paste0("\"", unlist(info$names_pre10), "\"", collapse = ", ")
      lines <- c(lines, sprintf("  expect_true(all(c(%s) %%in%% names(.out)))", nms_quoted))
    }
  } else if (info$is_numeric) {
    lines <- c(lines, "  expect_type(.out, ifelse(is.integer(.out), \"integer\", \"double\"))")
    if (isTRUE(info$all_finite)) {
      lines <- c(lines, "  expect_true(all(is.finite(.out)))")
    }
  } else if (info$is_logical) {
    lines <- c(lines, "  expect_type(.out, \"logical\")")
  } else if (info$is_char) {
    lines <- c(lines, "  expect_type(.out, \"character\")")
  }
  if (length(info$dim) >= 1L) {
    dim_vec <- paste(info$dim, collapse = "L, ")
    lines <- c(lines, sprintf("  expect_equal(dim(.out), c(%sL))", dim_vec))
  } else if (info$length > 0L && !info$is_df) {
    lines <- c(lines, sprintf("  expect_length(.out, %dL)", info$length))
  }
  lines <- c(lines, "})", "")
  paste(lines, collapse = "\n")
}

blocks <- list()
for (fn in names(results)) {
  b <- build_test(fn, results[[fn]]$code, results[[fn]]$info)
  if (!is.null(b)) blocks[[fn]] <- b
}
cat(sprintf("built test blocks for %d functions\n", length(blocks)))

# 5. Save to split files
if (APPLY) {
  hdr <- "# SPDX-License-Identifier: AGPL-3.0-or-later\n# Auto-generated by tools/generate_unit_tests.R\n# Real testthat blocks for exported functions whose return value the\n# generator's probe could classify. Each block calls the function and\n# asserts properties of the return (class, dims, finite-ness, names).\n\n"
  block_names <- names(blocks)
  n_per_file <- 50L
  n_files <- ceiling(length(blocks) / n_per_file)
  for (i in seq_len(n_files)) {
    chunk <- block_names[(((i - 1L) * n_per_file) + 1L):min(i * n_per_file, length(block_names))]
    chunk <- chunk[!is.na(chunk)]
    out_path <- sprintf("tests/testthat/test-cov-gen-%s.R", LETTERS[i])
    body <- paste(unlist(blocks[chunk]), collapse = "\n")
    writeLines(paste0(hdr, body), out_path)
    cat(sprintf("wrote %s (%d tests)\n", out_path, length(chunk)))
  }
}
