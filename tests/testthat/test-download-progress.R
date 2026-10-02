# The download helper: progress lines off a terminal, silence when quiet, bytes intact.

test_that(".morie_dl copies a local file with milestone progress and honours quiet", {
  src <- withr::local_tempfile(fileext = ".bin")
  writeBin(as.raw(rep(65L, 3 * 1048576 + 7)), src)
  dest <- withr::local_tempfile(fileext = ".bin")
  withr::local_options(morie.progress = TRUE, morie.quiet = NULL)
  msgs <- utils::capture.output(
    .morie_dl(paste0("file://", src), dest, label = "three-mb", size = file.size(src)),
    type = "message"
  )
  expect_identical(file.size(dest), file.size(src))
  expect_true(any(grepl("three-mb: downloading 3.0 MB", msgs, fixed = TRUE)))
  expect_true(any(grepl("100%", msgs, fixed = TRUE)))
  expect_true(any(grepl("three-mb: 3.0 MB in", msgs, fixed = TRUE)))

  withr::local_options(morie.quiet = TRUE)
  quiet <- utils::capture.output(.morie_dl(paste0("file://", src), dest, label = "q"), type = "message")
  expect_identical(quiet, character())
  expect_identical(file.size(dest), file.size(src))
})

test_that(".morie_dl_quiet follows the session and the options", {
  withr::local_options(morie.progress = NULL, morie.quiet = NULL)
  expect_identical(.morie_dl_quiet(), !interactive())
  withr::local_options(morie.progress = TRUE)
  expect_false(.morie_dl_quiet())
  withr::local_options(morie.quiet = TRUE)
  expect_true(.morie_dl_quiet())
})

test_that(".morie_fmt_bytes and the line renderer", {
  expect_identical(.morie_fmt_bytes(512), "512 B")
  expect_identical(.morie_fmt_bytes(1536), "1.5 KB")
  expect_identical(.morie_fmt_bytes(583 * 1048576), "583.0 MB")
  line <- .morie_dl_line("x", 50, 100, proc.time()[["elapsed"]] - 1, 0L)
  expect_match(line, "\\[############\\.{13}\\]  50%  50 B / 100 B")
  rows <- .morie_dl_line("ckan", 32000, 40931, proc.time()[["elapsed"]] - 1, 0L, unit = "rows")
  expect_match(rows, " 78%  32,000 rows / 40,931 rows", fixed = TRUE)
})
