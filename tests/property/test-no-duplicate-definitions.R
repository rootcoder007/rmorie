test_that("no function is defined twice across R/", {
  # R sources R/ alphabetically and the LAST definition silently wins --
  # no error at build, install or check time.  Five functions had been
  # lost this way.  morie_ols was the dangerous one: its two definitions
  # took (y, X) and (X, y), so callers were transposing their own data
  # and getting a plausible, wrong answer.
  candidates <- c("r-package/morie/R", "../../R",
                  "../../../R", "R", "../R")
  r_dir <- NULL
  for (cand in candidates) {
    if (dir.exists(cand) && length(list.files(cand, pattern = "[.]R$"))) {
      r_dir <- cand
      break
    }
  }
  # only legitimate when the sources are not shipped, i.e. inside .Rcheck
  skip_if(is.null(r_dir), "package R/ sources are not in this tree")

  files <- list.files(r_dir, pattern = "[.]R$", full.names = TRUE)
  expect_gt(length(files), 0)
  defs <- list()
  for (f in files) {
    src <- readLines(f, warn = FALSE)
    hits <- grep("^[A-Za-z._][A-Za-z0-9._]*[ ]*<-[ ]*function", src,
                 value = TRUE)
    for (h in hits) {
      nm <- sub("[ ]*<-.*$", "", h)
      defs[[nm]] <- c(defs[[nm]], basename(f))
    }
  }
  dups <- defs[vapply(defs, length, integer(1)) > 1]
  if (length(dups)) {
    msg <- paste(sprintf("%s defined in %s", names(dups),
                         vapply(dups, paste, character(1), collapse = " + ")),
                 collapse = "; ")
  } else {
    msg <- ""
  }
  expect_identical(msg, "")
})
