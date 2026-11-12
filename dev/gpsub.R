# goodpractice on rmorie, the subset that bears on duplication, complexity and naming
# (the full ~/tmp/gpci-m.R set also installs the package for cyclocomp, checks every URL
# on the network and runs every lintr linter over 3,200 files, too heavy for l14).
#
# R_ENVIRON_USER=/dev/null R_PROFILE_USER=/dev/null Rscript dev/gpsub.R
.libPaths(c("~/tmp/gplib", .libPaths()))
cat("goodpractice", as.character(packageVersion("goodpractice")), "\n")
# the tidyverse_* checks are left out: their preparation lints all 3,200 files with the
# whole tidyverse linter set and did not finish in 90 minutes on l14 (exit 124); naming
# is covered by lintr's object_name/object_length linters on the family files instead
ck <- c("complexity_function_length", "complexity_unused_internal", "duplicate_function_bodies",
        "no_export_pattern", "rd_has_examples", "rd_has_return", "roxygen2_duplicate_params",
        "roxygen2_has_export_or_nord", "has_readme", "has_news", "r_file_extension")
stopifnot(all(ck %in% goodpractice::all_checks()))
g <- goodpractice::gp(".", checks = ck, quiet = TRUE)
failed <- goodpractice::failed_checks(g)
cat("GP checks run:", length(ck), "(", paste(ck, collapse = ", "), ")\nfailed:", length(failed),
    paste(failed, collapse = ", "), "\n")
saveRDS(g, file.path(tempdir(), "gp.rds"))
for (k in failed) {
  r <- goodpractice::results(g)
  cat("\n==", k, "\n")
  pos <- g[["checks"]][[k]][["positions"]]
  if (!is.null(pos) && length(pos)) {
    fl <- vapply(pos, function(p) paste0(p[["filename"]], ":", p[["line_number"]]), "")
    cat(length(fl), "positions; first:", head(fl, 10), sep = "\n  ")
    saveRDS(fl, file.path("~/tmp", paste0("gp-", k, ".rds")))
  }
}
cat("GPSUB_DONE\n")
