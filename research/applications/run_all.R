# Runs every data application and assembles RESULTS.md. From the package root:
#   MORIE_DATA_ROOT=~/data/datasets Rscript research/applications/run_all.R
app_dir <- Sys.getenv("MORIE_APP_DIR", unset = file.path("research", "applications"))
scripts <- c("p02_veil_nashville.R", "p03_toronto_neighbourhood_network.R", "p03_street_segments.R", "p07_concentration_toronto.R",
             "p10_near_repeat_bne.R", "p11_custody_reconviction.R", "p12_toronto_ecological.R", "p14_cook_judges.R", "p15_otis_placement_lengths.R",
             "p16_ocj_backlog.R", "p17_reconviction_incapacitation.R", "p18_ny_pretrial.R")
status <- character()
for (s in scripts) {
  message("== ", s)
  r <- tryCatch({
    source(file.path(app_dir, s), local = new.env())
    "ok"
  }, error = function(e) conditionMessage(e))
  status[s] <- r
  if (r != "ok") message("FAILED: ", r)
}
res <- list.files(file.path(app_dir, "results"), pattern = "\\.md$", full.names = TRUE)
out <- c("# Ledger data applications: results", "",
         sprintf("Generated %s by research/applications/run_all.R (rmorie %s).", format(Sys.Date()),
                 as.character(utils::packageVersion("rmorie"))),
         "", "| script | status |", "|---|---|",
         sprintf("| %s | %s |", names(status), status), "")
for (p in res) out <- c(out, readLines(p, warn = FALSE), "", "---", "")
writeLines(out, file.path(app_dir, "RESULTS.md"))
if (any(status != "ok")) quit(status = 1L)
