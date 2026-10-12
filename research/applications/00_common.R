# Shared pieces of the ledger's data applications. Each script sources this file, reads its
# data under MORIE_DATA_ROOT (the local copies of the public files named in README.md), runs
# the rmorie research functions and writes one Markdown result under results/.
suppressPackageStartupMessages(library(rmorie))

data_root <- path.expand(Sys.getenv("MORIE_DATA_ROOT", unset = "~/data/datasets"))
dpath <- function(...) file.path(data_root, ...)
app_dir <- Sys.getenv("MORIE_APP_DIR", unset = file.path("research", "applications"))
out_dir <- file.path(app_dir, "results")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Whole numbers are printed in full with a thousands separator; other numbers to `digits`
# significant figures. A column named year is a label, not a quantity.
fmt <- function(x, digits = 3) {
  if (!is.numeric(x)) return(as.character(x))
  whole <- is.finite(x) & x == round(x)
  out <- character(length(x))
  out[whole] <- format(x[whole], big.mark = ",", scientific = FALSE, trim = TRUE)
  out[!whole] <- trimws(formatC(x[!whole], digits = digits, format = "fg"))
  out
}

md_table <- function(df, digits = 3) {
  cells <- lapply(names(df), function(nm) {
    col <- df[[nm]]
    if (nm %in% c("year", "fiscal_year", "tau")) as.character(col) else fmt(col, digits)
  })
  m <- do.call(cbind, cells)
  if (is.null(dim(m))) m <- matrix(m, nrow = 1L)
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    apply(m, 1L, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}

write_result <- function(name, lines) {
  p <- file.path(out_dir, paste0(name, ".md"))
  writeLines(lines, p)
  message("wrote ", p)
  invisible(p)
}

need_file <- function(p) {
  if (!file.exists(p)) stop("data file not found: ", p, " (set MORIE_DATA_ROOT)", call. = FALSE)
  p
}
