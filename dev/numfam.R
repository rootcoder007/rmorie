# Numbered function families in rmorie (a short prefix plus a number:
# Msm001, k02z, fzt12, ...) and how much of them is duplicated.
#
# Parses R/ (no package load), lists every family of three or more members,
# says for each member whether it is exported, documented, tested and called,
# then clusters bodies after normalising them: comments and layout dropped
# (deparse of the parsed body), formals and local variables renamed in order
# of appearance ("exact" clusters), and additionally every number and string
# literal replaced by a placeholder ("near" clusters: same code, other
# constants). Numbers are recomputed each run.
#
# R_ENVIRON_USER=/dev/null R_PROFILE_USER=/dev/null Rscript dev/numfam.R [outdir]
a <- commandArgs(TRUE)
outdir <- if (length(a)) a[[1]] else tempdir()
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

defs <- list()
for (f in sort(list.files("R", "\\.[Rr]$", full.names = TRUE))) {
  ex <- tryCatch(parse(f, keep.source = TRUE), error = function(e) NULL)
  if (is.null(ex)) next
  sr <- attr(ex, "srcref")
  for (i in seq_along(ex)) {
    e <- ex[[i]]
    if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
          is.call(e[[3]]) && identical(as.character(e[[3]][[1]]), "function")) {
      defs[[length(defs) + 1L]] <- list(
        name = as.character(e[[2]]), file = f,
        line = sr[[i]][[1]], nlines = sr[[i]][[3]] - sr[[i]][[1]] + 1L,
        fn = e[[3]]
      )
    }
  }
}
nm <- vapply(defs, `[[`, "", "name")
cat("functions defined in R/:", length(nm), "\n")

# family = the name without its trailing number (and an optional letter after it)
rx <- "^(\\.?[A-Za-z][A-Za-z_.]*?)_?([0-9]+)([a-z]?)$"
num <- grepl(rx, nm, perl = TRUE)
fam <- ifelse(num, tolower(sub(rx, "\\1", nm, perl = TRUE)), NA_character_)
tab <- sort(table(fam[num]), decreasing = TRUE)
fams <- names(tab)[tab >= 3L]

ns <- parseNamespaceFile(basename(getwd()), dirname(getwd()))
exports <- ns[["exports"]]
rd <- list.files("man", "\\.Rd$", full.names = TRUE)
aliases <- unique(unlist(lapply(rd, function(p) {
  l <- readLines(p, warn = FALSE)
  sub("^\\\\alias\\{(.*)\\}$", "\\1", grep("^\\\\alias\\{", l, value = TRUE))
})))
# file texts; comment lines dropped (roxygen examples are not callers)
grepfiles <- function(dir, pat = "\\.(R|Rmd|qmd)$") {
  if (!dir.exists(dir)) return(character())
  fs <- list.files(dir, pat, full.names = TRUE, recursive = TRUE)
  stats::setNames(vapply(fs, function(p) {
    l <- readLines(p, warn = FALSE)
    paste(l[!grepl("^\\s*#", l)], collapse = "\n")
  }, ""), fs)
}
tests <- grepfiles("tests")
vign <- grepfiles("vignettes")
rsrc <- grepfiles("R")
others <- c(
  grepfiles(path.expand("~/work/morie-k/src/morie"), "\\.py$"),
  grepfiles(path.expand("~/work/rmoriedata-k/R")),
  grepfiles(path.expand("~/work/bl-k/R"))
)
# every identifier followed by "(" in each file, once: lookups are then a table read
callidx <- function(txt) {
  ids <- lapply(txt, function(t) {
    m <- regmatches(t, gregexpr("(?<![A-Za-z0-9_.])[.A-Za-z][A-Za-z0-9_.]*(?=\\()", t, perl = TRUE))[[1]]
    unique(m)
  })
  table(unlist(ids, use.names = FALSE))
}
idx_tests <- callidx(tests)
idx_vign <- callidx(vign)
idx_r <- callidx(rsrc)
idx_oth <- callidx(others)
hits <- function(idx, n) if (n %in% names(idx)) as.integer(idx[[n]]) else 0L

# normalisation
rename_locals <- function(fn) {
  map <- character()
  sym <- function(s) {
    if (!s %in% names(map)) map[[s]] <<- paste0("v", length(map) + 1L)
    map[[s]]
  }
  for (f in names(fn[[2]])) sym(f)
  local_names <- function(x) {
    if (is.call(x)) {
      h <- x[[1]]
      if (is.name(h) && as.character(h) %in% c("<-", "=", "<<-") && is.name(x[[2]])) sym(as.character(x[[2]]))
      if (is.name(h) && identical(as.character(h), "for") && is.name(x[[2]])) sym(as.character(x[[2]]))
      if (is.name(h) && identical(as.character(h), "function")) for (f in names(x[[2]])) sym(f)
      for (i in seq_along(x)) if (!is.null(x[[i]])) local_names(x[[i]])
    }
  }
  local_names(fn[[3]])
  walk <- function(x, lits) {
    if (is.name(x)) {
      s <- as.character(x)
      if (nzchar(s) && s %in% names(map)) return(as.name(map[[s]]))
      return(x)
    }
    if (lits && is.numeric(x) && length(x) == 1L) return(quote(K))
    if (lits && is.character(x) && length(x) == 1L) return(quote(S))
    if (is.call(x)) {
      for (i in seq_along(x)) if (!is.null(x[[i]])) x[[i]] <- walk(x[[i]], lits)
      return(x)
    }
    if (is.pairlist(x)) return(x)
    x
  }
  fm <- names(fn[[2]])
  b1 <- walk(fn[[3]], FALSE)
  b2 <- walk(fn[[3]], TRUE)
  list(exact = paste(c(length(fm), deparse(b1, width.cutoff = 500L)), collapse = "\n"),
       near = paste(c(length(fm), deparse(b2, width.cutoff = 500L)), collapse = "\n"))
}

rows <- list()
for (fa in fams) {
  idx <- which(fam == fa)
  for (i in idx) {
    d <- defs[[i]]
    n <- d[["name"]]
    nz <- tryCatch(rename_locals(d[["fn"]]), error = function(e) list(exact = NA, near = NA))
    rows[[length(rows) + 1L]] <- data.frame(
      family = fa, name = n, file = d[["file"]], line = d[["line"]], nlines = d[["nlines"]],
      exported = n %in% exports,
      rd = as.integer(n %in% aliases),
      tested = hits(idx_tests, n) > 0L,
      callers_R = hits(idx_r, n),
      vignettes = hits(idx_vign, n),
      other_pkgs = hits(idx_oth, n),
      exact = nz[["exact"]], near = nz[["near"]],
      stringsAsFactors = FALSE
    )
  }
}
m <- do.call(rbind, rows)
m[["exact_id"]] <- match(m[["exact"]], unique(m[["exact"]]))
m[["near_id"]] <- match(m[["near"]], unique(m[["near"]]))
ce <- table(m[["exact_id"]])
cn <- table(m[["near_id"]])
m[["exact_size"]] <- as.integer(ce[as.character(m[["exact_id"]])])
m[["near_size"]] <- as.integer(cn[as.character(m[["near_id"]])])

fsum <- do.call(rbind, lapply(split(m, m[["family"]]), function(g) {
  data.frame(family = g[["family"]][[1]], n = nrow(g), lines = sum(g[["nlines"]]),
             exported = sum(g[["exported"]]), documented = sum(g[["rd"]] > 0L),
             tested = sum(g[["tested"]]), called_in_R = sum(g[["callers_R"]] > 0L),
             in_vignettes = sum(g[["vignettes"]] > 0L), in_other_pkgs = sum(g[["other_pkgs"]] > 0L),
             files = length(unique(g[["file"]])),
             in_exact_dup = sum(g[["exact_size"]] > 1L), in_near_dup = sum(g[["near_size"]] > 1L))
}))
fsum <- fsum[order(-fsum[["n"]]), ]
cat("\nfamilies with 3 or more numbered members:", nrow(fsum), "; members:", sum(fsum[["n"]]),
    "; lines:", sum(fsum[["lines"]]), "\n")
print(fsum, row.names = FALSE)

clus <- function(key, size) {
  k <- m[m[[size]] > 1L, ]
  if (!nrow(k)) return(NULL)
  sp <- split(k, k[[key]])
  do.call(rbind, lapply(sp, function(g) {
    data.frame(cluster = g[[key]][[1]], size = nrow(g), lines = sum(g[["nlines"]]),
               rd_pages = sum(g[["rd"]] > 0L), families = paste(unique(g[["family"]]), collapse = ","),
               members = paste(g[["name"]], collapse = " "), stringsAsFactors = FALSE)
  }))
}
ex <- clus("exact_id", "exact_size")
nr <- clus("near_id", "near_size")
cat("\nexact clusters (same code after renaming locals):", if (is.null(ex)) 0L else nrow(ex),
    "; functions in them:", if (is.null(ex)) 0L else sum(ex[["size"]]),
    "; lines:", if (is.null(ex)) 0L else sum(ex[["lines"]]),
    "; Rd pages:", if (is.null(ex)) 0L else sum(ex[["rd_pages"]]), "\n")
if (!is.null(ex)) print(ex[order(-ex[["size"]]), ], row.names = FALSE, right = FALSE)
cat("\nnear clusters (also constants and strings abstracted):", if (is.null(nr)) 0L else nrow(nr),
    "; functions in them:", if (is.null(nr)) 0L else sum(nr[["size"]]),
    "; lines:", if (is.null(nr)) 0L else sum(nr[["lines"]]),
    "; Rd pages:", if (is.null(nr)) 0L else sum(nr[["rd_pages"]]), "\n")
if (!is.null(nr)) print(nr[order(-nr[["size"]]), ], row.names = FALSE, right = FALSE)

# the whole package, for scale: exact duplicates among all definitions
# (identical bodies after comments and layout are dropped; locals not renamed, to keep it quick)
allz <- vapply(defs, function(d) paste(c(names(d[["fn"]][[2]]), deparse(d[["fn"]][[3]])), collapse = "\n"), "")
small <- vapply(defs, `[[`, 1L, "nlines") < 4L
dup <- allz %in% allz[duplicated(allz)] & !small
cat("\nwhole package: functions", length(defs), "; with a body identical to another's (4+ lines):", sum(dup),
    "; clusters:", length(unique(allz[dup])), "\n")

utils::write.table(m[, setdiff(names(m), c("exact", "near"))], file.path(outdir, "numfam_members.tsv"),
                   sep = "\t", quote = FALSE, row.names = FALSE)
utils::write.table(fsum, file.path(outdir, "numfam_families.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
if (!is.null(nr)) utils::write.table(nr, file.path(outdir, "numfam_near.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
if (!is.null(ex)) utils::write.table(ex, file.path(outdir, "numfam_exact.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
cat("tables in", outdir, "\n")
