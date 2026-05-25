# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Generates R/datasets_tps_arcgis_hub_wrappers.R from the bundled
# TPS Hub catalog fixture. Reproducible: re-run after the catalog
# refreshes (e.g. when TPS publishes new datasets) and the
# generated file gets overwritten cleanly.
#
# Skips wrappers whose name would collide with one already exported
# by 3EE / 3FF / etc. The dedupe map is printed at the end so
# follow-up phases can review and either rename the existing
# wrappers or skip the catalog entries.
#
# Run from the package root with:
#   Rscript data-raw/generate_tps_hub_wrappers.R

# ---------------------------------------------------------------------------
# Step 1 -- load the catalog
# ---------------------------------------------------------------------------

cat_path <- "inst/extdata/tps_arcgis_hub_catalog.csv"
stopifnot(file.exists(cat_path))
cat <- utils::read.csv(cat_path, stringsAsFactors = FALSE,
                        check.names = FALSE)
cat("[gen] loaded catalog: ", nrow(cat), " rows\n", sep = "")

# ---------------------------------------------------------------------------
# Step 2 -- slug rules (title -> snake_case identifier)
# ---------------------------------------------------------------------------

slugify <- function(title) {
  s <- title
  # Strip ASR / RBDC table codes that disambiguate but inflate names.
  s <- gsub("\\s*\\(ASR-[A-Z]+-TBL-\\d+\\)", "", s)
  s <- gsub("\\s*\\(RBDC-[A-Z]+-TBL-\\d+\\)", "", s)
  # Strip "Open Data" suffix noise.
  s <- gsub("\\s*Open Data", "", s, ignore.case = TRUE)
  # Family prefixes -> short forms to keep names tab-completion friendly.
  s <- gsub("^Use of Force:\\s*", "Use of Force ", s)
  # Lowercase + alphanumeric.
  s <- tolower(s)
  s <- gsub("[^a-z0-9]+", "_", s)
  s <- gsub("^_+|_+$", "", s)
  s <- gsub("_+", "_", s)
  s
}

cat$slug <- vapply(cat$title, slugify, character(1L))
cat$wrapper_name <- paste0("morie_datasets_tps_", cat$slug)

# ---------------------------------------------------------------------------
# Step 3 -- dedupe vs existing exports
# ---------------------------------------------------------------------------
#
# The existing 3EE / 3FF / pre-3CC PSDP wrappers used slug forms
# without underscores between words ("hatecrimes" not "hate_crimes",
# "autotheft" not "auto_theft", "intimate_partner_family_violence"
# rather than the catalog's "intimate_partner_and_family_violence").
# We treat each existing name as an alias for any slug that would
# collapse to it after stripping non-letter characters AND that
# corresponds to the same catalog hub_id family.

existing <- readLines("NAMESPACE")
existing <- existing[grepl("^export\\(morie_datasets_tps_", existing)]
existing_names <- sub("^export\\(([^)]+)\\)$", "\\1", existing)

# Build a "compressed" form (strip underscores) for fuzzy match.
compress <- function(x) gsub("_", "", x)
existing_compressed <- compress(existing_names)

cat$compressed <- compress(cat$wrapper_name)
cat$skipped <- cat$wrapper_name %in% existing_names |
               cat$compressed %in% existing_compressed

# Also treat the "and"-bearing catalog titles as aliases for the
# "and"-stripped existing wrappers (3FF used the shorter form).
and_strip <- function(x) gsub("_and_", "_", x)
cat$skipped <- cat$skipped |
               and_strip(cat$wrapper_name) %in% existing_names

# Manual aliases (catalog title -> existing wrapper). These are
# cases where the existing wrapper uses a semantic abbreviation
# (e.g. "mha" for "mental_health_act") that fuzzy matching can't
# reach without spelling out the mapping.
manual_aliases <- list(
  mental_health_act_apprehensions = "morie_datasets_tps_mha_apprehensions"
)
for (slug in names(manual_aliases)) {
  cat$skipped[cat$slug == slug] <- TRUE
}

n_skipped <- sum(cat$skipped)
n_emit    <- sum(!cat$skipped)
cat("[gen] skipping ", n_skipped, " collisions; emitting ",
    n_emit, " new wrappers\n", sep = "")

# Sanity: dedupe within the catalog itself (titles that produce
# the same slug after normalization).
dup_slugs <- unique(cat$slug[duplicated(cat$slug) & !cat$skipped])
if (length(dup_slugs) > 0L) {
  stop(sprintf("[gen] internal slug collisions: %s",
               paste(dup_slugs, collapse = ", ")))
}

# ---------------------------------------------------------------------------
# Step 4 -- emit R source
# ---------------------------------------------------------------------------

out_path <- "R/datasets_tps_arcgis_hub_wrappers.R"
con <- file(out_path, "w")
on.exit(close(con), add = TRUE)

writeLines(c(
"# SPDX-License-Identifier: AGPL-3.0-or-later",
"#",
"# THIS FILE IS GENERATED. Do not edit by hand.",
"#",
"# Regenerate with:",
"#   Rscript data-raw/generate_tps_hub_wrappers.R",
"#",
"# Source: inst/extdata/tps_arcgis_hub_catalog.csv (71 TPS Hub items,",
"# canonical count verified live 2026-05-24).",
"#",
"# Each wrapper is a thin dispatch to morie_datasets_tps_arcgis_hub_by_id",
"# with the hub item_id hard-coded. Skipped catalog entries whose",
"# slug collides with an existing TPS export at generation time:",
sprintf("#   %s", paste(cat$wrapper_name[cat$skipped], collapse = ", ")),
"",
""), con)

emit <- cat[!cat$skipped, , drop = FALSE]
# Stable ordering: alphabetical by wrapper name.
emit <- emit[order(emit$wrapper_name), , drop = FALSE]

for (i in seq_len(nrow(emit))) {
  e <- emit[i, ]
  title  <- e$title
  hub_id <- e$hub_id
  wname  <- e$wrapper_name
  tags   <- e$tags
  snippet <- e$snippet
  fs_path <- sub("^https://services\\.arcgis\\.com/", "", e$feature_server_url)
  # Multi-line description.
  desc_lines <- if (nzchar(snippet)) {
    paste0("#'   ", strwrap(snippet, width = 65))
  } else {
    "#'   Toronto Police Service dataset."
  }
  writeLines(c(
    sprintf("#' %s", title),
    "#'",
    "#' Toronto PS ArcGIS Hub dataset wrapper. Thin dispatch to",
    "#' [morie_datasets_tps_arcgis_hub_by_id()] with the canonical",
    sprintf("#' hub item_id `%s`.", hub_id),
    "#'",
    desc_lines,
    "#'",
    sprintf("#' Tags: %s", if (nzchar(tags)) tags else "(none)"),
    "#'",
    "#' @inheritParams morie_datasets_tps_arcgis_hub_by_id",
    "#' @return A data.frame / GeoJSON list / file path; see",
    "#'   [morie_datasets_tps_arcgis_hub_by_id()] for per-format semantics.",
    "#' @export",
    sprintf("%s <- function(format = \"json\",", wname),
    paste0(strrep(" ", nchar(wname) + 17L),
            "where = \"1=1\","),
    paste0(strrep(" ", nchar(wname) + 17L),
            "max_features = NULL,"),
    paste0(strrep(" ", nchar(wname) + 17L),
            "layer_idx = 0L,"),
    paste0(strrep(" ", nchar(wname) + 17L),
            "offline = TRUE,"),
    paste0(strrep(" ", nchar(wname) + 17L),
            "dest = NULL) {"),
    sprintf("  morie_datasets_tps_arcgis_hub_by_id(\"%s\",", hub_id),
    "                                       format = format,",
    "                                       where = where,",
    "                                       max_features = max_features,",
    "                                       layer_idx = layer_idx,",
    "                                       offline = offline,",
    "                                       dest = dest)",
    "}",
    ""), con)
}

cat("[gen] wrote ", nrow(emit), " wrappers to ", out_path, "\n", sep = "")

# ---------------------------------------------------------------------------
# Step 5 -- audit log (helpful for the commit message)
# ---------------------------------------------------------------------------

cat("\n--- skipped (collide with existing 3EE/3FF) ---\n")
for (i in which(cat$skipped)) {
  cat(sprintf("  %s   <- '%s' (%s)\n",
               cat$wrapper_name[i], cat$title[i], cat$hub_id[i]))
}
cat(sprintf("\n--- emitted: %d wrappers ---\n", nrow(emit)))
