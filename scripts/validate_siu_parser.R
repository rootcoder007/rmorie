#!/usr/bin/env Rscript
# SPDX-License-Identifier: AGPL-3.0-or-later
#
# validate_siu_parser.R -- LOCAL DIAGNOSTIC for the C/C++ SIU parser.
# Not shipped to users; not the package's validation methodology.
# The package's validation contract is: every emitted row is
# reproducible from its cached HTML via the C++ parser (see
# morie_siu_audit_case()). This script is just a fast diagnostic for
# maintainers who want to spot-check changes against locally
# available reference files.
#
# Tier 1 (Schema): emitted columns match the reference SIU.csv header
#   exactly (order + names). Required to pass.
#
# Tier 2 (Replication parity): for a random sample of drids that the
#   prior parser version's SIU.csv produced rows for, re-fetch with
#   the current transport and re-parse. Report per-field exact-match
#   rate. Useful for catching unintended behavioural changes between
#   parser versions; not a correctness claim.
#
# Tier 3 (External agreement, informational only): for the subset of
#   sampled drids whose case_number also appears in a user-supplied
#   external coding (e.g. a hand-coded survey), report per-field
#   agreement. An external source is NOT ground truth -- the SIU
#   report HTML is. Agreement with an external source is a sanity
#   check; disagreements are not parser bugs by themselves, they
#   are leads to investigate by reading the HTML.
#
# Usage:
#   Rscript scripts/validate_siu_parser.R \
#     --ref-dir <local-refs> \
#     --sample 100 \
#     --out <validate-out.json>

suppressPackageStartupMessages({
  library(morie)
})

# ---------------------------- args --------------------------------
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default) {
  i <- which(args == flag)
  if (length(i) == 1L && i < length(args)) args[i + 1L] else default
}
ref_dir   <- get_arg("--ref-dir", "./refs")
sample_n  <- as.integer(get_arg("--sample", 100L))
out_path  <- get_arg("--out", "validate-out.json")
seed      <- as.integer(get_arg("--seed", 20260520L))
rate_rps  <- as.numeric(get_arg("--rate", 4.0))

stopifnot(dir.exists(ref_dir),
          file.exists(file.path(ref_dir, "SIU.csv")),
          file.exists(file.path(ref_dir, "SIU1a.xlsx")))

# -------------------- Tier 1: schema check ------------------------
cat("\n=== Tier 1: schema ===\n")
ref_hdr <- readLines(file.path(ref_dir, "SIU.csv"), n = 1L)
ref_cols <- strsplit(ref_hdr, ",", fixed = TRUE)[[1]]

# Parse our synthetic fixture to get our emitted column set in canonical
# order without hitting the network. The C++ parser ALWAYS emits the
# same 64-column structure regardless of input content.
fake_html <- paste0(
  "<html><body><h2 id=\"section_4\">The Investigation</h2>",
  "<p>Notification: On January 1, 2024 the Toronto Police Service ",
  "contacted the SIU. Director's Report for Case # 24-TFI-001.</p>",
  "</body></html>"
)
our_row <- morie:::.siu_parse_report(fake_html, 9999L, "test")
our_cols <- names(our_row)

schema_match <- identical(ref_cols, our_cols)
cat(sprintf("  ref cols:        %d\n  v0.2.0 cols:     %d\n  exact match:     %s\n",
            length(ref_cols), length(our_cols), schema_match))
if (!schema_match) {
  cat("  ref - ours:", setdiff(ref_cols, our_cols), "\n")
  cat("  ours - ref:", setdiff(our_cols, ref_cols), "\n")
}

# -------- pick the sample (drids that v0.1.0 parsed cleanly) -------
ref_csv <- utils::read.csv(file.path(ref_dir, "SIU.csv"),
                           colClasses = "character", check.names = FALSE)
ref_csv$drid <- suppressWarnings(as.integer(ref_csv$drid))
ref_valid <- ref_csv[is.finite(ref_csv$drid) & nzchar(ref_csv$case_number), ,
                     drop = FALSE]
cat(sprintf("\nReference SIU.csv: %d rows, %d with case_number\n",
            nrow(ref_csv), nrow(ref_valid)))

set.seed(seed)
sample_drids <- sort(sample(ref_valid$drid,
                            size = min(sample_n, nrow(ref_valid))))
cat(sprintf("Sampling %d drids (seed=%d)\n", length(sample_drids), seed))

# ---- re-fetch and re-parse with v0.2.0 throttled fetcher ---------
urls <- sprintf("https://www.siu.on.ca/en/directors_report_details.php?drid=%d",
                sample_drids)
cat(sprintf("\nFetching %d drids at %.1f rps ...\n", length(urls), rate_rps))
t0 <- Sys.time()
fetch <- morie:::.siu_http_get_many_with_status(
  urls, concurrency = 4L, timeout_s = 60L,
  rate_rps = rate_rps, max_retries = 3L
)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("  done in %.1fs (mean body: %.0f bytes, healthy 200s: %d/%d)\n",
            elapsed, mean(nchar(fetch$body)),
            sum(fetch$http_code == 200L), length(urls)))

ours <- lapply(seq_along(sample_drids), function(i) {
  if (nchar(fetch$body[i]) < 1000L) return(NULL)
  morie:::.siu_parse_report(fetch$body[i], sample_drids[i], urls[i])
})
ok <- !vapply(ours, is.null, logical(1))
ours_df <- as.data.frame(do.call(rbind, lapply(ours[ok], as.character)),
                         stringsAsFactors = FALSE)
names(ours_df) <- our_cols
ours_df$drid <- sample_drids[ok]
cat(sprintf("v0.2.0 parsed %d / %d sampled\n", sum(ok), length(sample_drids)))

# -------------------- Tier 2: replication parity -------------------
cat("\n=== Tier 2: replication parity (v0.2.0 vs v0.1.0) ===\n")
ref_joined <- merge(ours_df, ref_valid, by = "drid",
                    suffixes = c(".new", ".ref"))
parity_cols <- setdiff(our_cols, c("drid", "scraped_at_utc",
                                   "parser_version"))
parity_rate <- vapply(parity_cols, function(col) {
  a <- ref_joined[[paste0(col, ".new")]]
  b <- ref_joined[[paste0(col, ".ref")]]
  if (is.null(a) || is.null(b)) return(NA_real_)
  mean(trimws(a) == trimws(b), na.rm = TRUE)
}, numeric(1))
parity_overall <- mean(parity_rate, na.rm = TRUE)
cat(sprintf("  overall per-field exact-match: %.2f%%\n",
            100 * parity_overall))
cat("  worst 8 fields:\n")
worst <- sort(parity_rate)[1:8]
for (i in seq_along(worst)) {
  cat(sprintf("    %-40s %.1f%%\n", names(worst)[i], 100 * worst[i]))
}

# -------------------- Tier 3: human ground truth ------------------
cat("\n=== Tier 3: correctness vs SIU1a.xlsx (Qualtrics-coded) ===\n")
if (requireNamespace("readxl", quietly = TRUE)) {
  siu1a <- readxl::read_excel(file.path(ref_dir, "SIU1a.xlsx"))
  siu1a <- as.data.frame(siu1a)
  # First row is the question prose; drop it.
  if (any(grepl("Start Date", siu1a[1, ], fixed = TRUE))) {
    siu1a <- siu1a[-1L, , drop = FALSE]
  }
  # Q-col -> morie-field mapping (from SIU_diff_vs_1a.jsonl)
  q_map <- list(
    Q1  = "case_number",
    Q3  = "police_service",
    Q4  = "number_of_officers_involved",
    Q5  = "location_of_call",
    Q9  = "number_of_affected_persons",
    Q10 = "sex_gender_affected",
    Q11 = "age_affected",
    Q14 = "number_of_civilian_witnesses",
    Q16 = "number_of_subject_officials",
    Q19 = "number_of_witness_officials",
    Q26 = "charges_recommended"
  )
  siu1a$case_number <- as.character(siu1a$Q1)
  joined <- merge(ours_df, siu1a, by = "case_number")
  cat(sprintf("  cases in BOTH our parse + SIU1a Qualtrics: %d\n",
              nrow(joined)))

  normalise_val <- function(v) {
    v <- trimws(as.character(v))
    v <- gsub("\\.0+$", "", v)            # 2.0 -> 2
    v <- tolower(v)
    v[v %in% c("na", "n/a", "")] <- ""
    v
  }
  agree <- lapply(names(q_map), function(q) {
    fld <- q_map[[q]]
    if (!q %in% names(joined) || !fld %in% names(joined)) return(NULL)
    a <- normalise_val(joined[[fld]])
    b <- normalise_val(joined[[q]])
    keep <- nzchar(a) & nzchar(b)
    if (!any(keep)) return(NULL)
    data.frame(
      field = fld, q = q, n = sum(keep),
      agree = sum(a[keep] == b[keep]),
      rate = mean(a[keep] == b[keep]),
      stringsAsFactors = FALSE
    )
  })
  agree_df <- do.call(rbind, agree)
  cat("  per-field agreement (human-coded ground truth):\n")
  for (i in seq_len(nrow(agree_df))) {
    cat(sprintf("    %-40s %d/%d = %.1f%%\n",
                agree_df$field[i], agree_df$agree[i], agree_df$n[i],
                100 * agree_df$rate[i]))
  }
  overall_human <- weighted.mean(agree_df$rate, agree_df$n)
  cat(sprintf("  weighted overall correctness: %.2f%%\n",
              100 * overall_human))
} else {
  cat("  skipped: install.packages('readxl') to enable Tier 3\n")
  agree_df <- NULL
  overall_human <- NA_real_
}

# -------------------- write JSON summary --------------------------
summary <- list(
  schema_match = schema_match,
  ref_cols = length(ref_cols),
  our_cols = length(our_cols),
  sample_n = length(sample_drids),
  parsed_n = sum(ok),
  rate_rps = rate_rps,
  parity_overall = unname(parity_overall),
  parity_per_field = as.list(parity_rate),
  tier3_overall = unname(overall_human),
  tier3_per_field = if (!is.null(agree_df)) as.list(setNames(
    agree_df$rate, agree_df$field)) else NULL
)
writeLines(jsonlite::toJSON(summary, pretty = TRUE, auto_unbox = TRUE),
           out_path)
cat(sprintf("\nWrote summary to %s\n", out_path))
