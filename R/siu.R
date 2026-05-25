# SPDX-License-Identifier: AGPL-3.0-or-later
#
# siu.R -- orchestration for the all-C/C++ Ontario SIU parser.
#
# The HTTP transport (libcurl) and the 64-column HTML parsing both live
# in src/siu_parser.cpp. This file drives them: discover the published
# director's-report id range, concurrently fetch and parse every
# report page, fetch and parse the news releases they link, join the
# two, and write the 64-column SIU.csv.

# Highest director's-report id to fetch. Discovered from the SIU site's
# incremental index endpoint (its newest row is the highest indexed id),
# plus a margin: a report finalised after the index was last built can
# sit at a drid just above the newest indexed one, so iterating a little
# past it guarantees those are captured. Ids with no report parse to
# blank rows that are dropped, so the margin is free.
#
# As of 2026-05 the live max sits around drid ~5100; the default margin
# of 300 gives substantial headroom for reports added between manifest
# refreshes, and the `default` fallback (used only when the discovery
# endpoint is unreachable) is set to 6000 so cold-start sweeps still
# capture everything currently published.
.siu_discover_max_drid <- function(default = 6000L, margin = 300L) {
  html <- tryCatch(
    .siu_http_get(paste0(
      "https://www.siu.on.ca/ssi/get_more_drs.php",
      "?lang=en&lastCount=0"
    )),
    error = function(e) ""
  )
  hits <- regmatches(html, gregexpr('id="[0-9]+"', html))[[1L]]
  ids <- suppressWarnings(as.integer(gsub("\\D", "", hits)))
  ids <- ids[is.finite(ids)]
  if (length(ids)) max(ids) + as.integer(margin) else as.integer(default)
}

#' Fetch the Ontario SIU corpus into a 64-column SIU.csv
#'
#' Fetches and parses the Ontario Special Investigations Unit
#' (police-oversight) corpus -- every director's report and the news
#' releases they link -- into a single CSV with the canonical
#' 64-column schema, one row per case.
#'
#' The parser is implemented entirely in C/C++ (\code{src/siu_parser.cpp}):
#' libcurl drives the HTTP transport and a concurrent \code{curl_multi}
#' pool fetches the ~9,000+ pages, while the 64-field extraction is C++
#' \code{std::regex} parsing. There is no Python dependency.
#'
#' This is the \emph{Ontario} Special Investigations Unit -- distinct
#' from the federal Structured Intervention Units and from OTIS. The
#' parsed corpus is not shipped with the package; each user runs the
#' parser themselves, which is fair use of public oversight reports.
#'
#' @param cache_dir Output directory. Defaults to a session-scoped
#'   subdirectory of \code{\link[base]{tempdir}()} that R cleans up
#'   automatically. For persistent cross-session caching pass
#'   \code{cache_dir = morie_cache_dir("siu")} instead; see
#'   \code{\link{morie_cache_dir}} and \code{\link{morie_cache_clear}}.
#' @param overwrite Logical; if \code{FALSE} and \code{SIU.csv} already
#'   exists in \code{cache_dir}, its path is returned without reparsing.
#' @param max_drid Highest director's-report id to fetch. \code{NULL}
#'   (default) uses the shipped manifest's max + a small margin, falling
#'   back to discovery from the SIU site.
#' @param concurrency Maximum simultaneous HTTP transfers. Default
#'   \code{4} is a polite rate paired with \code{rate_rps = 4}; raising
#'   either above ~8/8 risks triggering WAF interstitials that return
#'   short non-report HTML.
#' @param rate_rps Maximum request starts per second across the pool
#'   (token-bucket throttle). Default \code{4} is the rate the package
#'   was empirically validated against; lower it on poor connections
#'   or contested endpoints.
#' @param use_manifest If \code{TRUE} (default), restrict the sweep to
#'   the known-valid drids in the shipped manifest
#'   (\code{inst/extdata/siu_drid_manifest.csv.gz}), still topping up
#'   with any drid above the manifest's max up to \code{max_drid}.
#'   Cuts the fetch by ~30-50 percent on a typical run by skipping holes.
#' @param lang Language filter on the manifest. \code{"all"} (default,
#'   back-compat) fetches every known-valid drid -- English and
#'   French copies of each case -- and then collapses to one row per
#'   case_number with English winning the dedupe. \code{"en"} fetches
#'   only the English drids (about half the size of the corpus and
#'   half the network round trips); \code{"fr"} fetches only French.
#'   Use \code{"en"} for the fastest cold-start when you only need
#'   the canonical English text.
#' @param cache_html If \code{TRUE}, gzip and save the raw HTML of
#'   every fetched director's-report and news-release page under
#'   \code{<cache_dir>/html/drid_NNNN.html.gz} and
#'   \code{<cache_dir>/html/nrid_NNNN.html.gz}. This is the persistent
#'   ground truth for every row in the emitted CSV: any later
#'   discrepancy between the parser and a human coder can be
#'   adjudicated against the saved HTML without re-hitting SIU. Adds
#'   ~80-100 MB to \code{cache_dir} for a full run; default
#'   \code{FALSE} (the harvester remains lean unless you ask).
#' @param progress Logical; print progress messages.
#' @return Path to the written \code{SIU.csv}.
#' @examples
#' \dontrun{
#' # Network: parses the full Ontario SIU corpus (~15-25 min at the
#' # default polite rate of 4 RPS).
#' csv <- morie_fetch_siu(cache_dir = tempdir())
#' siu <- utils::read.csv(csv)
#' nrow(siu)
#' }
#' @export
morie_fetch_siu <- function(cache_dir = file.path(tempdir(), "morie", "siu"),
                            overwrite = FALSE, max_drid = NULL,
                            concurrency = 4L, rate_rps = 4.0,
                            use_manifest = TRUE,
                            lang = c("all", "en", "fr"),
                            cache_html = FALSE,
                            progress = TRUE) {
  lang <- match.arg(lang)
  cache_dir <- path.expand(cache_dir)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  html_dir <- file.path(cache_dir, "html")
  if (cache_html) {
    dir.create(html_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }
  out_path <- file.path(cache_dir, "SIU.csv")
  if (file.exists(out_path) && !overwrite) {
    return(out_path)
  }

  manifest <- if (use_manifest) .siu_load_manifest() else NULL

  # Always probe past the *live* max so new reports added since the
  # manifest snapshot are captured. The manifest is a *floor* on the
  # known-valid id space, not a ceiling on what we sweep.
  if (is.null(max_drid)) {
    live_max <- .siu_discover_max_drid()
    manifest_max <- if (!is.null(manifest)) {
      max(manifest$drid, na.rm = TRUE)
    } else {
      0L
    }
    max_drid <- max(live_max, manifest_max + 300L)
  }
  max_drid <- as.integer(max_drid)
  base_r <- "https://www.siu.on.ca/en/directors_report_details.php?drid="
  base_n <- "https://www.siu.on.ca/en/news_template.php?nrid="

  # -- 1. fetch and parse every director's-report page --
  if (!is.null(manifest)) {
    # Known-valid drids from the manifest, optionally filtered by
    # language to save half the fetch when the user only needs EN
    # (or only FR). The manifest's _language column was populated
    # at refresh time by the parser's own language detector.
    in_lang <- if (lang == "all") {
      rep(TRUE, nrow(manifest))
    } else {
      manifest[["_language"]] %in% c(lang, "unknown")
    }
    manifest_max <- max(manifest$drid[in_lang], na.rm = TRUE)
    drids_below <- manifest$drid[in_lang & manifest$drid <= max_drid]
    # Above the manifest's max we don't yet know each new drid's
    # language; probe all of them and let parser drop non-matching
    # rows at dedupe time.
    drids_above <- if (max_drid > manifest_max) {
      seq.int(manifest_max + 1L, max_drid)
    } else {
      integer(0)
    }
    drids <- sort(unique(c(drids_below, drids_above)))
  } else {
    drids <- seq_len(max_drid)
  }
  if (progress) {
    message(
      "SIU: fetching ", length(drids), " report pages + ",
      "news releases (concurrency=", concurrency,
      ", rate=", rate_rps, " req/s, interleaved batches) ..."
    )
  }

  # -- 1+2. Interleaved fetch: each batch fires this batch's reports
  # in the SAME rate-limited pool as the PREVIOUS batch's news
  # releases. While the next 250 reports are downloading, the news
  # pages whose nrids we just parsed are downloading alongside.
  # Roughly halves wall time vs the prior two-phase serial flow,
  # without changing the per-second rate the SIU site sees.
  rep_html_all <- character(length(drids))
  news_html_by_nrid <- list()
  pending_nrids <- character(0)
  rep_rows_acc <- list()
  batch_size <- 250L
  batches <- split(
    seq_along(drids),
    ceiling(seq_along(drids) / batch_size)
  )

  for (bi in seq_along(batches)) {
    idx <- batches[[bi]]
    rep_urls <- paste0(base_r, drids[idx])
    news_urls <- if (length(pending_nrids)) {
      paste0(base_n, pending_nrids)
    } else {
      character(0)
    }
    combined <- c(rep_urls, news_urls)
    if (progress) {
      message(sprintf(
        "SIU batch %d/%d: %d reports + %d news ...",
        bi, length(batches),
        length(rep_urls), length(news_urls)
      ))
    }
    results <- .siu_http_get_many(
      combined,
      as.integer(concurrency),
      60L,
      as.numeric(rate_rps),
      3L
    )
    rep_results <- results[seq_along(rep_urls)]
    news_results <- if (length(news_urls)) {
      results[(length(rep_urls) + 1L):length(results)]
    } else {
      character(0)
    }

    # Persist HTML if requested
    if (cache_html) {
      for (k in seq_along(idx)) {
        if (nzchar(rep_results[k])) {
          .siu_write_html_cache(
            html_dir,
            sprintf("drid_%d.html.gz", drids[idx][k]),
            rep_results[k]
          )
        }
      }
      if (length(pending_nrids)) {
        for (k in seq_along(pending_nrids)) {
          if (nzchar(news_results[k])) {
            .siu_write_html_cache(
              html_dir,
              sprintf(
                "nrid_%s.html.gz",
                pending_nrids[k]
              ),
              news_results[k]
            )
          }
        }
      }
    }

    # Stow this batch's HTML
    rep_html_all[idx] <- rep_results
    if (length(pending_nrids)) {
      for (k in seq_along(pending_nrids)) {
        news_html_by_nrid[[pending_nrids[k]]] <- news_results[k]
      }
    }

    # Parse this batch's reports immediately so we can queue the
    # next batch's nrids for fetching alongside the next reports.
    batch_rep_rows <- lapply(seq_along(idx), function(k) {
      .siu_parse_report(
        rep_results[k], drids[idx][k],
        paste0(base_r, drids[idx][k])
      )
    })
    rep_rows_acc[idx] <- batch_rep_rows

    # Extract new nrids from this batch's reports
    batch_nrids <- vapply(
      batch_rep_rows,
      function(r) as.character(r[["nrid"]]),
      character(1)
    )
    batch_nrids <- batch_nrids[nzchar(batch_nrids)]
    # Only fetch each nrid once across the whole run
    pending_nrids <- setdiff(
      unique(batch_nrids),
      names(news_html_by_nrid)
    )
  }

  # Final cleanup: fetch the last batch's nrids that haven't been
  # paired with a next-batch's report fetch yet.
  if (length(pending_nrids)) {
    if (progress) {
      message(sprintf(
        "SIU final: %d remaining news pages ...",
        length(pending_nrids)
      ))
    }
    final_news <- .siu_http_get_many(
      paste0(base_n, pending_nrids),
      as.integer(concurrency),
      60L,
      as.numeric(rate_rps),
      3L
    )
    if (cache_html) {
      for (k in seq_along(pending_nrids)) {
        if (nzchar(final_news[k])) {
          .siu_write_html_cache(
            html_dir,
            sprintf(
              "nrid_%s.html.gz",
              pending_nrids[k]
            ),
            final_news[k]
          )
        }
      }
    }
    for (k in seq_along(pending_nrids)) {
      news_html_by_nrid[[pending_nrids[k]]] <- final_news[k]
    }
  }

  # Build the report data frame from the accumulated rows.
  df <- as.data.frame(
    do.call(rbind, lapply(rep_rows_acc, as.character)),
    stringsAsFactors = FALSE
  )
  names(df) <- names(rep_rows_acc[[1L]])

  # -- 3. Parse the news-release HTML we collected during the interleave --
  if (length(news_html_by_nrid)) {
    parsed_nrids <- names(news_html_by_nrid)
    news_rows <- lapply(seq_along(parsed_nrids), function(i) {
      .siu_parse_news(
        news_html_by_nrid[[parsed_nrids[i]]],
        as.integer(parsed_nrids[i]),
        paste0(base_n, parsed_nrids[i])
      )
    })
    news_df <- as.data.frame(
      do.call(rbind, lapply(news_rows, as.character)),
      stringsAsFactors = FALSE
    )
    names(news_df) <- names(news_rows[[1L]])

    # -- 4. join the news fields onto the report rows by nrid --
    j <- match(df$nrid, news_df$nrid)
    hit <- !is.na(j)
    for (col in c(
      "source_url_news", "news_release_title",
      "news_release_date_iso", "news_release_date_raw",
      "news_release_summary"
    )) {
      df[[col]][hit] <- news_df[[col]][j[hit]]
    }
  }

  # One row per case. Drop pages with no case number (non-existent or
  # draft drids), then collapse the English and French copies of a case
  # to a single row -- keeping the English one and its drid / nrid, so
  # every emitted row is one case identified by one case_number.
  df <- df[nzchar(df$case_number), , drop = FALSE]
  if (nrow(df) > 0L) {
    df <- df[order(df$case_number, df[["_language"]] != "en"), ,
      drop = FALSE
    ]
    df <- df[!duplicated(df$case_number), , drop = FALSE]
    rownames(df) <- NULL
  }

  # SIU pages (notably the French releases) are UTF-8.
  for (col in names(df)) Encoding(df[[col]]) <- "UTF-8"

  # Apply canonical overrides last (so they win over any regex
  # extraction). Merges the shipped maintainer-verified table with
  # the user-side cache_dir/canonical_overrides.csv if present.
  # This is how the parser "learns": every verified correction we
  # record becomes part of the output on the next run, no C++ build
  # needed.
  overrides <- .siu_load_canonical_overrides(user_cache_dir = cache_dir)
  if (!is.null(overrides)) {
    n_before_overrides <- sum(vapply(
      seq_len(nrow(overrides)),
      function(i) {
        case <- overrides$case_number[i]
        fld <- overrides$field[i]
        if (!fld %in% names(df)) {
          return(0L)
        }
        idx <- which(df$case_number == case)
        if (length(idx) == 1L &&
          df[[fld]][idx] != overrides$verified_value[i]) {
          1L
        } else {
          0L
        }
      }, integer(1)
    ))
    df <- .siu_apply_canonical_overrides(df, overrides)
    if (progress && n_before_overrides > 0L) {
      message(
        "SIU: applied ", n_before_overrides,
        " canonical-override correction(s) ",
        "(", nrow(overrides), " total in override table)"
      )
    }
  }

  utils::write.csv(df, out_path, row.names = FALSE, na = "")
  if (progress) {
    message(
      "SIU: wrote ", nrow(df), " rows (",
      sum(nzchar(df$case_number)), " with a case number) to ",
      out_path
    )
  }
  out_path
}

#' SIU drid → case_number → language index
#'
#' Returns the shipped drid manifest as a data frame -- one row per
#' director's-report id morie has verified, with the parsed case
#' number, detected language, and the canonical drid (the English
#' drid for that case, or the first drid if no English version
#' exists). This is the index \code{morie_fetch_siu()} uses
#' internally; exposing it lets users:
#'
#' \itemize{
#'   \item see exactly which drids ship as known-valid (no need
#'         to fetch to find out);
#'   \item subset to English-only or French-only case lists
#'         without running the full harvester;
#'   \item map between drid (URL fragment) and case_number (SIU's
#'         own identifier) offline.
#' }
#'
#' The manifest is refreshed by maintainers via
#' \code{morie_siu_refresh_manifest()}; it ships gzipped under
#' \code{inst/extdata/} at ~50 KB.
#'
#' @param lang Filter rows by detected language. \code{"all"}
#'   (default) returns every entry; \code{"en"} returns only the
#'   English drids; \code{"fr"} returns only French; \code{"valid"}
#'   returns every drid whose case_number was successfully parsed
#'   (drops blank / draft drids).
#' @param canonical_only If \code{TRUE}, returns one row per
#'   case_number (the canonical drid for that case, English
#'   preferred). Useful when you want a unique-cases index.
#' @return A data frame with columns \code{drid}, \code{http_code},
#'   \code{body_bytes}, \code{attempts}, \code{case_number},
#'   \code{_language}, \code{source}, \code{retrieved_at_utc},
#'   \code{canonical_drid}.
#' @examples
#' idx <- morie_siu_index(lang = "en")
#' head(idx)
#' # How many drids are English vs French vs unknown?
#' table(morie_siu_index()$`_language`)
#' # Unique-case index (English-preferred)
#' canon <- morie_siu_index(canonical_only = TRUE)
#' nrow(canon)
#' @export
morie_siu_index <- function(lang = c("all", "en", "fr", "valid"),
                            canonical_only = FALSE) {
  lang <- match.arg(lang)
  m <- .siu_load_manifest_raw()
  if (is.null(m)) {
    stop("No shipped SIU drid manifest available. The package ",
      "build is missing inst/extdata/siu_drid_manifest.csv.gz.",
      call. = FALSE
    )
  }
  if (lang == "valid") {
    m <- m[nzchar(m$case_number), , drop = FALSE]
  } else if (lang %in% c("en", "fr")) {
    m <- m[m[["_language"]] == lang, , drop = FALSE]
  }
  if (canonical_only && "canonical_drid" %in% names(m)) {
    m <- m[!is.na(m$canonical_drid) & m$drid == m$canonical_drid, ,
      drop = FALSE
    ]
  }
  rownames(m) <- NULL
  m
}

# Internal: read the unfiltered shipped manifest. Returns NULL on
# any failure. Unlike .siu_load_manifest() (which restricts to
# healthy 200s for harvester use), this returns ALL columns + rows
# so morie_siu_index() can serve the full table.
.siu_load_manifest_raw <- function() {
  p <- system.file("extdata", "siu_drid_manifest.csv.gz",
    package = "morie"
  )
  if (!nzchar(p) || !file.exists(p)) {
    return(NULL)
  }
  tryCatch(
    utils::read.csv(gzfile(p),
      colClasses = "character",
      check.names = FALSE
    ),
    error = function(e) NULL
  )
}

# Internal: read the shipped canonical override table if present.
# This is a (case_number, field, verified_value) long table -- one row
# per cell we've verified against the report HTML, by maintainer
# review or LLM consensus.  Applied by morie_fetch_siu() AFTER the
# regex extraction, so the parser "learns": every correction we
# commit propagates to all users on the next package update without
# any C++ rebuild.
#
# The user-side cache at <cache_dir>/canonical_overrides.csv mirrors
# the shipped one and is merged in too -- so individual users can
# record their own corrections without touching the package source.
# Maintainer-confirmed corrections get promoted into the shipped
# table; user-side corrections stay local until then.
.siu_load_canonical_overrides <- function(user_cache_dir = NULL) {
  read_one <- function(p) {
    if (!nzchar(p) || !file.exists(p)) {
      return(NULL)
    }
    df <- tryCatch(
      if (endsWith(p, ".gz")) {
        utils::read.csv(gzfile(p),
          colClasses = "character",
          check.names = FALSE
        )
      } else {
        utils::read.csv(p, colClasses = "character", check.names = FALSE)
      },
      error = function(e) NULL
    )
    if (is.null(df) || !nrow(df)) {
      return(NULL)
    }
    if (!all(c("case_number", "field", "verified_value") %in% names(df))) {
      return(NULL)
    }
    df
  }
  shipped <- read_one(system.file("extdata",
    "siu_canonical_overrides.csv.gz",
    package = "morie"
  ))
  user <- if (!is.null(user_cache_dir)) {
    read_one(file.path(
      path.expand(user_cache_dir),
      "canonical_overrides.csv"
    ))
  } else {
    NULL
  }
  if (is.null(shipped) && is.null(user)) {
    return(NULL)
  }
  out <- rbind(
    shipped[, c("case_number", "field", "verified_value"), drop = FALSE],
    user[, c("case_number", "field", "verified_value"), drop = FALSE]
  )
  # User overrides win on conflict: rev() so most-recent insertions
  # land at the top of unique().
  out <- out[!duplicated(rev(out)[, c("case_number", "field")]), ,
    drop = FALSE
  ]
  out
}

# Internal: apply a canonical-overrides table to a parsed SIU data
# frame. Each row of `overrides` is (case_number, field, verified_value);
# for any match, overwrite df[[field]] at the row whose case_number
# matches. Silent on misses (override for a case not in the parse,
# or field not in the schema).
.siu_apply_canonical_overrides <- function(df, overrides) {
  if (is.null(overrides) || !nrow(overrides)) {
    return(df)
  }
  for (i in seq_len(nrow(overrides))) {
    case <- overrides$case_number[i]
    fld <- overrides$field[i]
    val <- overrides$verified_value[i]
    if (!fld %in% names(df)) next
    row_idx <- which(df$case_number == case)
    if (length(row_idx) == 1L) {
      df[[fld]][row_idx] <- val
    }
  }
  df
}

#' Record a verified correction to the SIU parser's output
#'
#' Saves a (case_number, field, verified_value) tuple to a local
#' overrides CSV at \code{<cache_dir>/canonical_overrides.csv}. Every
#' subsequent \code{morie_fetch_siu()} on that \code{cache_dir} will
#' overlay these corrections onto the regex-parsed output. The shipped
#' \code{inst/extdata/siu_canonical_overrides.csv.gz} carries
#' maintainer-confirmed corrections; this function lets users add
#' their own without touching the package source.
#'
#' This is the "memory" of the parser: every wrong cell you find and
#' fix becomes permanent for that cache_dir. Maintainers can submit
#' corrections upstream by sharing the resulting CSV file.
#'
#' @param case_number SIU case number, e.g. \code{"17-OVI-201"}.
#' @param field Name of the column in the SIU schema (e.g.
#'   \code{"location_of_call"}).
#' @param verified_value The correct value, verified against the
#'   cached HTML (see \code{morie_siu_audit_case()}).
#' @param note Optional one-line note describing the basis for the
#'   correction (HTML excerpt, LLM verdict, etc.).
#' @param cache_dir Directory holding the harvester's SIU.csv.
#' @return Invisibly, the path to the updated overrides CSV.
#' @examples
#' \donttest{
#' # Writes the correction to a temp cache so the example never
#' # touches the per-user cache directory.
#' tmp <- tempfile("morie_siu_"); dir.create(tmp, recursive = TRUE)
#' morie_siu_record_correction(
#'   case_number = "17-OVI-201",
#'   field = "location_of_call",
#'   verified_value = "Clair Road East, City of Guelph",
#'   note = "HTML excerpt: 'on Clair Road East in the City of Guelph'",
#'   cache_dir = tmp
#' )
#' unlink(tmp, recursive = TRUE)
#' }
#' @export
morie_siu_record_correction <- function(case_number, field,
                                        verified_value, note = "",
                                        cache_dir = file.path(tempdir(), "morie", "siu")) {
  stopifnot(
    is.character(case_number), length(case_number) == 1L,
    is.character(field), length(field) == 1L,
    is.character(verified_value), length(verified_value) == 1L,
    field %in% .siu_field_list()
  )
  cache_dir <- path.expand(cache_dir)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(cache_dir, "canonical_overrides.csv")
  existing <- if (file.exists(path)) {
    utils::read.csv(path, colClasses = "character", check.names = FALSE)
  } else {
    data.frame(
      case_number = character(), field = character(),
      verified_value = character(), note = character(),
      recorded_at_utc = character(),
      stringsAsFactors = FALSE
    )
  }
  # De-dupe: last write wins for any (case, field) pair.
  existing <- existing[!(existing$case_number == case_number &
    existing$field == field), , drop = FALSE]
  new_row <- data.frame(
    case_number = case_number,
    field = field,
    verified_value = verified_value,
    note = note,
    recorded_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  out <- rbind(existing, new_row)
  utils::write.csv(out, path, row.names = FALSE)
  invisible(path)
}

# Internal: read the shipped DRID manifest if present. Returns NULL on
# any failure so the harvester degrades gracefully to a full sweep.
.siu_load_manifest <- function() {
  p <- system.file("extdata", "siu_drid_manifest.csv.gz", package = "morie")
  if (!nzchar(p) || !file.exists(p)) {
    return(NULL)
  }
  m <- tryCatch(
    utils::read.csv(gzfile(p), colClasses = c(
      drid = "integer", http_code = "integer", body_bytes = "integer",
      case_number = "character", source = "character",
      retrieved_at_utc = "character"
    )),
    error = function(e) NULL
  )
  if (is.null(m) || !nrow(m) || !"drid" %in% names(m)) {
    return(NULL)
  }
  # Keep only confirmed-valid rows (200 OK + non-trivial body + parsed
  # case_number). These are the drids worth re-fetching on a run; the
  # rest are deliberately skipped to save bandwidth.
  m <- m[is.finite(m$drid) & m$http_code == 200L &
    m$body_bytes >= 1000L & nzchar(m$case_number), , drop = FALSE]
  if (!nrow(m)) {
    return(NULL)
  }
  m
}

#' Rebuild the Ontario SIU DRID manifest by probing the live site
#'
#' Sweeps director's-report ids \code{1..max_drid} and writes a small
#' CSV recording which ids return a healthy report page, the parsed
#' case number, and the response body size. The harvester
#' (\code{morie_fetch_siu}) then uses this manifest to short-circuit
#' the ~30-50 percent of ids that have no report, saving bandwidth and
#' WAF-trigger risk on every run.
#'
#' The shipped manifest at \code{inst/extdata/siu_drid_manifest.csv.gz}
#' is a snapshot. Users who want the latest can call this function;
#' it is also how morie maintainers regenerate the snapshot.
#'
#' @param out_path Path to write the gzipped CSV. Default is the
#'   in-place manifest location (only useful for maintainers building
#'   from a source checkout).
#' @param max_drid Highest drid to probe. Default \code{NULL}
#'   auto-discovers from the SIU index endpoint and adds a margin.
#' @param min_drid Lowest drid to probe (default \code{1L}).
#' @param concurrency Maximum simultaneous transfers (default \code{4}).
#' @param rate_rps Maximum request starts per second (default \code{4}).
#' @param progress Logical; print a per-batch progress line.
#' @return Invisibly, a data frame of the full sweep (every probed drid,
#'   including misses), parallel to what was written to \code{out_path}.
#' @examples
#' \dontrun{
#' # Network: refreshes the manifest by probing the SIU site
#' # (~25-40 min at the default polite rate of 4 RPS for ~6000 ids).
#' df <- morie_siu_refresh_manifest(out_path = tempfile(fileext = ".csv.gz"))
#' table(df$http_code)
#' }
#' @export
morie_siu_refresh_manifest <- function(
  out_path = NULL, max_drid = NULL, min_drid = 1L,
  concurrency = 4L, rate_rps = 4.0, progress = TRUE
) {
  # Manifest refresh sweeps a generous range so the resulting snapshot
  # stays useful for several months without re-probing. Default is
  # max(live-discovery + margin, 6000) — the live max currently sits
  # around drid ~5100, and 6000 gives headroom for ~one year of new
  # reports at the SIU's historical publish cadence.
  if (is.null(max_drid)) max_drid <- max(.siu_discover_max_drid(), 6000L)
  min_drid <- as.integer(min_drid)
  max_drid <- as.integer(max_drid)
  stopifnot(min_drid >= 1L, max_drid >= min_drid)

  base_r <- "https://www.siu.on.ca/en/directors_report_details.php?drid="
  drids <- seq.int(min_drid, max_drid)
  if (progress) {
    message(
      "SIU manifest: probing ", length(drids), " drids ",
      "[", min_drid, "..", max_drid, "] at ",
      rate_rps, " req/s ..."
    )
  }
  res <- .siu_http_get_many_with_status(
    paste0(base_r, drids),
    as.integer(concurrency),
    60L,
    as.numeric(rate_rps),
    3L
  )

  # Parse case_number from each body so the manifest can ship the
  # canonical id mapping. Empty body => empty case_number.
  case_no <- vapply(res$body, function(html) {
    if (!nzchar(html) || nchar(html) < 1000L) {
      return("")
    }
    parsed <- tryCatch(.siu_parse_report(html, 0L, ""),
      error = function(e) NULL
    )
    if (is.null(parsed)) "" else as.character(parsed[["case_number"]])
  }, character(1), USE.NAMES = FALSE)

  df <- data.frame(
    drid = drids,
    http_code = as.integer(res$http_code),
    body_bytes = as.integer(nchar(res$body)),
    attempts = as.integer(res$attempts),
    case_number = case_no,
    source = "siu.on.ca",
    retrieved_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )

  if (!is.null(out_path)) {
    gz <- gzfile(out_path, "w")
    utils::write.csv(df, gz, row.names = FALSE)
    close(gz)
    if (progress) {
      ok <- sum(df$http_code == 200L & df$body_bytes >= 1000L)
      message(
        "SIU manifest: wrote ", nrow(df), " rows (",
        ok, " healthy 200s) to ", out_path
      )
    }
  }
  invisible(df)
}

# Internal: write one HTML page to <html_dir>/<name>, gzipped. Called
# from morie_fetch_siu() when cache_html = TRUE.
.siu_write_html_cache <- function(html_dir, name, html) {
  con <- gzfile(file.path(html_dir, name), "w")
  on.exit(close(con), add = TRUE)
  writeChar(html, con, eos = NULL)
}

# Internal: read a gzipped cached HTML page if it exists, else "".
.siu_read_html_cache <- function(html_dir, name) {
  p <- file.path(html_dir, name)
  if (!file.exists(p)) {
    return("")
  }
  con <- gzfile(p, "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, "raw", n = file.info(p)$size * 50L)
  rawToChar(bytes)
}

#' Audit one SIU case end-to-end: parser output + raw HTML
#'
#' For any case_number (or drid), return the parser's 64-column row
#' together with the raw HTML pages it was extracted from -- the
#' director's-report page and, when linked, the news-release page.
#' This is the per-row ground truth: every field in the emitted CSV
#' is reproducible from \code{report_html} via the parser, and any
#' disagreement with another data source can be adjudicated against
#' the saved HTML.
#'
#' Reads from the local cache at \code{<cache_dir>/html/} (populated
#' by \code{morie_fetch_siu(cache_html = TRUE)}) when available, and
#' falls back to a polite live fetch when the cache is missing.
#'
#' @param case_number An SIU case number (e.g. \code{"17-OVI-201"}),
#'   or an integer drid.
#' @param cache_dir Directory holding the harvester's SIU.csv and the
#'   optional \code{html/} subdirectory.
#' @param fetch_if_missing If \code{TRUE} (default), fetch the page
#'   from SIU when the local cache misses. Set \code{FALSE} to work
#'   strictly from the cache.
#' @return A list with elements \code{row} (the parser's 1-row data
#'   frame for this case), \code{drid}, \code{nrid},
#'   \code{report_html}, \code{news_html}, \code{report_text}
#'   (HTML-stripped plain text of the report) and \code{news_text}.
#' @examples
#' \dontrun{
#' a <- morie_siu_audit_case(
#'   "17-OVI-201",
#'   cache_dir = file.path(tempdir(), "morie", "siu")
#' )
#' cat(substr(a$report_text, 1, 1000), "\n")
#' }
#' @export
morie_siu_audit_case <- function(case_number,
                                 cache_dir = file.path(tempdir(), "morie", "siu"),
                                 fetch_if_missing = TRUE) {
  cache_dir <- path.expand(cache_dir)
  html_dir <- file.path(cache_dir, "html")
  csv_path <- file.path(cache_dir, "SIU.csv")
  if (!file.exists(csv_path)) {
    stop("No SIU.csv at '", csv_path, "'; run morie_fetch_siu() first.",
      call. = FALSE
    )
  }
  df <- utils::read.csv(csv_path,
    colClasses = "character",
    check.names = FALSE
  )
  # Caller may pass a case_number string OR a numeric drid.
  if (is.numeric(case_number)) {
    drid <- as.integer(case_number)
    row <- df[df$drid == as.character(drid), , drop = FALSE]
  } else {
    row <- df[df$case_number == case_number, , drop = FALSE]
  }
  if (!nrow(row)) {
    stop("No row found for '", case_number, "' in ", csv_path,
      call. = FALSE
    )
  }
  drid <- as.integer(row$drid[1L])
  nrid <- suppressWarnings(as.integer(row$nrid[1L]))

  fetch_one <- function(url) {
    tryCatch(.siu_http_get(url), error = function(e) "")
  }

  report_html <- .siu_read_html_cache(
    html_dir,
    sprintf("drid_%d.html.gz", drid)
  )
  if (!nzchar(report_html) && fetch_if_missing) {
    report_html <- fetch_one(paste0(
      "https://www.siu.on.ca/en/directors_report_details.php?drid=",
      drid
    ))
  }

  news_html <- ""
  if (!is.na(nrid)) {
    news_html <- .siu_read_html_cache(
      html_dir,
      sprintf("nrid_%d.html.gz", nrid)
    )
    if (!nzchar(news_html) && fetch_if_missing) {
      news_html <- fetch_one(paste0(
        "https://www.siu.on.ca/en/news_template.php?nrid=", nrid
      ))
    }
  }

  list(
    row = row,
    drid = drid,
    nrid = nrid,
    report_html = report_html,
    news_html = news_html,
    report_text = .siu_html_to_text(report_html),
    news_text = .siu_html_to_text(news_html)
  )
}

# Internal: R-side HTML-to-text helper. Strips tags + decodes the
# most common entities so reports + news releases can be displayed
# as plain text. Mirrors the C++ html_to_text() but with the safer
# linear single-pass approach (no std::regex backtracking risk).
.siu_html_to_text <- function(h) {
  if (!nzchar(h)) {
    return("")
  }
  # Drop <script>...</script> and <style>...</style> chunks first.
  h <- gsub("(?is)<script\\b[^>]*>.*?</script>", " ", h, perl = TRUE)
  h <- gsub("(?is)<style\\b[^>]*>.*?</style>", " ", h, perl = TRUE)
  h <- gsub("<[^>]+>", " ", h, perl = TRUE)
  # Common named entities (small set covering ~99% of SIU pages).
  ents <- c(
    "&amp;" = "&", "&lt;" = "<", "&gt;" = ">", "&quot;" = "\"",
    "&apos;" = "'", "&#39;" = "'", "&nbsp;" = " ",
    "&rsquo;" = "'", "&lsquo;" = "'", "&ldquo;" = "\"",
    "&rdquo;" = "\"", "&ndash;" = "-", "&mdash;" = "-",
    "&hellip;" = "..."
  )
  for (k in names(ents)) h <- gsub(k, ents[[k]], h, fixed = TRUE)
  # Numeric entities (decimal + hex).
  h <- gsub("&#([0-9]+);", "\\1",
    h,
    perl = TRUE
  ) # leaves digits; cheap fallback
  h <- gsub("\\s+", " ", h, perl = TRUE)
  trimws(h)
}

#' Field-by-field SIU comparison against a user-supplied external table
#'
#' For one case_number, line up the parser's value against the same
#' field in a user-supplied external data source -- and, critically,
#' show the surrounding report HTML so the user can adjudicate any
#' disagreement against the actual source document.
#'
#' \strong{The ground truth is the SIU director's-report HTML
#' itself.} The HTML is what the SIU published; the parser's job is
#' to extract structured fields from it faithfully, and any field's
#' correctness is decidable by reading the cached HTML for that
#' case. Any external reference -- a hand-coded survey, an
#' independently-scraped CSV, a colleague's analysis -- is just
#' another extraction attempt, possibly with its own errors. This
#' function does not endorse any external source; it only displays
#' both side-by-side with the HTML excerpt so you can decide.
#'
#' The default field map covers the common SIU-extraction column
#' layout (\code{Q1 = case_number}, \code{Q3 = police_service},
#' \code{Q4 = number_of_officers_involved}, ...). Pass a custom
#' \code{field_map} for any other external schema.
#'
#' @param case_number A case number (e.g. \code{"17-OVI-201"}).
#' @param external A data frame of external answers, OR a path to an
#'   \code{.xlsx} file (read with \code{readxl}). Must contain a
#'   column whose values match SIU case numbers (default
#'   \code{external_case_col = "Q1"}).
#' @param field_map A named list mapping external-column names to
#'   morie field names.
#' @param external_case_col Name of the external column carrying the
#'   case-number key.
#' @param cache_dir Directory holding the harvester's SIU.csv and
#'   optional cached HTML.
#' @return A data frame with one row per mapped field: \code{field},
#'   \code{parser_value}, \code{external_value}, \code{agree}, and
#'   \code{html_excerpt} (a 240-character window around the first
#'   occurrence of either value in the cleaned report text). When
#'   parser and external disagree, the \code{html_excerpt} is the
#'   tie-breaker.
#' @examples
#' \dontrun{
#' # Caller supplies their own external table; nothing about the
#' # mapping or the file format is canonical to morie.
#' external <- data.frame(case_id = "17-OVI-201", officers = 1L)
#' cmp <- morie_siu_compare(
#'   "17-OVI-201",
#'   external = external,
#'   field_map = list(officers = "number_of_officers_involved"),
#'   external_case_col = "case_id"
#' )
#' subset(cmp, !agree)
#' }
#' @export
morie_siu_compare <- function(case_number, external,
                              field_map = NULL,
                              external_case_col = "Q1",
                              cache_dir = file.path(tempdir(), "morie", "siu")) {
  if (is.null(field_map)) {
    # Convenience default for the most common SIU-extraction column
    # naming. Override with your own field_map for any other schema.
    # No endorsement of any particular external source is intended.
    field_map <- list(
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
  }

  if (is.character(external) && length(external) == 1L &&
    file.exists(external)) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Install 'readxl' to read .xlsx, or pass a data frame ",
        "instead.",
        call. = FALSE
      )
    }
    external <- as.data.frame(readxl::read_excel(external))
    # Many survey exports prefix data with a row of question prose;
    # drop it if the first cell doesn't look like an SIU case number.
    if (nrow(external) >= 1L &&
      !grepl(
        "^[0-9]{2}-[A-Z]{3,4}-[0-9]{3}",
        as.character(external[[external_case_col]][1L])
      )) {
      external <- external[-1L, , drop = FALSE]
    }
  }
  if (!external_case_col %in% names(external)) {
    stop("External has no column '", external_case_col, "'.",
      call. = FALSE
    )
  }

  audit <- morie_siu_audit_case(case_number, cache_dir = cache_dir)
  e_row <- external[as.character(external[[external_case_col]]) ==
    case_number, , drop = FALSE]
  if (!nrow(e_row)) {
    stop("External has no row for '", case_number, "'.", call. = FALSE)
  }

  text <- audit$report_text
  excerpt_for <- function(needle) {
    if (!nzchar(needle) || !nzchar(text)) {
      return("")
    }
    p <- regexpr(needle, text, fixed = TRUE)
    if (p < 0L) {
      return("")
    }
    start <- max(1L, p - 80L)
    end <- min(nchar(text), p + nchar(needle) + 80L)
    substr(text, start, end)
  }
  norm <- function(x) {
    x <- trimws(as.character(x))
    x <- gsub("\\.0+$", "", x)
    tolower(x)
  }

  rows <- lapply(names(field_map), function(k) {
    fld <- field_map[[k]]
    if (!(k %in% names(e_row)) || !(fld %in% names(audit$row))) {
      return(NULL)
    }
    pv <- as.character(audit$row[[fld]][1L])
    ev <- as.character(e_row[[k]][1L])
    data.frame(
      field = fld,
      parser_value = pv,
      external_value = ev,
      agree = norm(pv) == norm(ev),
      html_excerpt = if (nzchar(pv)) {
        excerpt_for(pv)
      } else {
        excerpt_for(ev)
      },
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# ===========================================================================
# LLM-assisted SIU extraction. Optional helpers that send a cached SIU
# director's-report HTML page through a large-language-model endpoint
# and return the same 64-column row format (for morie_siu_llm_extract)
# or per-field "does this extraction match the report?" verdicts (for
# morie_siu_anomaly_check). The cached HTML remains the ground truth;
# the LLM output is just another extraction attempt that can be
# diffed against the C++ parser via morie_siu_compare().
#
# Providers are configured via env vars so secrets never appear in
# the package or in chat:
#   GOOGLE_API_KEY     -> Gemini (default; cheapest)
#   ANTHROPIC_API_KEY  -> Claude
# Both functions hard-fail with a clear message if the relevant env
# var is missing.
# ===========================================================================

# Internal: minimal provider table. Each entry's `build()` returns a
# fully-resolved request spec (url, headers, body); the dispatcher
# below sends it via httr2 and the `extract()` pulls the model's
# text out of the parsed response. Hand-written rather than pulling
# a heavy LLM client library so morie's dep surface stays tiny
# (httr2 + jsonlite, both in Suggests).
#
# Providers:
#   gemini  -- closed, paid, fast.  env: GOOGLE_API_KEY
#   claude  -- closed, paid, fast.  env: ANTHROPIC_API_KEY
#   ollama  -- open-weight models   env: OLLAMA_HOST (e.g.
#              over a local or self-       "http://localhost:11434"
#              hosted REST endpoint;       or any hosted Ollama-
#              free/OllamaFreeAPI-         compatible base URL),
#              compatible.                 optional OLLAMA_MODEL
#                                          (default "llama3.2:3b")
# Internal: default LLM HTTP timeout in seconds. 600s (10 min)
# accommodates slow CPU-only local inference on a Raspberry Pi.
# Override globally via MORIE_LLM_TIMEOUT_S env var.
.siu_llm_default_timeout <- function() {
  v <- Sys.getenv("MORIE_LLM_TIMEOUT_S", unset = "")
  t <- suppressWarnings(as.integer(v))
  if (!is.finite(t) || t < 1L) 600L else t
}

.siu_llm_providers <- function() {
  list(
    gemini = list(
      env_required = "GOOGLE_API_KEY",
      build = function(env, prompt) {
        list(
          url = paste0(
            "https://generativelanguage.googleapis.com/v1beta/",
            "models/gemini-2.5-flash:generateContent?key=",
            env[["GOOGLE_API_KEY"]]
          ),
          headers = list("content-type" = "application/json"),
          body = list(
            contents = list(list(parts = list(list(text = prompt)))),
            generationConfig = list(
              temperature = 0,
              response_mime_type = "application/json"
            )
          )
        )
      },
      extract = function(resp) {
        x <- resp$candidates[[1L]]$content$parts[[1L]]$text
        if (is.null(x)) stop("Gemini returned empty text", call. = FALSE)
        x
      }
    ),
    claude = list(
      env_required = "ANTHROPIC_API_KEY",
      build = function(env, prompt) {
        list(
          url = "https://api.anthropic.com/v1/messages",
          headers = list(
            "x-api-key" = env[["ANTHROPIC_API_KEY"]],
            "anthropic-version" = "2023-06-01",
            "content-type" = "application/json"
          ),
          body = list(
            model = "claude-sonnet-4-6",
            max_tokens = 8192L,
            messages = list(list(role = "user", content = prompt))
          )
        )
      },
      extract = function(resp) {
        x <- resp$content[[1L]]$text
        if (is.null(x)) stop("Claude returned empty text", call. = FALSE)
        x
      }
    ),
    vertex = list(
      # Google Cloud Vertex AI Gemini. Cheaper than AI Studio's
      # consumer endpoint; routes through a GCP project for billing
      # (e.g. against an existing GCP credit). The bearer token is
      # whatever you have in VERTEX_ACCESS_TOKEN -- typically the
      # output of `gcloud auth print-access-token` on a machine
      # where gcloud is signed in to the relevant project.
      #
      # GCP_PROJECT defaults to "hadesllm" and GCP_LOCATION to
      # "us-central1" if unset; override either via env. Model
      # defaults to gemini-2.5-flash; override via VERTEX_MODEL.
      env_required = "VERTEX_ACCESS_TOKEN",
      build = function(env, prompt) {
        project <- if (nzchar(Sys.getenv("GCP_PROJECT", ""))) {
          Sys.getenv("GCP_PROJECT")
        } else {
          "hadesllm"
        }
        location <- if (nzchar(Sys.getenv("GCP_LOCATION", ""))) {
          Sys.getenv("GCP_LOCATION")
        } else {
          "us-central1"
        }
        model <- if (nzchar(Sys.getenv("VERTEX_MODEL", ""))) {
          Sys.getenv("VERTEX_MODEL")
        } else {
          "gemini-2.5-flash"
        }
        list(
          url = sprintf(
            paste0(
              "https://%s-aiplatform.googleapis.com/v1/projects/%s",
              "/locations/%s/publishers/google/models/%s",
              ":generateContent"
            ),
            location, project, location, model
          ),
          headers = list(
            "authorization" =
              paste("Bearer", env[["VERTEX_ACCESS_TOKEN"]]),
            "content-type" = "application/json"
          ),
          body = list(
            contents = list(list(
              role = "user",
              parts = list(list(text = prompt))
            )),
            generationConfig = list(
              temperature = 0,
              responseMimeType = "application/json"
            )
          )
        )
      },
      extract = function(resp) {
        x <- resp$candidates[[1L]]$content$parts[[1L]]$text
        if (is.null(x)) stop("Vertex returned empty text", call. = FALSE)
        x
      }
    ),
    ollama = list(
      # OLLAMA_HOST is the documented env var, but if unset we silently
      # try a local daemon at http://localhost:11434 -- that's the
      # zero-config path for a user who's just installed `ollama` and
      # pulled `gemma3:4b` (or any Gemma / Functiongemma variant). No
      # API key, no paid subscription, no signup needed.
      #
      # On CPU-only hardware (e.g. Raspberry Pi 5) the default
      # gemma3:4b model is slow (~3 tok/sec); set OLLAMA_MODEL to
      # gemma3:270m (~290 MB, ~50 tok/sec) for a 10x speedup with
      # only modest quality loss. OLLAMA_KEEP_ALIVE keeps the loaded
      # model resident across requests so we don't pay the 10s
      # cold-start tax per case.
      env_required = "OLLAMA_HOST_OR_DEFAULT",
      build = function(env, prompt) {
        host <- sub("/+$", "", env[["OLLAMA_HOST_OR_DEFAULT"]])
        headers <- list("content-type" = "application/json")
        # Optional bearer token, for hosted Ollama-compatible APIs
        # (e.g. OllamaFreeAPI gateways) that require auth. Local
        # Ollama at localhost:11434 doesn't need it.
        api_key <- Sys.getenv("OLLAMA_API_KEY", unset = "")
        if (nzchar(api_key)) {
          headers[["authorization"]] <- paste("Bearer", api_key)
        }
        list(
          url = paste0(host, "/api/generate"),
          headers = headers,
          body = list(
            model = if (nzchar(Sys.getenv("OLLAMA_MODEL", ""))) {
              Sys.getenv("OLLAMA_MODEL")
            } else {
              "gemma3:4b"
            },
            prompt = prompt,
            format = "json",
            stream = FALSE,
            keep_alive = if (nzchar(Sys.getenv("OLLAMA_KEEP_ALIVE", ""))) {
              Sys.getenv("OLLAMA_KEEP_ALIVE")
            } else {
              "30m"
            },
            options = list(temperature = 0, num_ctx = 16384L)
          )
        )
      },
      extract = function(resp) {
        x <- resp$response
        if (is.null(x)) {
          stop("Ollama returned empty response",
            call. = FALSE
          )
        }
        x
      }
    )
  )
}

# Internal: fire ONE LLM request through a single provider. Returns
# the model's raw text reply. Errors propagate to the caller so the
# chain dispatcher below can decide whether to fall back.
#
# Default timeout is 600s (10 min) -- long enough to accommodate
# slow CPU-only local Ollama generation on a Raspberry Pi. Override
# via MORIE_LLM_TIMEOUT_S env var or the timeout_s arg.
.siu_llm_call_one <- function(model, prompt,
                              timeout_s = .siu_llm_default_timeout()) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("LLM helpers require the 'httr2' package: ",
      "install.packages('httr2')",
      call. = FALSE
    )
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("LLM helpers require the 'jsonlite' package", call. = FALSE)
  }
  providers <- .siu_llm_providers()
  if (!model %in% names(providers)) {
    stop("Unknown LLM model: '", model, "'. Available: ",
      paste(names(providers), collapse = ", "),
      call. = FALSE
    )
  }
  p <- providers[[model]]
  # Ollama gets a localhost:11434 default if OLLAMA_HOST is unset --
  # that's the zero-config "install ollama, pull gemma3:4b, done"
  # path. All other providers still hard-require their API key env.
  if (p$env_required == "OLLAMA_HOST_OR_DEFAULT") {
    env_val <- Sys.getenv("OLLAMA_HOST", unset = "")
    if (!nzchar(env_val)) env_val <- "http://localhost:11434"
  } else {
    env_val <- Sys.getenv(p$env_required, unset = "")
    if (!nzchar(env_val)) {
      stop("Env var '", p$env_required, "' is not set; cannot call ",
        model, ". Set it, or use model = \"ollama\" with a local ",
        "Ollama daemon for a free zero-config alternative.",
        call. = FALSE
      )
    }
  }
  env <- setNames(list(env_val), p$env_required)
  req_spec <- p$build(env, prompt)
  req <- httr2::request(req_spec$url)
  if (!is.null(req_spec$headers)) {
    req <- httr2::req_headers(req, !!!req_spec$headers)
  }
  req <- httr2::req_body_json(req, req_spec$body)
  req <- httr2::req_timeout(req, timeout_s)
  resp <- httr2::req_perform(req)
  parsed <- httr2::resp_body_json(resp)
  p$extract(parsed)
}

# Internal: try `model` in order. The first one whose env var is set
# AND whose request returns without erroring wins. `model` may be a
# character vector for failover (e.g. c("gemini", "ollama")). The
# `mock_response_text` arg exists ONLY so unit tests can exercise
# the surrounding R glue without hitting the network.
.siu_llm_call <- function(model, prompt,
                          timeout_s = .siu_llm_default_timeout(),
                          mock_response_text = NULL) {
  if (!is.null(mock_response_text)) {
    return(mock_response_text)
  }
  if (!length(model)) {
    stop("`model` must be a non-empty character vector",
      call. = FALSE
    )
  }
  errs <- character(0)
  for (m in model) {
    res <- tryCatch(
      .siu_llm_call_one(m, prompt, timeout_s = timeout_s),
      error = function(e) structure(conditionMessage(e), class = "err")
    )
    if (!inherits(res, "err")) {
      return(res)
    }
    errs <- c(errs, sprintf("[%s] %s", m, as.character(res)))
  }
  stop("All LLM providers failed:\n  ",
    paste(errs, collapse = "\n  "),
    call. = FALSE
  )
}

# The canonical 64-column SIU schema. Hard-coded so the LLM gets the
# exact field list and order the C++ parser emits.
.siu_field_list <- function() {
  c(
    "case_number", "drid", "nrid", "source_url_report", "source_url_news",
    "scraped_at_utc", "parser_version", "date_of_incident_iso",
    "date_of_incident_raw", "time_of_incident_raw", "date_of_injury_iso",
    "date_of_injury_raw", "incident_to_injury_raw", "date_siu_notified_iso",
    "date_siu_notified_raw", "time_of_notification_raw", "notifying_party",
    "notifying_party_other_text", "date_of_director_decision_iso",
    "date_of_director_decision_raw", "time_of_director_decision_raw",
    "siu_investigators", "siu_forensics_investigators", "police_service",
    "number_of_officers_involved", "location_of_call",
    "type_of_building_or_scene", "reason_for_interaction",
    "injuries_sustained", "injuries_other_text", "specific_injuries",
    "location_of_treatment", "number_of_affected_persons",
    "sex_gender_affected", "age_affected", "affected_interviewed",
    "date_of_affected_interview_iso", "date_of_affected_interview_raw",
    "number_of_civilian_witnesses", "date_of_witness_interview_raw",
    "number_of_subject_officials", "subject_official_interviewed_or_notes",
    "date_of_subject_interview_raw", "number_of_witness_officials",
    "date_of_witness_official_interview_raw", "evidence_types",
    "evidence_other_text", "evidence_features", "narrative_summary",
    "relevant_legislation", "legislation_other_text",
    "weapons_or_force_used", "weapons_other_text", "charges_recommended",
    "directors_decision_reasonable", "supplemental_materials",
    "news_links_extra", "mental_health_or_race_indications", "_language",
    "news_release_title", "news_release_date_iso", "news_release_date_raw",
    "news_release_summary", "directors_name"
  )
}

#' Extract SIU report fields with an LLM (Gemini or Claude)
#'
#' Sends the cached director's-report HTML for one case through a
#' large-language-model endpoint and asks it to return the 64-column
#' morie schema as JSON. The result is in the SAME row format as the
#' C++ parser, so it drops straight into \code{morie_siu_compare()}
#' as the \code{external} argument for an independent diff against
#' the parser.
#'
#' The cached HTML remains the ground truth. This function does not
#' claim the LLM is more accurate than the regex parser; it provides
#' a fast second extraction so disagreements between two independent
#' methods (regex vs. LLM) can be flagged for human review against
#' the saved report.
#'
#' Credentials are read from environment variables only -- never
#' hard-coded, never passed as function arguments -- so secrets do
#' not leak into call traces, logs, or scripts. Set
#' \code{GOOGLE_API_KEY} for Gemini, \code{ANTHROPIC_API_KEY} for
#' Claude, or \code{OLLAMA_HOST} (e.g.
#' \code{"http://localhost:11434"} or an OllamaFreeAPI base URL) plus
#' optionally \code{OLLAMA_MODEL} (default \code{"llama3.2:3b"}) for
#' Ollama-compatible open-weight endpoints.
#'
#' @param case_number An SIU case number (e.g. \code{"17-OVI-201"}).
#' @param model One of \code{"ollama"} (default; free, runs locally,
#'   zero-config when an Ollama daemon is on \code{localhost:11434}),
#'   \code{"gemini"} (paid), or \code{"claude"} (paid). A character
#'   vector enables fail-over: the first model whose call succeeds
#'   wins. The default \code{c("ollama", "gemini")} tries the local
#'   free model first and only escalates to paid Gemini if Ollama
#'   isn't installed or fails -- so morie costs $0 to use as long
#'   as you have a free Gemma / Qwen / Llama running locally
#'   (e.g. \code{ollama pull gemma3:4b}).
#' @param cache_dir Directory holding the harvester's SIU.csv and
#'   the optional \code{html/} subdirectory.
#' @param max_html_chars Soft cap on the HTML payload sent to the
#'   model (default 80,000 -- larger than any real SIU report,
#'   small enough to stay under typical context budgets).
#' @param mock_response_text For testing only: if non-NULL, skip the
#'   network call and use this string as the model's raw reply.
#' @return A one-row data frame with the 64 morie SIU columns. Any
#'   field the model could not extract is the empty string
#'   (matching the C++ parser's convention).
#' @examples
#' \dontrun{
#' Sys.setenv(GOOGLE_API_KEY = "your-gemini-key")
#' r <- morie_siu_llm_extract("17-OVI-201", model = "gemini")
#' # Diff parser vs LLM against the HTML:
#' morie_siu_compare(
#'   "17-OVI-201",
#'   external = r,
#'   field_map = setNames(as.list(names(r)), names(r)),
#'   external_case_col = "case_number"
#' )
#' }
#' @export
morie_siu_llm_extract <- function(case_number, model = c("ollama", "gemini"),
                                  cache_dir = file.path(tempdir(), "morie", "siu"),
                                  max_html_chars = 80000L,
                                  mock_response_text = NULL) {
  audit <- morie_siu_audit_case(case_number,
    cache_dir = cache_dir,
    fetch_if_missing = is.null(mock_response_text)
  )
  html <- audit$report_html
  if (!nzchar(html)) {
    stop("No HTML available for '", case_number, "'.",
      call. = FALSE
    )
  }
  if (nchar(html) > max_html_chars) html <- substr(html, 1L, max_html_chars)

  fields <- .siu_field_list()
  prompt <- paste(
    "You are extracting structured data from an Ontario Special",
    "Investigations Unit (SIU) director's report. Return ONLY a JSON",
    "object with these exact keys, in this order. Use the empty string",
    "for fields the report does not state. Use ISO 8601 (YYYY-MM-DD)",
    "for any *_iso date field; keep the report's original wording in",
    "the matching *_raw field. For boolean fields, return \"true\" or",
    "\"false\". Do not invent any values.\n\n",
    "Keys:\n", paste(fields, collapse = ", "), "\n\n",
    "Report HTML:\n", html
  )

  text <- .siu_llm_call(model, prompt,
    mock_response_text = mock_response_text
  )
  # Some models wrap JSON in ```json ... ```; strip if present.
  text <- gsub("^```(?:json)?\\s*|\\s*```$", "", text, perl = TRUE)
  parsed <- jsonlite::fromJSON(text, simplifyVector = TRUE)
  # Coerce to a row of `fields` exact length + order, blanks where missing.
  vals <- vapply(fields, function(f) {
    v <- parsed[[f]]
    if (is.null(v) || (length(v) == 1L && is.na(v))) {
      ""
    } else {
      as.character(v)[1L]
    }
  }, character(1))
  out <- as.data.frame(t(vals), stringsAsFactors = FALSE)
  names(out) <- fields
  # Always overwrite the bookkeeping columns with what we know.
  out$case_number <- as.character(audit$row$case_number[1L])
  out$drid <- as.character(audit$drid)
  if (!is.na(audit$nrid)) out$nrid <- as.character(audit$nrid)
  out$source_url_report <- paste0(
    "https://www.siu.on.ca/en/directors_report_details.php?drid=",
    audit$drid
  )
  out$parser_version <- paste0("llm-", paste(model, collapse = "+"))
  out$scraped_at_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  out
}

#' Per-field anomaly check: does the parser's extraction match the HTML?
#'
#' For each populated field in the parser's row, ask the LLM whether
#' the extracted value is supported by the cached report HTML. Used
#' to surface fields where the regex parser is plausibly wrong --
#' the LLM's verdicts are not authoritative, just an automated way
#' to triage which rows a human should re-read against the HTML.
#'
#' One API call is made per case (all fields batched into a single
#' prompt with structured-JSON output).
#'
#' @inheritParams morie_siu_llm_extract
#' @return A data frame with one row per populated parser field:
#'   \code{field}, \code{parser_value}, \code{verdict} (one of
#'   \code{"agree"} / \code{"disagree"} / \code{"unclear"}), and
#'   \code{reason} (a short sentence pointing to the report passage).
#' @examples
#' \dontrun{
#' Sys.setenv(GOOGLE_API_KEY = "your-gemini-key")
#' a <- morie_siu_anomaly_check("17-OVI-201", model = "gemini")
#' subset(a, verdict == "disagree")
#' }
#' @export
morie_siu_anomaly_check <- function(case_number, model = c("ollama", "gemini"),
                                    cache_dir = file.path(tempdir(), "morie", "siu"),
                                    max_html_chars = 80000L,
                                    mock_response_text = NULL) {
  audit <- morie_siu_audit_case(case_number,
    cache_dir = cache_dir,
    fetch_if_missing = is.null(mock_response_text)
  )
  html <- audit$report_html
  if (!nzchar(html)) {
    stop("No HTML available for '", case_number, "'.",
      call. = FALSE
    )
  }
  if (nchar(html) > max_html_chars) html <- substr(html, 1L, max_html_chars)

  # Build the (field, parser_value) pairs we'll ask the LLM to check.
  # Skip empty fields and pure-metadata columns the parser sets
  # mechanically (source_url, drid, nrid, scraped_at_utc, etc.).
  meta_cols <- c(
    "drid", "nrid", "source_url_report", "source_url_news",
    "scraped_at_utc", "parser_version", "_language"
  )
  populated <- Filter(
    function(p) nzchar(p$value) && !(p$field %in% meta_cols),
    lapply(setdiff(names(audit$row), meta_cols), function(col) {
      list(field = col, value = as.character(audit$row[[col]][1L]))
    })
  )
  if (!length(populated)) {
    return(data.frame(
      field = character(0), parser_value = character(0),
      verdict = character(0), reason = character(0),
      stringsAsFactors = FALSE
    ))
  }

  pairs_block <- paste(vapply(populated, function(p) {
    sprintf(
      "- %s: %s", p$field,
      substr(p$value, 1L, 400L)
    ) # cap each value at 400 chars
  }, character(1)), collapse = "\n")

  prompt <- paste0(
    "You are auditing a regex-based extractor against an Ontario SIU\n",
    "director's-report HTML page. For each field below, decide if the\n",
    "extracted VALUE is supported by the REPORT.\n\n",
    "Return ONLY a JSON array of objects, one per field, with keys:\n",
    "  field (string), verdict (one of \"agree\", \"disagree\",\n",
    "  \"unclear\"), reason (one short sentence quoting the report\n",
    "  passage if possible).\n",
    "Use \"unclear\" if the report neither confirms nor contradicts.\n\n",
    "FIELDS:\n", pairs_block, "\n\n",
    "REPORT HTML:\n", html
  )

  text <- .siu_llm_call(model, prompt,
    mock_response_text = mock_response_text
  )
  text <- gsub("^```(?:json)?\\s*|\\s*```$", "", text, perl = TRUE)
  rows <- jsonlite::fromJSON(text, simplifyVector = TRUE)
  if (is.null(rows) || (is.data.frame(rows) && !nrow(rows))) {
    return(data.frame(
      field = character(0), parser_value = character(0),
      verdict = character(0), reason = character(0),
      stringsAsFactors = FALSE
    ))
  }
  # Merge parser_value back in.
  parser_vals <- setNames(
    vapply(populated, function(p) p$value, character(1)),
    vapply(populated, function(p) p$field, character(1))
  )
  rows <- as.data.frame(rows, stringsAsFactors = FALSE)
  if (!"field" %in% names(rows)) {
    stop("LLM response missing 'field' column", call. = FALSE)
  }
  rows$parser_value <- unname(parser_vals[rows$field])
  rows <- rows[, c("field", "parser_value", "verdict", "reason")]
  rows
}

#' Row-level sanity check on a parsed SIU table (regex-only, no LLM)
#'
#' For every row in a parser-emitted SIU table, flag cells that
#' don't match the expected format for their column -- `case_number`
#' that doesn't look like an SIU case id, `date_*_iso` that isn't a
#' valid ISO 8601 date, `number_of_*` that isn't a positive integer,
#' `charges_recommended` that isn't "Yes" / "No", etc. Returns a
#' data frame ranked by issue count so the most-broken rows surface
#' at the top for manual inspection against the cached HTML.
#'
#' Designed to be a fast first-pass quality filter -- runs in
#' milliseconds, no network, no LLM, no API key. Doesn't try to
#' verify correctness against the underlying report (that's what
#' \code{morie_siu_audit_columns()} is for); just checks that each
#' value MATCHES THE EXPECTED FORMAT for its field. A clean sanity
#' check is necessary but not sufficient for correctness.
#'
#' @param df A data frame in the morie SIU 64-column schema, or a
#'   path to such a CSV.
#' @return A data frame with one row per source row, columns:
#'   \code{case_number}, \code{drid}, \code{issues_count} (integer
#'   number of suspicious cells), \code{issues} (semicolon-separated
#'   string of \code{field:reason} pairs). Ordered descending by
#'   \code{issues_count}.
#' @examples
#' \dontrun{
#' csv <- morie_fetch_siu(cache_dir = tempdir(), cache_html = TRUE)
#' sanity <- morie_siu_sanity_check(csv)
#' head(sanity, 10) # worst 10 rows -- inspect against HTML
#' table(sanity$issues_count)
#' }
#' @export
morie_siu_sanity_check <- function(df) {
  if (is.character(df) && length(df) == 1L && file.exists(df)) {
    df <- utils::read.csv(df,
      colClasses = "character",
      check.names = FALSE
    )
  }
  stopifnot(is.data.frame(df), "case_number" %in% names(df))

  check_case <- function(v) {
    bad <- nzchar(v) & !grepl("^[0-9]{2}-[A-Z]{2,4}-[0-9]{3}$", v)
    ifelse(bad, "case_number:bad-format", "")
  }
  check_iso <- function(v, col) {
    bad <- nzchar(v) & !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", v)
    ifelse(bad, paste0(col, ":bad-iso"), "")
  }
  check_int <- function(v, col) {
    bad <- nzchar(v) & !grepl("^[0-9]+$", v)
    ifelse(bad, paste0(col, ":not-int"), "")
  }
  check_yn <- function(v, col) {
    bad <- nzchar(v) & !v %in% c("Yes", "No")
    ifelse(bad, paste0(col, ":not-Yes/No"), "")
  }
  check_gender <- function(v) {
    bad <- nzchar(v) & !v %in% c("Male", "Female", "Non-binary")
    ifelse(bad, "sex_gender_affected:bad-value", "")
  }
  check_officer_count <- function(v) {
    # Should be "N SO" or "N SO M WO" or "N WO"
    bad <- nzchar(v) &
      !grepl("^[0-9]+ (SO|WO)( [0-9]+ WO)?$", v)
    ifelse(bad, "number_of_officers_involved:bad-format", "")
  }
  check_nonempty <- function(v, col) {
    # Used for fields that should always populate when case_number is set
    ifelse(nzchar(df$case_number) & !nzchar(v),
      paste0(col, ":empty-when-expected"), ""
    )
  }
  check_short <- function(v, col, min_chars) {
    bad <- nzchar(v) & nchar(v) < min_chars
    ifelse(bad, paste0(col, ":suspiciously-short"), "")
  }
  check_chrome <- function(v, col) {
    # Page-chrome words that should never leak into report fields
    bad <- grepl("Liaison Program|twitter\\.com|skipNavigation|sitemap",
      v,
      ignore.case = FALSE
    )
    ifelse(bad, paste0(col, ":page-chrome-leak"), "")
  }

  cells <- list(
    check_case(df$case_number),
    check_iso(df$date_of_incident_iso, "date_of_incident_iso"),
    check_iso(df$date_of_injury_iso, "date_of_injury_iso"),
    check_iso(df$date_siu_notified_iso, "date_siu_notified_iso"),
    check_iso(
      df$date_of_director_decision_iso,
      "date_of_director_decision_iso"
    ),
    check_int(
      df$number_of_affected_persons,
      "number_of_affected_persons"
    ),
    check_int(
      df$number_of_civilian_witnesses,
      "number_of_civilian_witnesses"
    ),
    check_int(
      df$number_of_subject_officials,
      "number_of_subject_officials"
    ),
    check_int(
      df$number_of_witness_officials,
      "number_of_witness_officials"
    ),
    check_int(df$age_affected, "age_affected"),
    check_officer_count(df$number_of_officers_involved),
    check_yn(df$charges_recommended, "charges_recommended"),
    check_yn(
      df$directors_decision_reasonable,
      "directors_decision_reasonable"
    ),
    check_gender(df$sex_gender_affected),
    check_nonempty(df$police_service, "police_service"),
    check_nonempty(df$narrative_summary, "narrative_summary"),
    check_short(df$narrative_summary, "narrative_summary", 100L),
    check_chrome(df$narrative_summary, "narrative_summary"),
    check_chrome(df$supplemental_materials, "supplemental_materials"),
    check_chrome(
      df$mental_health_or_race_indications,
      "mental_health_or_race_indications"
    )
  )

  collapsed <- do.call(paste, c(cells, sep = ";"))
  collapsed <- gsub(";+", ";", collapsed)
  collapsed <- gsub("^;|;$", "", collapsed)
  issues_count <- vapply(
    strsplit(collapsed, ";"),
    function(x) sum(nzchar(x)), integer(1)
  )
  out <- data.frame(
    case_number = df$case_number,
    drid = df$drid,
    issues_count = issues_count,
    issues = collapsed,
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$issues_count, out$drid), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Translate SIU report text into any target language via local LLM
#'
#' For SIU cases whose parser-emitted text isn't in the reader's
#' preferred language, translate the long-form text fields into
#' \code{target_lang} via a local Ollama model (default $0 cost,
#' no API key) and save each translation as a canonical override.
#' Subsequent \code{morie_fetch_siu()} runs then return text in
#' \code{target_lang} for those cases automatically.
#'
#' Use cases:
#' \itemize{
#'   \item French-only SIU reports (a few per year of SIU output)
#'         that have no English-paired drid -- translate to "en"
#'         so downstream analyses can join them with the rest.
#'   \item English SIU reports that a Hindi / Spanish / Mandarin /
#'         Punjabi / Arabic / etc. reader needs -- translate to
#'         their first language for accessibility.
#'   \item Any cross-language pivot for community-oriented
#'         publication, where the reader's first language isn't
#'         what the SIU originally published in.
#' }
#'
#' Idempotent (skips cases that already have an override on file
#' for this \code{target_lang}). Self-improving (every translation
#' accumulates in \code{<cache_dir>/canonical_overrides.csv}, so
#' the SIU table becomes more accessible every time you run this).
#' Maintainers can promote the resulting overrides into the
#' shipped \code{inst/extdata/siu_canonical_overrides.csv.gz}.
#'
#' For best speed/quality on multilingual translation use
#' \code{OLLAMA_MODEL=translategemma:latest} -- a Gemma model
#' fine-tuned for translation. Falls back to whatever model
#' \code{OLLAMA_MODEL} points at.
#'
#' @param target_lang Target ISO 639-1 language code (or full
#'   language name). Defaults to \code{Sys.getenv("MORIE_USER_LANG")}
#'   or, failing that, the first two characters of
#'   \code{Sys.getenv("LANG")} -- so it picks up the user's
#'   system locale automatically.
#' @param source_lang Source language code, or \code{NULL} (default)
#'   to use each row's parsed \code{_language} field.
#' @param case_numbers Character vector of SIU case numbers to
#'   translate. Defaults to every row whose \code{_language}
#'   differs from \code{target_lang} and has no override yet.
#' @param model LLM model chain (see \code{\link{morie_siu_llm_extract}}).
#'   Default \code{"ollama"} for $0 cost via local Gemma / etc.
#' @param fields Which text fields to translate. Defaults to the
#'   long-form fields that benefit most from translation:
#'   \code{narrative_summary}, \code{news_release_summary},
#'   \code{news_release_title}, \code{relevant_legislation}.
#' @param cache_dir Directory holding the harvester's SIU.csv and
#'   cached HTML.
#' @param progress Print per-case progress.
#' @return Invisibly, a data frame of newly-recorded
#'   (case_number, field, verified_value) translations.
#' @examples
#' \dontrun{
#' Sys.setenv(
#'   OLLAMA_HOST = "http://localhost:11434",
#'   OLLAMA_MODEL = "translategemma:latest"
#' )
#' csv <- morie_fetch_siu(cache_html = TRUE)
#' # Translate every non-English row to English:
#' morie_siu_translate(target_lang = "en")
#' # Or translate everything to Hindi for a Hindi-first reader:
#' morie_siu_translate(target_lang = "hi")
#' # Re-fetch picks up the new overrides automatically:
#' csv <- morie_fetch_siu(overwrite = TRUE)
#' }
#' @export
morie_siu_translate <- function(
  target_lang = NULL, source_lang = NULL,
  case_numbers = NULL, model = "ollama",
  fields = c(
    "narrative_summary", "news_release_summary",
    "news_release_title", "relevant_legislation"
  ),
  cache_dir = file.path(tempdir(), "morie", "siu"), progress = TRUE
) {
  if (is.null(target_lang) || !nzchar(target_lang)) {
    target_lang <- Sys.getenv("MORIE_USER_LANG", unset = "")
    if (!nzchar(target_lang)) {
      sys_lang <- Sys.getenv("LANG", unset = "")
      target_lang <- if (nzchar(sys_lang)) {
        substr(sys_lang, 1L, 2L)
      } else {
        "en"
      }
    }
  }
  # Two-letter ISO is enough for the model to know what to do;
  # also accept full names like "English", "Hindi".
  target_lang <- tolower(target_lang)
  .siu_translate_impl(
    target_lang = target_lang, source_lang = source_lang,
    case_numbers = case_numbers, model = model,
    fields = fields, cache_dir = cache_dir, progress = progress
  )
}

#' @rdname morie_siu_translate
#' @description \code{morie_siu_translate_fr_to_en} is a thin
#'   back-compat wrapper that calls \code{morie_siu_translate}
#'   with \code{target_lang = "en", source_lang = "fr"}.
#' @export
morie_siu_translate_fr_to_en <- function(
  case_numbers = NULL, model = "ollama",
  fields = c(
    "narrative_summary", "news_release_summary",
    "news_release_title", "relevant_legislation"
  ),
  cache_dir = file.path(tempdir(), "morie", "siu"), progress = TRUE
) {
  .siu_translate_impl(
    target_lang = "en", source_lang = "fr",
    case_numbers = case_numbers, model = model,
    fields = fields, cache_dir = cache_dir, progress = progress
  )
}

.siu_translate_impl <- function(
  target_lang, source_lang, case_numbers, model,
  fields, cache_dir, progress
) {
  cache_dir <- path.expand(cache_dir)
  csv_path <- file.path(cache_dir, "SIU.csv")
  if (!file.exists(csv_path)) {
    stop("No SIU.csv at '", csv_path, "'; run morie_fetch_siu() first.",
      call. = FALSE
    )
  }
  df <- utils::read.csv(csv_path,
    colClasses = "character",
    check.names = FALSE
  )

  # Default to every row whose source language differs from the
  # target AND that has no override yet for the narrative_summary
  # field. (One override is enough to mark "already translated";
  # we don't re-translate the same row.)
  existing <- .siu_load_canonical_overrides(user_cache_dir = cache_dir)
  done <- if (!is.null(existing)) {
    existing$case_number[existing$field == "narrative_summary"]
  } else {
    character(0)
  }

  if (is.null(case_numbers)) {
    lang_col <- df[["_language"]]
    if (is.null(source_lang)) {
      # Translate every row whose detected language is NOT the
      # target language (covers fr, unknown, etc.).
      candidates <- df$case_number[lang_col != target_lang &
        nzchar(df$case_number)]
    } else {
      candidates <- df$case_number[lang_col == source_lang &
        nzchar(df$case_number)]
    }
    case_numbers <- setdiff(candidates, done)
  } else {
    case_numbers <- setdiff(case_numbers, done)
  }
  if (!length(case_numbers)) {
    if (progress) message("Nothing to translate; all caught up.")
    return(invisible(data.frame()))
  }
  if (progress) {
    message(
      "Translating ", length(case_numbers),
      " case(s) to ", sQuote(target_lang), " via ",
      paste(model, collapse = "+"), " ..."
    )
  }
  # Friendly full-name lookup for common target codes; falls back
  # to whatever the caller passed.
  lang_full <- c(
    en = "English", fr = "French", es = "Spanish",
    hi = "Hindi", pa = "Punjabi", ur = "Urdu",
    zh = "Mandarin Chinese", ar = "Arabic",
    de = "German", it = "Italian", pt = "Portuguese",
    ja = "Japanese", ko = "Korean", ru = "Russian",
    tr = "Turkish", vi = "Vietnamese", tl = "Tagalog",
    so = "Somali", am = "Amharic", ta = "Tamil",
    bn = "Bengali", gu = "Gujarati"
  )
  target_name <- if (target_lang %in% names(lang_full)) {
    lang_full[[target_lang]]
  } else {
    target_lang
  }

  acc <- list()
  for (i in seq_along(case_numbers)) {
    cn <- case_numbers[i]
    row <- df[df$case_number == cn, , drop = FALSE]
    if (!nrow(row)) next
    # Build a single prompt translating each non-empty field at once.
    items <- lapply(fields, function(f) {
      v <- as.character(row[[f]][1L])
      if (!nzchar(v)) {
        return(NULL)
      }
      list(field = f, value = substr(v, 1L, 8000L))
    })
    items <- Filter(Negate(is.null), items)
    if (!length(items)) next

    body <- paste(vapply(items, function(it) {
      sprintf("[%s]\n%s\n[/%s]", it$field, it$value, it$field)
    }, character(1)), collapse = "\n\n")
    prompt <- paste0(
      "Translate the following Ontario SIU director's-report text\n",
      "fields into clear professional ", target_name, ".\n",
      "Preserve all proper names, dates, case identifiers, and\n",
      "legal references verbatim. Return ONLY a JSON object whose\n",
      "keys are the field names and whose values are the\n",
      target_name, " translations.\n\n", body
    )

    text <- tryCatch(
      .siu_llm_call(model, prompt, timeout_s = 120L),
      error = function(e) {
        if (progress) message("  skip ", cn, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(text)) next
    text <- gsub("^```(?:json)?\\s*|\\s*```$", "", text, perl = TRUE)
    parsed <- tryCatch(jsonlite::fromJSON(text, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.null(parsed)) next

    for (it in items) {
      en_val <- parsed[[it$field]]
      if (is.null(en_val) || !nzchar(as.character(en_val))) next
      morie_siu_record_correction(
        case_number = cn, field = it$field,
        verified_value = as.character(en_val)[1L],
        note = sprintf(
          "auto-translated to %s via %s",
          target_lang, paste(model, collapse = "+")
        ),
        cache_dir = cache_dir
      )
      acc[[length(acc) + 1L]] <- data.frame(
        case_number = cn, field = it$field,
        verified_value = as.character(en_val)[1L],
        stringsAsFactors = FALSE
      )
    }
    if (progress) {
      message("  ", i, "/", length(case_numbers), " ", cn, " done")
    }
  }

  if (!length(acc)) {
    return(invisible(data.frame()))
  }
  out <- do.call(rbind, acc)
  rownames(out) <- NULL
  if (progress) {
    message(
      "Translated ", nrow(out),
      " (case, field) cells; written to ",
      file.path(cache_dir, "canonical_overrides.csv")
    )
  }
  invisible(out)
}

#' Per-column accuracy audit: estimate every SIU column's correctness
#'
#' Runs \code{morie_siu_anomaly_check()} on a vector of case_numbers
#' and aggregates per-field across them. Output is a data frame with
#' one row per SIU column, ordered by how often the LLM auditor
#' agreed with the C++ parser. The worst-ranked rows are the
#' parser fields that most deserve regex / extraction-logic fixes.
#'
#' Examples of LLM-flagged disagreements are attached as the
#' \code{"examples"} attribute of the returned data frame (one
#' nested data frame per field), with at most
#' \code{max_examples_per_field} cases each. Each example carries
#' the case_number, the parser_value, and the LLM's one-sentence
#' reason -- enough for a maintainer to pop the cached HTML for
#' that case, see who's right, and decide whether to refine the
#' regex pattern for that field.
#'
#' Designed for cheap local audit: with \code{model = "ollama"}
#' pointed at a local Gemma / Qwen / DeepSeek instance, auditing
#' 50-100 cases costs zero API spend and finishes in a few
#' minutes. With \code{model = c("gemini", "ollama")} the chain
#' uses paid Gemini first and silently falls back to the local
#' model on quota / network errors.
#'
#' @inheritParams morie_siu_anomaly_check
#' @param case_numbers Character vector of SIU case numbers to audit.
#' @param max_examples_per_field Maximum disagreement examples
#'   retained per field (default 5).
#' @param progress Logical; print a per-case progress line.
#' @return A data frame with columns \code{field}, \code{n_audited},
#'   \code{n_agree}, \code{n_disagree}, \code{n_unclear},
#'   \code{agree_rate}. Sorted ascending by \code{agree_rate} so the
#'   most-broken fields land at the top. The \code{"examples"}
#'   attribute holds nested data frames of flagged cases per field.
#' @examples
#' \dontrun{
#' Sys.setenv(
#'   OLLAMA_HOST = "http://localhost:11434",
#'   OLLAMA_MODEL = "gemma3:4b"
#' )
#' csv <- morie_fetch_siu(cache_html = TRUE)
#' df <- utils::read.csv(csv, colClasses = "character")
#' sample <- sample(df$case_number[nzchar(df$case_number)], 50L)
#' audit <- morie_siu_audit_columns(sample, model = "ollama")
#' # Worst 8 fields, ripe for parser fixes:
#' head(audit, 8)
#' # See concrete disagreements for the worst field:
#' attr(audit, "examples")[[audit$field[1L]]]
#' }
#' @export
morie_siu_audit_columns <- function(case_numbers, model = c("ollama", "gemini"),
                                    cache_dir = file.path(tempdir(), "morie", "siu"),
                                    max_html_chars = 80000L,
                                    max_examples_per_field = 5L,
                                    progress = TRUE) {
  case_numbers <- as.character(case_numbers)
  if (!length(case_numbers)) {
    stop("`case_numbers` must be non-empty.", call. = FALSE)
  }
  verdicts <- vector("list", length(case_numbers))
  for (i in seq_along(case_numbers)) {
    if (progress) {
      message(
        "auditing ", case_numbers[i], " [", i, "/",
        length(case_numbers), "] ..."
      )
    }
    v <- tryCatch(
      morie_siu_anomaly_check(case_numbers[i],
        model = model,
        cache_dir = cache_dir,
        max_html_chars = max_html_chars
      ),
      error = function(e) {
        if (progress) message("  skipped: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(v) && nrow(v)) {
      v$case_number <- case_numbers[i]
      verdicts[[i]] <- v
    }
  }
  verdicts <- Filter(Negate(is.null), verdicts)
  if (!length(verdicts)) {
    stop("All audit attempts failed. Check your model / API key.",
      call. = FALSE
    )
  }
  all <- do.call(rbind, verdicts)

  fields <- sort(unique(all$field))
  per_field <- lapply(fields, function(fld) {
    sub <- all[all$field == fld, , drop = FALSE]
    n <- nrow(sub)
    agree <- sum(sub$verdict == "agree")
    disagree <- sum(sub$verdict == "disagree")
    unclear <- sum(sub$verdict == "unclear")
    # Sample disagreement examples (and append any "unclear" until we
    # hit the cap, so even mostly-unclear fields show what tripped them).
    bad <- sub[sub$verdict %in% c("disagree", "unclear"), , drop = FALSE]
    bad <- bad[order(match(bad$verdict, c("disagree", "unclear"))), ,
      drop = FALSE
    ]
    examples <- if (nrow(bad)) {
      bad[seq_len(min(max_examples_per_field, nrow(bad))),
        c("case_number", "parser_value", "verdict", "reason"),
        drop = FALSE
      ]
    } else {
      NULL
    }
    list(
      field = fld, n = n, agree = agree, disagree = disagree,
      unclear = unclear, examples = examples
    )
  })

  out <- data.frame(
    field = vapply(per_field, function(x) x$field, character(1)),
    n_audited = vapply(per_field, function(x) x$n, integer(1)),
    n_agree = vapply(per_field, function(x) x$agree, integer(1)),
    n_disagree = vapply(per_field, function(x) x$disagree, integer(1)),
    n_unclear = vapply(per_field, function(x) x$unclear, integer(1)),
    stringsAsFactors = FALSE
  )
  out$agree_rate <- ifelse(out$n_audited > 0L,
    out$n_agree / out$n_audited, NA_real_
  )
  out <- out[order(out$agree_rate, out$field), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "examples") <- setNames(
    lapply(per_field, function(x) x$examples),
    vapply(per_field, function(x) x$field, character(1))
  )
  out
}
