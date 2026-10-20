# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Data capsules: a CSV pinned by its provenance record, checked, signed,
# bundled and compared with a fresh copy. Thin wrappers over
# rmoriebricklayer (vrfcps, cpssign, cpsbndl, cpsdrft, mkmnf, recrd,
# wrmnfjs, mnfrcmp); the same layer is in morie (Python) and in the
# rmbl-action GitHub Action.

# one non-empty string, named as the caller knows it
.morie_capsule_string <- function(x, what) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("`%s` must be a single non-empty string", what), call. = FALSE)
  }
  x
}

# a plain path relative to the capsule directory: no absolute path, no `..`
.morie_capsule_rel <- function(x, what) {
  x <- .morie_capsule_string(x, what)
  if (grepl("^(/|~|[A-Za-z]:)|(^|[/\\\\])\\.\\.([/\\\\]|$)", x)) {
    stop(sprintf("`%s` must be a path relative to `dir`, inside it: %s", what, x), call. = FALSE)
  }
  x
}

.morie_capsule_dir <- function(dir) {
  dir <- .morie_capsule_string(dir, "dir")
  if (!dir.exists(dir)) stop("`dir` is not an existing directory: ", dir, call. = FALSE)
  dir
}

.morie_capsule_flag <- function(x, what) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE", what), call. = FALSE)
  }
  x
}

.morie_capsule_csv <- function(path, what) {
  tryCatch(utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
           error = function(e) {
             stop(sprintf("`%s` could not be read as a CSV (%s): %s", what, conditionMessage(e), path),
                  call. = FALSE)
           })
}

#' Write the provenance record of a data capsule
#'
#' Pins a local CSV: writes \code{data_provenance.json} next to it with the
#' file's name, SHA-256, size in bytes and number of data rows, where it came
#' from and when. That record is what [morie_capsule_verify()] (and
#' \code{rmoriebricklayer::vrfcps()}, and the \code{rmbl-action} GitHub Action)
#' checks the data against later.
#'
#' @details
#' The record written is
#' \preformatted{{"name": ..., "source": {"url", "retrieved"}, "resource": {"filename",
#'  "sha256", "size_bytes", "row_count_data_rows"}, "table": ..., "script": {"filename",
#'  "sha256"}}}
#' in that order, the order of the rmbl-action examples and of
#' \code{morie.capsule.write()} in Python, so the three writers agree byte for
#' byte. \code{source$url} is left out when \code{url} is \code{NULL},
#' \code{table} and \code{script} when not given. The row count
#' is what \code{utils::read.csv()} reads, header excluded, which is how
#' the verification counts them. An existing provenance file is overwritten.
#'
#' Errors: \code{dir} not an existing directory; \code{data_file} or
#' \code{provenance_file} not a relative path inside \code{dir}; a
#' \code{data_file} that is not a \code{.csv} file in \code{dir} or cannot be
#' read; \code{url}, \code{name} or \code{retrieved} not one string;
#' \code{table} not a list; \code{script_file} not a file inside \code{dir}.
#'
#' @param dir The capsule directory, a single string.
#' @param data_file The CSV, relative to \code{dir}.
#' @param url Where the data were downloaded from (\code{source$url}), or
#'   \code{NULL}.
#' @param name The capsule's name. \code{NULL} (the default): the data file's
#'   name without \code{.csv}.
#' @param retrieved The retrieval date, \code{YYYY-MM-DD}. Default: today.
#' @param provenance_file The record's file name, relative to \code{dir}.
#' @param table \code{NULL}, or a list stored as the record's \code{table}
#'   (the column schema the verification's \code{schema_valid} row reads).
#' @param script_file \code{NULL}, or an analysis script relative to
#'   \code{dir}, pinned as \code{script$filename} and \code{script$sha256}.
#' @return The provenance (a list), invisibly, with the file written as
#'   attribute \code{"path"}.
#' @family capsule
#' @examples
#' dir <- file.path(tempdir(), "capsule-write")
#' dir.create(dir, showWarnings = FALSE)
#' write.csv(data.frame(id = 1:3, value = c(2.5, 3.1, 4.0)),
#'           file.path(dir, "data.csv"), row.names = FALSE)
#' prov <- morie_capsule_write(dir, "data.csv",
#'                             url = "https://example.org/data.csv",
#'                             retrieved = "2026-10-20")
#' str(prov$resource)
#' morie_capsule_verify(dir)$ok
#'
#' # a path out of the capsule is refused
#' try(morie_capsule_write(dir, "../data.csv"))
#' unlink(dir, recursive = TRUE)
#' @export
morie_capsule_write <- function(dir, data_file, url = NULL, name = NULL,
                                retrieved = format(Sys.Date()),
                                provenance_file = "data_provenance.json", table = NULL,
                                script_file = NULL) {
  dir <- .morie_capsule_dir(dir)
  data_file <- .morie_capsule_rel(data_file, "data_file")
  provenance_file <- .morie_capsule_rel(provenance_file, "provenance_file")
  if (!is.null(url)) url <- .morie_capsule_string(url, "url")
  if (is.null(name)) name <- sub("\\.csv$", "", basename(data_file), ignore.case = TRUE)
  name <- .morie_capsule_string(name, "name")
  if (!is.null(table) && (!is.list(table) || is.data.frame(table))) {
    stop("`table` must be a list, or NULL", call. = FALSE)
  }
  if (!is.null(script_file)) {
    script_file <- .morie_capsule_rel(script_file, "script_file")
    spath <- file.path(dir, script_file)
    if (!file.exists(spath) || dir.exists(spath)) {
      stop("`script_file` is not a file in `dir`: ", script_file, call. = FALSE)
    }
  }
  retrieved <- .morie_capsule_string(retrieved, "retrieved")
  path <- file.path(dir, data_file)
  if (!grepl("\\.csv$", data_file, ignore.case = TRUE)) {
    stop("`data_file` must be a .csv file: ", data_file, call. = FALSE)
  }
  if (!file.exists(path) || dir.exists(path)) {
    stop("`data_file` is not a file in `dir`: ", data_file, call. = FALSE)
  }
  rows <- nrow(.morie_capsule_csv(path, "data_file"))
  prov <- list(
    name = name,
    source = c(if (!is.null(url)) list(url = url), list(retrieved = retrieved)),
    resource = list(filename = data_file, sha256 = rmoriebricklayer::sha256file(path),
                    size_bytes = file.size(path), row_count_data_rows = rows)
  )
  prov$table <- table
  if (!is.null(script_file)) {
    prov$script <- list(filename = script_file, sha256 = rmoriebricklayer::sha256file(spath))
  }
  out <- file.path(dir, provenance_file)
  writeLines(rmoriebricklayer::tojs(prov, auto_unbox = TRUE, pretty = TRUE, digits = NA), out,
             useBytes = TRUE)
  invisible(structure(prov, path = out))
}

#' Verify a data capsule
#'
#' Re-checks a capsule directory against its provenance record, one row per
#' check: the record is readable, the data file is present with its pinned
#' SHA-256, size and row count, the recorded schema still validates, a
#' results manifest still reproduces its recorded verdicts, a pinned script
#' still has its digest, and the data are not synthetic. This is
#' \code{rmoriebricklayer::vrfcps()}; see its help for every row.
#'
#' Entirely offline; nothing is written. A failed check is a row, not an
#' error: a provenance record that does not parse as JSON is a failed
#' \code{provenance_readable} row, as in morie (\code{vrfcps()} itself stops
#' on one). The errors are \code{dir} not an existing directory and an
#' argument that is not one string.
#'
#' @param dir The capsule directory, a single string.
#' @param provenance_file The provenance record, relative to \code{dir}.
#' @param data_file The data file, relative to \code{dir}. \code{NULL} uses
#'   the record's \code{resource$filename}.
#' @param manifest_file A results manifest (JSON) relative to \code{dir}.
#'   \code{NULL} uses \code{manifest.json} when the capsule has one.
#' @param script_file An analysis script relative to \code{dir}, checked
#'   against the pinned script digest. \code{NULL} uses the one the record
#'   or the manifest names.
#' @return A \code{bricklayer_capsule_check}: a list with \code{ok}
#'   (logical), \code{checks} (data frame of \code{check}, \code{ok},
#'   \code{detail}), \code{required} and \code{missing} (character).
#' @family capsule
#' @examples
#' dir <- file.path(tempdir(), "capsule-verify")
#' dir.create(dir, showWarnings = FALSE)
#' write.csv(data.frame(id = 1:3), file.path(dir, "data.csv"), row.names = FALSE)
#' morie_capsule_write(dir, "data.csv", retrieved = "2026-10-20")
#' morie_capsule_verify(dir)
#'
#' # an edited data file fails its digest, size and row count
#' write.csv(data.frame(id = 1:4), file.path(dir, "data.csv"), row.names = FALSE)
#' chk <- morie_capsule_verify(dir)
#' chk$checks[!chk$checks$ok, c("check", "detail")]
#' unlink(dir, recursive = TRUE)
#' @export
morie_capsule_verify <- function(dir, provenance_file = "data_provenance.json", data_file = NULL,
                                 manifest_file = NULL, script_file = NULL) {
  dir <- .morie_capsule_dir(dir)
  provenance_file <- .morie_capsule_string(provenance_file, "provenance_file")
  if (!is.null(data_file)) data_file <- .morie_capsule_string(data_file, "data_file")
  if (!is.null(manifest_file)) manifest_file <- .morie_capsule_string(manifest_file, "manifest_file")
  if (!is.null(script_file)) script_file <- .morie_capsule_string(script_file, "script_file")
  ppath <- file.path(dir, provenance_file)
  # ponytail: rmoriebricklayer 1.0.0 vrfcps() stops on a record that does not parse; checked as a
  # missing record instead (a failed provenance_readable row, as in morie) until vrfcps() does it
  unreadable <- file.exists(ppath) &&
    inherits(tryCatch(rmoriebricklayer::loadprov(ppath), error = function(e) e), "error")
  provfile <- if (unreadable) basename(tempfile("absent-", tmpdir = dir)) else provenance_file
  out <- rmoriebricklayer::vrfcps(dir, provfile = provfile, dfile = data_file,
                                  mnffile = manifest_file, scrfile = script_file)
  if (unreadable) out$checks$detail[out$checks$check == "provenance_readable"] <- ppath
  out
}

#' Sign a capsule digest or manifest
#'
#' Signs \code{message} so a reader can tell it came from the key's holder
#' and was not changed since. Pass a manifest (a list, from
#' [morie_capsule_manifest()]) to sign its canonical digest
#' (\code{rmoriebricklayer::mnfdgst()}), or a string such as the SHA-256 of
#' a provenance file. This is \code{rmoriebricklayer::cpssign()}; verify
#' with \code{rmoriebricklayer::cpsvrf()}.
#'
#' @details
#' The schemes are the ones rmoriebricklayer offers: a standardised key
#' from \code{rmoriebricklayer::fipskg()} (ML-DSA-44, -65 or -87, FIPS 204;
#' any SLH-DSA set, FIPS 205), whose scheme is taken from the key; an XMSS
#' key from \code{rmoriebricklayer::pqckg()} (\code{scheme = "xmss"},
#' stateful: advance the key with \code{rmoriebricklayer::xmssadv()} after
#' every signature); or a shared secret string (\code{scheme = "hmac"},
#' HMAC-SHA-256, symmetric). For new work a \code{fipskg()} key is the
#' simplest: it signs any number of messages with no state to keep.
#'
#' Errors: \code{message} not one string, a raw vector or a list;
#' \code{scheme} not \code{NULL} or one string; \code{deterministic} not
#' \code{TRUE} or \code{FALSE}; and every refusal of \code{cpssign()} (a key
#' of the wrong kind for the scheme, a context over 255 bytes, an unknown
#' pre-hash, an exhausted XMSS key).
#'
#' @param message A string, a raw vector, or a manifest (a list) whose
#'   digest is signed.
#' @param key A key from \code{rmoriebricklayer::fipskg()} or
#'   \code{rmoriebricklayer::pqckg()}, or a shared secret for \code{"hmac"}.
#' @param scheme \code{NULL} (taken from \code{key}), \code{"xmss"} or
#'   \code{"hmac"}.
#' @param context Optional context string (at most 255 bytes) bound into a
#'   standardised signature, so one key can sign for two purposes.
#' @param deterministic \code{TRUE} for the deterministic variant of the
#'   standardised schemes (the signature depends only on key, message and
#'   context).
#' @param prehash \code{"none"} (the default) or a hash for HashML-DSA /
#'   HashSLH-DSA, as in \code{cpssign()}.
#' @return A \code{bricklayer_signature} list (\code{scheme},
#'   \code{signature}, and per scheme \code{context}, \code{prehash} or the
#'   XMSS fields).
#' @family capsule
#' @examples
#' man <- morie_capsule_manifest(list(project = "demo"), env = FALSE)
#' key <- rmoriebricklayer::fipskg("ML-DSA-44")
#' sig <- morie_capsule_sign(man, key, context = "release")
#' sig$scheme
#' rmoriebricklayer::cpsvrf(rmoriebricklayer::mnfdgst(man), sig, key,
#'                          ctx = "release")
#'
#' # a shared secret
#' tag <- morie_capsule_sign("sha256:abc", "shared-secret", scheme = "hmac")
#' rmoriebricklayer::cpsvrf("sha256:abc", tag, "shared-secret")
#'
#' # a number is not a message
#' try(morie_capsule_sign(1, key))
#' @export
morie_capsule_sign <- function(message, key, scheme = NULL, context = NULL,
                               deterministic = FALSE, prehash = "none") {
  if (is.list(message) && !is.data.frame(message)) {
    message <- rmoriebricklayer::mnfdgst(message)
  } else if (!is.raw(message)) {
    message <- tryCatch(.morie_capsule_string(message, "message"), error = function(e) {
      stop("`message` must be one string, a raw vector or a manifest (a list)", call. = FALSE)
    })
  }
  if (!is.null(scheme)) scheme <- .morie_capsule_string(scheme, "scheme")
  deterministic <- .morie_capsule_flag(deterministic, "deterministic")
  rmoriebricklayer::cpssign(message, key, scheme = scheme, ctx = context,
                            determ = deterministic, prehash = prehash)
}

#' Bundle a capsule into one signed file
#'
#' Hashes every file of the capsule (or the ones named), records the
#' manifest's digest, and signs both together into
#' \code{capsule_bundle.json}. A list of digests that is not itself signed
#' can be rewritten to match whatever the files now say; here the digests
#' are inside the signature. This is \code{rmoriebricklayer::cpsbndl()};
#' check a bundle with \code{rmoriebricklayer::cpsbndlvrf()}, which also
#' reports any file on disk the bundle does not list.
#'
#' Errors: \code{dir} not an existing directory; \code{manifest} not a list;
#' \code{files} not \code{NULL} or file names in \code{dir}; \code{path} not
#' one string; a key \code{cpsatst()} cannot sign with.
#'
#' @param dir The capsule directory, a single string.
#' @param manifest A manifest, from [morie_capsule_manifest()].
#' @param key A key from \code{rmoriebricklayer::fipskg()} or
#'   \code{rmoriebricklayer::pqckg()}.
#' @param files File names relative to \code{dir}. \code{NULL} (the
#'   default) bundles every regular file under \code{dir}, dot-files
#'   included, except the bundle itself.
#' @param context,prehash As in [morie_capsule_sign()].
#' @param note An optional sentence stored with the signature.
#' @param path Where the bundle is written. \code{NULL} is
#'   \code{capsule_bundle.json} in \code{dir}.
#' @return The bundle (a \code{bricklayer_bundle} list of \code{files},
#'   \code{manifest_digest}, \code{bundled_utc} and \code{attestation}),
#'   invisibly, with the file written as attribute \code{"path"}.
#' @family capsule
#' @examples
#' dir <- file.path(tempdir(), "capsule-bundle")
#' dir.create(dir, showWarnings = FALSE)
#' write.csv(data.frame(id = 1:3), file.path(dir, "data.csv"), row.names = FALSE)
#' morie_capsule_write(dir, "data.csv", retrieved = "2026-10-20")
#' man <- morie_capsule_manifest(list(project = "demo"), env = FALSE)
#' key <- rmoriebricklayer::fipskg("ML-DSA-44")
#' b <- morie_capsule_bundle(dir, man, key, note = "as published")
#' rmoriebricklayer::cpsbndlvrf(attr(b, "path"), dir, man = man)$ok
#' unlink(dir, recursive = TRUE)
#' @export
morie_capsule_bundle <- function(dir, manifest, key, files = NULL, context = NULL,
                                 prehash = "none", note = NULL, path = NULL) {
  dir <- .morie_capsule_dir(dir)
  if (!is.list(manifest) || is.data.frame(manifest)) {
    stop("`manifest` must be a manifest (a list), from morie_capsule_manifest()", call. = FALSE)
  }
  if (!is.null(files) && (!is.character(files) || !length(files) || anyNA(files))) {
    stop("`files` must be file names relative to `dir`, or NULL for every file", call. = FALSE)
  }
  if (!is.null(path)) path <- .morie_capsule_string(path, "path")
  rmoriebricklayer::cpsbndl(dir, manifest, key, files = files, ctx = context,
                            prehash = prehash, note = note, path = path)
}

#' Compare a fresh copy of a capsule's data with the pinned one
#'
#' Reads the CSV the capsule pins (\code{resource$filename} in its
#' provenance record) and compares it, column by column, with a freshly
#' downloaded copy: a digest only says the bytes changed, this says whether
#' the distributions did. This is \code{rmoriebricklayer::cpsdrft()}
#' (Kolmogorov-Smirnov and PSI for numeric and date columns, a two-sample
#' homogeneity test for categorical ones; added and removed columns listed).
#'
#' Errors: \code{dir} not an existing directory; no provenance record, or
#' one whose \code{resource$filename} is missing or not a relative path
#' inside \code{dir}; a pinned or current file that cannot be read as a CSV;
#' \code{current} neither a data frame nor one path; and the refusals of
#' \code{cpsdrft()} (\code{alpha} outside (0, 1), \code{psi_min_n}
#' negative, \code{id_levels} below 2).
#'
#' @param dir The capsule directory, a single string.
#' @param current The fresh data: a data frame, or the path to a CSV.
#' @param provenance_file The provenance record, relative to \code{dir}.
#' @param alpha Significance level for a column's \code{drifted} flag.
#' @param psi_threshold PSI above which a column is flagged on its own once
#'   both samples have at least \code{psi_min_n} values.
#' @param psi_min_n Minimum size of both samples before the PSI may flag.
#' @param bins Bins for the PSI.
#' @param id_levels A text column with more distinct values than this is an
#'   identifier and is not tested.
#' @param seed Seed for the Monte Carlo branch of the homogeneity test; the
#'   caller's random stream is left untouched.
#' @return A \code{bricklayer_drift} list: \code{columns} (one row per
#'   shared column: \code{column}, \code{type}, \code{statistic},
#'   \code{p_value}, \code{psi}, \code{jsdiv}, \code{unseen_share},
#'   \code{drifted}, \code{note}), \code{added}, \code{removed},
#'   \code{nref}, \code{ncur}, \code{alpha} and \code{anydrft}.
#' @family capsule
#' @examples
#' dir <- file.path(tempdir(), "capsule-drift")
#' dir.create(dir, showWarnings = FALSE)
#' set.seed(1)
#' write.csv(data.frame(x = stats::rnorm(200)), file.path(dir, "data.csv"),
#'           row.names = FALSE)
#' morie_capsule_write(dir, "data.csv", retrieved = "2026-10-20")
#' morie_capsule_drift(dir, data.frame(x = stats::rnorm(200)))$anydrft
#' morie_capsule_drift(dir, data.frame(x = stats::rnorm(200, mean = 2)))$anydrft
#' unlink(dir, recursive = TRUE)
#' @export
morie_capsule_drift <- function(dir, current, provenance_file = "data_provenance.json",
                                alpha = 0.01, psi_threshold = 0.25, psi_min_n = 1000L,
                                bins = 10L, id_levels = 100L, seed = 1L) {
  dir <- .morie_capsule_dir(dir)
  provenance_file <- .morie_capsule_rel(provenance_file, "provenance_file")
  prov <- rmoriebricklayer::loadprov(file.path(dir, provenance_file))
  if (is.null(prov)) stop("no provenance record in `dir`: ", provenance_file, call. = FALSE)
  pinned <- prov$resource$filename
  if (!is.character(pinned) || length(pinned) != 1L) {
    stop("the provenance records no resource$filename: nothing is pinned", call. = FALSE)
  }
  pinned <- .morie_capsule_rel(pinned, "resource$filename")
  ref <- .morie_capsule_csv(file.path(dir, pinned), "resource$filename")
  if (!is.data.frame(current)) {
    current <- .morie_capsule_string(current, "current")
    current <- .morie_capsule_csv(current, "current")
  }
  rmoriebricklayer::cpsdrft(ref, current, alpha = alpha, psithr = psi_threshold, psiminn = psi_min_n,
                            bins = bins, idelvls = id_levels, seed = seed)
}

#' Build, and optionally write, a capsule's results manifest
#'
#' Creates the record of one analysis run (\code{rmoriebricklayer::mkmnf()}),
#' adds a cross-check per row of \code{checks}
#' (\code{rmoriebricklayer::recrd()}: \code{PASS} within \code{tol},
#' \code{DIFFER} outside it, \code{INFO} on synthetic data), and writes it as
#' JSON when \code{path} is given (\code{rmoriebricklayer::wrmnfjs()}).
#' [morie_capsule_verify()] re-checks the recorded verdicts,
#' [morie_capsule_compare()] recomputes the numbers, and
#' [morie_capsule_sign()] / [morie_capsule_bundle()] sign it.
#'
#' @details
#' With \code{env = TRUE} the R version, platform and package versions are
#' recorded. The package versions are kept as a named list rather than the
#' named vector \code{mkmnf()} returns, so the manifest's digest is the same
#' after it is written and read back.
#'
#' Errors: \code{meta} not a list; \code{checks} not a data frame with
#' columns \code{name}, \code{observed} and \code{expected}, or with a
#' repeated name; \code{path} not one string; \code{env} or
#' \code{canonical} not \code{TRUE} or \code{FALSE}.
#'
#' @param meta A named list of run metadata (project, author, ...).
#' @param checks \code{NULL}, or a data frame with one cross-check per row:
#'   \code{name}, \code{observed}, \code{expected} and optionally \code{tol}
#'   (absolute; default 1e-4) and \code{group} (default \code{"general"}).
#' @param path Where to write the manifest as JSON, or \code{NULL} to write
#'   nothing.
#' @param env Record the R environment.
#' @param canonical Write the one-line canonical form (keys sorted, what the
#'   digest is taken over) rather than pretty JSON.
#' @return The manifest: a list with \code{meta}, \code{results} and, with
#'   \code{env = TRUE}, \code{environment}. When \code{path} is given it is
#'   returned invisibly, with the path as attribute \code{"path"}.
#' @family capsule
#' @examples
#' checks <- data.frame(name = c("n", "mean_x"), observed = c(10, 5.5),
#'                      expected = c(10, 5.5))
#' man <- suppressMessages(morie_capsule_manifest(list(project = "demo"),
#'                                                checks, env = FALSE))
#' vapply(man$results, `[[`, "", "status")
#' path <- tempfile(fileext = ".json")
#' suppressMessages(morie_capsule_manifest(list(project = "demo"), checks,
#'                                         path = path, env = FALSE))
#' file.exists(path)
#' try(morie_capsule_manifest(list(), data.frame(name = "n")))
#' @export
morie_capsule_manifest <- function(meta = list(), checks = NULL, path = NULL, env = TRUE,
                                   canonical = FALSE) {
  if (!is.list(meta)) stop("`meta` must be a list", call. = FALSE)
  env <- .morie_capsule_flag(env, "env")
  canonical <- .morie_capsule_flag(canonical, "canonical")
  if (!is.null(path)) path <- .morie_capsule_string(path, "path")
  if (!is.null(checks)) {
    if (!is.data.frame(checks) || !all(c("name", "observed", "expected") %in% names(checks))) {
      stop("`checks` must be a data frame with columns name, observed and expected", call. = FALSE)
    }
    if (anyDuplicated(checks[["name"]])) stop("`checks` repeats a name: each check needs its own", call. = FALSE)
  }
  man <- rmoriebricklayer::mkmnf(meta, env = env)
  # ponytail: rmoriebricklayer 1.0.0 writes a named vector as a JSON array and does not sort its
  # names in the digest, so a manifest's digest changed once written and read back; as a list it holds
  if (env) man$environment$packages <- as.list(man$environment$packages)
  for (i in seq_len(NROW(checks))) {
    man <- rmoriebricklayer::recrd(man, as.character(checks[["name"]][[i]]), obs = checks[["observed"]][[i]],
                                   expd = checks[["expected"]][[i]],
                                   tol = if (is.null(checks[["tol"]])) 1e-4 else checks[["tol"]][[i]],
                                   grp = if (is.null(checks[["group"]])) "general" else as.character(checks[["group"]][[i]]))
  }
  if (is.null(path)) return(man)
  rmoriebricklayer::wrmnfjs(man, path, canon = canonical)
  invisible(structure(man, path = path))
}

#' Recompute a manifest's recorded results against the data
#'
#' Re-evaluates named statistics on \code{data} and compares each with the
#' value the manifest recorded under that name: the check that the record
#' is right, not only consistent with itself. Results recorded but not
#' recomputed are listed as \code{unchecked}, and \code{ok} is \code{FALSE}
#' while any are. This is \code{rmoriebricklayer::mnfrcmp()}.
#'
#' Errors: \code{manifest} neither a manifest with results nor the path to
#' one; \code{stats} not a non-empty named list of functions; \code{tol}
#' not \code{NULL} or one non-negative number.
#'
#' @param manifest A manifest (from [morie_capsule_manifest()]) or the path
#'   to one written as JSON.
#' @param data The data the statistics are computed from, passed unchanged
#'   to each function.
#' @param stats A named list of functions of \code{data}, each returning one
#'   value; the names are the recorded result names.
#' @param tol An absolute tolerance overriding each result's recorded one, or
#'   \code{NULL} to use the recorded one.
#' @return A \code{bricklayer_recompute} list: \code{ok}, \code{results}
#'   (data frame of \code{name}, \code{recorded}, \code{recomputed},
#'   \code{delta}, \code{tol}, \code{status} -- \code{"MATCH"},
#'   \code{"DIFFER"}, \code{"NOT_RECORDED"} or \code{"ERROR"} -- and
#'   \code{detail}) and \code{unchecked}.
#' @family capsule
#' @examples
#' d <- data.frame(x = 1:10)
#' checks <- data.frame(name = c("n", "mean_x"), observed = c(10, 5.5),
#'                      expected = c(10, 5.5))
#' path <- tempfile(fileext = ".json")
#' suppressMessages(morie_capsule_manifest(list(project = "demo"), checks,
#'                                         path = path, env = FALSE))
#' stats <- list(n = nrow, mean_x = function(z) mean(z$x))
#' morie_capsule_compare(path, d, stats)$ok
#' morie_capsule_compare(path, data.frame(x = 1:11), stats)$results
#' @export
morie_capsule_compare <- function(manifest, data, stats, tol = NULL) {
  if (is.character(manifest)) {
    manifest <- .morie_capsule_string(manifest, "manifest")
    if (!file.exists(manifest) || dir.exists(manifest)) {
      stop("`manifest` is not a manifest file: ", manifest, call. = FALSE)
    }
    manifest <- rmoriebricklayer::loadprov(manifest)
  }
  rmoriebricklayer::mnfrcmp(manifest, data, stats, tol = tol)
}
