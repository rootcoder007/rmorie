# SPDX-License-Identifier: AGPL-3.0-or-later

# Central execution guard for rmorie's process-execution and
# deserialization sinks.
#
# TRUST MODEL
# -----------
# rmorie runs code and file paths supplied by the LOCAL USER (function
# arguments, their own R session) or by an agent CLI the user has
# explicitly configured. Nothing here runs remote or network-supplied
# code. Even so, every process-exec sink and every readRDS is funnelled
# through this file so that a locked-down environment can disable them
# with `MORIE_NO_EXEC=1` (the same knob name honoured by the Python morie
# package), and so that .rds provenance policy lives in one place.
#
# These helpers are internal (not exported); they add a kill-switch and a
# single choke point, they do NOT change default behaviour.

# TRUE when an environment variable is set to a truthy value.
#' TRUE when an environment variable is set to a truthy value
#'
#' A step of the exec_guard implementation. Called by \code{.morie_exec_disabled}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param name See Usage.
#' @return The value of \code{%in%}.
#' @export
.morie_env_true <- function(name) {
  val <- Sys.getenv(name, unset = "")
  tolower(trimws(val)) %in% c("1", "true", "yes", "on")
}

# TRUE when dynamic execution is disabled via MORIE_NO_EXEC.
#' TRUE when dynamic execution is disabled via MORIE_NO_EXEC
#'
#' A step of the exec_guard implementation. Called by \code{.morie_ensure_exec_allowed}, \code{.morie_safe_readRDS}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return The value of \code{.morie_env_true}.
#' @export
.morie_exec_disabled <- function() {
  .morie_env_true("MORIE_NO_EXEC")
}

# Stop with a clear message if MORIE_NO_EXEC is set. Call at the head of
# every sink that spawns an external process.
#' Stop with a clear message if MORIE_NO_EXEC is set. Call at the head
#' of
#'
#' every sink that spawns an external process.
#'
#' @param feature Defaults to \code{"dynamic execution"}.
#' @return Invisibly,a logical value.
#' @export
.morie_ensure_exec_allowed <- function(feature = "dynamic execution") {
  if (.morie_exec_disabled()) {
    stop(sprintf(
      paste0(
        "%s is disabled because MORIE_NO_EXEC is set. ",
        "Unset MORIE_NO_EXEC to allow it on this machine."
      ),
      feature
    ), call. = FALSE)
  }
  invisible(TRUE)
}

# Single choke point for readRDS. readRDS can execute code while loading
# (S4/ALTREP/reference-object unserialize hooks), so a locked-down
# environment can forbid it entirely via MORIE_NO_EXEC. By default it
# loads normally -- the path is user-chosen or a first-party cache file;
# provenance is the caller's responsibility. Feed it only .rds files you
# trust.
#' Single choke point for readRDS. readRDS can execute code while
#' loading
#'
#' (S4/ALTREP/reference-object unserialize hooks), so a locked-down
#' environment can forbid it entirely via MORIE_NO_EXEC. By default it
#' loads normally -- the path is user-chosen or a first-party cache
#' file; provenance is the caller\'s responsibility. Feed it only .rds
#' files you trust.
#'
#' @param path See Usage.
#' @param feature Defaults to \code{"reading an .rds file"}.
#' @return The value of \code{readRDS}.
#' @export
.morie_safe_readRDS <- function(path, feature = "reading an .rds file") {
  if (.morie_exec_disabled()) {
    stop(sprintf(
      paste0(
        "%s is disabled because MORIE_NO_EXEC is set (readRDS can ",
        "execute code while loading). Unset MORIE_NO_EXEC to allow it."
      ),
      feature
    ), call. = FALSE)
  }
  readRDS(path)
}

# Validate a git ref (branch/tag) before passing it to `git clone`.
# Rejects option-injection (leading '-', e.g. "--upload-pack=...") and
# any whitespace/metacharacters, while allowing normal ref names.
#' Validate a git ref (branch/tag) before passing it to `git clone`
#'
#' Rejects option-injection (leading \'-\', e.g. "--upload-pack=...")
#' and any whitespace/metacharacters, while allowing normal ref names.
#'
#' @param ref A vector; its length is taken.
#' @return A logical value.
#' @export
.morie_valid_git_ref <- function(ref) {
  length(ref) == 1L && grepl("^[A-Za-z0-9][A-Za-z0-9._/-]*$", ref)
}

# Structured report of every trust knob and its state (used by
# diagnostics so the active posture is visible).
#' Structured report of every trust knob and its state (used by
#'
#' diagnostics so the active posture is visible).
#'
#' @return A data frame.
#' @export
.morie_knob_status <- function() {
  knobs <- c(
    MORIE_NO_EXEC = paste(
      "when set: process execution and .rds loading are disabled"
    )
  )
  data.frame(
    name = names(knobs),
    enabled = vapply(names(knobs), .morie_env_true, logical(1)),
    detail = unname(knobs),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
