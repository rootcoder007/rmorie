# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie_describe() and morie_describe_by_name() —
# pedagogical-narrative lookup for morie callables.
#
# This file ships the R-side mirror of the Python
# `morie.describe()` function. It loads the bundled
# describe_corpus.Rds (a named character vector containing the
# ~36,000 describe_<name>.md narratives shipped under
# src/morie/fn/) and prints the relevant narrative for a given
# callable. The corpus is loaded once per session and cached in
# the package-private .morie_describe_env environment.
#
# Closes the v0.9.5.4 describe()-parity gap; shipped in v0.9.5.5.

# Package-level cache for the describe corpus (lazy-loaded).
.morie_describe_env <- new.env(parent = emptyenv())

#' Load the bundled describe corpus (lazy, cached for the session).
#'
#' @return Named character vector. Names are the callable mnemonics
#'   (the short 4--7 character forms); values are the markdown
#'   narrative bodies.
#' @noRd
.morie_load_describe_corpus <- function() {
  if (!is.null(.morie_describe_env$corpus)) {
    return(.morie_describe_env$corpus)
  }
  rds <- system.file("extdata", "describe_corpus.Rds", package = "morie")
  if (!nzchar(rds) || !file.exists(rds)) {
    stop(
      "describe_corpus.Rds not found in the morie installation. ",
      "Re-install morie, or run `Rscript tools/bundle-describe-files.R` ",
      "from the morie repo root to regenerate it."
    )
  }
  .morie_describe_env$corpus <- readRDS(rds)
  .morie_describe_env$corpus
}

#' Normalise a name for describe-corpus lookup.
#'
#' Strips the leading `morie_` prefix and the legacy `describe_`
#' prefix; also strips a trailing `.md` extension if present.
#'
#' @param name Character scalar, the user-supplied lookup key.
#' @return Normalised name (lowercase mnemonic).
#' @noRd
.morie_normalise_describe_name <- function(name) {
  name <- as.character(name)[[1L]]
  name <- sub("^morie_",    "", name)
  name <- sub("^describe_", "", name)
  name <- sub("\\.md$",     "", name)
  name
}

#' Print the pedagogical narrative for a morie callable.
#'
#' Loads the describe_<name>.md narrative shipped in the package's
#' \code{inst/extdata/describe_corpus.Rds} bundle and prints it to
#' the console. This is the R-side mirror of the Python
#' \code{morie.describe()} function (closing the v0.9.5.4 parity
#' gap; shipped in v0.9.5.5).
#'
#' The lookup is forgiving: a leading \code{morie_} prefix on the
#' callable name is stripped automatically, so
#' \code{morie_describe("aalen")} and
#' \code{morie_describe("morie_aalen")} resolve to the same
#' narrative.
#'
#' @param callable A morie callable, as a function object (passed
#'   unquoted), or a character scalar name. The lookup strips the
#'   leading \code{morie_} prefix automatically.
#'
#' @return Invisibly returns the narrative as a character scalar.
#'   If no matching describe entry is found, returns \code{NULL}
#'   and prints a helpful diagnostic.
#'
#' @examples
#' \dontrun{
#' morie_describe("aalen")
#' morie_describe("morie_aalen")  # leading prefix stripped
#' morie_describe(morie_aalen)    # function-object form
#' }
#'
#' @seealso \code{\link{morie_describe_by_name}} for the
#'   string-only variant that does not capture symbol names.
#' @export
morie_describe <- function(callable) {
  name <- if (is.function(callable)) {
    n <- deparse(substitute(callable))
    if (!is.character(n) || length(n) != 1L || !nzchar(n)) {
      stop("morie_describe(): could not infer callable name; ",
           "pass a character scalar instead.")
    }
    n
  } else if (is.character(callable)) {
    callable[[1L]]
  } else {
    stop("morie_describe(): expected a function or a character scalar; ",
         "got ", typeof(callable), ".")
  }
  morie_describe_by_name(name)
}

#' String-only variant of \code{\link{morie_describe}}.
#'
#' Use this when you want to pass a name as a string and avoid the
#' unquoted-symbol capture behaviour of \code{morie_describe}.
#'
#' @param name Character scalar, the callable's mnemonic name
#'   (with or without the \code{morie_} prefix).
#'
#' @return Invisibly returns the narrative as a character scalar.
#'   If no matching describe entry is found, returns \code{NULL}
#'   and prints a helpful diagnostic.
#'
#' @examples
#' \dontrun{
#' morie_describe_by_name("aalen")
#' morie_describe_by_name("morie_aalen")
#' }
#'
#' @export
morie_describe_by_name <- function(name) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("morie_describe_by_name(): expected a non-empty ",
         "character scalar; got ", typeof(name), ".")
  }
  corpus <- .morie_load_describe_corpus()
  key <- .morie_normalise_describe_name(name)
  if (!key %in% names(corpus)) {
    message(sprintf("morie_describe(): no narrative for %s (key='%s').",
                    sQuote(name), key))
    message(sprintf("The describe corpus contains %d entries.",
                    length(corpus)))
    message("To browse: head(names(morie:::.morie_load_describe_corpus()), 20).")
    return(invisible(NULL))
  }
  body <- corpus[[key]]
  cat(body, "\n", sep = "")
  invisible(body)
}
