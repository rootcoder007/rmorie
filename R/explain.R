# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie - Multi-domain Open Research and Inferential Estimation
# Copyright (C) 2026 Vansh Singh Ruhela and morie contributors
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with this program.  If not, see
# <https://www.gnu.org/licenses/>.

# ---------------------------------------------------------------------------
# rmorie::explain - human-readable descriptions of module-output CSVs
# ---------------------------------------------------------------------------
# R port of src/morie/explain.py.  Backs `rmorie::explain_file()` and
# `rmorie::.explain_cheatsheet()` (R analogues of the `morie explain` and
# `morie cheatsheet` CLI subcommands).  Explanations target a user
# who just ran a morie module and is staring at 10-15 CSVs not
# knowing where to start.
#
# Adding a new module's output files means appending an entry to
# `.morie_explanations()`.

# .morie_explanations(), the purpose lines and the glossary live in explain_registry.R (generated).

#' Human-readable description of a morie output CSV
#'
#' Looks up a one-paragraph + short-table explanation by filename
#' (any leading directory components are stripped).  Falls back to
#' matching on the filename stem if the extension differs.
#'
#' @param filename The CSV filename, with or without a path.
#'
#' @return A character scalar containing the explanation.  If no
#'   registered entry matches, returns a fallback listing the known
#'   files.
#'
#' @examples
#' cat(explain_file("power_summary.csv"))
#'
#' @export
explain_file <- function(filename) {
  explanations <- .morie_explanations()
  name <- basename(gsub("\\\\", "/", filename))  # a Windows path names the file after a backslash
  if (name %in% names(explanations)) {
    return(explanations[[name]])
  }
  table <- .morie_explain_table(name, filename)  # every module table: purpose + the columns the file really has
  if (!is.null(table)) return(table)

  base <- tools::file_path_sans_ext(name)
  for (candidate in names(explanations)) {
    if (tools::file_path_sans_ext(candidate) == base) {
      return(explanations[[candidate]])
    }
  }

  paste0(
    sprintf("No registered explanation for '%s'.\
\
", name),
    "Known files:\
",
    paste(sprintf("  - %s", sort(names(explanations))), collapse = "\
"),
    "\
\
If you think this file should be explained, file an issue at ",
    "https://github.com/rootcoder007/rmorie/issues."
  )
}

#' Print the morie cheat sheet
#'
#' Mirrors the \code{morie cheatsheet} CLI subcommand: a one-screen
#' reference of install / learn / run / pull / ingest / help commands.
#'
#' @return Invisibly returns a character scalar of the cheatsheet.
#'   Called for its side effect of printing to the console.
#'
#' @examples
#' .explain_cheatsheet()
#'
#' @export
.explain_cheatsheet <- function() {
  body <- paste(
    "morie cheat sheet",
    "=================",
    "",
    "Install",
    "  curl -fsSL https://rootcoder007.github.io/morie/install.sh | bash",
    "  brew tap rootcoder007/morie && brew install morie",
    "  pip install morie",
    "  install.packages('morie', repos = 'https://rootcoder007.r-universe.dev')",
    "  docker run --rm ghcr.io/rootcoder007/morie:latest morie --help",
    "",
    "Learn",
    "  morie tutorial                  Interactive walkthrough",
    "  morie cheatsheet                This card",
    "  morie list-modules              List all 23 analysis modules",
    "  morie list-datasets             List built-in datasets",
    "  morie explain power_summary.csv What does this output mean?",
    "",
    "Run",
    "  morie run-module power-design --output-dir out/",
    "  morie run-module descriptive-statistics --output-dir out/",
    "  morie run-module frequentist-inference --output-dir out/",
    "  morie run-modules all --output-dir out/",
    "",
    "Pull",
    "  morie pull tps-major --year 2024 --out tps-2024.csv",
    "  morie pull tps-shootings --year 2024",
    "  morie pull tps-homicide --year 2024",
    "  morie pull tps-layers                                   # registry",
    "  morie pull cpads --out cpads.csv                        # synth or real",
    "  morie pull otis-a01-toy --out otis.csv                  # toy",
    "  morie pull siu-toy --out siu.csv                        # toy SIU report",
    "",
    "Ingest",
    "  morie ingest tps --layer major-crime --year 2024 --out tps.csv",
    "  morie ingest ckan --portal https://open.canada.ca/data --search alcohol",
    "  morie ingest siu --report-id 22-OFD-001 --out report/",
    "",
    "Help",
    "  morie ask \"I have a treatment-control design; what module fits?\"",
    "  morie doctor                    Check what's installed and working",
    "  morie --help                    Top-level help",
    "",
    "Refs",
    "  Docs:     https://rootcoder007.github.io/morie/",
    "  Issues:   https://github.com/rootcoder007/rmorie/issues",
    "  PyPI:     https://pypi.org/project/morie/",
    "  R:        https://rootcoder007.r-universe.dev/morie",
    sep = "\
"
  )
  cat(body, "\
", sep = "")
  invisible(body)
}

#' Names of all morie output CSVs with registered explanations
#'
#' @return Character vector of filenames.
#' @examples
#' v <- explain_known_files()
#' head(v)
#' @export
explain_known_files <- function() sort(names(.morie_explanations()))
