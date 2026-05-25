# SPDX-License-Identifier: AGPL-3.0-or-later
#' City-agnostic data profiles for the predictive-policing audit
#'
#' R port of the Python module \code{morie.fairness.cityprofile}.
#' The disparity audit operates on a canonical per-area schema:
#' \code{area}, \code{risk}, \code{outcome}, \code{population},
#' \code{group}. A \code{morie_city_profile} records which columns
#' of one city's open-data export carry those five canonical fields,
#' and \code{\link{morie_fairness_apply_profile}} renames an arbitrary
#' city \code{data.frame} onto the canonical schema so the audit code
#' never needs to know which city the data came from.
#'
#' Functions
#' ---------
#'
#' \itemize{
#'   \item \code{\link{morie_fairness_city_profile}}: constructor for a
#'     city profile object.
#'   \item \code{\link{morie_fairness_register_city}}: register a
#'     profile in the process-local registry.
#'   \item \code{\link{morie_fairness_get_city}}: look up a registered
#'     profile by case-insensitive name.
#'   \item \code{\link{morie_fairness_list_cities}}: list registered
#'     profile names.
#'   \item \code{\link{morie_fairness_apply_profile}}: rename a
#'     \code{data.frame} onto the canonical schema.
#' }
#'
#' @name fairness_cityprofile
NULL


#' The five canonical per-area fields the audit consumes.
#' @export
MORIE_FAIRNESS_CANONICAL_FIELDS <- c(
  "area", "risk", "outcome", "population", "group"
)


# ---------------------------------------------------------------------------
# Internal registry (process-local; mirrors Python's _REGISTRY dict)
# ---------------------------------------------------------------------------

.morie_fairness_registry <- new.env(parent = emptyenv())


.morie_fairness_init_registry <- function() {
  if (!exists("generic", envir = .morie_fairness_registry, inherits = FALSE)) {
    assign("generic",
           morie_fairness_city_profile(
             name = "generic",
             area_col = "area",
             risk_col = "risk",
             outcome_col = "outcome",
             population_col = "population",
             group_col = "group",
             notes = paste0(
               "Identity profile - the data.frame already uses the ",
               "canonical column names."
             )
           ),
           envir = .morie_fairness_registry)
  }
}


# ---------------------------------------------------------------------------
# 1. constructor
# ---------------------------------------------------------------------------

#' Construct a city profile (column map onto the canonical audit schema)
#'
#' Each \code{*_col} argument names the column, in that city's own
#' export, that carries the corresponding canonical field.
#' \code{risk_col} and \code{outcome_col} may be \code{NULL} when a
#' city only supplies one side (e.g. risk scores but no realised-
#' outcome feed); the missing side must then be supplied separately
#' to the audit.
#'
#' @param name Character identifier used with
#'   \code{\link{morie_fairness_get_city}}.
#' @param area_col Column holding the area / district / precinct
#'   identifier. Required.
#' @param risk_col,outcome_col,population_col,group_col Optional
#'   columns for predicted risk, realised-outcome count, area
#'   population, and protected attribute.
#' @param notes Free-text provenance or caveats, surfaced to the user.
#' @return A list of class \code{morie_city_profile}.
#' @examples
#' p <- morie_fairness_city_profile(
#'   "chicago", area_col = "community_area",
#'   risk_col = "rti", group_col = "majority_race"
#' )
#' p$name
#' @export
morie_fairness_city_profile <- function(name,
                                        area_col,
                                        risk_col = NULL,
                                        outcome_col = NULL,
                                        population_col = NULL,
                                        group_col = NULL,
                                        notes = "") {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  stopifnot(is.character(area_col), length(area_col) == 1L)

  out <- list(
    name = name,
    area_col = area_col,
    risk_col = risk_col,
    outcome_col = outcome_col,
    population_col = population_col,
    group_col = group_col,
    notes = if (is.null(notes)) "" else as.character(notes)
  )
  class(out) <- c("morie_city_profile", "list")
  out
}


#' Column map for a city profile
#'
#' Returns a named character vector mapping source column names
#' (those defined on the profile) to canonical field names.
#'
#' @param profile A \code{morie_city_profile}.
#' @return A named character vector \code{c(source = canonical)}.
#' @export
morie_fairness_column_map <- function(profile) {
  stopifnot(inherits(profile, "morie_city_profile"))
  pairs <- list(
    area = profile$area_col,
    risk = profile$risk_col,
    outcome = profile$outcome_col,
    population = profile$population_col,
    group = profile$group_col
  )
  keep <- !vapply(pairs, is.null, logical(1))
  pairs <- pairs[keep]
  out <- setNames(names(pairs), unlist(pairs, use.names = FALSE))
  out
}


# ---------------------------------------------------------------------------
# 2. register / get / list
# ---------------------------------------------------------------------------

#' Register a city profile in the process-local registry
#'
#' @param profile A \code{morie_city_profile}.
#' @param overwrite If \code{FALSE} (default), registering an existing
#'   name raises an error; pass \code{TRUE} to replace it.
#' @return Invisibly returns the registered profile.
#' @export
morie_fairness_register_city <- function(profile, overwrite = FALSE) {
  stopifnot(inherits(profile, "morie_city_profile"))
  .morie_fairness_init_registry()
  key <- tolower(trimws(profile$name))
  if (exists(key, envir = .morie_fairness_registry, inherits = FALSE) &&
      !isTRUE(overwrite)) {
    stop(sprintf(
      "city '%s' is already registered; pass overwrite=TRUE to replace it.",
      key
    ), call. = FALSE)
  }
  assign(key, profile, envir = .morie_fairness_registry)
  invisible(profile)
}


#' Look up a registered city profile by case-insensitive name
#'
#' @param name Character. The profile name (case-insensitive).
#' @return A \code{morie_city_profile}.
#' @export
morie_fairness_get_city <- function(name) {
  stopifnot(is.character(name), length(name) == 1L)
  .morie_fairness_init_registry()
  key <- tolower(trimws(name))
  if (!exists(key, envir = .morie_fairness_registry, inherits = FALSE)) {
    have <- morie_fairness_list_cities()
    stop(sprintf(
      "no city profile '%s'; registered: %s. Register one with morie_fairness_register_city().",
      name, paste(have, collapse = ", ")
    ), call. = FALSE)
  }
  get(key, envir = .morie_fairness_registry, inherits = FALSE)
}


#' List registered city profile names
#'
#' @return Sorted character vector of registered profile names.
#' @export
morie_fairness_list_cities <- function() {
  .morie_fairness_init_registry()
  sort(ls(envir = .morie_fairness_registry))
}


# ---------------------------------------------------------------------------
# 3. apply_profile
# ---------------------------------------------------------------------------

#' Rename a city data.frame onto the canonical audit schema
#'
#' @param df A \code{data.frame} in the city's native column names.
#' @param profile A \code{morie_city_profile} or the name (character)
#'   of a registered profile.
#' @return A new \code{data.frame} with the profile's columns renamed
#'   to the canonical names, retaining only those canonical columns.
#' @examples
#' df <- data.frame(beat = c("A", "B"), score = c(0.1, 0.9))
#' p <- morie_fairness_city_profile(
#'   "demo", area_col = "beat", risk_col = "score"
#' )
#' morie_fairness_apply_profile(df, p)
#' @export
morie_fairness_apply_profile <- function(df, profile) {
  stopifnot(is.data.frame(df))
  if (is.character(profile)) {
    profile <- morie_fairness_get_city(profile)
  }
  stopifnot(inherits(profile, "morie_city_profile"))

  colmap <- morie_fairness_column_map(profile)  # source -> canonical
  src_cols <- names(colmap)
  missing <- setdiff(src_cols, names(df))
  if (length(missing) > 0L) {
    stop(sprintf(
      "profile '%s' expects column(s) %s which are not in the data.frame (columns: %s)",
      profile$name,
      paste(sprintf("'%s'", missing), collapse = ", "),
      paste(sprintf("'%s'", names(df)), collapse = ", ")
    ), call. = FALSE)
  }
  out <- df[, src_cols, drop = FALSE]
  names(out) <- as.character(colmap[src_cols])
  out
}


# ---------------------------------------------------------------------------
# print
# ---------------------------------------------------------------------------

#' @export
print.morie_city_profile <- function(x, ...) {
  cat("morie_city_profile:", x$name, "\
", sep = " ")
  cat(strrep("-", 25 + nchar(x$name)), "\
", sep = "")
  fields <- c("area_col", "risk_col", "outcome_col",
              "population_col", "group_col")
  for (f in fields) {
    v <- x[[f]]
    cat(sprintf("  %-14s  %s\
", f,
                if (is.null(v)) "<unset>" else v))
  }
  if (nzchar(x$notes)) {
    cat("\
  notes: ", x$notes, "\
", sep = "")
  }
  invisible(x)
}
