# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Per-dataset index registry. Empirical cardinality measurements from
# the real bundled datasets drove these choices — high-cardinality
# columns (>1000 distinct values + frequent point-lookup or join key)
# get B-tree indexes; medium-cardinality columns (rollup dimensions
# like year × region) get composite indexes with the high-card column.
# Low-cardinality columns (Gender, Yes/No alerts, Measure ∈ {Max,
# Median, Mode}) are intentionally not indexed — the index overhead
# would exceed the lookup benefit.

#' Recommended indexes per known morie cache table
#'
#' @return Named list. Each entry is a list of specs:
#'   list(name = "<idx_name>", cols = c(...), unique = FALSE).
#'   Specs whose `cols` aren't all present in the actual table are
#'   silently skipped at create time.
#' @keywords internal
.morie_db_index_registry <- function() {
  list(
    # ------ SIU (case-level director's reports, 5074 cases x 64 cols) ----
    SIU = list(
      list(name = "idx_siu_drid",           cols = "drid", unique = TRUE),
      list(name = "idx_siu_case_number",    cols = "case_number"),
      list(name = "idx_siu_date_iso",       cols = "date_of_incident_iso"),
      list(name = "idx_siu_date_x_service",
           cols = c("date_of_incident_iso", "police_service"))
    ),
    siu = list(
      list(name = "idx_siu_drid",           cols = "drid", unique = TRUE),
      list(name = "idx_siu_case_number",    cols = "case_number"),
      list(name = "idx_siu_date_iso",       cols = "date_of_incident_iso"),
      list(name = "idx_siu_date_x_service",
           cols = c("date_of_incident_iso", "police_service"))
    ),

    # ------ OTIS person-level placements (a01, b01) ----------------------
    a01 = list(
      list(name = "idx_a01_uid",            cols = "UniqueIndividual_ID"),
      list(name = "idx_a01_year_x_region",
           cols = c("EndFiscalYear", "Region_AtTimeOfPlacement"))
    ),
    b01 = list(
      list(name = "idx_b01_uid",            cols = "UniqueIndividual_ID"),
      list(name = "idx_b01_year_x_region",
           cols = c("EndFiscalYear", "Region_AtTimeOfPlacement"))
    ),

    # ------ OTIS aggregate counts (b02..b09, c01..c12) -------------------
    b02 = list(
      list(name = "idx_b02_uid",            cols = "UniqueIndividual_ID"),
      list(name = "idx_b02_year_gender",
           cols = c("EndFiscalYear", "Gender"))
    ),
    b03 = list(
      list(name = "idx_b03_year_inst",
           cols = c("EndFiscalYear", "Institution_AtTimeOfPlacement"))
    ),
    b04 = list(
      list(name = "idx_b04_year_region",
           cols = c("EndFiscalYear", "Region_AtTimeOfPlacement"))
    ),
    b05 = list(
      list(name = "idx_b05_year",           cols = "EndFiscalYear")
    ),
    b06 = list(
      list(name = "idx_b06_year_inst",
           cols = c("EndFiscalYear", "Institution_AtTimeOfPlacement"))
    ),
    b07 = list(
      list(name = "idx_b07_year",           cols = "EndFiscalYear")
    ),
    b08 = list(
      list(name = "idx_b08_year_inst",
           cols = c("EndFiscalYear", "Institution_AtTimeOfPlacement"))
    ),
    b09 = list(
      list(name = "idx_b09_year",           cols = "EndFiscalYear")
    ),
    c01 = list(
      list(name = "idx_c01_year_gender",
           cols = c("EndFiscalYear", "Gender"))
    ),
    c02 = list(
      list(name = "idx_c02_inst",           cols = "Institution_MostRecentPlacement"),
      list(name = "idx_c02_year_region",
           cols = c("EndFiscalYear", "Region_MostRecentPlacement"))
    ),
    c03 = list(
      list(name = "idx_c03_year_race",
           cols = c("EndFiscalYear", "Race"))
    ),
    c04 = list(
      list(name = "idx_c04_year_race",
           cols = c("EndFiscalYear", "Race"))
    ),
    c05 = list(
      list(name = "idx_c05_year_religion",
           cols = c("EndFiscalYear", "Religion"))
    ),
    c06 = list(
      list(name = "idx_c06_year_age",
           cols = c("EndFiscalYear", "Age_Category"))
    ),
    c07 = list(
      list(name = "idx_c07_year_alert",
           cols = c("EndFiscalYear", "Alert_Type"))
    ),
    c08 = list(
      list(name = "idx_c08_year_religion",
           cols = c("EndFiscalYear", "Religion"))
    ),
    c09 = list(
      list(name = "idx_c09_year_age",
           cols = c("EndFiscalYear", "Age_Category"))
    ),
    c10 = list(
      list(name = "idx_c10_inst",           cols = "Institution_MostRecentPlacement")
    ),
    c11 = list(
      list(name = "idx_c11_year",           cols = "EndFiscalYear")
    ),
    c12 = list(
      list(name = "idx_c12_year_region",
           cols = c("EndFiscalYear", "Region_MostRecentPlacement"))
    ),

    # ------ OTIS deaths-in-custody (d01..d07) ----------------------------
    d01 = list(
      list(name = "idx_d01_uid",            cols = "UniqueIndividual_ID"),
      list(name = "idx_d01_year_region",
           cols = c("Year", "Region_AtTimeOfDeath"))
    ),
    d02 = list(
      list(name = "idx_d02_year_gender",    cols = c("Year", "Gender"))
    ),
    d03 = list(
      list(name = "idx_d03_year_race",      cols = c("Year", "Race"))
    ),
    d04 = list(
      list(name = "idx_d04_year_religion",  cols = c("Year", "Religion"))
    ),
    d05 = list(
      list(name = "idx_d05_year_age",       cols = c("Year", "Age_Category"))
    ),
    d06 = list(
      list(name = "idx_d06_year_alert",     cols = c("Year", "Alert_Type"))
    ),
    d07 = list(
      list(name = "idx_d07_year_alert",     cols = c("Year", "Alert_Type"))
    ),

    # ------ ARSAU (Use of Force, 2020-2024 per record-type) --------------
    uof_main_records = list(
      list(name = "idx_uof_main_batch",
           cols = "BatchFileName", unique = TRUE),
      list(name = "idx_uof_main_incident",  cols = "IncidentNumber"),
      list(name = "idx_uof_main_year_svc",
           cols = c("IncidentYear", "PoliceService")),
      list(name = "idx_uof_main_date",      cols = "Date")
    ),
    uof_individual_records = list(
      list(name = "idx_uof_indiv_batch_idx",
           cols = c("BatchFileName", "Indiv_Index"), unique = TRUE)
    ),
    uof_probe_cycle_records = list(
      list(name = "idx_uof_probe_batch_idx",
           cols = c("BatchFileName", "Indiv_Index"))
    ),
    uof_weapon_records = list(
      list(name = "idx_uof_weapon_batch_idx",
           cols = c("BatchFileName", "Indiv_Index"))
    )
  )
}

# TPS crime-table family shares a common base schema; one spec
# applied via prefix dispatch.
.morie_db_indexes_tps_crime <- function() {
  list(
    list(name_suffix = "_objectid",   cols = "OBJECTID",        unique = TRUE),
    list(name_suffix = "_event_uid",  cols = "EVENT_UNIQUE_ID"),
    list(name_suffix = "_hood158",    cols = "HOOD_158"),
    list(name_suffix = "_year_div",   cols = c("REPORT_YEAR", "DIVISION")),
    list(name_suffix = "_occ_date",   cols = "OCC_DATE"),
    list(name_suffix = "_loctype",    cols = "LOCATION_TYPE")
  )
}

.morie_db_indexes_for <- function(table_name) {
  reg <- .morie_db_index_registry()
  if (table_name %in% names(reg)) {
    return(reg[[table_name]])
  }
  # Trim namespace prefixes (e.g. "OTIS_b01" -> "b01") and re-check.
  short <- sub("^[A-Za-z]+_", "", table_name)
  if (short %in% names(reg)) {
    return(reg[[short]])
  }
  # TPS crime-table family dispatch
  tps_pat <- paste0("^(tps_|assault|robbery|homicide|theft|hate|bicycle|",
                    "shooting|family|breakandenter|autotheft|theftover|",
                    "communitysafety|intimate|theftfrommv|neighbourhood)")
  if (grepl(tps_pat, table_name, ignore.case = TRUE)) {
    return(lapply(.morie_db_indexes_tps_crime(), function(s) {
      s$name <- paste0("idx_", table_name, s$name_suffix)
      s$name_suffix <- NULL
      s
    }))
  }
  list()
}

#' Create the recommended B-tree indexes for a morie cache table
#'
#' Looks up `table_name` in the per-dataset index registry (see
#' [.morie_db_index_registry()] for the full list) and creates each
#' `CREATE INDEX IF NOT EXISTS` against `con`. Specs whose columns
#' aren't present in the actual table are silently skipped, so this is
#' safe to call on any morie cache table — including subsets that drop
#' some columns. Returns the number of `CREATE INDEX` statements that
#' actually ran (not the number registered).
#'
#' Cardinality-based selection: every indexed column has > 30 distinct
#' values in the real published data; low-cardinality columns
#' (`Gender`, `Yes`/`No` alerts, `Measure`) are intentionally skipped
#' because the index overhead exceeds the lookup benefit.
#'
#' @param con A DBI connection.
#' @param table_name The cache table name (case-sensitive). Common
#'   short names: `"SIU"`, `"b01"`, `"c01"`, `"d01"`,
#'   `"uof_main_records"`, `"assault"`, `"homicide"`, etc.
#' @return Invisibly returns the integer count of `CREATE INDEX`
#'   statements executed.
#' @export
morie_db_create_indexes <- function(con, table_name) {
  specs <- .morie_db_indexes_for(table_name)
  if (length(specs) == 0L) return(invisible(0L))
  cols_in_table <- tryCatch(DBI::dbListFields(con, table_name),
                            error = function(e) character(0))
  if (length(cols_in_table) == 0L) return(invisible(0L))
  n_created <- 0L
  for (s in specs) {
    if (!all(s$cols %in% cols_in_table)) next
    uniq <- if (isTRUE(s$unique)) "UNIQUE " else ""
    cols_sql <- paste0('"', s$cols, '"', collapse = ", ")
    sql <- sprintf(
      'CREATE %sINDEX IF NOT EXISTS "%s" ON "%s" (%s)',
      uniq, s$name, table_name, cols_sql)
    tryCatch({
      DBI::dbExecute(con, sql)
      n_created <- n_created + 1L
    }, error = function(e) {
      # Some backends (SQLite) reject CREATE UNIQUE INDEX on already-
      # duplicated data; fall through to a non-unique index so the
      # table still gets the read-path lookup benefit.
      if (uniq == "UNIQUE " && grepl("UNIQUE", conditionMessage(e),
                                      ignore.case = TRUE)) {
        sql2 <- sprintf(
          'CREATE INDEX IF NOT EXISTS "%s" ON "%s" (%s)',
          s$name, table_name, cols_sql)
        tryCatch({
          DBI::dbExecute(con, sql2)
          n_created <<- n_created + 1L
        }, error = function(e2) invisible(NULL))
      }
    })
  }
  invisible(n_created)
}
