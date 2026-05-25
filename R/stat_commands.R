# SPDX-License-Identifier: AGPL-3.0-or-later
#' Central command registry for the morie R surface
#'
#' R port of \code{morie.stat_commands}. Maintains a flat registry of
#' R-callable statistical command entries plus aliases, allowing
#' downstream tooling (the Go TIDE bridge, headless workers, REPL
#' frontends) to enumerate, resolve, and dispatch operations from a
#' single namespace.
#'
#' Entries are stored in the package-level environment
#' \code{.morie_stat_commands} so that the registry is shared across
#' sessions within a single R process and can be appended by extension
#' packages.
#'
#' Functions
#' ---------
#' \itemize{
#'   \item \code{\link{stat_command}}: constructor for a single command.
#'   \item \code{\link{register_stat_command}}: add an entry to the
#'     registry, validating uniqueness of name and aliases.
#'   \item \code{\link{resolve_stat_command}}: look up by canonical
#'     name or alias.
#'   \item \code{\link{all_stat_command_names}}: sorted vector of all
#'     names + aliases.
#'   \item \code{\link{commands_by_category}}: list of entries grouped
#'     by category.
#'   \item \code{\link{run_stat_command}}: invoke a command's REPL
#'     handler with the supplied arguments.
#' }
#'
#' @name morie_stat_commands
NULL


# ---------------------------------------------------------------------------
# Registry storage
# ---------------------------------------------------------------------------

# Package-private environment, set up once on package load.
.morie_stat_commands <- new.env(parent = emptyenv())
.morie_stat_commands$registry <- list()
.morie_stat_commands$aliases <- list()
.morie_stat_commands$categories <- list()


#' Construct a stat_command entry
#'
#' @param name Canonical command name (character scalar).
#' @param category Category label used for grouping.
#' @param usage One-line usage string for help screens.
#' @param description Short description.
#' @param handler_repl Function implementing the command.
#' @param handler_stat Optional terminal handler taking
#'   \code{(parts, log, store)}. Defaults to a wrapper around
#'   \code{handler_repl}.
#' @param aliases Character vector of additional names.
#' @param module Source module string (informational).
#' @param is_compound Logical; flags compound workflows.
#' @param is_r_bridge Logical; flags Python <-> R bridge calls.
#' @return A list with class \code{morie_stat_command}.
#' @export
stat_command <- function(name, category, usage, description,
                          handler_repl,
                          handler_stat = NULL,
                          aliases = character(0),
                          module = "",
                          is_compound = FALSE,
                          is_r_bridge = FALSE) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  stopifnot(is.function(handler_repl))

  if (is.null(handler_stat)) {
    handler_stat <- function(parts, log, store) {
      args <- if (length(parts) > 1L) parts[-1L] else list()
      out <- tryCatch(do.call(handler_repl, as.list(args)),
                      error = function(e) {
                        if (is.function(log)) log(sprintf("Error: %s", conditionMessage(e)))
                        else if (is.list(log) && is.function(log$write))
                          log$write(sprintf("Error: %s", conditionMessage(e)))
                        else message("Error: ", conditionMessage(e))
                        NULL
                      })
      if (!is.null(out) && !is.null(store)) {
        if (is.function(store)) store(format(out))
      }
      invisible(out)
    }
  }

  out <- list(
    name = name,
    category = category,
    usage = usage,
    description = description,
    handler_repl = handler_repl,
    handler_stat = handler_stat,
    aliases = as.character(aliases),
    module = module,
    is_compound = isTRUE(is_compound),
    is_r_bridge = isTRUE(is_r_bridge)
  )
  class(out) <- c("morie_stat_command", "list")
  out
}


#' Register a stat_command in the package-level registry
#'
#' @param cmd A \code{morie_stat_command} constructed by \code{stat_command}.
#' @return The command name, invisibly.
#' @export
register_stat_command <- function(cmd) {
  if (!inherits(cmd, "morie_stat_command")) {
    stop("cmd must be a morie_stat_command")
  }
  reg <- .morie_stat_commands$registry
  aliases <- .morie_stat_commands$aliases
  cats <- .morie_stat_commands$categories

  reg[[cmd$name]] <- cmd
  cats[[cmd$category]] <- unique(c(cats[[cmd$category]], cmd$name))
  for (a in cmd$aliases) {
    aliases[[a]] <- cmd$name
  }

  .morie_stat_commands$registry <- reg
  .morie_stat_commands$aliases <- aliases
  .morie_stat_commands$categories <- cats
  invisible(cmd$name)
}


#' Resolve a command by canonical name or alias
#'
#' @param name Character scalar.
#' @return A \code{morie_stat_command} or \code{NULL}.
#' @export
resolve_stat_command <- function(name) {
  if (!is.character(name) || length(name) != 1L) {
    return(NULL)
  }
  reg <- .morie_stat_commands$registry
  if (name %in% names(reg)) {
    return(reg[[name]])
  }
  alias <- .morie_stat_commands$aliases[[name]]
  if (!is.null(alias) && alias %in% names(reg)) {
    return(reg[[alias]])
  }
  NULL
}


#' Sorted vector of all command names + aliases
#' @export
all_stat_command_names <- function() {
  sort(unique(c(names(.morie_stat_commands$registry),
                names(.morie_stat_commands$aliases))))
}


#' Commands grouped by category
#' @return Named list of character vectors of command names.
#' @export
commands_by_category <- function() {
  cats <- .morie_stat_commands$categories
  reg <- .morie_stat_commands$registry
  out <- list()
  for (cat_name in sort(names(cats))) {
    names_in_cat <- cats[[cat_name]]
    names_in_cat <- names_in_cat[names_in_cat %in% names(reg)]
    out[[cat_name]] <- names_in_cat
  }
  out
}


#' Run a command's REPL handler with positional/keyword arguments
#'
#' @param name Command name or alias.
#' @param ... Arguments forwarded to the REPL handler.
#' @return Whatever the handler returns. Stops with an informative
#'   error if the command is not registered.
#' @export
run_stat_command <- function(name, ...) {
  cmd <- resolve_stat_command(name)
  if (is.null(cmd)) {
    stop(sprintf("Unknown morie stat command: '%s'", name))
  }
  cmd$handler_repl(...)
}


#' Total number of registered commands (excluding aliases)
#' @export
n_stat_commands <- function() {
  length(.morie_stat_commands$registry)
}


#' Reset the registry (test / debugging helper)
#' @return The number of commands removed, invisibly.
#' @keywords internal
#' @export
clear_stat_commands <- function() {
  n <- length(.morie_stat_commands$registry)
  .morie_stat_commands$registry <- list()
  .morie_stat_commands$aliases <- list()
  .morie_stat_commands$categories <- list()
  invisible(n)
}


# ---------------------------------------------------------------------------
# Seed registrations
# ---------------------------------------------------------------------------

# Register a curated, small first wave so the registry has reachable
# entries even before downstream packages append their own. The full
# 620-command tree lives in Python; the R surface starts with the
# multiple-testing and semiparametric callables ported alongside this
# file.
.morie_seed_stat_commands <- function() {
  seeds <- list(
    list(
      name = "bonferroni", category = "Multiple Testing",
      usage = "bonferroni(p_values, alpha)",
      description = "Bonferroni FWER correction",
      handler = function(...) bonferroni(...),
      aliases = c("bonf")
    ),
    list(
      name = "holm", category = "Multiple Testing",
      usage = "holm(p_values, alpha)",
      description = "Holm step-down FWER procedure",
      handler = function(...) holm(...),
      aliases = character(0)
    ),
    list(
      name = "benjamini_hochberg", category = "Multiple Testing",
      usage = "benjamini_hochberg(p_values, alpha)",
      description = "BH false discovery rate control",
      handler = function(...) benjamini_hochberg(...),
      aliases = c("bh", "bh_fdr")
    ),
    list(
      name = "benjamini_yekutieli", category = "Multiple Testing",
      usage = "benjamini_yekutieli(p_values, alpha)",
      description = "BY false discovery rate under dependence",
      handler = function(...) benjamini_yekutieli(...),
      aliases = c("by_fdr")
    ),
    list(
      name = "storey_q", category = "Multiple Testing",
      usage = "storey_q(p_values, alpha, lambda_param)",
      description = "Adaptive Storey q-value FDR",
      handler = function(...) storey_q(...),
      aliases = c("qvalue")
    ),
    list(
      name = "fisher_combined", category = "Multiple Testing",
      usage = "fisher_combined(p_values)",
      description = "Fisher's combined p-value",
      handler = function(...) fisher_combined(...),
      aliases = c("fisher_comb")
    ),
    list(
      name = "stouffer_combined", category = "Multiple Testing",
      usage = "stouffer_combined(p_values, weights)",
      description = "Stouffer's z-score combination",
      handler = function(...) stouffer_combined(...),
      aliases = c("stouffer")
    ),
    list(
      name = "harmonic_mean_p", category = "Multiple Testing",
      usage = "harmonic_mean_p(p_values)",
      description = "Harmonic mean p-value",
      handler = function(...) harmonic_mean_p(...),
      aliases = c("hmp")
    ),
    list(
      name = "adjust_p_values", category = "Multiple Testing",
      usage = "adjust_p_values(p_values, method, alpha)",
      description = "Dispatcher across all FWER and FDR methods",
      handler = function(...) adjust_p_values(...),
      aliases = c("padjust")
    ),
    list(
      name = "estimate_pi0", category = "Multiple Testing",
      usage = "estimate_pi0(p_values, method)",
      description = "Estimate the proportion of true null hypotheses",
      handler = function(...) estimate_pi0(...),
      aliases = c("pi0")
    ),
    list(
      name = "n_effective_tests", category = "Multiple Testing",
      usage = "n_effective_tests(correlation_matrix, method)",
      description = "Effective number of independent tests",
      handler = function(...) n_effective_tests(...),
      aliases = c("meff")
    ),
    list(
      name = "nw_regression", category = "Semiparametric",
      usage = "nw_regression(x, y, x_eval, bandwidth)",
      description = "Nadaraya-Watson kernel regression",
      handler = function(...) nw_regression(...),
      aliases = c("nw")
    ),
    list(
      name = "local_linear", category = "Semiparametric",
      usage = "local_linear(x, y, x_eval, bandwidth)",
      description = "Local linear kernel regression",
      handler = function(...) local_linear(...),
      aliases = c("locallinear")
    ),
    list(
      name = "kde", category = "Semiparametric",
      usage = "kde(x, x_eval, bandwidth, kernel)",
      description = "Kernel density estimation",
      handler = function(...) kde(...),
      aliases = c("kerneldens")
    ),
    list(
      name = "silverman_bandwidth", category = "Semiparametric",
      usage = "silverman_bandwidth(x)",
      description = "Silverman's rule-of-thumb bandwidth",
      handler = function(...) silverman_bandwidth(...),
      aliases = c("h_silverman")
    ),
    list(
      name = "loocv_bandwidth", category = "Semiparametric",
      usage = "loocv_bandwidth(x, y, bw_min, bw_max, n_grid)",
      description = "Leave-one-out CV bandwidth for NW regression",
      handler = function(...) loocv_bandwidth(...),
      aliases = c("h_loocv")
    ),
    list(
      name = "gam_smoother", category = "Semiparametric",
      usage = "gam_smoother(x, y, k)",
      description = "mgcv::gam thin-plate smoother",
      handler = function(...) gam_smoother(...),
      aliases = c("gam")
    )
  )

  for (s in seeds) {
    if (is.null(resolve_stat_command(s$name))) {
      register_stat_command(stat_command(
        name = s$name,
        category = s$category,
        usage = s$usage,
        description = s$description,
        handler_repl = s$handler,
        aliases = s$aliases
      ))
    }
  }
  invisible(length(seeds))
}

# Run seed registration on package load. Wrapped in try() so a downstream
# missing dependency does not abort attachment.
local({
  try(.morie_seed_stat_commands(), silent = TRUE)
})


# ---------------------------------------------------------------------------
# Print method
# ---------------------------------------------------------------------------

#' @export
print.morie_stat_command <- function(x, ...) {
  cat(sprintf("morie stat command: %s\
", x$name))
  cat(sprintf("  category : %s\
", x$category))
  cat(sprintf("  usage    : %s\
", x$usage))
  cat(sprintf("  module   : %s\
", x$module))
  if (length(x$aliases) > 0L) {
    cat(sprintf("  aliases  : %s\
", paste(x$aliases, collapse = ", ")))
  }
  if (nzchar(x$description)) {
    cat("\
", x$description, "\
", sep = "")
  }
  invisible(x)
}


# --- APPENDED 2026-05-22 -----------------------------------------------------
# Auto-registration: walk the package namespace and register every exported
# function matching ^morie_ as a stat_command, inferring a category from the
# filename prefix (e.g. R/morie_did_*.R -> "DiD").  Fires once on .onLoad().
# ----------------------------------------------------------------------------

# Map a filename prefix to a human-readable category.  Extend as new
# module families land in the package.
.MORIE_CATEGORY_PREFIX_MAP <- list(
  morie_did       = "DiD",
  morie_tps       = "TPS Spatial",
  morie_llm       = "LLM",
  morie_run       = "Pipelines",
  morie_compare   = "Inference",
  morie_estimate  = "Inference",
  morie_bootstrap = "Bootstrap",
  morie_audit     = "Audit",
  morie_otis      = "OTIS",
  morie_hajek     = "Causal",
  morie_iv        = "Causal",
  morie_dml       = "Causal",
  morie_doob      = "Time Series",
  morie_describe  = "Documentation"
)

# Infer a category for a function `fn_name` by searching the installed
# R/ directory for files whose names begin with a known prefix.  Falls
# back to scanning the function's source attributes when available.
.morie_infer_category <- function(fn_name) {
  # Cheap path: prefix match against the static map.
  for (px in names(.MORIE_CATEGORY_PREFIX_MAP)) {
    if (startsWith(fn_name, paste0(px, "_")) ||
        identical(fn_name, px)) {
      return(.MORIE_CATEGORY_PREFIX_MAP[[px]])
    }
  }
  # Try matching against installed R/ filenames.
  r_dir <- system.file("R", package = "morie")
  if (nzchar(r_dir) && dir.exists(r_dir)) {
    files <- list.files(r_dir, pattern = "\\\\.R$", full.names = FALSE)
    for (f in files) {
      stub <- tools::file_path_sans_ext(f)
      if (startsWith(fn_name, paste0(stub, "_")) ||
          identical(fn_name, stub)) {
        # Capitalise first letter for a readable category label.
        return(paste0(toupper(substr(stub, 1, 1)),
                      substring(stub, 2)))
      }
    }
  }
  "Misc"
}

#' Auto-register every exported ``morie_*`` function as a stat_command
#'
#' Walks ``getNamespaceExports("morie")``, filters to the ``^morie_`` family,
#' and registers each as a \code{morie_stat_command} unless one already
#' exists under that name.  The category is inferred from the function's
#' source filename prefix via \code{.MORIE_CATEGORY_PREFIX_MAP}.  Safe to
#' call repeatedly -- existing registrations are left untouched.
#'
#' @return Integer count of newly registered commands, invisibly.
#' @keywords internal
#' @export
.morie_auto_register_stat_commands <- function() {
  exports <- tryCatch(getNamespaceExports("morie"),
                      error = function(e) character(0))
  if (length(exports) == 0L) return(invisible(0L))
  candidates <- grep("^morie_", exports, value = TRUE)
  n_added <- 0L
  for (nm in candidates) {
    if (!is.null(resolve_stat_command(nm))) next
    fn <- tryCatch(getExportedValue("morie", nm), error = function(e) NULL)
    if (!is.function(fn)) next
    cat <- .morie_infer_category(nm)
    desc <- sprintf("Auto-registered from package namespace (%s).", nm)
    usage <- paste0(nm, "(...)")
    cmd <- tryCatch(
      stat_command(name = nm, category = cat, usage = usage,
                   description = desc, handler_repl = fn,
                   aliases = character(0), module = nm,
                   is_compound = FALSE, is_r_bridge = FALSE),
      error = function(e) NULL)
    if (is.null(cmd)) next
    tryCatch({
      register_stat_command(cmd)
      n_added <- n_added + 1L
    }, error = function(e) NULL)
  }
  invisible(n_added)
}


# --- Package-load hook -------------------------------------------------------
# Fires once when the package namespace is loaded.  Wrapped in try() so a
# downstream failure (e.g. missing optional dep used by a handler) never
# aborts the load.
.onLoad <- function(libname, pkgname) {
  try(.morie_auto_register_stat_commands(), silent = TRUE)
  invisible(NULL)
}
