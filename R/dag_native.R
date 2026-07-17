# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native causal DAG toolkit (feat/native-specializations, module 13).
# Pearl's graphical identification pipeline — build a DAG, identify the
# backdoor adjustment set (Bayes-Ball d-separation), estimate through
# the native effect estimators, and refute DoWhy-style — with no
# dagitty/V8 or Python runtime.

#' Build a causal DAG
#'
#' @param edges Character vector of edges, each `"A -> B"`.
#' @param exposure Name of the exposure/treatment node.
#' @param outcome Name of the outcome node.
#' @param latent Character vector of unobserved nodes (excluded from
#'   any adjustment set).
#' @return An object of class `morie_dag`: list with `nodes`, `edges`
#'   (2-column matrix from/to), `exposure`, `outcome`, `latent`.
#' @srrstats {G1.0} Pearl (2009, Causality, 2nd ed.) for the backdoor
#'   criterion; Shachter (1998) for the Bayes-Ball d-separation
#'   algorithm used by \code{morie_dag_identify}.
#' @examples
#' g <- morie_dag(c("race -> placement", "race -> outcome",
#'                  "placement -> outcome"),
#'                exposure = "placement", outcome = "outcome")
#' morie_dag_identify(g)
#' @export
morie_dag <- function(edges, exposure, outcome, latent = character()) {
  em <- do.call(rbind, lapply(edges, function(e) {
    p <- trimws(strsplit(e, "->", fixed = TRUE)[[1]])
    if (length(p) != 2L || !all(nzchar(p)))
      stop("edge must look like 'A -> B': ", e, call. = FALSE)
    p
  }))
  colnames(em) <- c("from", "to")
  nodes <- unique(c(em))
  if (!exposure %in% nodes) stop("exposure not in graph", call. = FALSE)
  if (!outcome %in% nodes) stop("outcome not in graph", call. = FALSE)
  # acyclicity via Kahn's algorithm
  indeg <- stats::setNames(rowSums(outer(nodes, em[, "to"], "==")), nodes)
  q <- names(indeg)[indeg == 0]
  seen <- 0L
  indeg2 <- indeg
  while (length(q)) {
    v <- q[1]; q <- q[-1]; seen <- seen + 1L
    for (w in em[em[, "from"] == v, "to"]) {
      indeg2[w] <- indeg2[w] - 1L
      if (indeg2[w] == 0L) q <- c(q, w)
    }
  }
  if (seen != length(nodes)) stop("graph has a cycle", call. = FALSE)
  structure(list(nodes = nodes, edges = em, exposure = exposure,
                 outcome = outcome, latent = latent),
            class = "morie_dag")
}

#' @examples
#' \donttest{
#' g <- morie_dag(c("race -> placement", "race -> outcome",
#'                  "placement -> outcome"),
#'                exposure = "placement", outcome = "outcome")
#' morie_dag_identify(g)
#' print(g)
#' }
#' @export
print.morie_dag <- function(x, ...) {
  cat("morie causal DAG:", length(x$nodes), "nodes,",
      nrow(x$edges), "edges\n")
  cat("  exposure:", x$exposure, " outcome:", x$outcome, "\n")
  if (length(x$latent)) cat("  latent:", paste(x$latent, collapse = ", "), "\n")
  apply(x$edges, 1, function(e) cat("  ", e[1], "->", e[2], "\n"))
  invisible(x)
}

#' Internal helper: descendants of a node (including itself)
#' @noRd
.morie_dag_desc <- function(g, v) {
  out <- v
  frontier <- v
  while (length(frontier)) {
    nxt <- unique(g$edges[g$edges[, "from"] %in% frontier, "to"])
    nxt <- setdiff(nxt, out)
    out <- c(out, nxt)
    frontier <- nxt
  }
  out
}

#' Internal helper: ancestors of a set (including itself)
#' @noRd
.morie_dag_anc <- function(g, vs) {
  out <- vs
  frontier <- vs
  while (length(frontier)) {
    nxt <- unique(g$edges[g$edges[, "to"] %in% frontier, "from"])
    nxt <- setdiff(nxt, out)
    out <- c(out, nxt)
    frontier <- nxt
  }
  out
}

#' Internal helper: d-separation of x and y given z (Bayes-Ball)
#' @noRd
.morie_dag_dsep <- function(g, x, y, z) {
  # reachability with collider logic; returns TRUE when x _||_ y | z
  anc_z <- .morie_dag_anc(g, z)
  # states: (node, direction) direction "up" = arrived from child,
  # "down" = arrived from parent
  visit <- list(c(x, "up"))
  seen <- character(0)
  while (length(visit)) {
    cur <- visit[[1]]; visit <- visit[-1]
    key <- paste(cur, collapse = "|")
    if (key %in% seen) next
    seen <- c(seen, key)
    v <- cur[1]; dir <- cur[2]
    if (v %in% y && !(v %in% z)) return(FALSE)
    if (dir == "up" && !(v %in% z)) {
      for (p in g$edges[g$edges[, "to"] == v, "from"])
        visit <- c(visit, list(c(p, "up")))
      for (c_ in g$edges[g$edges[, "from"] == v, "to"])
        visit <- c(visit, list(c(c_, "down")))
    } else if (dir == "down") {
      if (!(v %in% z)) {
        for (c_ in g$edges[g$edges[, "from"] == v, "to"])
          visit <- c(visit, list(c(c_, "down")))
      }
      if (v %in% anc_z) {
        for (p in g$edges[g$edges[, "to"] == v, "from"])
          visit <- c(visit, list(c(p, "up")))
      }
    }
  }
  TRUE
}

#' Identify the causal effect in a DAG (backdoor criterion)
#'
#' Uses the canonical adjustment set (observed ancestors of exposure
#' or outcome, minus descendants of the exposure) and verifies it with
#' Bayes-Ball d-separation on the graph with outgoing exposure edges
#' removed — if the canonical set fails, no backdoor set exists (van
#' der Zander, Liskiewicz & Textor 2014).
#'
#' @param dag A `morie_dag`.
#' @return List with `identified` (logical), `estimand` ("backdoor" or
#'   NA), `adjustment_set` (character).
#' @examples
#' g <- morie_dag(c("z -> x", "z -> y", "x -> y"), "x", "y")
#' morie_dag_identify(g)
#' @export
morie_dag_identify <- function(dag) {
  x <- dag$exposure; y <- dag$outcome
  cand <- setdiff(
    intersect(dag$nodes,
              .morie_dag_anc(dag, c(x, y))),
    c(.morie_dag_desc(dag, x), y, dag$latent))
  # backdoor graph: drop x's outgoing edges
  gb <- dag
  gb$edges <- dag$edges[dag$edges[, "from"] != x, , drop = FALSE]
  ok <- .morie_dag_dsep(gb, x, y, cand)
  list(identified = ok,
       estimand = if (ok) "backdoor" else NA_character_,
       adjustment_set = if (ok) cand else character(0))
}

#' Estimate the identified effect from data
#'
#' @param dag A `morie_dag`.
#' @param data Data frame containing the observed nodes.
#' @param method `"backdoor.aipw"` (default), `"backdoor.linear"`, or
#'   `"backdoor.dml"` — all native estimators.
#' @return The chosen estimator's result list, plus `adjustment_set`
#'   and `estimand`.
#' @examples
#' set.seed(1)
#' z <- rnorm(400); x <- rbinom(400, 1, plogis(z))
#' y <- 0.8 * x + z + rnorm(400)
#' df <- data.frame(z = z, x = x, y = y)
#' g <- morie_dag(c("z -> x", "z -> y", "x -> y"), "x", "y")
#' morie_dag_estimate(g, df, method = "backdoor.linear")
#' @export
morie_dag_estimate <- function(dag, data,
                               method = c("backdoor.aipw",
                                          "backdoor.linear",
                                          "backdoor.dml")) {
  method <- match.arg(method)
  id <- morie_dag_identify(dag)
  if (!id$identified)
    stop("effect not identified by the backdoor criterion in this DAG",
         call. = FALSE)
  zs <- id$adjustment_set
  x <- dag$exposure; y <- dag$outcome
  res <- if (!length(zs) && method != "backdoor.linear") {
    # unconfounded: difference in means with a linear fallback shape
    f <- stats::lm(stats::as.formula(paste(y, "~", x)), data = data)
    est <- unname(stats::coef(f)[x]); se <- summary(f)$coefficients[x, 2]
    list(ate = est, se = se, ci_lower = est - 1.96 * se,
         ci_upper = est + 1.96 * se, n = nrow(data))
  } else if (method == "backdoor.linear") {
    rhs <- paste(c(x, zs), collapse = " + ")
    f <- stats::lm(stats::as.formula(paste(y, "~", rhs)), data = data)
    est <- unname(stats::coef(f)[x]); se <- summary(f)$coefficients[x, 2]
    list(ate = est, se = se, ci_lower = est - 1.96 * se,
         ci_upper = est + 1.96 * se, n = nrow(data))
  } else if (method == "backdoor.aipw") {
    morie_estimate_aipw(data, treatment = x, outcome = y, covariates = zs)
  } else {
    morie_estimate_double_ml(data, outcome = y, treatment = x,
                             covariates = zs)
  }
  c(res, list(estimand = "backdoor", adjustment_set = zs,
              method = paste0(method, " (rmorie native)")))
}

#' Refute an estimated causal effect
#'
#' DoWhy-style robustness checks, all native: a placebo (permuted)
#' treatment should give an effect near zero; adding a random common
#' cause or re-estimating on subsets should leave the estimate stable.
#'
#' @param dag A `morie_dag`.
#' @param data The data frame used for estimation.
#' @param method One of `"placebo_treatment"`,
#'   `"random_common_cause"`, `"data_subset"`.
#' @param estimator Passed through to [morie_dag_estimate()].
#' @param n_reps Number of refutation replications (default 20).
#' @param seed Random seed.
#' @return List with `original`, `refuted` (mean over reps), `reps`,
#'   `passed` (logical heuristic), `method`.
#' @examples
#' set.seed(1)
#' z <- rnorm(400); x <- rbinom(400, 1, plogis(z))
#' y <- 0.8 * x + z + rnorm(400)
#' df <- data.frame(z = z, x = x, y = y)
#' g <- morie_dag(c("z -> x", "z -> y", "x -> y"), "x", "y")
#' morie_dag_refute(g, df, method = "placebo_treatment",
#'                  estimator = "backdoor.linear", n_reps = 5)
#' @export
morie_dag_refute <- function(dag, data,
                             method = c("placebo_treatment",
                                        "random_common_cause",
                                        "data_subset"),
                             estimator = "backdoor.linear",
                             n_reps = 20L, seed = 42L) {
  method <- match.arg(method)
  orig <- morie_dag_estimate(dag, data, method = estimator)$ate
  set.seed(seed)
  x <- dag$exposure
  reps <- vapply(seq_len(n_reps), function(i) {
    d2 <- data
    if (method == "placebo_treatment") {
      d2[[x]] <- sample(d2[[x]])
    } else if (method == "random_common_cause") {
      d2[["._rcc"]] <- stats::rnorm(nrow(d2))
    } else {
      d2 <- d2[sample.int(nrow(d2), floor(0.8 * nrow(d2))), , drop = FALSE]
    }
    g2 <- dag
    if (method == "random_common_cause") {
      # noise variable enters the adjustment set candidates
      g2$edges <- rbind(dag$edges, c("._rcc", dag$exposure),
                        c("._rcc", dag$outcome))
      g2$nodes <- c(dag$nodes, "._rcc")
    }
    morie_dag_estimate(g2, d2, method = estimator)$ate
  }, numeric(1))
  passed <- if (method == "placebo_treatment") {
    abs(mean(reps)) < max(0.1 * abs(orig), 0.05)
  } else {
    abs(mean(reps) - orig) < max(0.15 * abs(orig), 0.05)
  }
  list(original = orig, refuted = mean(reps), reps = reps,
       passed = passed, method = method)
}

#' Canonical MRM causal DAGs
#'
#' Ready-made `morie_dag` objects for the two structures the MRM
#' (Multilevel Reconciliation Methodology) analyses use most: carceral
#' placement (exposure) with demographic + prior-record confounding,
#' and use-of-force reporting with neighbourhood-level confounding.
#' Structures follow the confounding relations documented in the MRM
#' module docs; use them as starting points and edit edges to taste.
#'
#' @return Named list of `morie_dag` objects.
#' @examples
#' names(morie_mrm_dags())
#' morie_dag_identify(morie_mrm_dags()$placement)
#' @export
morie_mrm_dags <- function() {
  list(
    placement = morie_dag(
      c("race -> placement", "race -> outcome",
        "prior_record -> placement", "prior_record -> outcome",
        "age -> placement", "age -> outcome",
        "placement -> outcome"),
      exposure = "placement", outcome = "outcome"),
    use_of_force = morie_dag(
      c("neighbourhood -> police_contact", "neighbourhood -> force",
        "race -> police_contact", "race -> force",
        "police_contact -> force"),
      exposure = "police_contact", outcome = "force")
  )
}
