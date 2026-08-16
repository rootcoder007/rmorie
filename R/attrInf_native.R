# Model inversion attacks that exploit confidence information.
# Sources: Fredrikson, M., Jha, S., & Ristenpart, T. (2015) "Model
# Inversion Attacks that Exploit Confidence Information and Basic
# Countermeasures", CCS '15, 1322-1333. doi:10.1145/2810103.2813677.
# Figure 2 of the paper is the generic MAP inversion attack; Equation 1
# is the white-box-with-counts improvement.
#
# Native R mirror of morie.fn.attrInf: same maths, same argument
# names, same error conditions; candidates tie-break by their string
# representation, the same rule the Python arm uses, so both arms
# pick the same estimate when several scores tie.

# Private helper (mirrors Python's _leaf: a node is a leaf exactly when
# it is a dict that carries a "label" key).
.leaf <- function(node) {
  is.list(node) && !is.null(node) && "label" %in% names(node)
}

# Match Python's str() for the tie-breaking sort. Python's str(1.0) is
# "1.0" not "1", str(True) is "True" not "TRUE", str(None) is "None";
# reproducing those quirks keeps the sort identical to the Python arm.
.attrInf_py_str <- function(v) {
  if (is.null(v)) return("None")
  if (is.logical(v) && length(v) == 1L)
    return(if (isTRUE(v)) "True" else "False")
  if (is.integer(v)) return(as.character(v))
  if (is.numeric(v)) {
    s <- format(v, scientific = FALSE, trim = TRUE)
    if (!grepl(".", s, fixed = TRUE) && !grepl("[eE]", s) &&
        !grepl("Inf", s, fixed = TRUE) && !grepl("NaN", s, fixed = TRUE))
      s <- paste0(s, ".0")
    return(s)
  }
  as.character(v)
}

.attrInf_sort_keys <- function(vals) {
  if (length(vals) <= 1L) return(vals)
  strs <- vapply(vals, .attrInf_py_str, character(1))
  vals[order(strs)]
}

# Mirror Python's max(sorted(scores, key=str), key=scores.__getitem__):
# among ties, the key whose str() sorts earliest wins.
.attrInf_argmax <- function(scored) {
  if (length(scored) == 0L) return(NULL)
  strs <- vapply(scored, function(s) .attrInf_py_str(s$v), character(1))
  ord <- order(strs)
  sorted <- scored[ord]
  max_score <- max(vapply(sorted, function(s) s$score, numeric(1)))
  for (s in sorted) if (s$score == max_score) return(s$v)
  NULL
}

# Python-style == for the accuracy block: bool <-> int, int <-> float
# all match (R's == coerces); int vs str does not (mirrors Python:
# 1 == "1" is False, and isTRUE(1 == "1") is FALSE in R).
.attrInf_eq <- function(a, b) {
  if (is.null(a) || is.null(b)) return(is.null(a) && is.null(b))
  isTRUE(a == b)
}

#' Attribute inference by model inversion
#'
#' Runs the Fredrikson-Jha-Ristenpart (2015) model inversion attack
#' over a set of targets and scores it. For each target it scores
#' every candidate value of the sensitive feature, multiplies the
#' error-model probability by the marginal priors, and returns the
#' maximum a posteriori estimate. The black-box route uses
#' \code{err(y, y')} from a confusion matrix; the white-box route
#' additionally weights each root-to-leaf path by its training
#' count, which carries joint-distribution information the marginals
#' cannot reach on their own.
#'
#' @param tree Nested tree: each non-leaf is a list with
#'   \code{feature} (integer index) and \code{branches} (a named
#'   list whose names are the branch values mapping to child
#'   subtrees); each leaf is a list with \code{label} and, for the
#'   white-box route, \code{count}.
#' @param targets List of target lists, each with \code{known} (a
#'   named list mapping feature-index-as-string to value),
#'   \code{label}, and, when the truth is known, \code{truth}.
#' @param priors Named list mapping feature-index-as-string to a
#'   named numeric vector of marginal probabilities.
#' @param confusion Square confusion matrix (counts); required when
#'   \code{mode = "blackbox"}.
#' @param labels Optional labels for the confusion-matrix rows;
#'   defaults to \code{0:(nrow-1)}.
#' @param sensitive Integer index of the sensitive feature.
#' @param mode Either \code{"blackbox"} or \code{"whitebox"}.
#' @param candidates Optional list of candidate values for the
#'   sensitive feature; defaults to those appearing on the tree,
#'   falling back to the keys of the sensitive feature's prior.
#' @param unknown Optional integer vector of further feature indices
#'   whose values are unknown; Equation 1 is summed over them.
#' @return A named list mirroring the Python payload:
#'   \code{estimate}, \code{guesses}, \code{accuracy},
#'   \code{precision}, \code{recall}, \code{false_positives},
#'   \code{true_positives}, \code{mode}, \code{candidates},
#'   \code{n_paths}, \code{n_targets}, \code{method}, \code{note}.
#' @references Fredrikson, M., Jha, S., & Ristenpart, T. (2015).
#'   Model inversion attacks that exploit confidence information and
#'   basic countermeasures. In CCS '15 (pp. 1322-1333).
#'   doi:10.1145/2810103.2813677.
#' @export
morie_attrInf <- function(tree, targets, priors,
                          confusion = NULL, labels = NULL,
                          sensitive = 0L, mode = "blackbox",
                          candidates = NULL, unknown = NULL) {
  if (!(mode %in% c("blackbox", "whitebox")))
    stop("attrInf: mode must be one of ('blackbox', 'whitebox')")

  paths <- tree_paths(tree)
  snm <- as.character(sensitive)

  if (is.null(candidates)) {
    cs <- list()
    for (p in paths)
      if (snm %in% names(p$constraints))
        cs[[length(cs) + 1L]] <- p$constraints[[snm]]
    cand <- .attrInf_sort_keys(unique(cs))
    if (length(cand) == 0L) {
      prior_s <- priors[[snm]]
      if (!is.null(prior_s))
        cand <- .attrInf_sort_keys(as.list(names(prior_s)))
    }
  } else {
    cand <- as.list(candidates)
  }

  if (length(cand) == 0L)
    stop("attrInf: no candidate values for the sensitive feature")

  err <- NULL
  if (mode == "blackbox") {
    if (is.null(confusion))
      stop("attrInf: the black-box attack needs a confusion matrix")
    err <- confusion_error(confusion, labels)
  }

  guesses <- list()
  correct <- 0L
  n_truth <- 0L
  tp <- 0L
  fp <- 0L
  fn <- 0L
  positive <- cand[[length(cand)]]
  sens <- as.integer(sensitive)

  for (t in targets) {
    known <- t$known
    if (is.null(known)) known <- list()
    y <- t$label

    if (mode == "blackbox") {
      model <- function(x) .attrInf_tree_predict(tree, x)
      got <- map_invert(model, y, known, cand, err, priors, sens)
    } else {
      got <- wbwc_invert(tree, known, cand, priors, sens, unknown)
    }
    guesses[[length(guesses) + 1L]] <- got$estimate

    if ("truth" %in% names(t)) {
      n_truth <- n_truth + 1L
      truth <- t$truth
      if (.attrInf_eq(got$estimate, truth)) correct <- correct + 1L
      est_pos <- .attrInf_eq(got$estimate, positive)
      tru_pos <- .attrInf_eq(truth, positive)
      if (est_pos && tru_pos) tp <- tp + 1L
      else if (est_pos) fp <- fp + 1L
      else if (tru_pos) fn <- fn + 1L
    }
  }

  acc <- if (n_truth > 0L) correct / as.numeric(n_truth) else NULL
  prec <- if ((tp + fp) > 0L) as.numeric(tp) / as.numeric(tp + fp) else NULL
  rec <- if ((tp + fn) > 0L) as.numeric(tp) / as.numeric(tp + fn) else NULL

  list(
    estimate = guesses,
    guesses = guesses,
    accuracy = acc,
    precision = prec,
    recall = rec,
    false_positives = fp,
    true_positives = tp,
    mode = mode,
    candidates = cand,
    n_paths = length(paths),
    n_targets = length(targets),
    method = paste0(
      "model inversion (Fredrikson, Jha & Ristenpart 2015): ",
      if (mode == "blackbox") "generic Figure 2"
        else "white-box-with-counts, Equation 1",
      " MAP estimate of the sensitive feature"),
    note = paste0(
      "the black-box route uses err(y, y') from the confusion ",
      "matrix and the paper reports it has a prohibitively high ",
      "false positive rate; the white-box route adds the per-path ",
      "training counts, which carry joint-distribution information ",
      "the marginals cannot")
  )
}

.attrInf_tree_predict <- function(tree, x) {
  node <- tree
  while (!.leaf(node)) {
    if (!is.list(node) || is.null(node) ||
        !("feature" %in% names(node)) ||
        !("branches" %in% names(node)))
      stop(paste0("attrInf: a node needs 'feature' and 'branches', ",
                  "or 'label' for a leaf"))
    i <- node[["feature"]]
    br <- node[["branches"]]
    if (!is.list(br) || is.null(br))
      stop(paste0("attrInf: a node needs 'feature' and 'branches', ",
                  "or 'label' for a leaf"))
    nm <- as.character(i)
    if (!nm %in% names(x))
      stop(paste0("attrInf: no value supplied for feature ", nm,
                  ", which the tree needs"))
    v <- x[[nm]]
    if (is.null(v))
      stop(paste0("attrInf: no value supplied for feature ", nm,
                  ", which the tree needs"))
    bnm <- as.character(v)
    if (!bnm %in% names(br))
      stop(paste0("attrInf: no branch for value ", bnm, " of feature ",
                  nm))
    node <- br[[bnm]]
  }
  node[["label"]]
}

#' tree_paths
#'
#' Part of the attrInf_native implementation; see the file header for
#' the source it follows.
#'
#' @param tree See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
tree_paths <- function(tree) {
  out <- list()
  walk <- function(node, cons) {
    if (!is.list(node) || is.null(node))
      stop(paste0("attrInf: a node needs 'feature' and 'branches', ",
                  "or 'label' for a leaf"))
    if (.leaf(node)) {
      cnt <- node[["count"]]
      if (is.null(cnt)) cnt <- 0
      out[[length(out) + 1L]] <<- list(
        constraints = as.list(cons),
        label = node[["label"]],
        count = as.numeric(cnt))
      return(invisible(NULL))
    }
    if (!("feature" %in% names(node)) ||
        !("branches" %in% names(node)))
      stop(paste0("attrInf: a node needs 'feature' and 'branches', ",
                  "or 'label' for a leaf"))
    i <- node[["feature"]]
    br <- node[["branches"]]
    for (nm in names(br)) {
      new_cons <- as.list(cons)
      new_cons[[as.character(i)]] <- nm
      walk(br[[nm]], new_cons)
    }
  }
  walk(tree, list())
  if (length(out) == 0L) stop("attrInf: the tree has no paths")
  out
}

#' confusion_error
#'
#' Part of the attrInf_native implementation; see the file header for
#' the source it follows.
#'
#' @param C See Usage.
#' @param labels Defaults to \code{NULL}.
#' @return The value of \code{err}, as built in the body.
#' @export
confusion_error <- function(C, labels = NULL) {
  if (is.matrix(C)) {
    if (nrow(C) == 0L || ncol(C) == 0L)
      stop("attrInf: the confusion matrix must be square")
    rows <- lapply(seq_len(nrow(C)), function(i) as.numeric(C[i, ]))
  } else {
    rows <- lapply(C, function(r) as.numeric(r))
  }
  nr <- length(rows)
  if (nr == 0L ||
      any(vapply(rows, function(r) length(r) != nr, logical(1))))
    stop("attrInf: the confusion matrix must be square")
  if (any(vapply(rows, function(r) any(r < 0), logical(1))))
    stop("attrInf: confusion counts cannot be negative")

  if (is.null(labels)) labels <- as.list(seq_len(nr) - 1L)
  else labels <- as.list(labels)
  if (length(labels) != nr)
    stop("attrInf: one label per confusion-matrix row")

  label_keys <- vapply(labels, .attrInf_py_str, character(1))
  norm <- matrix(0, nrow = nr, ncol = nr)
  for (i in seq_len(nr)) {
    tot <- sum(rows[[i]])
    for (j in seq_len(nr))
      norm[i, j] <- if (tot > 0) rows[[i]][j] / tot else 0
  }

  err <- function(y, yp) {
    yk <- .attrInf_py_str(y)
    ypk <- .attrInf_py_str(yp)
    iy <- match(yk, label_keys)
    iyp <- match(ypk, label_keys)
    if (is.na(iy) || is.na(iyp))
      stop("attrInf: unknown label in the error model")
    norm[iy, iyp]
  }
  err
}

#' map_invert
#'
#' Part of the attrInf_native implementation; see the file header for
#' the source it follows.
#'
#' @param model See Usage.
#' @param y See Usage.
#' @param known See Usage.
#' @param candidates See Usage.
#' @param err See Usage.
#' @param priors See Usage.
#' @param sensitive Defaults to \code{0L}.
#' @return A list with \code{estimate}, \code{scores}.
#' @export
map_invert <- function(model, y, known, candidates, err, priors,
                       sensitive = 0L) {
  if (length(candidates) == 0L)
    stop("attrInf: no candidate values to score")
  scores <- list()
  for (v in candidates) {
    x <- known
    x[[as.character(sensitive)]] <- v
    e <- err(y, model(x))
    prod <- 1.0
    for (nm in names(x)) {
      val <- x[[nm]]
      p <- priors[[nm]]
      if (!is.null(p)) {
        pr <- p[[as.character(val)]]
        prod <- prod * (if (is.null(pr)) 0 else pr)
      }
    }
    scores[[length(scores) + 1L]] <- list(v = v, score = e * prod)
  }
  best <- .attrInf_argmax(scores)
  list(estimate = best, scores = scores)
}

#' wbwc_invert
#'
#' Part of the attrInf_native implementation; see the file header for
#' the source it follows.
#'
#' @param tree See Usage.
#' @param known See Usage.
#' @param candidates See Usage.
#' @param priors See Usage.
#' @param sensitive Defaults to \code{0L}.
#' @param unknown Defaults to \code{NULL}.
#' @return A list with \code{estimate}, \code{scores}, \code{n_paths}, \code{N}.
#' @export
wbwc_invert <- function(tree, known, candidates, priors,
                        sensitive = 0L, unknown = NULL) {
  if (length(candidates) == 0L)
    stop("attrInf: no candidate values to score")
  paths <- tree_paths(tree)
  N <- sum(vapply(paths, function(p) p$count, numeric(1)))
  if (N <= 0)
    stop(paste0("attrInf: white-box inversion needs path counts; ",
                "give each leaf a 'count'"))

  if (is.null(unknown)) unknown <- integer(0)
  unknown_str <- as.character(as.integer(unknown))
  sens_str <- as.character(sensitive)

  scores <- list()
  for (v in candidates) {
    assign <- known
    assign[[sens_str]] <- v
    assign_names <- names(assign)
    active <- 0
    total <- 0
    for (p in paths) {
      pi <- p$count / N
      ok <- TRUE
      for (nm in names(p$constraints)) {
        if (nm %in% unknown_str) next
        if (nm %in% assign_names &&
            !identical(assign[[nm]], p$constraints[[nm]])) {
          ok <- FALSE
          break
        }
      }
      if (ok) active <- active + pi
      total <- total + pi
    }
    prior <- priors[[sens_str]]
    prior_v <- if (is.null(prior)) 0 else {
      pr <- prior[[as.character(v)]]
      if (is.null(pr)) 0 else pr
    }
    denom <- if (active > 0) active else 1
    score <- if (active > 0) (active / denom) * active * prior_v else 0
    scores[[length(scores) + 1L]] <- list(v = v, score = score)
  }

  best <- .attrInf_argmax(scores)
  list(estimate = best, scores = scores,
       n_paths = length(paths), N = N)
}

# Alias mirroring attribute_inference = attrInf in the Python arm.
attribute_inference <- morie_attrInf
