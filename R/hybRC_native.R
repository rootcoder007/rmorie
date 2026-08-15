# morie.fn -- function file (rootcoder007/morie)
# r"""Hybrid recommenders: seven ways to combine, and they differ.
#
# Collaborative filtering cannot recommend an item nobody has rated --
# the **new-item** or cold-start problem -- and content-based filtering
# cannot surprise anyone, since it only returns more of what the user
# already liked. Each is strong where the other is weak, so combining
# them is obvious; *how* to combine them is not, and Burke's point is
# that the choices are genuinely different systems, not variations on
# one.
#
# **The seven methods**, each implemented here:
#
# * **Weighted** -- scores from several recommenders are combined
#   numerically. Simple, and it assumes the relative value of the
#   components is roughly uniform across the item space, which is
#   exactly what fails when one component is blind to new items.
# * **Switching** -- pick one recommender per case by some criterion.
#   Buys sensitivity to each component's strengths at the cost of a new
#   layer of parameterisation: the criterion itself.
# * **Mixed** -- present results from several side by side, no fusion.
# * **Feature combination** -- treat collaborative data as extra
#   *features* inside a single content-based algorithm.
# * **Cascade** -- one recommender ranks, the next breaks ties only.
#   Strictly ordered, so it is not commutative.
# * **Feature augmentation** -- one produces a feature that becomes
#   input to the next.
# * **Meta-level** -- one produces a whole *model* that is the next
#   one's input, which is a stronger coupling than a feature.
#
# **Order matters for some and not others**, and the module says which:
# weighted, mixed, switching and feature-combination are
# order-insensitive, so a CN/CF system is the same as CF/CN; cascade,
# feature augmentation and meta-level are not. ``is_order_sensitive``
# makes that checkable, and the anchor verifies cascade actually changes
# under a swap while weighted does not.
#
# References
# ----------
# Burke, R. (2002) "Hybrid Recommender Systems: Survey and
# Experiments", *User Modeling and User-Adapted Interaction* 12(4),
# 331-370, doi:10.1023/A:1021240730564. [PDF supplied by Vee.] The
# taxonomy of seven hybridisation methods -- weighted, switching, mixed,
# feature combination, cascade, feature augmentation and meta-level --
# with the weighted hybrid's implicit assumption that the relative value
# of the techniques is more or less uniform across the space of possible
# items; the note that switching hybrids introduce additional complexity
# because the switching criteria must be determined, adding another
# level of parameterisation, in exchange for sensitivity to the
# components' strengths and weaknesses; and the observation that four
# techniques -- weighted, mixed, switching and feature combination --
# are order-insensitive, so a CN/CF mixed system is no different from a
# CF/CN one.
#
# Balabanovic, M. & Shoham, Y. (1997) "Fab: content-based,
# collaborative recommendation", *Communications of the ACM* 40(3),
# 66-72, doi:10.1145/245108.245124. An early hybrid.
#
# Resnick, P. et al. (1994) "GroupLens", *CSCW '94*, 175-186,
# doi:10.1145/192844.192905. The collaborative half; implemented in
# :mod:`ucfR`.
# """

.EPS <- 1e-12

.METHODS <- c("weighted", "switching", "mixed", "feature_combination",
              "cascade", "feature_augmentation", "meta_level")

.ORDER_INSENSITIVE <- c("weighted", "switching", "mixed",
                        "feature_combination")

# Private helpers (prefixed to avoid R/ environment collisions)
.hybRC_vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  as.numeric(unlist(x))
}

.hybRC_mat <- function(x) {
  if (is.null(x)) return(matrix(numeric(0), nrow = 0, ncol = 0))
  if (is.list(x) && !is.data.frame(x)) {
    rows <- lapply(x, function(r) as.numeric(unlist(r)))
    out <- do.call(rbind, rows)
    if (is.null(out)) out <- matrix(numeric(0), nrow = 0, ncol = 0)
    out
  } else {
    as.matrix(x)
  }
}

is_order_sensitive <- function(method) {
  m <- as.character(method)
  if (!(m %in% .METHODS)) {
    stop(sprintf("hybRC: method must be one of %s, got %s",
                 paste(.METHODS, collapse = ", "),
                 as.character(method)))
  }
  list(
    method = m,
    order_sensitive = !(m %in% .ORDER_INSENSITIVE),
    note = "weighted, mixed, switching and feature combination are order-INsensitive; the other three are pipelines"
  )
}

weighted <- function(scores, weights = NULL) {
  S <- lapply(scores, function(s) as.list(s))
  if (length(S) == 0L) {
    stop("hybRC: no component scores given")
  }
  if (is.null(weights)) {
    w <- rep(1.0 / length(S), length(S))
  } else {
    w <- .hybRC_vec(weights)
  }
  if (length(w) != length(S)) {
    stop(sprintf("hybRC: %d weight(s) for %d components",
                 length(w), length(S)))
  }

  items <- sort(unique(unlist(lapply(S, names))))

  out <- vector("list", length(items))
  names(out) <- items
  partial <- logical(length(items))
  names(partial) <- items

  for (i in seq_along(items)) {
    it <- items[i]
    present_idx <- which(vapply(S, function(s) it %in% names(s),
                                logical(1)))
    vals <- vapply(present_idx, function(c) as.numeric(S[[c]][[it]]),
                   numeric(1))
    out[[i]] <- sum(w[present_idx] * vals)
    partial[i] <- length(present_idx) < length(S)
  }

  scores_vec <- as.numeric(out[items])
  ranking <- items[order(-scores_vec)]

  list(
    scores = out,
    ranking = as.list(ranking),
    partially_scored = as.list(items[partial]),
    note = "an item missing from a component is scored by the rest, which silently favours whoever HAS it"
  )
}

switching <- function(scores, criterion, context = NULL) {
  S <- lapply(scores, function(s) as.list(s))
  c_idx <- as.integer(criterion(context))
  if (c_idx < 0L || c_idx >= length(S)) {
    stop(sprintf("hybRC: the switching criterion chose component %d of %d",
                 c_idx, length(S)))
  }

  chosen_scores <- S[[c_idx + 1L]]  # R is 1-based
  items <- names(chosen_scores)
  scores_vec <- as.numeric(chosen_scores)
  ranking <- items[order(-scores_vec)]

  list(
    scores = chosen_scores,
    chosen = c_idx,
    ranking = as.list(ranking),
    note = "sensitive to each component's strengths, at the cost of another level of parameterisation"
  )
}

mixed <- function(recommendations, top_k = NULL) {
  L <- lapply(recommendations, function(r) as.list(r))
  if (length(L) == 0L) {
    stop("hybRC: no recommendation lists given")
  }

  out <- list()
  lens <- sapply(L, length)
  max_len <- max(lens)

  for (t in seq_len(max_len)) {
    for (src in seq_along(L)) {
      if (t <= lens[src]) {
        out[[length(out) + 1L]] <- list(item = L[[src]][[t]],
                                        source = src - 1L)
      }
    }
  }

  if (!is.null(top_k)) {
    k <- as.integer(top_k)
    if (k < length(out)) {
      out <- out[seq_len(k)]
    }
  }

  list(
    presented = out,
    n_sources = length(L),
    note = "no score is combined, so no comparability between components is assumed"
  )
}

feature_combination <- function(content_features, collaborative_features) {
  C <- .hybRC_mat(content_features)
  D <- .hybRC_mat(collaborative_features)
  if (nrow(C) != nrow(D)) {
    stop(sprintf("hybRC: %d content rows but %d collaborative rows",
                 nrow(C), nrow(D)))
  }

  if (nrow(C) == 0L) {
    features <- list()
    cd <- 0L
    dd <- 0L
  } else {
    features <- lapply(seq_len(nrow(C)),
                       function(i) c(C[i, ], D[i, ]))
    cd <- ncol(C)
    dd <- ncol(D)
  }

  list(
    features = features,
    content_dim = cd,
    collaborative_dim = dd,
    note = "one algorithm, wider input -- not two systems"
  )
}

cascade <- function(primary, secondary, tol = 1e-9) {
  P <- as.list(primary)
  S <- as.list(secondary)
  items <- sort(names(P))

  P_vals <- as.numeric(P[items])
  S_vals <- vapply(items, function(i) {
    v <- S[[i]]
    if (is.null(v)) 0 else as.numeric(v)
  }, numeric(1))

  ord <- items[order(-P_vals, -S_vals)]

  tie_key <- round(P_vals / max(tol, 1e-12))
  ties <- split(items, tie_key)
  broken <- sum(sapply(ties, length) > 1L)

  P_ordered <- as.numeric(P[ord])
  primary_respected <- all(diff(P_ordered) <= tol)

  list(
    ranking = as.list(ord),
    tie_groups_broken = broken,
    primary_respected = primary_respected,
    note = "the secondary NEVER overturns a strict preference of the primary"
  )
}

feature_augmentation <- function(base_output, consumer) {
  list(
    result = consumer(base_output),
    note = "a feature, not a model -- the consumer keeps its own learning algorithm"
  )
}

meta_level <- function(model_builder, consumer, data) {
  model <- model_builder(data)
  estimate <- consumer(model)
  list(
    estimate = estimate,
    result = estimate,
    model = model,
    method = "meta-level hybrid; Burke (2002)",
    note = "the consumer depends on the producer's internal representation, so the two cannot be swapped"
  )
}

cheatsheet <- function() {
  paste("hybRC: collaborative filtering cannot recommend what",
        "nobody rated; content-based filtering cannot surprise",
        "anyone. Combining is obvious, HOW is not -- and the seven",
        "ways are different systems. WEIGHTED (assumes components",
        "are comparably good everywhere, which is what fails on",
        "new items), SWITCHING (buys sensitivity, costs a new",
        "criterion to parameterise), MIXED (side by side, no",
        "fusion), FEATURE COMBINATION (collaborative data as extra",
        "features in ONE model), CASCADE (second breaks TIES",
        "only), FEATURE AUGMENTATION (output feeds the next),",
        "META-LEVEL (whole MODEL feeds the next). The first four",
        "are order-insensitive; the last three are pipelines.",
        sep = " ")
}

# Compact aliases per ledger/NAMING.md
hybrid_recommender <- weighted
hybrid_rec <- weighted
hybridrec <- weighted

# Entry point
morie_hybRC <- weighted

#' @rdname weighted
#' @export
morie_hybRC <- weighted

#' @rdname weighted
#' @export
morie_hybRC <- weighted

#' @rdname weighted
#' @export
morie_hybRC <- weighted

#' @rdname weighted
#' @export
morie_hybRC <- weighted

#' @rdname weighted
#' @export
morie_hybRC <- weighted

#' @rdname weighted
#' @export
morie_hybRC <- weighted

#' @rdname weighted
#' @export
morie_hybRC <- weighted

#' @rdname weighted
#' @export
morie_hybRC <- weighted

#' @rdname weighted
#' @export
morie_hybRC <- weighted
