# morie.fn -- clusmd native R arm
#
# Butina clustering: exclusion spheres over a similarity threshold.
#
# **The method.** Count, for every compound, how many others lie within
# a Tanimoto threshold of it. Take the compound with the most such
# neighbours as a cluster centre; it and all of its neighbours become a
# cluster and are removed from the pool. Repeat. Whatever is left with
# no neighbours becomes a singleton.
#
# The result is not a partition into "natural" groups -- it is a set of
# **exclusion spheres**. Every member of a cluster is within the
# threshold *of the centroid*, but two members need not be within the
# threshold of each other, and the centroids are guaranteed to be
# mutually dissimilar. That is a much weaker and much more honest claim
# than hierarchical clustering makes, and it is the reason the method
# is O(N^2) rather than O(N^3) and runs on databases.
#
# **One decision the paper leaves open.** After removing a cluster, the
# neighbour counts of the survivors are stale -- some of their
# neighbours are gone. ``recount=FALSE`` keeps the original counts
# (the exclusion-sphere reading, and what makes the method a single
# pass); ``recount=TRUE`` recomputes them each round, which costs more
# and can give different, usually more balanced, clusters. Both are
# provided because the choice changes the answer and should be visible.
#
# **The threshold is the model.** There is no objective function being
# optimised here and no number of clusters to choose: the threshold
# alone determines the outcome. At 1.0 only identical fingerprints
# cluster; at 0.0 everything is one cluster. Reporting the threshold
# alongside the clusters is not decoration.
#
# References
# ----------
# Butina, D. (1999) "Unsupervised data base clustering based on
# Daylight's fingerprint and Tanimoto similarity: a fast and automated
# way to cluster small and large data sets", *Journal of Chemical
# Information and Computer Sciences* 39(4), 747-750,
# doi:10.1021/ci9803381. The neighbour-count ordering, the exclusion
# sphere around each selected centroid, the singleton handling, and the
# 0.8 Tanimoto working threshold reproduced as the default.
#
# Willett, P., Barnard, J. M. & Downs, G. M. (1998) "Chemical
# similarity searching", *Journal of Chemical Information and Computer
# Sciences* 38(6), 983-996, doi:10.1021/ci9800211, for the Tanimoto
# coefficient itself.
#
# Native R port of morie.fn.clusmd. No external packages; the Tanimoto
# coefficient on binary fingerprints is computed inline (c / (a + b - c))
# so this arm is independent of sasimi_native.R.

# ---------------------------------------------------------------------------
# Fingerprint normalisation -- mirror sasimi.fingerprint on the R side.
# ---------------------------------------------------------------------------
# Accepts an integer set, a numeric vector of indices, or a 0/1 sequence.
# Returns an integer vector of on-bit indices (sorted, deduped).
.clusmd_fp <- function(bits, n_bits = NULL) {
  if (is.data.frame(bits))
    bits <- as.matrix(bits)
  if (is.matrix(bits)) {
    if (ncol(bits) == 1L) {
      bits <- as.integer(bits[, 1L])
    } else {
      # 0/1 matrix treated as a sequence
      idx <- which(bits != 0)
      if (any(bits[idx] != 1L))
        stop("clusmd: matrix fingerprint must contain only 0/1 values")
      bits <- idx - 1L
    }
  } else if (is.list(bits)) {
    bits <- unlist(bits, use.names = FALSE)
  }
  if (is.logical(bits)) {
    # 0/1-like logical sequence
    idx <- which(bits)
    bits <- idx - 1L
  } else {
    bits <- as.numeric(bits)
    if (length(bits) > 0L &&
        all(bits %in% c(0, 1)) &&
        (is.null(n_bits) || length(bits) == as.integer(n_bits))) {
      idx <- which(bits == 1)
      bits <- idx - 1L
    } else {
      # treat as a list of on-bit indices
      bits <- as.integer(bits)
    }
  }
  if (any(bits < 0L, na.rm = TRUE))
    stop("clusmd: a bit index cannot be negative")
  if (!is.null(n_bits) && length(bits) > 0L &&
      max(bits) >= as.integer(n_bits))
    stop(sprintf("clusmd: bit index outside the %d-bit fingerprint",
                 as.integer(n_bits)))
  sort(unique(bits))
}


# Tanimoto similarity of two fingerprint vectors (sorted int vectors).
# c / (a + b - c), guarding the all-zero case like sasimi._guard.
.clusmd_tanimoto <- function(a, b) {
  A <- .clusmd_fp(a)
  B <- .clusmd_fp(b)
  if (length(A) == 0L && length(B) == 0L)
    stop("clusmd: both fingerprints are empty; Tanimoto is undefined")
  c <- length(intersect(A, B))
  den <- length(A) + length(B) - c
  if (den <= 0L) return(0.0)
  c / den
}


# ---------------------------------------------------------------------------
# Neighbour lists: for each compound, the set of others within threshold.
# ---------------------------------------------------------------------------
# Returns a list of integer vectors (sorted ascending); index i holds the
# indices j != i with Tanimoto(fp_i, fp_j) >= threshold. Mirrors the
# Python neighbour_lists() function.
.clusmd_neighbour_lists <- function(fps, threshold = 0.8) {
  th <- as.numeric(threshold)
  if (is.na(th) || th < 0.0 || th > 1.0)
    stop("clusmd: the threshold must lie in [0, 1], got ", th)
  if (length(fps) < 1L)
    stop("clusmd: no compounds given")
  F <- lapply(fps, .clusmd_fp)
  n <- length(F)
  nb <- vector("list", n)
  for (i in seq_len(n)) nb[[i]] <- integer(0)
  for (i in seq_len(n - 1L)) {
    Ai <- F[[i]]
    for (j in (i + 1L):n) {
      Bj <- F[[j]]
      a <- length(Ai); b <- length(Bj)
      if (a == 0L && b == 0L) {
        # Both empty: Tanimoto undefined; the Python arm raises. Same here.
        stop("clusmd: both fingerprints are empty; Tanimoto is undefined")
      }
      c <- length(intersect(Ai, Bj))
      den <- a + b - c
      t <- if (den <= 0L) 0.0 else c / den
      if (t >= th) {
        nb[[i]] <- c(nb[[i]], j)
        nb[[j]] <- c(nb[[j]], i)
      }
    }
  }
  for (i in seq_len(n)) nb[[i]] <- sort(unique(nb[[i]]))
  nb
}


# ---------------------------------------------------------------------------
# Butina clusters: the exclusion-sphere clusters, largest sphere first.
# ---------------------------------------------------------------------------
# Mirrors Python butina_clusters(): repeatedly pick the live compound with
# the largest (live-respecting) neighbour count as a centroid, then take
# the centroid together with its live neighbours as the next cluster and
# remove them. Ties broken by ascending index for determinism.
# Final list is sorted by (-size, centroid).
.clusmd_butina_clusters <- function(fps, threshold = 0.8, recount = FALSE) {
  nb <- .clusmd_neighbour_lists(fps, threshold)
  n <- length(nb)
  live <- seq_len(n)
  clusters <- list()
  while (length(live) > 0L) {
    if (recount) {
      counts <- vapply(live, function(i) {
        length(intersect(nb[[i]], live))
      }, integer(1))
    } else {
      counts <- vapply(live, function(i) length(nb[[i]]), integer(1))
    }
    # Pick the live compound with the largest neighbour count, breaking
    # ties by ascending index. This is a faithful port of Python's
    # min(live, key=lambda i: (-counts[i], i)).
    best <- live[1L]
    best_neg <- -counts[1L]
    for (k in seq_along(live)[-1L]) {
      i <- live[k]
      cand_neg <- -counts[k]
      if (cand_neg < best_neg ||
          (cand_neg == best_neg && i < best)) {
        best <- i; best_neg <- cand_neg
      }
    }
    centre <- best
    members <- sort(unique(c(centre, intersect(nb[[centre]], live))))
    clusters[[length(clusters) + 1L]] <- list(
      centroid = centre, members = members, size = length(members))
    live <- setdiff(live, members)
  }
  # Final sort: by descending size, then ascending centroid.
  if (length(clusters) > 1L) {
    sizes <- vapply(clusters, function(c) c$size, integer(1))
    cents <- vapply(clusters, function(c) c$centroid, integer(1))
    ord <- order(-sizes, cents)
    clusters <- clusters[ord]
  }
  clusters
}


# ---------------------------------------------------------------------------
# Cluster summary: sizes, singleton count, and the assignment.
# ---------------------------------------------------------------------------
# Mirrors Python cluster_summary(). `assignment[m]` = k means compound m
# is in cluster k (0-indexed).
.clusmd_cluster_summary <- function(clusters) {
  n <- sum(vapply(clusters, function(c) c$size, integer(1)))
  assign <- vector("integer", n)
  for (k in seq_along(clusters)) {
    for (m in clusters[[k]]$members) assign[m] <- as.integer(k)
  }
  list(
    n_clusters   = length(clusters),
    n_compounds  = n,
    sizes        = vapply(clusters, function(c) c$size, integer(1)),
    n_singletons = sum(vapply(clusters, function(c) c$size == 1L,
                              logical(1))),
    assignment   = as.integer(assign),
    centroids    = vapply(clusters, function(c) c$centroid, integer(1))
  )
}


# ---------------------------------------------------------------------------
# Public entry point: cluster fingerprints by exclusion sphere.
# ---------------------------------------------------------------------------
# Returns a rich-result-shaped named list whose `$payload` carries the
# fields the Python arm puts in its RichResult payload, mirroring the
# key set exactly. The list itself (and `$payload`) is also a plain
# named list with the same names, so callers can use `result$size` or
# `result$payload$size` interchangeably, just like the Python arm.
.clusmd_rich <- function(title, summary_lines = list(), tables = list(),
                         interpretation = "", warnings = character(),
                         payload = list()) {
  out <- list(title = title, summary_lines = summary_lines, tables = tables,
              interpretation = interpretation,
              warnings = as.character(warnings), payload = payload)
  # Mirror the payload keys onto the top-level list (Python
  # RichResult.__getattr__ proxies payload access; we do the same by
  # pulling the payload keys up onto `out` as well, so
  # `result$clusters` and `result$payload$clusters` both work).
  if (length(payload) > 0L) {
    for (k in names(payload)) out[[k]] <- payload[[k]]
  }
  class(out) <- c("morie_clusmd_result", "morie_rich_result", "list")
  out
}


#' Neighbour lists within a Tanimoto threshold
#'
#' For each compound, return the integer vector of indices of the
#' other compounds whose Tanimoto similarity to it is at least
#' \code{threshold}. Mirrors \code{morie.fn.clusmd.neighbour_lists}.
#'
#' @param fps A list of fingerprints. Each fingerprint is either an
#'   integer vector of on-bit indices or a 0/1 sequence.
#' @param threshold Tanimoto threshold in \code{[0, 1]}; default 0.8
#'   (Butina's working value).
#' @return A list of length \code{length(fps)} of integer vectors.
#' @references Butina, D. (1999) J. Chem. Inf. Comput. Sci. 39(4),
#'   747-750. doi:10.1021/ci9803381.
#' @export
morie_clusmd_neighbour_lists <- function(fps, threshold = 0.8) {
  .clusmd_neighbour_lists(fps, threshold)
}


#' Butina exclusion-sphere clusters
#'
#' Apply the Butina (1999) exclusion-sphere clustering algorithm to a
#' set of fingerprints. At each round the live compound with the
#' largest neighbour count (live-restricted if \code{recount=TRUE})
#' is taken as a centroid; the centroid together with its live
#' neighbours form a cluster and are removed. Ties are broken by
#' ascending index. Clusters are returned sorted by descending size
#' then ascending centroid.
#'
#' @param fps A list of fingerprints.
#' @param threshold Tanimoto threshold in \code{[0, 1]}; default 0.8.
#' @param recount If \code{TRUE}, recompute neighbour counts each
#'   round against the currently live compounds (more expensive, can
#'   give more balanced clusters). Default \code{FALSE} preserves
#'   the original counts.
#' @return A list of clusters, each a named list with
#'   \code{centroid}, \code{members}, and \code{size}.
#' @references Butina, D. (1999) J. Chem. Inf. Comput. Sci. 39(4),
#'   747-750. doi:10.1021/ci9803381.
#' @export
morie_clusmd_butina_clusters <- function(fps, threshold = 0.8,
                                         recount = FALSE) {
  .clusmd_butina_clusters(fps, threshold, recount)
}


#' Summary of a Butina clustering
#'
#' Reduce a cluster list (as returned by
#' \code{morie_clusmd_butina_clusters}) to the headline numbers and a
#' flat \code{assignment} vector of length \code{n_compounds} giving
#' each compound's 0-indexed cluster index.
#'
#' @param clusters Output of \code{morie_clusmd_butina_clusters}.
#' @return A named list with \code{n_clusters}, \code{n_compounds},
#'   \code{sizes}, \code{n_singletons}, \code{assignment},
#'   \code{centroids}.
#' @export
morie_clusmd_cluster_summary <- function(clusters) {
  .clusmd_cluster_summary(clusters)
}


#' Butina clustering of fingerprints by exclusion sphere
#'
#' Entry point: cluster a list of fingerprints by the Butina (1999)
#' exclusion-sphere method and return a rich-result-shaped list whose
#' keys match the Python \code{RichResult} payload:
#' \code{estimate}, \code{clusters}, \code{threshold}, \code{recount},
#' \code{n_clusters}, \code{sizes}, \code{n_singletons},
#' \code{assignment}, \code{centroids}, \code{method}.
#'
#' @param fps A list of fingerprints.
#' @param threshold Tanimoto threshold in \code{[0, 1]}; default 0.8.
#' @param recount If \code{TRUE}, recompute neighbour counts each round.
#' @return A rich-result list of class \code{morie_clusmd_result}.
#' @references Butina, D. (1999) J. Chem. Inf. Comput. Sci. 39(4),
#'   747-750. doi:10.1021/ci9803381.
#' @export
morie_clusmd_butina_clustering <- function(fps, threshold = 0.8,
                                           recount = FALSE) {
  cl <- .clusmd_butina_clusters(fps, threshold, recount)
  s  <- .clusmd_cluster_summary(cl)
  th <- as.numeric(threshold)
  payload <- list(
    estimate      = cl,
    clusters      = cl,
    threshold     = th,
    recount       = as.logical(recount),
    n_clusters    = s$n_clusters,
    sizes         = s$sizes,
    n_singletons  = s$n_singletons,
    assignment    = s$assignment,
    centroids     = s$centroids,
    method        = sprintf("Butina (1999) exclusion-sphere clustering at Tanimoto >= %g",
                            th)
  )
  summary_lines <- list(
    list("threshold",    th),
    list("recount",      as.logical(recount)),
    list("n_clusters",   s$n_clusters),
    list("n_compounds",  s$n_compounds),
    list("n_singletons", s$n_singletons)
  )
  .clusmd_rich(
    title           = "Butina (1999) exclusion-sphere clustering",
    summary_lines   = summary_lines,
    interpretation  = sprintf("Threshold = %g; %d clusters covering %d compounds, %d singletons.",
                              th, s$n_clusters, s$n_compounds, s$n_singletons),
    payload         = payload
  )
}

#' @rdname morie_clusmd_neighbour_lists
#' @export
morie_clusmd <- morie_clusmd_neighbour_lists

#' @rdname morie_clusmd_neighbour_lists
#' @export
morie_clusmd <- morie_clusmd_neighbour_lists

#' @rdname morie_clusmd_neighbour_lists
#' @export
morie_clusmd <- morie_clusmd_neighbour_lists

#' @rdname morie_clusmd_neighbour_lists
#' @export
morie_clusmd <- morie_clusmd_neighbour_lists

#' @rdname morie_clusmd_neighbour_lists
#' @export
morie_clusmd <- morie_clusmd_neighbour_lists
