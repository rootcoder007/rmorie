# t-closeness via the Earth Mover's Distance.  Source: Li, N., Li, T. &
# Venkatasubramanian, S. (2007), t-Closeness: privacy beyond k-anonymity
# and l-diversity, Proc. 23rd IEEE ICDE, 106-115.  Sec. 5.1 ordered
# ground distance |i-j|/(m-1) with the printed closed form
#   D[P,Q] = 1/(m-1) * ( |r_1| + |r_1+r_2| + ... ),  r_i = p_i - q_i;
# Sec. 5.2 equal ground distance D = 1/2 sum |p_i - q_i|; Sec. 5.2
# hierarchical ground distance level(v1,v2)/H with
#   cost(N) = height(N)/H * min(pos_extra(N), neg_extra(N)),
#   D[P,Q] = sum over non-leaf N of cost(N).
# Native implementation mirroring Python morie.fn.tcls, loop for loop.
#
# CAVEAT, carried over from the Python arm: the ordered route reproduces
# three values printed in the paper exactly (0.375, 0.167, and Table 5's
# 0.167-closeness w.r.t. Salary).  The hierarchical route is written
# from the printed recursion but does not reproduce the paper's stated
# 0.278 for Table 5's Disease attribute.  Prefer ordered or equal.

#' .morie_tcls_dist
#'
#' A step of the tcls_native implementation. Called by \code{morie_tcls}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param labels A vector; its length is taken.
#' @param domain A vector; its length is taken.
#' @return The value of \code{p}, as built in the body.
#' @export
.morie_tcls_dist <- function(labels, domain) {
  n <- length(labels)
  if (n < 1) stop("morie_tcls: empty group of records")
  p <- numeric(length(domain))
  for (v in labels) {
    k <- match(v, domain)
    if (is.na(k))
      stop("morie_tcls: sensitive value ", v,
           " is absent from the table-wide domain")
    p[k] <- p[k] + 1 / n
  }
  p
}

#' .morie_tcls_equal
#'
#' A step of the tcls_native implementation. Called by \code{morie_emd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p Numeric; combined arithmetically in the body.
#' @param q Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.morie_tcls_equal <- function(p, q) 0.5 * sum(abs(p - q))

#' .morie_tcls_ordered
#'
#' A step of the tcls_native implementation. Called by \code{morie_emd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p A vector; its length is taken and its elements indexed.
#' @param q A vector; indexed elementwise.
#' @return A numeric value.
#' @export
.morie_tcls_ordered <- function(p, q) {
  m <- length(p)
  if (m < 2) return(0)
  run <- 0
  tot <- 0
  for (i in seq_len(m)) {
    run <- run + (p[i] - q[i])
    tot <- tot + abs(run)
  }
  tot / (m - 1)
}

#' .morie_tcls_height
#'
#' A step of the tcls_native implementation. Called by \code{.morie_tcls_hier}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n See Usage.
#' @param children A vector; indexed elementwise.
#' @return A numeric value.
#' @export
.morie_tcls_height <- function(n, children) {
  if (is.null(children[[n]])) return(0L)
  1L + max(vapply(children[[n]],
                  function(c) .morie_tcls_height(c, children), integer(1)))
}

#' .morie_tcls_hier
#'
#' A step of the tcls_native implementation. Called by \code{morie_emd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p A vector; indexed elementwise.
#' @param q A vector; indexed elementwise.
#' @param hierarchy The body requires: morie_tcls: hierarchy must have exactly one root, found.
#' @param domain The body requires: is not a domain value.
#' @return The value of \code{cost}, as built in the body.
#' @export
.morie_tcls_hier <- function(p, q, hierarchy, domain) {
  children <- hierarchy
  named <- names(children)
  is_child <- unlist(children, use.names = FALSE)
  roots <- named[!(named %in% is_child)]
  if (length(roots) != 1L)
    stop("morie_tcls: hierarchy must have exactly one root, found ",
         length(roots))
  H <- .morie_tcls_height(roots[1], children)
  if (H == 0L) stop("morie_tcls: hierarchy has no internal nodes")

  cost <- 0
  extra <- function(n) {
    if (is.null(children[[n]])) {
      k <- match(n, domain)
      if (is.na(k))
        stop("morie_tcls: hierarchy leaf ", n, " is not a domain value")
      return(p[k] - q[k])
    }
    kids <- vapply(children[[n]], extra, numeric(1))
    pos <- sum(kids[kids > 0])
    neg <- -sum(kids[kids < 0])
    # height(n)/H * min(pos_extra, neg_extra): the mass evened out AMONG
    # n's children, charged at n's level. Mass leaving the subtree is
    # charged at n's parent, hence min() rather than the total.
    cost <<- cost + (.morie_tcls_height(n, children) / H) * min(pos, neg)
    sum(kids)
  }
  extra(roots[1])
  cost
}

#' Earth Mover's Distance between two distributions over one domain
#'
#' @param p,q Distributions over a common ordered domain.
#' @param ground One of \code{"ordered"} (default), \code{"equal"} or
#'   \code{"hierarchical"}.
#' @param hierarchy Named list, node -> character vector of children,
#'   for \code{ground = "hierarchical"}.
#' @param domain The sensitive domain in order.
#' @return The EMD, a single number.
#' @references Li, N., Li, T., & Venkatasubramanian, S. (2007).
#'   t-Closeness: privacy beyond k-anonymity and l-diversity. ICDE.
#' @export
morie_emd <- function(p, q, ground = "ordered", hierarchy = NULL,
                      domain = NULL) {
  g <- tolower(as.character(ground)[1])
  if (!g %in% c("equal", "ordered", "hierarchical"))
    stop("morie_emd: ground must be equal, ordered or hierarchical")
  p <- as.numeric(p)
  q <- as.numeric(q)
  if (length(p) != length(q))
    stop("morie_emd: P has ", length(p), " cells but Q has ", length(q))
  if (g == "equal") return(.morie_tcls_equal(p, q))
  if (g == "ordered") return(.morie_tcls_ordered(p, q))
  if (is.null(hierarchy) || is.null(domain))
    stop("morie_emd: ground='hierarchical' needs both hierarchy and domain")
  .morie_tcls_hier(p, q, hierarchy, domain)
}

#' t-closeness of a release, per equivalence class and overall
#'
#' Mirrors Python \code{morie.fn.tcls}.
#'
#' @param X Records; only the length is used, to check it against the
#'   quasi-identifier blocks.
#' @param quasi_ids Quasi-identifier per record; records sharing one
#'   value form an equivalence class.
#' @param sensitive Sensitive value per record.
#' @param t The threshold.
#' @param ground Ground distance; see \code{\link{morie_emd}}.
#' @param hierarchy Node -> children list, hierarchical ground only.
#' @param domain The sensitive domain in order; defaults to the sorted
#'   distinct values present.
#' @return A list with the worst class distance as \code{estimate}, the
#'   per-class distances and sizes, and whether \code{t} is met.
#' @references Li, N., Li, T., & Venkatasubramanian, S. (2007).
#'   t-Closeness: privacy beyond k-anonymity and l-diversity. ICDE,
#'   106-115.
#' @export
morie_tcls <- function(X, quasi_ids, sensitive, t, ground = "ordered",
                       hierarchy = NULL, domain = NULL) {
  n <- length(X)
  qs <- if (is.matrix(quasi_ids)) apply(quasi_ids, 1, paste,
                                        collapse = "\r") else
    as.character(quasi_ids)
  sv <- sensitive
  if (length(qs) != n || length(sv) != n)
    stop("morie_tcls: X, quasi_ids and sensitive must agree in length")
  t <- as.numeric(t)[1]
  if (t < 0) stop("morie_tcls: t must be non-negative")

  dom <- if (!is.null(domain)) domain else sort(unique(sv))
  q <- .morie_tcls_dist(sv, dom)

  keys <- sort(unique(qs))
  dists <- numeric(length(keys))
  sizes <- integer(length(keys))
  for (i in seq_along(keys)) {
    grp <- sv[qs == keys[i]]
    p <- .morie_tcls_dist(grp, dom)
    dists[i] <- morie_emd(p, q, ground = ground, hierarchy = hierarchy,
                          domain = dom)
    sizes[i] <- length(grp)
  }

  worst <- if (length(dists)) max(dists) else 0
  list(estimate = worst,
       achieved_t = worst,
       satisfies = worst <= t,
       class_distances = dists,
       class_sizes = sizes,
       n_classes = length(keys),
       min_class_size = if (length(sizes)) min(sizes) else 0L,
       overall_distribution = q,
       domain = as.character(dom),
       ground = tolower(as.character(ground)[1]),
       t = t,
       n = as.integer(n),
       method = "t-closeness via EMD (Li, Li & Venkatasubramanian 2007)")
}
