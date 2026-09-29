#' FILTERSIM multiple-point simulation
#'
#' Training-image patterns (every full template window) are reduced to six
#' directional filter scores (average 1 - abs(u)/m, gradient u/m, curvature
#' 2 abs(u)/m - 1 along x and y), classified by k-means (k-means++ seeding)
#' on standardised scores, and their class means become prototypes. Along a
#' random path the data event is matched to the closest prototype and the
#' inner patch of a pattern drawn from that class is pasted onto uninformed
#' nodes; hard data are honoured. Philox draws; identical to the Python arm
#' \code{morie.fn.filtersim} (coordinates 0-based in hard_data).
#'
#' @param training_image Numeric matrix (rows y, columns x).
#' @param nx,ny Simulation grid size.
#' @param template Odd template width.
#' @param inner Half-width of the pasted patch (default template %/% 4).
#' @param n_classes Number of k-means classes.
#' @param hard_data Optional matrix with columns x, y (0-based) and value.
#' @param seed Philox seed.
#' @param max_kmeans Maximum Lloyd iterations.
#' @return List: realisation (ny by nx matrix), n_patterns, class_sizes,
#'   prototypes, classes_used (0-based).
#' @references Zhang, T., Switzer, P. and Journel, A. (2006). Filter-based
#'   classification of training image patterns for spatial simulation.
#'   Mathematical Geology 38, 63-80.
#' @examples
#' ti <- outer(1:24, 0:23, function(y, x) as.numeric((x %/% 3) %% 2 == 0))
#' FilterSim(ti, 12, 6, template = 5, n_classes = 4, seed = 2)$realisation[1, ]
#' @export
FilterSim <- function(training_image, nx, ny, template = 7L, inner = NULL, n_classes = 10L, hard_data = NULL,
                      seed = 1, max_kmeans = 50L) {
  ti <- as.matrix(training_image) + 0
  H <- nrow(ti)
  W <- ncol(ti)
  if (template %% 2 == 0 || template < 3) stop("template must be an odd integer >= 3")
  m <- template %/% 2
  ip <- if (is.null(inner)) m %/% 2 else inner
  if (H < template || W < template) stop("training image smaller than the template")
  offs <- as.matrix(expand.grid(du = -m:m, dv = -m:m))
  no <- nrow(offs)
  Fm <- matrix(0, 6, no)
  r <- 0
  for (axis in 1:2) {
    u <- offs[, axis]
    Fm[r + 1, ] <- 1 - abs(u) / m
    Fm[r + 2, ] <- u / m
    Fm[r + 3, ] <- 2 * abs(u) / m - 1
    r <- r + 3
  }
  pats <- list()
  scores <- list()
  for (y in m:(H - m - 1)) for (x in m:(W - m - 1)) {
    p <- ti[cbind(y + offs[, 2] + 1, x + offs[, 1] + 1)]
    pats[[length(pats) + 1]] <- p
    scores[[length(scores) + 1]] <- vapply(1:6, function(f) .fs_lsum(Fm[f, ] * p), 0)
  }
  n <- length(pats)
  K <- max(1, min(n_classes, n))
  Sm <- do.call(rbind, scores)
  mu <- vapply(1:6, function(j) .fs_lsum(Sm[, j]) / n, 0)
  sd <- vapply(1:6, function(j) {
    v <- sqrt(.fs_lsum((Sm[, j] - mu[j])^2) / n)
    if (v == 0) 1 else v
  }, 0)
  Z <- sweep(sweep(Sm, 2, mu), 2, sd, "/")
  U <- .fs_unif(seed)
  d2 <- function(a, b) {
    s <- 0
    for (j in 1:6) s <- s + (a[j] - b[j])^2
    s
  }
  cent <- list(Z[min(floor(U() * n), n - 1) + 1, ])
  while (length(cent) < K) {
    dist <- vapply(seq_len(n), function(i) min(vapply(cent, function(cc) d2(Z[i, ], cc), 0)), 0)
    tot <- .fs_lsum(dist)
    if (tot == 0) break
    rr <- U() * tot
    acc <- 0
    pick <- n
    for (i in seq_len(n)) {
      acc <- acc + dist[i]
      if (rr < acc) {
        pick <- i
        break
      }
    }
    cent[[length(cent) + 1]] <- Z[pick, ]
  }
  K <- length(cent)
  lab <- rep(1, n)
  for (itr in seq_len(max_kmeans)) {
    new <- vapply(seq_len(n), function(i) which.min(vapply(cent, function(cc) d2(Z[i, ], cc), 0)), 0)
    for (cc in seq_len(K)) {
      sel <- which(new == cc)
      if (length(sel)) cent[[cc]] <- vapply(1:6, function(j) .fs_lsum(Z[sel, j]) / length(sel), 0)
    }
    if (identical(new, lab) && itr > 1) break
    lab <- new
  }
  members <- lapply(seq_len(K), function(cc) which(lab == cc))
  proto <- lapply(members, function(mm) if (length(mm)) vapply(seq_len(no), function(k) .fs_lsum(vapply(mm, function(i) pats[[i]][k], 0)) / length(mm), 0) else NULL)
  grid <- matrix(NA_real_, ny, nx)
  if (!is.null(hard_data)) {
    hd <- matrix(hard_data, ncol = 3)
    for (r in seq_len(nrow(hd))) grid[hd[r, 2] + 1, hd[r, 1] + 1] <- hd[r, 3]
  }
  nodes <- as.matrix(expand.grid(x = 0:(nx - 1), y = 0:(ny - 1)))
  for (k in rev(seq_len(nrow(nodes) - 1))) {
    j <- min(floor(U() * (k + 1)), k)
    tmp <- nodes[k + 1, ]
    nodes[k + 1, ] <- nodes[j + 1, ]
    nodes[j + 1, ] <- tmp
  }
  used <- integer(0)
  for (q in seq_len(nrow(nodes))) {
    x <- nodes[q, 1]
    y <- nodes[q, 2]
    if (!is.na(grid[y + 1, x + 1])) next
    xx <- x + offs[, 1]
    yy <- y + offs[, 2]
    inside <- xx >= 0 & xx < nx & yy >= 0 & yy < ny
    val <- rep(NA_real_, no)
    val[inside] <- grid[cbind(yy[inside] + 1, xx[inside] + 1)]
    ev <- which(!is.na(val))
    if (!length(ev)) {
      rr <- U() * n
      acc <- 0
      cls <- K
      for (cc in seq_len(K)) {
        acc <- acc + length(members[[cc]])
        if (rr < acc) {
          cls <- cc
          break
        }
      }
    } else {
      best <- Inf
      cls <- 1
      for (cc in seq_len(K)) {
        if (is.null(proto[[cc]])) next
        s <- 0
        for (k in ev) s <- s + (proto[[cc]][k] - val[k])^2
        s <- s / length(ev)
        if (s < best) {
          best <- s
          cls <- cc
        }
      }
    }
    mm <- members[[cls]]
    pi <- mm[min(floor(U() * length(mm)), length(mm) - 1) + 1]
    used <- c(used, cls - 1L)
    for (k in seq_len(no)) {
      if (abs(offs[k, 1]) <= ip && abs(offs[k, 2]) <= ip && inside[k] && is.na(grid[yy[k] + 1, xx[k] + 1])) {
        grid[yy[k] + 1, xx[k] + 1] <- pats[[pi]][k]
      }
    }
  }
  list(realisation = grid, n_patterns = n, class_sizes = vapply(members, length, 0L), prototypes = proto,
       classes_used = used)
}

.fs_lsum <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

.fs_unif <- function(seed) {
  env <- new.env()
  env$block <- 0
  env$buf <- numeric(0)
  env$pos <- 0
  function() {
    if (env$pos >= length(env$buf)) {
      env$buf <- .morie_random_uniform(4096, seed = seed, stream = env$block)
      env$block <- env$block + 1
      env$pos <- 0
    }
    env$pos <- env$pos + 1
    env$buf[env$pos]
  }
}
