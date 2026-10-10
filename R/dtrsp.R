# SPDX-License-Identifier: AGPL-3.0-or-later

#' Decision tree split (R parity)
#'
#' Native CART classification tree, grown exactly as \code{rpart::rpart}
#' grows a \code{method = "class"} tree with \code{cp = 0},
#' \code{minsplit = 2}, \code{minbucket = 1}, \code{xval = 0} and the
#' default priors (class frequencies), loss (0-1), \code{maxcompete = 4},
#' \code{maxsurrogate = 5}, \code{usesurrogate = 2} and
#' \code{surrogatestyle = 0}: Gini (or information) improvement split
#' search over the midpoints between distinct values, rpart's
#' cost-complexity bookkeeping (a split whose subtree does not lower the
#' misclassification risk is collapsed), surrogate splits for missing
#' values, and rpart's variable importance (primary improvement plus
#' improvement times adjusted agreement for every surrogate). Returns the
#' root split structure and normalised feature importances.
#'
#' @param x Numeric predictor matrix.
#' @param y Response (factor for classification).
#' @param criterion "gini" or "entropy"; "entropy" uses rpart's
#'   "information" splitting index.
#' @param max_depth Max tree depth (1 to 30, as in rpart).
#' @param seed RNG seed (kept for API compatibility; the tree is
#'   deterministic).
#' @return Named list: estimate, train_accuracy, root_feature, root_threshold,
#'   root_impurity, n_leaves, feature_importances, criterion, n, method.
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(120), 60, 2)
#' y <- factor(ifelse(x[, 1] > 0, "pos", "neg"))
#' morie_decision_tree_split(x, y)
#' @export
morie_decision_tree_split <- function(x, y, criterion = "gini", max_depth = 30L,
                                      seed = 0L) {
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  x <- as.matrix(x)
  yf <- as.factor(y)
  colnames(x) <- colnames(x) %||% paste0("x", seq_len(ncol(x)) - 1L)
  if (max_depth > 30L) stop("Maximum depth is 30")
  if (max_depth < 1L) stop("Maximum depth must be at least 1")
  .rmorie_local_seed(seed)
  xm <- matrix(as.numeric(x), nrow(x), ncol(x))
  fit <- .morie_cart_class_fit(
    xm, yf,
    info = identical(criterion, "entropy"),
    maxdepth = max_depth
  )
  nodes <- fit$nodes
  root <- nodes[["1"]]
  if (!is.null(root$var)) {
    root_feat <- root$var - 1L # 0-indexed parity
    root_thr <- root$spoint
  } else {
    root_feat <- NA_integer_
    root_thr <- NA_real_
  }
  # Impurity of the root class distribution
  tab <- table(yf)
  pk <- tab / sum(tab)
  root_imp <- if (criterion == "entropy") {
    -sum(ifelse(pk > 0, pk * log(pk), 0))
  } else {
    1 - sum(pk^2)
  }
  preds <- .morie_cart_class_predict(fit, xm)
  acc <- mean(preds == yf)
  fi_full <- fit$importance
  names(fi_full) <- colnames(x)
  if (sum(fi_full) > 0) fi_full <- fi_full / sum(fi_full)
  n_leaves <- sum(vapply(nodes, function(nd) is.null(nd$var), logical(1)))
  list(
    estimate            = as.numeric(acc),
    train_accuracy      = as.numeric(acc),
    root_feature        = if (is.na(root_feat)) NA_integer_ else as.integer(root_feat),
    root_threshold      = root_thr,
    root_impurity       = as.numeric(root_imp),
    n_leaves            = as.integer(n_leaves),
    feature_importances = as.numeric(fi_full),
    criterion           = criterion,
    n                   = nrow(x),
    method              = sprintf("Decision tree (CART, %s)", criterion)
  )
}

# Internal: rpart-equivalent CART classification tree (all-continuous
# predictors, unit case weights, default priors and 0-1 loss). A port of
# rpart's partition/bsplit/gini/surrogate/choose_surg/nodesplit C
# routines, keeping their arithmetic order so improvements, tie-breaking
# and the complexity-based collapsing agree with rpart. Rows with a
# missing response or with every predictor missing are dropped from the
# fit (rpart's na.rpart). Returns list(nodes, importance, ylevels) where
# nodes is keyed by rpart node number; a split node carries var (1-based
# column), spoint, dir (-1: x < spoint goes left, +1: goes right),
# improve and surr (list of var/spoint/dir/agree/adj), every node carries
# n, yval (1-based class) and risk.
# @noRd
.morie_cart_class_fit <- function(x, yf, info = FALSE, maxdepth = 30L,
                                  minsplit = 2L, minbucket = 1L,
                                  maxsur = 5L) {
  LEFT <- -1L
  RIGHT <- 1L
  ylev <- levels(yf)
  keep <- !is.na(yf) & rowSums(is.na(x)) < ncol(x)
  X <- x[keep, , drop = FALSE]
  yc <- as.integer(yf)[keep]
  n <- length(yc)
  if (n == 0L) stop("No observations left after removing missing values")
  p <- ncol(X)
  K <- max(yc)
  # Priors / altered priors as rpart's R (rpart.class) and C (giniinit)
  # code compute them.
  counts <- tabulate(yc, K)
  prior0 <- counts / sum(counts)
  freq <- counts / n
  temp <- 0
  aprior <- numeric(K)
  for (i in seq_len(K)) {
    for (j in seq_len(K)) {
      l <- if (i == j) 0 else 1
      temp <- temp + l * prior0[i]
      aprior[i] <- aprior[i] + l * prior0[i]
    }
  }
  prior <- prior0
  for (i in seq_len(K)) {
    if (freq[i] > 0) {
      prior[i] <- prior[i] / freq[i]
      aprior[i] <- aprior[i] / (temp * freq[i])
    }
  }
  unit_ap <- K < 2L || isTRUE(all(aprior[counts > 0] == 1))
  imp <- if (info) {
    function(q) ifelse(q == 0, 0, -q * log(q))
  } else {
    function(q) q * (1 - q)
  }
  maxnode <- 2^maxdepth - 1
  st <- new.env(parent = emptyenv())
  st$iscale <- 0
  st$nodes <- list()

  node_eval <- function(idx) {
    f <- tabulate(yc[idx], K)
    best <- 0
    cls <- 1L
    for (i in seq_len(K)) {
      tt <- 0
      for (j in seq_len(K)) {
        if (j != i) tt <- tt + f[j] * prior[j]
      }
      if (i == 1L || tt < best) {
        cls <- i
        best <- tt
      }
    }
    list(yval = cls, risk = best)
  }

  # rpart gini(): best split of one sorted continuous variable
  gini_split <- function(xs, ys) {
    k <- length(xs)
    w <- aprior[ys]
    if (unit_ap) {
      rwt0 <- as.numeric(k)
      lwt <- seq_len(k - 1L)
      rwt <- rwt0 - lwt
    } else {
      rwt0 <- 0
      for (i in seq_len(k)) rwt0 <- rwt0 + w[i]
      lwt <- numeric(k - 1L)
      rwt <- numeric(k - 1L)
      lw <- 0
      rw <- rwt0
      for (i in seq_len(k - 1L)) {
        rw <- rw - w[i]
        lw <- lw + w[i]
        lwt[i] <- lw
        rwt[i] <- rw
      }
    }
    tot <- tabulate(ys, K)
    total_ss <- 0
    for (j in seq_len(K)) {
      total_ss <- total_ss + rwt0 * imp(aprior[j] * tot[j] / rwt0)
    }
    m <- k - 1L
    tmp <- numeric(m)
    lmean <- numeric(m)
    rmean <- numeric(m)
    for (j in seq_len(K)) {
      lc <- cumsum(ys == j)[seq_len(m)]
      rc <- tot[j] - lc
      pl <- aprior[j] * lc / lwt
      tmp <- tmp + lwt * imp(pl)
      lmean <- lmean + pl * (j - 1L)
      pr <- aprior[j] * rc / rwt
      tmp <- tmp + rwt * imp(pr)
      rmean <- rmean + pr * (j - 1L)
    }
    pos <- seq_len(m)
    ok <- pos >= minbucket & (k - pos) >= minbucket &
      xs[pos + 1L] != xs[pos]
    best <- total_ss
    where <- NA_integer_
    if (any(ok)) {
      mn <- min(tmp[ok])
      if (mn < total_ss) {
        where <- which(ok & tmp == mn)[1L]
        best <- mn
      }
    }
    improve <- total_ss - best
    if (improve > 0 && !is.na(where)) {
      list(
        improve = improve,
        dir = if (lmean[where] < rmean[where]) LEFT else RIGHT,
        spoint = (xs[where] + xs[where + 1L]) / 2
      )
    } else {
      list(improve = improve, dir = LEFT, spoint = NA_real_)
    }
  }

  bsplit <- function(n1, n2) {
    bestv <- NULL
    for (v in seq_len(p)) {
      ids <- st$sorts[n1:n2, v]
      xv <- X[ids, v]
      ok <- is.finite(xv)
      if (!any(ok)) next
      xs <- xv[ok]
      ys <- yc[ids][ok]
      if (xs[1L] == xs[length(xs)]) next
      g <- gini_split(xs, ys)
      improve <- g$improve
      if (improve > st$iscale) st$iscale <- improve
      if (improve > st$iscale * 1e-10) {
        if (is.null(bestv) || improve > bestv$improve) {
          bestv <- list(var = v, improve = improve, spoint = g$spoint,
                        dir = g$dir)
        }
      }
    }
    bestv
  }

  # rpart choose_surg() for a continuous surrogate
  choose_surg <- function(xv, ty, tleft, tright) {
    ok <- is.finite(xv)
    xs <- xv[ok]
    yy <- ty[ok]
    if (length(xs) == 0L) return(NULL)
    u <- sort(unique(xs))
    gidx <- match(xs, u)
    m <- length(u)
    nl <- tabulate(gidx[yy == LEFT], m)
    nr <- tabulate(gidx[yy == RIGHT], m)
    llwt0 <- sum(nl)
    rlwt0 <- sum(nr)
    agree0 <- if (llwt0 > rlwt0) llwt0 else rlwt0
    if (m < 2L) return(NULL)
    # moved (x < u_g) counts for g = 2..m
    lr <- cumsum(nl)[seq_len(m - 1L)]
    rr <- cumsum(nr)[seq_len(m - 1L)]
    ll <- llwt0 - lr
    rl <- rlwt0 - rr
    elig <- (ll + rl) >= 2 & (lr + rr) >= 2
    # the march stops for good once fewer than 2 remain
    elig <- elig & cumprod((ll + rl) >= 2) == 1
    c1 <- ll + rr
    c2 <- lr + rl
    cand <- pmax(c1, c2)
    cand[!elig] <- -Inf
    if (!any(elig) || max(cand) <= agree0) return(NULL)
    g <- which(cand == max(cand))[1L]
    agree <- cand[g]
    dir <- if (c1[g] == agree) RIGHT else LEFT
    split <- (u[g + 1L] + u[g]) / 2
    total <- tleft + tright
    majority <- if (tleft > tright) tleft else tright
    agreement <- agree / total
    majority <- majority / total
    list(agree = agreement, spoint = split, dir = dir,
         adj = (agreement - majority) / (1 - majority))
  }

  surrogate <- function(idx, prim) {
    xp <- X[idx, prim$var]
    ty <- ifelse(!is.finite(xp), 0L,
                 ifelse(xp < prim$spoint, prim$dir, -prim$dir))
    lcount <- sum(ty == LEFT)
    rcount <- sum(ty == RIGHT)
    last <- if (lcount < rcount) RIGHT else if (lcount > rcount) LEFT else 0L
    sl <- list()
    for (v in seq_len(p)) {
      if (v == prim$var) next
      cs <- choose_surg(X[idx, v], ty, lcount, rcount)
      if (is.null(cs) || cs$adj <= 1e-10) next
      cs$var <- v
      pos <- 1L
      while (pos <= length(sl) && !(cs$agree > sl[[pos]]$agree)) pos <- pos + 1L
      if (pos > maxsur) next
      sl <- append(sl, list(cs), after = pos - 1L)
      if (length(sl) > maxsur) sl <- sl[seq_len(maxsur)]
    }
    list(surr = sl, last = last)
  }

  # rpart nodesplit(): route the node's observations and reorder every
  # column of the sorts matrix as {left, right, stays} keeping order.
  nodesplit <- function(n1, n2, prim, surr, last) {
    idx <- st$sorts[n1:n2, 1L]
    xp <- X[idx, prim$var]
    d <- ifelse(!is.finite(xp), 0L, ifelse(xp < prim$spoint, prim$dir, -prim$dir))
    miss <- which(d == 0L)
    for (i in miss) {
      for (s in surr) {
        xs <- X[idx[i], s$var]
        if (!is.finite(xs)) next
        d[i] <- if (xs < s$spoint) s$dir else -s$dir
        break
      }
    }
    if (last != 0L) d[d == 0L] <- last
    dd <- integer(n)
    dd[idx] <- d
    for (v in seq_len(p)) {
      seg <- st$sorts[n1:n2, v]
      ds <- dd[seg]
      st$sorts[n1:n2, v] <- c(seg[ds == LEFT], seg[ds == RIGHT], seg[ds == 0L])
    }
    c(nleft = sum(d == LEFT), nright = sum(d == RIGHT))
  }

  drop_sub <- function(nodenum) {
    for (ch in c(2 * nodenum, 2 * nodenum + 1)) {
      key <- format(ch, scientific = FALSE)
      if (!is.null(st$nodes[[key]])) {
        drop_sub(ch)
        st$nodes[[key]] <- NULL
      }
    }
  }

  # rpart partition(): returns list(sumrisk, nsplit, risk, complexity)
  partition <- function(nodenum, n1, n2, complexity) {
    key <- format(nodenum, scientific = FALSE)
    idx <- if (n2 >= n1) st$sorts[n1:n2, 1L] else integer(0)
    ev <- node_eval(idx)
    me <- list(n = length(idx), yval = ev$yval, risk = ev$risk)
    if (nodenum == 1) complexity <- ev$risk # rpart.c: root cp = its risk
    tempcp <- min(ev$risk, complexity)
    leaf <- function() {
      st$nodes[[key]] <- me
      list(sumrisk = me$risk, nsplit = 0, risk = me$risk, complexity = 0)
    }
    if (length(idx) < minsplit || tempcp <= 0 || nodenum > maxnode) {
      return(leaf())
    }
    prim <- bsplit(n1, n2)
    if (is.null(prim)) return(leaf())
    su <- surrogate(idx, prim)
    ns <- nodesplit(n1, n2, prim, su$surr, su$last)
    me$var <- prim$var
    me$spoint <- prim$spoint
    me$dir <- prim$dir
    me$improve <- prim$improve
    me$surr <- su$surr
    st$nodes[[key]] <- me
    L <- partition(2 * nodenum, n1, n1 + ns[["nleft"]] - 1L, tempcp)
    tempcp <- (me$risk - L$sumrisk) / (L$nsplit + 1)
    tempcp2 <- me$risk - L$risk
    if (tempcp < tempcp2) tempcp <- tempcp2
    if (tempcp > complexity) tempcp <- complexity
    R <- partition(2 * nodenum + 1, n1 + ns[["nleft"]],
                   n1 + ns[["nleft"]] + ns[["nright"]] - 1L, tempcp)
    left_risk <- L$sumrisk
    left_split <- L$nsplit
    right_risk <- R$sumrisk
    right_split <- R$nsplit
    tempcp <- (me$risk - (left_risk + right_risk)) /
      (left_split + right_split + 1)
    if (R$complexity > L$complexity) {
      if (tempcp > L$complexity) {
        left_risk <- L$risk
        left_split <- 0
        tempcp <- (me$risk - (left_risk + right_risk)) /
          (left_split + right_split + 1)
        if (tempcp > R$complexity) {
          right_risk <- R$risk
          right_split <- 0
        }
      }
    } else if (tempcp > R$complexity) {
      right_split <- 0
      right_risk <- R$risk
      tempcp <- (me$risk - (left_risk + right_risk)) /
        (left_split + right_split + 1)
      if (tempcp > L$complexity) {
        left_risk <- L$risk
        left_split <- 0
      }
    }
    cpx <- (me$risk - (left_risk + right_risk)) /
      (left_split + right_split + 1)
    if (cpx <= 0) {
      drop_sub(nodenum)
      me$var <- NULL
      me$spoint <- NULL
      me$dir <- NULL
      me$improve <- NULL
      me$surr <- NULL
      st$nodes[[key]] <- me
      return(list(sumrisk = me$risk, nsplit = 0, risk = me$risk,
                  complexity = cpx))
    }
    list(sumrisk = left_risk + right_risk,
         nsplit = left_split + right_split + 1,
         risk = me$risk, complexity = cpx)
  }

  # Per-variable sort indices, built once as rpart.c does (non-finite
  # values sorted as 0). Within-tie order only matters when the altered
  # priors are not all exactly 1, so rpart's own quicksort is replayed
  # in that case to reproduce its floating-point summation order.
  st$sorts <- matrix(0L, n, p)
  for (v in seq_len(p)) {
    xv <- X[, v]
    xv[!is.finite(xv)] <- 0
    st$sorts[, v] <- if (unit_ap) order(xv) else .morie_rpart_mysort(xv)
  }
  partition(1, 1L, n, NA_real_)
  importance <- numeric(p)
  for (nd in st$nodes) {
    if (is.null(nd$var)) next
    importance[nd$var] <- importance[nd$var] + nd$improve
    for (s in nd$surr) {
      importance[s$var] <- importance[s$var] + nd$improve * s$adj
    }
  }
  list(nodes = st$nodes, importance = importance, ylevels = ylev)
}

# Internal: class prediction from a .morie_cart_class_fit() tree, walking
# it as rpart's pred_rpart does with usesurrogate = 2 (primary split,
# then surrogates in order, then the larger child; an observation that
# cannot be routed stops at the current node).
# @noRd
.morie_cart_class_predict <- function(fit, x) {
  nodes <- fit$nodes
  yv <- integer(nrow(x))
  walk <- function(nodenum, idx) {
    if (length(idx) == 0L) return(invisible())
    nd <- nodes[[format(nodenum, scientific = FALSE)]]
    if (is.null(nd$var)) {
      yv[idx] <<- nd$yval
      return(invisible())
    }
    d <- integer(length(idx))
    xp <- x[idx, nd$var]
    ok <- !is.na(xp)
    d[ok] <- ifelse(xp[ok] < nd$spoint, nd$dir, -nd$dir)
    for (s in nd$surr) {
      todo <- d == 0L
      if (!any(todo)) break
      xs <- x[idx, s$var]
      ok <- todo & !is.na(xs)
      d[ok] <- ifelse(xs[ok] < s$spoint, s$dir, -s$dir)
    }
    if (any(d == 0L)) {
      nl <- nodes[[format(2 * nodenum, scientific = FALSE)]]$n
      nr <- nodes[[format(2 * nodenum + 1, scientific = FALSE)]]$n
      if (nl != nr) d[d == 0L] <- if (nl > nr) -1L else 1L
    }
    yv[idx[d == 0L]] <<- nd$yval
    walk(2 * nodenum, idx[d == -1L])
    walk(2 * nodenum + 1, idx[d == 1L])
  }
  walk(1, seq_len(nrow(x)))
  factor(fit$ylevels[yv], levels = fit$ylevels)
}

# Internal: replay of rpart's mysort() (median-of-three quicksort with
# insertion sort for short runs; not stable). Returns the permutation it
# produces, which fixes rpart's within-tie observation order.
# @noRd
.morie_rpart_mysort <- function(x) {
  cvec <- seq_along(x)
  srt <- function(start, stop) {
    while (start < stop) {
      if ((stop - start) < 11L) {
        if (start + 1L <= stop) {
          for (i in (start + 1L):stop) {
            temp <- x[i]
            tempd <- cvec[i]
            j <- i - 1L
            while (j >= start && x[j] > temp) {
              x[j + 1L] <<- x[j]
              cvec[j + 1L] <<- cvec[j]
              j <- j - 1L
            }
            x[j + 1L] <<- temp
            cvec[j + 1L] <<- tempd
          }
        }
        return(invisible())
      }
      i <- start
      j <- stop
      k <- (start + stop) %/% 2L
      median <- x[k]
      if (x[i] >= x[k]) {
        if (x[j] > x[k]) {
          median <- if (x[i] > x[j]) x[j] else x[i]
        }
      } else if (x[j] < x[k]) {
        median <- if (x[i] > x[j]) x[i] else x[j]
      }
      while (i < j) {
        while (x[i] < median) i <- i + 1L
        while (x[j] > median) j <- j - 1L
        if (i < j) {
          if (x[i] > x[j]) {
            tt <- x[i]
            x[i] <<- x[j]
            x[j] <<- tt
            td <- cvec[i]
            cvec[i] <<- cvec[j]
            cvec[j] <<- td
          }
          i <- i + 1L
          j <- j - 1L
        }
      }
      while (x[i] >= median && i > start) i <- i - 1L
      while (x[j] <= median && j < stop) j <- j + 1L
      if ((i - start) < (stop - j)) {
        if ((i - start) > 0L) srt(start, i)
        start <- j
      } else {
        if ((stop - j) > 0L) srt(j, stop)
        stop <- i
      }
    }
    invisible()
  }
  if (length(x) > 1L) srt(1L, length(x))
  cvec
}
