```r
# morie.fn -- function file (rootcoder007/morie)
# DELLY: structural variant discovery from paired-end and split reads.
#
# Rausch, T., Zichner, T., Schlattl, A., Stütz, A. M., Benes, V., &
# Korbel, J. O. (2012) "DELLY: structural variant discovery by integrated
# paired-end and split-read analysis", *Bioinformatics* 28(18), i333-i339.
# doi:10.1093/bioinformatics/bts378
#
# Two components, run in that order (the paper's Figure 1).
#
# Paired-end mapping analysis (Section 2.1). Each library has a default
# read-pair orientation and an insert size distribution; a pair is
# discordant if its orientation is wrong or its insert size is more than
# n_sd standard deviations above the median (three, by default). Each
# class of rearrangement leaves its own signature (DEL, DUP, INV, TRA).
# Discordant pairs of one signature become the nodes of an undirected
# weighted graph; an edge joins two pairs that could support the same
# breakpoint and carries the disagreement between the implied SV sizes.
# Within each component DELLY grows a clique from the lowest-weight edge
# and reports it when maximal.
#
# Split-read analysis (Section 2.2) turns an interval into a base.
# Every non-deletion type is first rewritten so a plain "deletion-type"
# search works (Figure 4). Per candidate SV: k-mer index the reference
# region, bin read k-mer hits by alignment diagonal, keep diagonals
# with at least k_min hits, take the gap between consecutive diagonals
# as the SV size, build a gapless consensus by majority vote, align
# that consensus to the region with two Gotoh matrices (forward and
# reverse), and split at argmax_{i<j} f_i + r_j. Accept the refinement
# only if the split-read length agrees with the paired-end estimate
# to within max_length_diff.

.SV_TYPES <- c("DEL", "DUP", "INV", "TRA")
.COMPLEMENT <- c(A="T", C="G", G="C", T="A", N="N")

.sv_dl_pair <- function(p) {
  if (is.null(p$chrom1) || is.null(p$pos1) || is.null(p$strand1) ||
      is.null(p$chrom2) || is.null(p$pos2) || is.null(p$strand2)) {
    stop("sv_dl: a pair needs chrom1/pos1/strand1 and chrom2/pos2/strand2")
  }
  c1 <- p$chrom1
  p1 <- as.integer(p$pos1)
  s1 <- p$strand1
  c2 <- p$chrom2
  p2 <- as.integer(p$pos2)
  s2 <- p$strand2
  l1 <- as.integer(if (!is.null(p$len1)) p$len1
              else if (!is.null(p$read_length)) p$read_length
              else 100L)
  l2 <- as.integer(if (!is.null(p$len2)) p$len2
              else if (!is.null(p$read_length)) p$read_length
              else 100L)
  if (l1 < 1L || l2 < 1L) stop("sv_dl: read lengths must be positive")
  if (p1 < 0L || p2 < 0L) stop("sv_dl: alignment positions must be non-negative")
  if (!(s1 %in% c("+", "-")) || !(s2 %in% c("+", "-"))) {
    stop("sv_dl: strands must be '+' or '-'")
  }
  swap <- FALSE
  if (c2 < c1) swap <- TRUE
  else if (c2 == c1 && p2 < p1) swap <- TRUE
  if (swap) {
    tmp <- c1; c1 <- c2; c2 <- tmp
    tmp <- p1; p1 <- p2; p2 <- tmp
    tmp <- s1; s1 <- s2; s2 <- tmp
    tmp <- l1; l1 <- l2; l2 <- tmp
  }
  list(chrom1=c1, pos1=p1, strand1=s1, len1=l1,
       chrom2=c2, pos2=p2, strand2=s2, len2=l2,
       seq=if (!is.null(p$seq)) p$seq else NULL,
       id=if (!is.null(p$id)) p$id else NULL)
}

.sv_dl_median <- function(v) {
  s <- sort(as.numeric(v))
  n <- length(s)
  if (n == 0L) stop("sv_dl: no values to take a median of")
  if (n %% 2L == 1L) s[n %/% 2L + 1L]
  else 0.5 * (s[n %/% 2L] + s[n %/% 2L + 1L])
}

.sv_dl_insert <- function(p) {
  (p$pos2 + p$len2) - p$pos1
}

.sv_dl_sv_size <- function(p, label, median) {
  if (label[1] == "TRA") return(NULL)
  .sv_dl_insert(p) - median
}

insert_size_stats <- function(pairs, orientation=NULL, spread="mad") {
  ps <- lapply(pairs, .sv_dl_pair)
  same <- ps[vapply(ps, function(p) p$chrom1 == p$chrom2, logical(1))]
  if (length(same) == 0L) {
    stop("sv_dl: no same-chromosome pairs to estimate the insert size distribution from")
  }
  if (is.null(orientation)) {
    counts <- list()
    for (p in same) {
      key <- paste0(p$strand1, ",", p$strand2)
      if (is.null(counts[[key]])) counts[[key]] <- 0L
      counts[[key]] <- counts[[key]] + 1L
    }
    keys_sorted <- sort(names(counts))
    best_key <- keys_sorted[which.max(vapply(keys_sorted,
                                             function(k) counts[[k]],
                                             integer(1)))]
    orientation <- strsplit(best_key, ",")[[1]]
  } else {
    orientation <- as.character(orientation)
  }
  if (length(orientation) != 2L ||
      !(orientation[1] %in% c("+", "-")) ||
      !(orientation[2] %in% c("+", "-"))) {
    stop("sv_dl: orientation must be a pair of strands")
  }
  d1 <- orientation[1]
  d2 <- orientation[2]
  concordant <- numeric(0)
  for (p in same) {
    if (p$strand1 == d1 && p$strand2 == d2) {
      concordant <- c(concordant, .sv_dl_insert(p))
    }
  }
  if (length(concordant) == 0L) stop("sv_dl: no pairs in the default orientation")
  if (!(spread %in% c("mad", "sd"))) stop("sv_dl: spread must be 'mad' or 'sd'")
  med <- .sv_dl_median(concordant)
  if (length(concordant) > 1L) {
    if (spread == "mad") {
      sd_val <- 1.4826 * .sv_dl_median(abs(concordant - med))
    } else {
      mu <- sum(concordant) / length(concordant)
      var <- sum((concordant - mu)^2) / (length(concordant) - 1)
      sd_val <- sqrt(var)
    }
  } else {
    sd_val <- 0
  }
  list(median=as.numeric(med), sd=as.numeric(sd_val), spread=spread,
       orientation=orientation, n=length(concordant))
}

classify_pair <- function(p, median, sd, orientation=c("+", "-"), n_sd=3.0) {
  p <- .sv_dl_pair(p)
  d1 <- orientation[1]
  d2 <- orientation[2]
  if (p$chrom1 != p$chrom2) {
    t <- 2L * (1L * (p$strand1 != d1)) + (1L * (p$strand2 != d2))
    return(c("TRA", as.character(t)))
  }
  s1 <- p$strand1
  s2 <- p$strand2
  if (s1 == s2) {
    return(c("INV", if (s1 == d1) "left" else "right"))
  }
  if (s1 == d2 && s2 == d1) {
    return(c("DUP", ""))
  }
  if (s1 == d1 && s2 == d2) {
    if (.sv_dl_insert(p) > median + n_sd * sd) {
      return(c("DEL", ""))
    }
    return(NULL)
  }
  NULL
}

build_sv_graph <- function(pairs, median, sd, label, orientation=c("+", "-"),
                           n_sd=3.0, window=NULL) {
  if (is.null(window)) window <- median + n_sd * sd
  ps <- lapply(pairs, .sv_dl_pair)
  sizes <- lapply(ps, function(p) .sv_dl_sv_size(p, label, median))
  n <- length(ps)
  edge_list <- list()
  if (n >= 2L) {
    for (i in 1:(n - 1L)) {
      for (j in (i + 1L):n) {
        a <- ps[[i]]
        b <- ps[[j]]
        if (a$chrom1 != b$chrom1 || a$chrom2 != b$chrom2) next
        if (abs(a$pos1 - b$pos1) > window) next
        if (abs(a$pos2 - b$pos2) > window) next
        if (label[1] == "TRA") {
          w <- abs(a$pos1 - b$pos1) + abs(a$pos2 - b$pos2)
        } else {
          w <- abs(sizes[[i]] - sizes[[j]])
        }
        edge_list[[length(edge_list) + 1L]] <- c(as.numeric(w),
                                                  as.integer(i),
                                                  as.integer(j))
      }
    }
  }
  if (length(edge_list) == 0L) {
    edges <- matrix(numeric(0), ncol=3L)
    colnames(edges) <- c("w", "i", "j")
  } else {
    edges <- do.call(rbind, edge_list)
    colnames(edges) <- c("w", "i", "j")
  }
  list(nodes=ps, edges=edges, sizes=sizes, label=label)
}

.sv_dl_components <- function(n, edges) {
  if (n == 0L) return(list())
  parent <- 1:n
  find <- function(a) {
    while (parent[a] != a) {
      parent[a] <<- parent[parent[a]]
      a <- parent[a]
    }
    a
  }
  if (!is.null(edges) && nrow(edges) > 0L) {
    for (k in 1:nrow(edges)) {
      i <- edges[k, "i"]
      j <- edges[k, "j"]
      ri <- find(i)
      rj <- find(j)
      if (ri != rj) parent[ri] <<- rj
    }
  }
  groups <- list()
  for (i in 1:n) {
    r <- find(i)
    key <- as.character(r)
    if (is.null(groups[[key]])) groups[[key]] <- integer(0)
    groups[[key]] <- c(groups[[key]], i)
  }
  comps <- lapply(groups, sort)
  comps[vapply(comps, length, integer(1)) > 1L]
}

maximal_clique <- function(members, edges) {
  keep <- as.integer(members)
  if (is.null(edges) || nrow(edges) == 0L) return(integer(0))
  sub_idx <- which(edges[, "i"] %in% keep & edges[, "j"] %in% keep)
  if (length(sub_idx) == 0L) return(integer(0))
  sub <- edges[sub_idx, , drop=FALSE]
  sub <- sub[order(sub[, "w"], sub[, "i"], sub[, "j"]), , drop=FALSE]
  adj <- list()
  for (k in 1:nrow(sub)) {
    w <- sub[k, "w"]
    i <- sub[k, "i"]
    j <- sub[k, "j"]
    i_key <- as.character(i)
    j_key <- as.character(j)
    if (is.null(adj[[i_key]])) adj[[i_key]] <- list()
    if (is.null(adj[[j_key]])) adj[[j_key]] <- list()
    adj[[i_key]][[j_key]] <- w
    adj[[j_key]][[i_key]] <- w
  }
  clique <- as.integer(sub[1L, c("i", "j")])
  repeat {
    best <- NULL
    for (k in 1:nrow(sub)) {
      w <- sub[k, "w"]
      i <- sub[k, "i"]
      j <- sub[k, "j"]
      i_in <- i %in% clique
      j_in <- j %in% clique
      inside <- as.integer(i_in) + as.integer(j_in)
      if (inside != 1L) next
      outside <- if (i_in) j else i
      all_adj <- TRUE
      adj_outside <- adj[[as.character(outside)]]
      if (is.null(adj_outside)) {
        all_adj <- FALSE
      } else {
        for (m in clique) {
          if (is.null(adj_outside[[as.character(m)]])) {
            all_adj <- FALSE
            break
          }
        }
      }
      if (all_adj) {
        if (is.null(best) || w < best[1L]) {
          best <- c(w, outside)
        }
      }
    }
    if (is.null(best)) break
    clique <- c(clique, best[2L])
  }
  sort(clique)
}

paired_end_calls <- function(pairs, median=NULL, sd=NULL, orientation=NULL,
                             n_sd=3.0, min_support=2L, window=NULL,
                             spread="mad") {
  ps <- lapply(pairs, .sv_dl_pair)
  if (length(ps) == 0L) stop("sv_dl: no read pairs given")
  if (is.null(median) || is.null(sd) || is.null(orientation)) {
    st <- insert_size_stats(ps, orientation, spread)
    if (is.null(median)) median <- st$median
    if (is.null(sd)) sd <- st$sd
    if (is.null(orientation)) orientation <- st$orientation
  } else {
    orientation <- as.character(orientation)
  }
  if (n_sd < 0) stop("sv_dl: n_sd must be non-negative")
  if (min_support < 1L) stop("sv_dl: min_support must be at least 1")
  by_label <- list()
  for (p in ps) {
    lab <- classify_pair(p, median, sd, orientation, n_sd)
    if (!is.null(lab)) {
      key <- paste(lab, collapse=",")
      if (is.null(by_label[[key]])) by_label[[key]] <- list()
      by_label[[key]][[length(by_label[[key]]) + 1L]] <- p
    }
  }
  calls <- list()
  for (key in sort(names(by_label))) {
    parts <- strsplit(key, ",")[[1]]
    lab <- parts[1:2]
    group <- by_label[[key]]
    g <- build_sv_graph(group, median, sd, lab, orientation, n_sd, window)
    for (comp in .sv_dl_components(length(g$nodes), g$edges)) {
      members <- maximal_clique(comp, g$edges)
      if (length(members) < min_support) next
      sel <- g$nodes[members]
      start <- max(vapply(sel, function(p) p$pos1 + p$len1, integer(1)))
      end <- min(vapply(sel, function(p) p$pos2, integer(1)))
      size <- NULL
      if (lab[1] != "TRA") {
        sz <- vapply(members, function(i) g$sizes[[i]], numeric(1))
        size <- mean(sz)
      }
      calls[[length(calls) + 1L]] <- list(
        type=lab[1],
        subtype=lab[2],
        chrom=sel[[1]]$chrom1,
        chrom2=sel[[1]]$chrom2,
        start=as.integer(start),
        end=as.integer(end),
        size=if (is.null(size)) NULL else as.numeric(size),
        support=length(members),
        pairs=lapply(sel, function(p) as.list(p)),
        precise=FALSE
      )
    }
  }
  if (length(calls) > 0L) {
    sort_keys <- vapply(calls,
                        function(c) paste(c(c$type, c$chrom, c$start),
                                          collapse="\r"),
                        character(1))
    calls <- calls[order(sort_keys)]
  }
  calls
}

.sv_dl_revcomp <- function(s) {
  s <- toupper(as.character(s))
  if (nchar(s) == 0L) return("")
  chars <- strsplit(s, "")[[1]]
  comp <- vapply(chars, function(c) {
    val <- .COMPLEMENT[[c]]
    if (is.null(val)) "N" else val
  }, character(1))
  paste0(rev(comp), collapse="")
}

deletion_type_reference <- function(ref, sv_type) {
  if (!(sv_type %in% .SV_TYPES)) {
    stop(sprintf("sv_dl: sv_type must be one of %s",
                 paste(.SV_TYPES, collapse=", ")))
  }
  s <- toupper(as.character(ref))
  if (sv_type == "DEL") return(s)
  chars <- strsplit(s, "")[[1]]
  h <- length(chars) %/% 2L
  a <- paste0(chars[1:h], collapse="")
  b <- paste0(chars[(h + 1L):length(chars)], collapse="")
  if (sv_type == "DUP") return(paste0(b, a
