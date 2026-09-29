#' Read assembly and protein disorder
#'
#' \code{OlcAssembly}: overlap-layout-consensus assembly. Duplicate and
#' contained reads are dropped, longest suffix-prefix overlaps (at most
#' floor(max_error L) mismatches) are accepted greedily longest first
#' without branching or cycles, and each contig is the column-majority
#' consensus of its layout. \code{ProteinDisorder}: FoldIndex
#' 2.785 H - abs(R) - 1.151 from rescaled Kyte-Doolittle hydropathy and net
#' charge, globally and in a sliding window. Identical to the Python arm
#' \code{morie.fn.bioseqx} (read indices and layouts 0-based there,
#' 1-based here).
#'
#' @param long_reads Character vector of reads.
#' @param min_overlap Minimum overlap length.
#' @param max_error Maximum mismatch fraction inside an overlap.
#' @param sequence Protein sequence (one-letter codes).
#' @param window Odd sliding-window length.
#' @param min_region Minimum length of a reported disordered region.
#' @return List. OlcAssembly: contigs, layouts (matrices of read, offset),
#'   overlaps, n_reads_used. ProteinDisorder: fold_index, mean_hydropathy,
#'   mean_net_charge, profile, disordered, regions, fraction_disordered.
#' @references Myers, E. W. (2005). The fragment assembly string graph.
#'   Bioinformatics 21 (suppl. 2), ii79-ii85.
#'
#'   Prilusky, J. et al. (2005). FoldIndex: a simple tool to predict whether
#'   a given protein sequence is intrinsically unfolded. Bioinformatics 21,
#'   3435-3438.
#'
#'   Uversky, V. N., Gillespie, J. R. and Fink, A. L. (2000). Why are
#'   natively unfolded proteins unstructured under physiologic conditions?
#'   Proteins 41, 415-427.
#' @examples
#' OlcAssembly(c("ATGGCGT", "GCGTGCA", "TGCAATG", "CAATGGA"))$contigs
#' ProteinDisorder("MKKEEEKKPSEESKEDKKSEEGAPLLIVAVLLFAGIVLLVAWFILK", window = 11)$regions
#' @export
OlcAssembly <- function(long_reads, min_overlap = 3L, max_error = 0) {
  reads <- toupper(as.character(long_reads))
  n <- length(reads)
  keep <- integer(0)
  for (i in seq_len(n)) {
    r <- reads[i]
    dup <- i > 1 && any(reads[seq_len(i - 1)] == r)
    cont <- any(nchar(reads) > nchar(r) & vapply(reads, function(s) grepl(r, s, fixed = TRUE), TRUE))
    if (!dup && !cont) keep <- c(keep, i)
  }
  ch <- lapply(reads, function(s) strsplit(s, "")[[1]])
  edges <- list()
  for (i in keep) for (j in keep) {
    if (i != j) {
      o <- .bq_overlap(ch[[i]], ch[[j]], min_overlap, max_error)
      if (o[1] > 0) edges[[length(edges) + 1]] <- c(o, i, j)
    }
  }
  E <- if (length(edges)) do.call(rbind, edges) else matrix(0, 0, 4)
  E <- E[order(-E[, 1], E[, 2], E[, 3], E[, 4]), , drop = FALSE]
  succ <- rep(NA_integer_, n)
  sL <- rep(NA_real_, n)
  pred <- rep(NA_integer_, n)
  comp <- seq_len(n)
  find <- function(x) {
    while (comp[x] != x) x <- comp[x]
    x
  }
  used <- list()
  for (k in seq_len(nrow(E))) {
    L <- E[k, 1]
    i <- E[k, 3]
    j <- E[k, 4]
    if (!is.na(succ[i]) || !is.na(pred[j]) || find(i) == find(j)) next
    succ[i] <- j
    sL[i] <- L
    pred[j] <- i
    comp[find(j)] <- find(i)
    used[[length(used) + 1]] <- c(i, j, L, E[k, 2])
  }
  contigs <- character(0)
  layouts <- list()
  for (s in keep) {
    if (!is.na(pred[s])) next
    lay <- matrix(0, 0, 2)
    off <- 0
    cur <- s
    repeat {
      lay <- rbind(lay, c(cur, off))
      if (is.na(succ[cur])) break
      off <- off + nchar(reads[cur]) - sL[cur]
      cur <- succ[cur]
    }
    width <- max(lay[, 2] + nchar(reads[lay[, 1]]))
    sq <- character(width)
    for (col in seq_len(width) - 1) {
      got <- character(0)
      for (r in seq_len(nrow(lay))) {
        k <- lay[r, 1]
        o <- lay[r, 2]
        if (o <= col && col < o + nchar(reads[k])) got <- c(got, ch[[k]][col - o + 1])
      }
      tb <- table(got)
      nm <- names(tb)
      sq[col + 1] <- nm[order(-as.vector(tb), nm, method = "radix")][1]
    }
    contigs <- c(contigs, paste(sq, collapse = ""))
    layouts[[length(layouts) + 1]] <- lay
  }
  o <- order(-nchar(contigs), contigs, method = "radix")
  list(contigs = contigs[o], layouts = layouts[o], overlaps = used, n_reads_used = length(keep))
}

.bq_overlap <- function(a, b, min_overlap, max_error) {
  la <- length(a)
  top <- min(la, length(b)) - 1
  if (top < min_overlap) return(c(0, 0))
  for (L in top:min_overlap) {
    mm <- 0
    lim <- floor(max_error * L)
    ok <- TRUE
    for (t in seq_len(L)) {
      if (a[la - L + t] != b[t]) {
        mm <- mm + 1
        if (mm > lim) {
          ok <- FALSE
          break
        }
      }
    }
    if (ok) return(c(L, mm))
  }
  c(0, 0)
}

.bq_kd <- c(A = 1.8, R = -4.5, N = -3.5, D = -3.5, C = 2.5, Q = -3.5, E = -3.5, G = -0.4, H = -3.2, I = 4.5,
            L = 3.8, K = -3.9, M = 1.9, F = 2.8, P = -1.6, S = -0.8, T = -0.7, W = -0.9, Y = -1.3, V = 4.2)

.bq_fold <- function(a) {
  h <- 0
  q <- 0
  for (x in a) {
    h <- h + (.bq_kd[[x]] + 4.5) / 9
    q <- q + if (x %in% c("K", "R")) 1 else if (x %in% c("D", "E")) -1 else 0
  }
  n <- length(a)
  c(2.785 * (h / n) - abs(q / n) - 1.151, h / n, q / n)
}

#' @rdname OlcAssembly
#' @export
ProteinDisorder <- function(sequence, window = 51L, min_region = 5L) {
  a <- strsplit(gsub("\\s", "", toupper(sequence)), "")[[1]]
  bad <- setdiff(a, names(.bq_kd))
  if (length(bad)) stop("unknown residues: ", paste(sort(bad), collapse = ""))
  n <- length(a)
  if (n == 0) stop("empty sequence")
  if (window %% 2 == 0 || window < 1) stop("window must be a positive odd integer")
  g <- .bq_fold(a)
  if (n <= window) {
    prof <- rep(g[1], n)
  } else {
    half <- window %/% 2
    cen <- vapply(seq_len(n - window + 1), function(s) .bq_fold(a[s:(s + window - 1)])[1], 0)
    prof <- cen[pmin(pmax(seq_len(n) - 1 - half, 0), n - window) + 1]
  }
  dis <- prof < 0
  regions <- list()
  i <- 1
  while (i <= n) {
    if (dis[i]) {
      j <- i
      while (j + 1 <= n && dis[j + 1]) j <- j + 1
      if (j - i + 1 >= min_region) regions[[length(regions) + 1]] <- c(i, j)
      i <- j + 1
    } else {
      i <- i + 1
    }
  }
  list(fold_index = g[1], mean_hydropathy = g[2], mean_net_charge = g[3], profile = prof, disordered = dis,
       regions = regions, fraction_disordered = sum(dis) / n)
}
