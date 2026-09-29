.dssp_dist <- function(a, b) {
  d <- a - b
  sqrt(d[1] * d[1] + d[2] * d[2] + d[3] * d[3])
}

#' DSSP secondary-structure assignment from backbone coordinates
#'
#' \code{DsspAssign} assigns the DSSP 8-state secondary structure (Kabsch and
#' Sander 1983, with the DSSP 2.2 rules) from per-residue backbone atoms
#' \code{N, CA, C, O}. The amide H is placed 1 A from N along the preceding
#' C=O direction (at N for a chain start); prolines donate no H-bond. Energies
#' (\code{DsspHbondEnergy}, \code{E = 0.084 * 332 (1/r_ON + 1/r_CH - 1/r_OH -
#' 1/r_CN)} kcal/mol, floored at -9.9) are computed for residue pairs with
#' CA-CA below 9 A; each donor keeps its two lowest-energy acceptors and a
#' bond needs E below -0.5. A chain break is a C-N distance above 2.5 A or a
#' change of chain. n-turns (n = 3, 4, 5) give H (two consecutive 4-turns),
#' G and I where free (\code{prefer_pi} lets I overwrite H, as DSSP 2.2 and
#' later do); parallel and antiparallel bridges form ladders, joined across
#' bulges; a lone bridge is B, a ladder residue E; T marks residues inside
#' turns, S bends (CA(i-2), CA(i), CA(i+2) angle above 70 degrees), and
#' \code{-} the rest. Identical to the Python arm \code{morie.fn.protdssp}.
#'
#' @param coords List of residues, each a list (or 4 x 3 matrix) of the
#'   N, CA, C and O coordinates in angstrom.
#' @param sequence Optional one-letter sequence (only prolines matter).
#' @param chain Optional chain identifier per residue.
#' @param prefer_pi Let pi helices override alpha helices.
#' @param n,h,c,o Coordinates of the donor N and H and the acceptor C and O.
#' @return \code{DsspAssign}: list with \code{ss} (string over H, B, E, G,
#'   I, T, S and -), \code{hbonds} (per donor, up to two acceptor/energy
#'   pairs, acceptors 1-based), \code{bridges} and \code{bend}.
#'   \code{DsspHbondEnergy}: the energy in kcal/mol.
#' @references Kabsch, W. and Sander, C. (1983). Dictionary of protein
#'   secondary structure: pattern recognition of hydrogen-bonded and
#'   geometrical features. Biopolymers 22, 2577-2637.
#'
#'   McGibbon, R. T. et al. (2015). MDTraj: a modern open library for the
#'   analysis of molecular dynamics trajectories. Biophysical Journal 109,
#'   1528-1532.
#' @examples
#' DsspHbondEnergy(c(0, 0, 0), c(1, 0, 0), c(4.2, 0, 0), c(3, 0, 0))
#' res <- lapply(0:11, function(i) {
#'   t <- 100 * i * pi / 180
#'   list(c(1.55 * cos(t - 0.45), 1.55 * sin(t - 0.45), 1.5 * i - 0.85),
#'        c(2.3 * cos(t), 2.3 * sin(t), 1.5 * i),
#'        c(1.65 * cos(t + 0.42), 1.65 * sin(t + 0.42), 1.5 * i + 0.55),
#'        c(1.95 * cos(t + 0.62), 1.95 * sin(t + 0.62), 1.5 * i + 1.7))
#' })
#' DsspAssign(res)$ss
#' @export
DsspAssign <- function(coords, sequence = NULL, chain = NULL, prefer_pi = TRUE) {
  n <- length(coords)
  get <- function(k) lapply(coords, function(r) as.numeric(if (is.matrix(r)) r[k, ] else r[[k]]))
  N <- get(1)
  CA <- get(2)
  C <- get(3)
  O <- get(4)
  if (is.null(chain)) chain <- rep(0, n)
  pro <- if (is.null(sequence)) rep(FALSE, n) else toupper(strsplit(sequence, "")[[1]][seq_len(n)]) == "P"
  brk <- rep(FALSE, n)
  for (i in seq_len(n)[-1]) brk[i] <- chain[i] != chain[i - 1] || .dssp_dist(C[[i - 1]], N[[i]]) > 2.5
  nobreak <- function(a, b) !any(brk[seq_len(b - a) + a])
  H <- vector("list", n)
  for (i in seq_len(n)) {
    if (i > 1 && !brk[i]) {
      u <- C[[i - 1]] - O[[i - 1]]
      d <- sqrt(u[1] * u[1] + u[2] * u[2] + u[3] * u[3])
      H[[i]] <- N[[i]] + u / d
    } else {
      H[[i]] <- N[[i]]
    }
  }
  acc <- matrix(-1, n, 2)
  en <- matrix(0, n, 2)
  store <- function(donor, acceptor) {
    if (pro[donor]) return(invisible())
    e <- DsspHbondEnergy(N[[donor]], H[[donor]], C[[acceptor]], O[[acceptor]])
    if (e >= -0.5) return(invisible())
    if (acc[donor, 1] < 0 || e < en[donor, 1]) {
      acc[donor, 2] <<- acc[donor, 1]
      en[donor, 2] <<- en[donor, 1]
      acc[donor, 1] <<- acceptor
      en[donor, 1] <<- e
    } else if (acc[donor, 2] < 0 || e < en[donor, 2]) {
      acc[donor, 2] <<- acceptor
      en[donor, 2] <<- e
    }
  }
  for (i in seq_len(n)) {
    for (j in seq_len(n - i) + i) {
      if (.dssp_dist(CA[[i]], CA[[j]]) < 9) {
        store(i, j)
        if (j != i + 1) store(j, i)
      }
    }
  }
  bond <- function(donor, acceptor) acc[donor, 1] == acceptor || acc[donor, 2] == acceptor
  ss <- rep("-", n)
  bridges <- list()
  for (i in seq_len(max(n - 5, 0)) + 1) {
    for (j in seq_len(max(n - i - 3, 0)) + i + 2) {
      if (!(nobreak(i - 1, i + 1) && nobreak(j - 1, j + 1))) next
      a <- i - 1
      b <- i
      c <- i + 1
      d <- j - 1
      e <- j
      f <- j + 1
      if ((bond(c, e) && bond(e, a)) || (bond(f, b) && bond(b, d))) {
        typ <- "parallel"
      } else if ((bond(c, d) && bond(f, a)) || (bond(e, b) && bond(b, e))) {
        typ <- "antiparallel"
      } else {
        next
      }
      found <- FALSE
      for (k in seq_along(bridges)) {
        br <- bridges[[k]]
        if (typ != br$type || i != br$i[length(br$i)] + 1) next
        if (typ == "parallel" && br$j[length(br$j)] + 1 == j) {
          bridges[[k]]$i <- c(br$i, i)
          bridges[[k]]$j <- c(br$j, j)
          found <- TRUE
          break
        }
        if (typ == "antiparallel" && br$j[1] - 1 == j) {
          bridges[[k]]$i <- c(br$i, i)
          bridges[[k]]$j <- c(j, br$j)
          found <- TRUE
          break
        }
      }
      if (!found) bridges[[length(bridges) + 1]] <- list(type = typ, i = i, j = j, ci = chain[i], cj = chain[j])
    }
  }
  if (length(bridges)) bridges <- bridges[order(vapply(bridges, function(br) br$i[1], numeric(1)), seq_along(bridges))]
  k <- 1
  while (k <= length(bridges)) {
    m <- k + 1
    while (m <= length(bridges)) {
      bi <- bridges[[k]]
      bj <- bridges[[m]]
      ibi <- bi$i[1]
      iei <- bi$i[length(bi$i)]
      jbi <- bi$j[1]
      jei <- bi$j[length(bi$j)]
      ibj <- bj$i[1]
      iej <- bj$i[length(bj$i)]
      jbj <- bj$j[1]
      jej <- bj$j[length(bj$j)]
      # DSSP does this arithmetic on unsigned integers: a negative difference never passes a "<" test
      if (bi$type != bj$type || bi$ci != bj$ci || bi$cj != bj$cj || ibj < iei || ibj - iei >= 6 ||
            (iei >= ibj && ibi <= iej)) {
        m <- m + 1
        next
      }
      g <- if (bi$type == "parallel") jbj - jei else jbi - jej
      bulge <- g >= 0 && ((g < 6 && ibj - iei < 3) || g < 3)
      if (bulge) {
        bridges[[k]]$i <- c(bi$i, bj$i)
        bridges[[k]]$j <- if (bi$type == "parallel") c(bi$j, bj$j) else c(bj$j, bi$j)
        bridges[[m]] <- NULL
      } else {
        m <- m + 1
      }
    }
    k <- k + 1
  }
  for (br in bridges) {
    code <- if (length(br$i) > 1) "E" else "B"
    for (r in c(br$i[1]:br$i[length(br$i)], br$j[1]:br$j[length(br$j)])) if (ss[r] != "E") ss[r] <- code
  }
  flag <- matrix("none", n, 5)
  for (s in 3:5) {
    for (i in seq_len(max(n - s, 0))) {
      if (nobreak(i, i + s) && bond(i + s, i)) {
        flag[i + s, s] <- "end"
        for (j in seq_len(s - 1) + i) if (flag[j, s] == "none") flag[j, s] <- "middle"
        flag[i, s] <- if (flag[i, s] == "end") "startend" else "start"
      }
    }
  }
  start <- function(s, i) flag[i, s] %in% c("start", "startend")
  for (i in seq_len(max(n - 5, 0)) + 1) {
    if (start(4, i) && start(4, i - 1)) ss[i:(i + 3)] <- "H"
  }
  for (i in seq_len(max(n - 4, 0)) + 1) {
    if (start(3, i) && start(3, i - 1) && all(ss[i:(i + 2)] %in% c("-", "G"))) ss[i:(i + 2)] <- "G"
  }
  free5 <- if (prefer_pi) c("-", "I", "H") else c("-", "I")
  for (i in seq_len(max(n - 6, 0)) + 1) {
    if (start(5, i) && start(5, i - 1) && all(ss[i:(i + 4)] %in% free5)) ss[i:(i + 4)] <- "I"
  }
  bend <- rep(FALSE, n)
  for (i in seq_len(max(n - 4, 0)) + 2) {
    if (nobreak(i - 2, i + 2)) {
      u <- CA[[i]] - CA[[i - 2]]
      v <- CA[[i + 2]] - CA[[i]]
      cs <- (u[1] * v[1] + u[2] * v[2] + u[3] * v[3]) /
        sqrt((u[1] * u[1] + u[2] * u[2] + u[3] * u[3]) * (v[1] * v[1] + v[2] * v[2] + v[3] * v[3]))
      bend[i] <- acos(max(-1, min(1, cs))) * 180 / pi > 70
    }
  }
  for (i in seq_len(max(n - 2, 0)) + 1) {
    if (ss[i] == "-") {
      turn <- FALSE
      for (s in 3:5) for (k in seq_len(s - 1)) if (i - k >= 1 && start(s, i - k)) turn <- TRUE
      if (turn) {
        ss[i] <- "T"
      } else if (bend[i]) {
        ss[i] <- "S"
      }
    }
  }
  hb <- lapply(seq_len(n), function(i) {
    keep <- acc[i, ] > 0
    list(acceptor = acc[i, keep], energy = en[i, keep])
  })
  list(ss = paste(ss, collapse = ""), hbonds = hb,
       bridges = lapply(bridges, function(br) list(type = br$type, i = br$i, j = br$j)), bend = bend)
}

#' @rdname DsspAssign
#' @export
DsspHbondEnergy <- function(n, h, c, o) {
  r_on <- .dssp_dist(o, n)
  r_ch <- .dssp_dist(c, h)
  r_oh <- .dssp_dist(o, h)
  r_cn <- .dssp_dist(c, n)
  if (min(r_on, r_ch, r_oh, r_cn) < 0.5) return(-9.9)
  e <- 27.888 * (1 / r_on + 1 / r_ch - 1 / r_oh - 1 / r_cn)
  if (e < -9.9) -9.9 else e
}
