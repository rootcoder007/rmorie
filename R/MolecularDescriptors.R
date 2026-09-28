#' Molecular descriptors from SMILES
#'
#' \code{SmilesMolecularWeight}: average molecular weight with implicit
#' hydrogens. \code{SmilesHba}, \code{SmilesHbd}: Lipinski N+O acceptors and
#' NH+OH donors. \code{SmilesRotatableBonds}: Veber rotatable bonds (amide
#' C-N excluded). \code{SmilesTpsa}: Ertl topological polar surface area.
#' \code{LipinskiDescriptors}: all of them. SMILES are read by the Avalon
#' parser. Identical to the Python arm \code{morie.fn.moldesc}.
#'
#' @param smiles SMILES string.
#' @param exclude_amide Exclude amide C-N bonds.
#' @return Number or list.
#' @references Ertl, P., Rohde, B. and Selzer, P. (2000). Fast calculation of
#'   molecular polar surface area as a sum of fragment-based contributions.
#'   Journal of Medicinal Chemistry 43, 3714-3717.
#'
#'   Lipinski, C. A., Lombardo, F., Dominy, B. W. and Feeney, P. J. (1997).
#'   Experimental and computational approaches to estimate solubility and
#'   permeability. Advanced Drug Delivery Reviews 23, 3-25.
#'
#'   Veber, D. F. et al. (2002). Molecular properties that influence the oral
#'   bioavailability of drug candidates. Journal of Medicinal Chemistry 45,
#'   2615-2623.
#' @examples
#' SmilesMolecularWeight("CC(=O)Oc1ccccc1C(=O)O")
#' SmilesTpsa("CC(=O)Oc1ccccc1C(=O)O")$tpsa
#' @export
SmilesMolecularWeight <- function(smiles) {
  m <- .md_mol(smiles)
  .md_ss(.md_mass[m$el]) + .md_ss(m$hs) * .md_mass[["H"]]
}

.md_mass <- c(H = 1.008, B = 10.812, C = 12.011, N = 14.007, O = 15.999, F = 18.998, P = 30.974, S = 32.067,
              Cl = 35.453, Br = 79.904, I = 126.904)

.md_ss <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  unname(s)
}

.md_mol <- function(smiles) {
  p <- morie_avalon_parse(smiles)
  hs <- morie_avalon_h(p$el, p$arom, p$chg, p$hexp, p$bonds)
  B <- if (length(p$bonds)) do.call(rbind, p$bonds) else matrix(0, 0, 3)
  n <- length(p$el)
  ringarom <- vapply(seq_len(nrow(B)), function(k) {
    B[k, 3] == 1 && p$arom[B[k, 1] + 1] == 1 && p$arom[B[k, 2] + 1] == 1 && .md_ringbond(n, B, k)
  }, TRUE)
  B[ringarom, 3] <- 4
  list(el = p$el, arom = p$arom, chg = p$chg, hs = hs, B = B)
}

#' @rdname SmilesMolecularWeight
#' @export
SmilesHba <- function(smiles) sum(.md_mol(smiles)$el %in% c("N", "O"))

#' @rdname SmilesMolecularWeight
#' @export
SmilesHbd <- function(smiles) {
  m <- .md_mol(smiles)
  as.integer(sum(m$hs[m$el %in% c("N", "O")]))
}

.md_ringbond <- function(n, B, k) {
  a <- B[k, 1] + 1
  b <- B[k, 2] + 1
  adj <- vector("list", n)
  for (j in seq_len(nrow(B))) {
    if (j == k) next
    u <- B[j, 1] + 1
    v <- B[j, 2] + 1
    adj[[u]] <- c(adj[[u]], v)
    adj[[v]] <- c(adj[[v]], u)
  }
  seen <- a
  stack <- a
  while (length(stack)) {
    u <- stack[length(stack)]
    stack <- stack[-length(stack)]
    for (v in adj[[u]]) {
      if (!v %in% seen) {
        seen <- c(seen, v)
        stack <- c(stack, v)
      }
    }
  }
  b %in% seen
}

#' @rdname SmilesMolecularWeight
#' @export
SmilesRotatableBonds <- function(smiles, exclude_amide = TRUE) {
  m <- .md_mol(smiles)
  el <- m$el
  B <- m$B
  n <- length(el)
  deg <- tabulate(c(B[, 1], B[, 2]) + 1, n)
  carbonyl <- function(c) {
    if (el[c + 1] != "C") return(FALSE)
    for (j in seq_len(nrow(B))) {
      if (B[j, 3] == 2 && (B[j, 1] == c || B[j, 2] == c)) {
        o <- if (B[j, 1] == c) B[j, 2] else B[j, 1]
        if (el[o + 1] == "O") return(TRUE)
      }
    }
    FALSE
  }
  cnt <- 0L
  for (k in seq_len(nrow(B))) {
    a <- B[k, 1]
    b <- B[k, 2]
    if (B[k, 3] != 1 || deg[a + 1] < 2 || deg[b + 1] < 2 || .md_ringbond(n, B, k)) next
    if (exclude_amide && ((el[a + 1] == "N" && carbonyl(b)) || (el[b + 1] == "N" && carbonyl(a)))) next
    cnt <- cnt + 1L
  }
  cnt
}

#' @rdname SmilesMolecularWeight
#' @export
SmilesTpsa <- function(smiles) {
  m <- .md_mol(smiles)
  el <- m$el
  B <- m$B
  n <- length(el)
  ns <- nd <- nt <- na <- nb <- integer(n)
  nbr <- vector("list", n)
  for (k in seq_len(nrow(B))) {
    a <- B[k, 1] + 1
    b <- B[k, 2] + 1
    o <- B[k, 3]
    nb[c(a, b)] <- nb[c(a, b)] + 1L
    nbr[[a]] <- c(nbr[[a]], b)
    nbr[[b]] <- c(nbr[[b]], a)
    if (o == 4) na[c(a, b)] <- na[c(a, b)] + 1L else if (o == 1) ns[c(a, b)] <- ns[c(a, b)] + 1L else if (o == 2) {
      nd[c(a, b)] <- nd[c(a, b)] + 1L
    } else {
      nt[c(a, b)] <- nt[c(a, b)] + 1L
    }
  }
  contrib <- numeric(n)
  for (i in seq_len(n)) {
    e <- el[i]
    if (!e %in% c("N", "O")) next
    h <- m$hs[i]
    cg <- m$chg[i]
    r3 <- FALSE
    for (j in nbr[[i]]) for (k in nbr[[i]]) if (j < k && k %in% nbr[[j]]) r3 <- TRUE
    kk <- nb[i]
    t <- -1
    if (e == "N") {
      if (kk == 1) {
        t <- if (h == 0 && cg == 0 && nt[i] == 1) 23.79 else if (h == 1 && cg == 0 && nd[i] == 1) 23.85 else
          if (h == 2 && cg == 0 && ns[i] == 1) 26.02 else if (h == 2 && cg == 1 && nd[i] == 1) 25.59 else
            if (h == 3 && cg == 1 && ns[i] == 1) 27.64 else -1
      } else if (kk == 2) {
        t <- if (h == 0 && cg == 0 && ns[i] == 1 && nd[i] == 1) 12.36 else
          if (h == 0 && cg == 0 && nt[i] == 1 && nd[i] == 1) 13.60 else
            if (h == 1 && cg == 0 && ns[i] == 2 && r3) 21.94 else
              if (h == 1 && cg == 0 && ns[i] == 2 && !r3) 12.03 else
                if (h == 0 && cg == 1 && nt[i] == 1 && ns[i] == 1) 4.36 else
                  if (h == 1 && cg == 1 && nd[i] == 1 && ns[i] == 1) 13.97 else
                    if (h == 2 && cg == 1 && ns[i] == 2) 16.61 else
                      if (h == 0 && cg == 0 && na[i] == 2) 12.89 else
                        if (h == 1 && cg == 0 && na[i] == 2) 15.79 else
                          if (h == 1 && cg == 1 && na[i] == 2) 14.14 else -1
      } else if (kk == 3) {
        t <- if (h == 0 && cg == 0 && ns[i] == 3 && r3) 3.01 else
          if (h == 0 && cg == 0 && ns[i] == 3 && !r3) 3.24 else
            if (h == 0 && cg == 0 && ns[i] == 1 && nd[i] == 2) 11.68 else
              if (h == 0 && cg == 1 && ns[i] == 2 && nd[i] == 1) 3.01 else
                if (h == 1 && cg == 1 && ns[i] == 3) 4.44 else
                  if (h == 0 && cg == 0 && na[i] == 3) 4.41 else
                    if (h == 0 && cg == 0 && ns[i] == 1 && na[i] == 2) 4.93 else
                      if (h == 0 && cg == 0 && nd[i] == 1 && na[i] == 2) 8.39 else
                        if (h == 0 && cg == 1 && na[i] == 3) 4.10 else
                          if (h == 0 && cg == 1 && ns[i] == 1 && na[i] == 2) 3.88 else -1
      } else if (kk == 4 && h == 0 && ns[i] == 4 && cg == 1) {
        t <- 0
      }
      if (t < 0) t <- max(30.5 - kk * 8.2 + h * 1.5, 0)
    } else {
      if (kk == 1) {
        t <- if (h == 0 && cg == 0 && nd[i] == 1) 17.07 else if (h == 1 && cg == 0 && ns[i] == 1) 20.23 else
          if (h == 0 && cg == -1 && ns[i] == 1) 23.06 else -1
      } else if (kk == 2) {
        t <- if (h == 0 && cg == 0 && ns[i] == 2 && r3) 12.53 else
          if (h == 0 && cg == 0 && ns[i] == 2 && !r3) 9.23 else
            if (h == 0 && cg == 0 && na[i] == 2) 13.14 else -1
      }
      if (t < 0) t <- max(28.5 - kk * 8.6 + h * 1.5, 0)
    }
    contrib[i] <- t
  }
  list(tpsa = .md_ss(contrib), contributions = contrib)
}

#' @rdname SmilesMolecularWeight
#' @export
LipinskiDescriptors <- function(smiles) {
  list(molecular_weight = SmilesMolecularWeight(smiles), hba = SmilesHba(smiles), hbd = SmilesHbd(smiles),
       rotatable_bonds = SmilesRotatableBonds(smiles), tpsa = SmilesTpsa(smiles)$tpsa)
}
