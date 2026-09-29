#' SMARTS search, Crippen logP and PAINS alerts
#'
#' \code{SmartsMatch}: SMARTS substructure search on a SMILES molecule read
#' with \code{\link{morie_avalon_parse}} (implicit hydrogens to the lowest
#' allowed valence, ring bonds as non-bridges, smallest set of smallest
#' rings, Hueckel 4n + 2 perception of rings written in Kekule form).
#' \code{ClogpEstimate}: Wildman-Crippen logP and molar refractivity by
#' first-match atom typing with explicit hydrogens (the RDKit Crippen
#' table). \code{PainsFilter}: the 480 PAINS alerts (Baell and Holloway
#' 2010) in the RDKit/WEHI SMARTS form with query hydrogens merged into
#' at-least-k H counts. Identical to the Python arm
#' \code{morie.fn.chemsmarts} (atom indices 1-based here, 0-based there).
#'
#' @param smiles SMILES string.
#' @param smarts SMARTS pattern.
#' @param merge_hs Fold query hydrogens into H-count queries (RDKit mergeHs).
#' @return List. SmartsMatch: matched, anchors, n_atoms. ClogpEstimate:
#'   logp, mr, types, contributions. PainsFilter: flagged, names, n_alerts.
#' @references Wildman, S. A. and Crippen, G. M. (1999). Prediction of
#'   physicochemical parameters by atomic contributions. Journal of Chemical
#'   Information and Computer Sciences 39, 868-873.
#'
#'   Baell, J. B. and Holloway, G. A. (2010). New substructure filters for
#'   removal of pan assay interference compounds (PAINS) from screening
#'   libraries and for their exclusion in bioassays. Journal of Medicinal
#'   Chemistry 53, 2719-2740.
#'
#'   Daylight Chemical Information Systems. SMARTS - A Language for
#'   Describing Molecular Patterns. Daylight Theory Manual.
#' @examples
#' SmartsMatch("CC(=O)Oc1ccccc1C(=O)O", "[CX3](=O)[OX2H1]")$anchors
#' ClogpEstimate("c1ccccc1O")$logp
#' PainsFilter("Oc1ccc(CC)cc1O")$names
#' @export
SmartsMatch <- function(smiles, smarts, merge_hs = FALSE) {
  m <- .cs_molecule(smiles)
  q <- .cs_parse(smarts)
  if (merge_hs) q <- .cs_merge_hs(q)
  cache <- new.env()
  anchors <- which(vapply(seq_len(m$n), function(i) .cs_embed(q, m, i, cache), TRUE))
  list(matched = length(anchors) > 0, anchors = anchors, n_atoms = m$n)
}

.cs_symbols <- strsplit(paste(
  "H He Li Be B C N O F Ne Na Mg Al Si P S Cl Ar K Ca Sc Ti V Cr Mn Fe Co Ni Cu Zn Ga Ge As Se Br Kr",
  "Rb Sr Y Zr Nb Mo Tc Ru Rh Pd Ag Cd In Sn Sb Te I Xe Cs Ba La Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm Yb",
  "Lu Hf Ta W Re Os Ir Pt Au Hg Tl Pb Bi Po At Rn"
), " ")[[1]]
.cs_Z <- stats::setNames(seq_along(.cs_symbols), .cs_symbols)
.cs_aromsym <- c(c = 6, n = 7, o = 8, s = 16, p = 15, b = 5, se = 34, as = 33)
.cs_val <- list("1" = 1, "5" = 3, "6" = 4, "7" = 3, "8" = 2, "9" = 1, "14" = 4, "15" = c(3, 5), "16" = c(2, 4, 6),
                "17" = 1, "35" = 1, "53" = 1)

.cs_bridges <- function(n, B) {
  vapply(seq_len(nrow(B)), function(k) {
    adj <- vector("list", n)
    for (j in seq_len(nrow(B))) {
      if (j == k) next
      adj[[B[j, 1]]] <- c(adj[[B[j, 1]]], B[j, 2])
      adj[[B[j, 2]]] <- c(adj[[B[j, 2]]], B[j, 1])
    }
    seen <- B[k, 1]
    stack <- B[k, 1]
    while (length(stack)) {
      u <- stack[length(stack)]
      stack <- stack[-length(stack)]
      for (v in adj[[u]]) {
        if (!(v %in% seen)) {
          seen <- c(seen, v)
          stack <- c(stack, v)
        }
      }
    }
    B[k, 2] %in% seen
  }, TRUE)
}

.cs_rings <- function(n, B, ringb) {
  nb <- nrow(B)
  adj <- vector("list", n)
  for (k in seq_len(nb)) {
    adj[[B[k, 1]]] <- rbind(adj[[B[k, 1]]], c(B[k, 2], k))
    adj[[B[k, 2]]] <- rbind(adj[[B[k, 2]]], c(B[k, 1], k))
  }
  cands <- list()
  for (k in seq_len(nb)) {
    if (!ringb[k]) next
    a <- B[k, 1]
    b <- B[k, 2]
    pv <- rep(NA_real_, n)
    pk <- rep(NA_real_, n)
    seen <- rep(FALSE, n)
    seen[a] <- TRUE
    q <- a
    h <- 1
    while (h <= length(q) && !seen[b]) {
      u <- q[h]
      h <- h + 1
      for (r in seq_len(nrow(adj[[u]]))) {
        v <- adj[[u]][r, 1]
        kk <- adj[[u]][r, 2]
        if (kk != k && !seen[v]) {
          seen[v] <- TRUE
          pv[v] <- u
          pk[v] <- kk
          q <- c(q, v)
        }
      }
    }
    path <- b
    edges <- k
    v <- b
    while (v != a) {
      edges <- c(edges, pk[v])
      path <- c(path, pv[v])
      v <- pv[v]
    }
    cands[[length(cands) + 1]] <- list(path = rev(path), edges = edges)
  }
  if (!length(cands)) return(list())
  o <- order(vapply(cands, function(cc) length(cc$path), 0), seq_along(cands))
  rings <- list()
  pivots <- list()
  for (cc in cands[o]) {
    vec <- rep(FALSE, nb)
    vec[cc$edges] <- TRUE
    while (any(vec)) {
      top <- max(which(vec))
      key <- as.character(top)
      if (!is.null(pivots[[key]])) {
        vec <- xor(vec, pivots[[key]])
      } else {
        pivots[[key]] <- vec
        rings[[length(rings) + 1]] <- list(path = cc$path, edges = sort(cc$edges))
        break
      }
    }
  }
  rings
}

.cs_hcount <- function(z, arom, chg, hexp, B) {
  n <- length(z)
  used <- numeric(n)
  for (k in seq_len(nrow(B))) {
    w <- if (B[k, 3] == 4) 1 else B[k, 3]
    used[B[k, 1]] <- used[B[k, 1]] + w
    used[B[k, 2]] <- used[B[k, 2]] + w
  }
  vapply(seq_len(n), function(i) {
    if (hexp[i] >= 0) return(as.numeric(hexp[i]))
    vals <- if (z[i] %in% c(5, 6, 7, 8, 15, 16)) .cs_val[[as.character(z[i] - chg[i])]] else if (chg[i] == 0) .cs_val[[as.character(z[i])]] else NULL
    if (is.null(vals)) return(0)
    tot <- used[i] + (if (arom[i] && !(z[i] %in% c(8, 16))) 1 else 0)
    for (v in vals) if (v >= tot) return(v - tot)
    0
  }, 0)
}

.cs_more_en <- c(7, 8, 9, 15, 16, 17, 33, 34, 35, 51, 52, 53)

.cs_electrons <- function(i, z, chg, nbr, ringb, hs) {
  nb <- nbr[[i]]
  dbl <- nb[nb[, 3] == 2, , drop = FALSE]
  deg <- nrow(nb) + hs[i]
  if (any(nb[, 3] == 3) || nrow(dbl) > 1 || deg > 3) return(NA)
  if (nrow(dbl)) {
    if (ringb[dbl[1, 2]]) return(1)
    if (z[i] != 6) return(NA)
    return(if (z[dbl[1, 1]] %in% .cs_more_en) 0 else 1)
  }
  if (z[i] == 6) return(if (chg[i] == -1) 2 else if (chg[i] == 1) 0 else NA)
  if (z[i] == 7) return(if ((chg[i] == 0 && deg == 3) || chg[i] == -1) 2 else NA)
  if (z[i] %in% c(8, 16, 34)) return(if (chg[i] == 0 && deg == 2) 2 else NA)
  if (z[i] == 5) return(if (deg == 3) 0 else NA)
  NA
}

.cs_kekulize <- function(n, z, arom, chg, hs, B, ringb) {
  nb <- nrow(B)
  order <- B[, 3]
  cand <- vapply(seq_len(nb), function(k) arom[B[k, 1]] && arom[B[k, 2]] && ringb[k] && B[k, 3] %in% c(1, 4), TRUE)
  cur <- hs
  for (k in seq_len(nb)) {
    w <- if (cand[k] || B[k, 3] == 4) 1 else B[k, 3]
    cur[B[k, 1]] <- cur[B[k, 1]] + w
    cur[B[k, 2]] <- cur[B[k, 2]] + w
  }
  need <- vapply(seq_len(n), function(i) {
    if (!arom[i]) return(FALSE)
    vals <- if (z[i] %in% c(5, 6, 7, 8, 15, 16)) .cs_val[[as.character(z[i] - chg[i])]] else .cs_val[[as.character(z[i])]]
    length(vals) > 0 && !(cur[i] %in% vals) && ((cur[i] + 1) %in% vals)
  }, TRUE)
  adj <- lapply(seq_len(n), function(i) matrix(0, 0, 2))
  for (pref in c(4, 1)) {
    for (k in seq_len(nb)) {
      if (cand[k] && B[k, 3] == pref) {
        adj[[B[k, 1]]] <- rbind(adj[[B[k, 1]]], c(B[k, 2], k))
        adj[[B[k, 2]]] <- rbind(adj[[B[k, 2]]], c(B[k, 1], k))
      }
    }
  }
  st <- new.env()
  st$mate <- rep(0, n)
  st$dbl <- rep(FALSE, nb)
  todo <- which(need)
  solve <- function(t) {
    while (t <= length(todo) && st$mate[todo[t]] > 0) t <- t + 1
    if (t > length(todo)) return(TRUE)
    u <- todo[t]
    for (r in seq_len(nrow(adj[[u]]))) {
      v <- adj[[u]][r, 1]
      k <- adj[[u]][r, 2]
      if (need[v] && st$mate[v] == 0) {
        st$mate[u] <- v
        st$mate[v] <- u
        st$dbl[k] <- TRUE
        if (solve(t + 1)) return(TRUE)
        st$mate[u] <- 0
        st$mate[v] <- 0
        st$dbl[k] <- FALSE
      }
    }
    FALSE
  }
  if (!solve(1)) stop("cannot kekulize the aromatic system")
  for (k in seq_len(nb)) {
    if (st$dbl[k]) {
      order[k] <- 2
    } else if (cand[k] || B[k, 3] == 4) {
      order[k] <- 1
    }
  }
  order
}

.cs_molecule <- function(smiles, explicit_h = FALSE) {
  p <- morie_avalon_parse(smiles)
  n <- length(p$el)
  z <- unname(.cs_Z[p$el])
  arom <- p$arom == 1
  chg <- as.numeric(p$chg)
  B <- if (length(p$bonds)) do.call(rbind, p$bonds) + 0 else matrix(0, 0, 3)
  B[, 1:2] <- B[, 1:2] + 1
  hs <- .cs_hcount(z, arom, chg, p$hexp, B)
  ringb <- if (nrow(B)) .cs_bridges(n, B) else logical(0)
  rs <- .cs_rings(n, B, ringb)
  order <- .cs_kekulize(n, z, arom, chg, hs, B, ringb)
  arom <- rep(FALSE, n)
  nbr <- lapply(seq_len(n), function(i) {
    ks <- which(B[, 1] == i | B[, 2] == i)
    if (!length(ks)) return(matrix(0, 0, 3))
    cbind(ifelse(B[ks, 1] == i, B[ks, 2], B[ks, 1]), ks, order[ks])
  })
  ec <- vapply(seq_len(n), function(i) .cs_electrons(i, z, chg, nbr, ringb, hs), 0)
  nr <- length(rs)
  if (nr) {
    fused <- lapply(seq_len(nr), function(a) which(vapply(seq_len(nr), function(b) b != a && length(intersect(rs[[a]]$edges, rs[[b]]$edges)) > 0, TRUE)))
    seen <- character(0)
    frontier <- as.list(seq_len(nr))
    for (size in 1:6) {
      nxt <- list()
      for (sub in frontier) {
        key <- paste(sort(sub), collapse = ",")
        if (key %in% seen) next
        seen <- c(seen, key)
        atoms <- unique(unlist(lapply(sub, function(a) rs[[a]]$path)))
        e <- ec[atoms]
        if (length(sub) > 2) {
          nin <- vapply(atoms, function(v) sum(vapply(sub, function(a) v %in% rs[[a]]$path, TRUE)), 0)
          if (any(nin > 2)) e <- NA
        }
        if (!anyNA(e) && sum(e) %% 4 == 2) {
          arom[atoms] <- TRUE
          ed <- unlist(lapply(sub, function(a) rs[[a]]$edges))
          tb <- table(ed)
          order[as.integer(names(tb)[tb == 1])] <- 4
        }
        if (size < 6) {
          for (a in sub) for (b in fused[[a]]) if (!(b %in% sub)) nxt[[length(nxt) + 1]] <- sort(c(sub, b))
        }
      }
      frontier <- nxt
    }
  }
  if (explicit_h) {
    for (i in seq_len(n)) {
      for (t in seq_len(hs[i])) {
        z <- c(z, 1)
        arom <- c(arom, FALSE)
        chg <- c(chg, 0)
        B <- rbind(B, c(i, length(z), 1))
        order <- c(order, 1)
        ringb <- c(ringb, FALSE)
      }
      hs[i] <- 0
    }
    hs <- c(hs, rep(0, length(z) - n))
  }
  N <- length(z)
  nbr <- lapply(seq_len(N), function(i) {
    ks <- which(B[, 1] == i | B[, 2] == i)
    if (!length(ks)) return(matrix(0, 0, 2))
    cbind(ifelse(B[ks, 1] == i, B[ks, 2], B[ks, 1]), ks)
  })
  nring <- numeric(N)
  smallest <- numeric(N)
  for (r in rs) {
    for (v in r$path) {
      nring[v] <- nring[v] + 1
      if (smallest[v] == 0 || length(r$path) < smallest[v]) smallest[v] <- length(r$path)
    }
  }
  rbcount <- vapply(seq_len(N), function(i) sum(ringb[nbr[[i]][, 2]]), 0)
  val2 <- vapply(seq_len(N), function(i) {
    s <- 2 * hs[i]
    for (k in nbr[[i]][, 2]) s <- s + if (order[k] == 4) 3 else 2 * order[k]
    s
  }, 0)
  hn <- vapply(seq_len(N), function(i) sum(z[nbr[[i]][, 1]] == 1), 0)
  list(z = z, arom = arom, chg = chg, h = hs, hn = hn, nbr = nbr, order = order, ringbond = ringb, nring = nring,
       smallest = smallest, rbcount = rbcount, val2 = val2, n = N)
}

# ---------------------------------------------------------------- SMARTS parsing
.cs_env <- new.env()
.cs_env$next_id <- 0

.cs_match_close <- function(ch, i, op, cl) {
  depth <- 0
  for (j in i:length(ch)) {
    if (ch[j] == op) depth <- depth + 1
    if (ch[j] == cl) {
      depth <- depth - 1
      if (depth == 0) return(j)
    }
  }
  stop("unbalanced SMARTS")
}

.cs_bracket_end <- function(ch, i) {
  depth <- 0
  for (j in i:length(ch)) {
    if (ch[j] %in% c("[", "(")) depth <- depth + 1
    if (ch[j] %in% c("]", ")")) {
      depth <- depth - 1
      if (depth == 0 && ch[j] == "]") return(j)
    }
  }
  stop("unclosed bracket atom")
}

.cs_num <- function(ch, i) {
  j <- i
  while (j <= length(ch) && grepl("^[0-9]$", ch[j])) j <- j + 1
  list(v = if (j > i) as.numeric(paste(ch[i:(j - 1)], collapse = "")) else NULL, j = j)
}

.cs_atom_prim <- function(ch, i, first) {
  c0 <- ch[i]
  nx <- if (i + 1 <= length(ch)) ch[i + 1] else ""
  if (c0 == "$") {
    j <- .cs_match_close(ch, i + 1, "(", ")")
    return(list(node = list(t = "rec", q = .cs_parse(paste(ch[(i + 2):(j - 1)], collapse = ""))), j = j + 1))
  }
  if (c0 == "*") return(list(node = list(t = "true"), j = i + 1))
  if (c0 == "#") {
    r <- .cs_num(ch, i + 1)
    return(list(node = list(t = "z", v = r$v), j = r$j))
  }
  if (grepl("^[0-9]$", c0)) return(list(node = list(t = "true"), j = .cs_num(ch, i)$j))
  if (c0 %in% c("+", "-")) {
    sg <- if (c0 == "+") 1 else -1
    r <- .cs_num(ch, i + 1)
    if (!is.null(r$v)) return(list(node = list(t = "chg", v = sg * r$v), j = r$j))
    q <- sg
    j <- r$j
    while (j <= length(ch) && ch[j] == c0) {
      q <- q + sg
      j <- j + 1
    }
    return(list(node = list(t = "chg", v = q), j = j))
  }
  if (c0 == "@") {
    j <- i + 1
    while (j <= length(ch) && ch[j] == "@") j <- j + 1
    return(list(node = list(t = "true"), j = j))
  }
  if (c0 == "H" && first && (nx == "" || nx %in% c("]", "+", "-", ";", "&", ","))) {
    return(list(node = list(t = "elem", z = 1, ar = FALSE), j = i + 1))
  }
  if (c0 %in% c("H", "D", "X", "v", "R", "r", "x")) {
    r <- .cs_num(ch, i + 1)
    v <- r$v
    node <- switch(c0,
      H = list(t = "h", v = if (is.null(v)) 1 else v),
      D = list(t = "D", v = if (is.null(v)) 1 else v),
      X = list(t = "X", v = if (is.null(v)) 1 else v),
      v = list(t = "v", v = if (is.null(v)) 1 else v),
      R = if (is.null(v)) list(t = "inring") else list(t = "R", v = v),
      r = if (is.null(v)) list(t = "inring") else list(t = "r", v = v),
      x = if (is.null(v)) list(t = "xge") else list(t = "x", v = v)
    )
    return(list(node = node, j = r$j))
  }
  if (c0 == "a") return(list(node = list(t = "arom"), j = i + 1))
  if (c0 == "A") return(list(node = list(t = "aliph"), j = i + 1))
  if (grepl("^[a-z]$", c0)) {
    two <- paste0(c0, nx)
    if (nchar(two) == 2 && two %in% names(.cs_aromsym)) return(list(node = list(t = "elem", z = .cs_aromsym[[two]], ar = TRUE), j = i + 2))
    if (c0 %in% names(.cs_aromsym)) return(list(node = list(t = "elem", z = .cs_aromsym[[c0]], ar = TRUE), j = i + 1))
    stop("unknown SMARTS primitive ", c0)
  }
  if (grepl("^[A-Z]$", c0)) {
    two <- paste0(c0, nx)
    if (grepl("^[a-z]$", nx) && two %in% names(.cs_Z)) return(list(node = list(t = "elem", z = .cs_Z[[two]], ar = FALSE), j = i + 2))
    if (c0 %in% names(.cs_Z)) return(list(node = list(t = "elem", z = .cs_Z[[c0]], ar = FALSE), j = i + 1))
  }
  stop("unknown SMARTS primitive ", c0)
}

.cs_bond_prim <- function(ch, i, first) {
  c0 <- ch[i]
  node <- switch(c0,
    "-" = , "/" = , "\\" = list(t = "single"),
    "=" = list(t = "double"),
    "#" = list(t = "triple"),
    ":" = list(t = "aromatic"),
    "~" = list(t = "true"),
    "@" = list(t = "ringbond"),
    stop("unknown SMARTS bond ", c0)
  )
  list(node = node, j = i + 1)
}

.cs_logic <- function(s, prim) {
  ch <- strsplit(s, "")[[1]]
  st <- new.env()
  st$pos <- 1
  peek <- function() if (st$pos <= length(ch)) ch[st$pos] else ""
  unary <- function() {
    if (peek() == "!") {
      st$pos <- st$pos + 1
      return(list(t = "not", x = unary()))
    }
    r <- prim(ch, st$pos, st$pos == 1)
    st$pos <- r$j
    r$node
  }
  andh <- function() {
    items <- list(unary())
    while (peek() != "" && !(peek() %in% c(",", ";"))) {
      if (peek() == "&") st$pos <- st$pos + 1
      items[[length(items) + 1]] <- unary()
    }
    if (length(items) == 1) items[[1]] else list(t = "and", x = items)
  }
  comma <- function() {
    items <- list(andh())
    while (peek() == ",") {
      st$pos <- st$pos + 1
      items[[length(items) + 1]] <- andh()
    }
    if (length(items) == 1) items[[1]] else list(t = "or", x = items)
  }
  items <- list(comma())
  while (peek() == ";") {
    st$pos <- st$pos + 1
    items[[length(items) + 1]] <- comma()
  }
  out <- if (length(items) == 1) items[[1]] else list(t = "and", x = items)
  if (st$pos != length(ch) + 1) stop("cannot parse SMARTS expression ", s)
  out
}

.cs_default_bond <- list(t = "or", x = list(list(t = "single"), list(t = "aromatic")))
.cs_bond_chars <- c("-", "=", "#", ":", "~", "@", "!", "/", "\\", ",", ";", "&")

.cs_parse <- function(sm) {
  ch <- strsplit(sm, "")[[1]]
  atoms <- list()
  bonds <- list()
  starts <- integer(0)
  stack <- integer(0)
  prev <- 0
  pend <- NULL
  rings <- list()
  i <- 1
  while (i <= length(ch)) {
    c0 <- ch[i]
    if (c0 == "(") {
      stack <- c(stack, prev)
      i <- i + 1
    } else if (c0 == ")") {
      prev <- stack[length(stack)]
      stack <- stack[-length(stack)]
      i <- i + 1
    } else if (c0 == ".") {
      prev <- 0
      i <- i + 1
    } else if (c0 %in% .cs_bond_chars) {
      j <- i
      while (j <= length(ch) && ch[j] %in% .cs_bond_chars) j <- j + 1
      pend <- .cs_logic(paste(ch[i:(j - 1)], collapse = ""), .cs_bond_prim)
      i <- j
    } else if (grepl("^[0-9]$", c0) || c0 == "%") {
      if (c0 == "%") {
        lab <- paste(ch[(i + 1):(i + 2)], collapse = "")
        i <- i + 3
      } else {
        lab <- c0
        i <- i + 1
      }
      if (!is.null(rings[[lab]])) {
        rb <- rings[[lab]]
        bp <- if (!is.null(pend)) pend else if (!is.null(rb$bp)) rb$bp else .cs_default_bond
        bonds[[length(bonds) + 1]] <- list(a = rb$a, b = prev, bp = bp)
        rings[[lab]] <- NULL
      } else {
        rings[[lab]] <- list(a = prev, bp = pend)
      }
      pend <- NULL
    } else {
      if (c0 == "[") {
        j <- .cs_bracket_end(ch, i)
        pred <- .cs_logic(paste(ch[(i + 1):(j - 1)], collapse = ""), .cs_atom_prim)
        i <- j + 1
      } else {
        two <- if (i + 1 <= length(ch)) paste0(c0, ch[i + 1]) else ""
        if (two %in% c("Cl", "Br")) {
          pred <- list(t = "elem", z = .cs_Z[[two]], ar = FALSE)
          i <- i + 2
        } else if (c0 %in% c("B", "C", "N", "O", "S", "P", "F", "I")) {
          pred <- list(t = "elem", z = .cs_Z[[c0]], ar = FALSE)
          i <- i + 1
        } else if (c0 %in% c("c", "n", "o", "s", "p", "b")) {
          pred <- list(t = "elem", z = .cs_aromsym[[c0]], ar = TRUE)
          i <- i + 1
        } else if (c0 == "*") {
          pred <- list(t = "true")
          i <- i + 1
        } else if (c0 == "a") {
          pred <- list(t = "arom")
          i <- i + 1
        } else if (c0 == "A") {
          pred <- list(t = "aliph")
          i <- i + 1
        } else {
          stop("unexpected SMARTS character ", c0)
        }
      }
      atoms[[length(atoms) + 1]] <- pred
      cur <- length(atoms)
      if (prev > 0) {
        bonds[[length(bonds) + 1]] <- list(a = prev, b = cur, bp = if (!is.null(pend)) pend else .cs_default_bond)
      } else {
        starts <- c(starts, cur)
      }
      prev <- cur
      pend <- NULL
    }
  }
  if (length(rings) || length(stack)) stop("unclosed ring or branch in SMARTS")
  .cs_env$next_id <- .cs_env$next_id + 1
  list(atoms = atoms, bonds = bonds, starts = starts, id = .cs_env$next_id)
}

.cs_is_h <- function(p) identical(p, list(t = "z", v = 1)) || identical(p, list(t = "elem", z = 1, ar = FALSE))

.cs_merge_pred <- function(p) {
  if (p$t == "rec") return(list(t = "rec", q = .cs_merge_hs(p$q)))
  if (p$t %in% c("and", "or")) return(list(t = p$t, x = lapply(p$x, .cs_merge_pred)))
  if (p$t == "not") return(list(t = "not", x = .cs_merge_pred(p$x)))
  p
}

.cs_merge_hs <- function(q) {
  atoms <- lapply(q$atoms, .cs_merge_pred)
  na <- length(atoms)
  deg <- numeric(na)
  for (b in q$bonds) {
    deg[b$a] <- deg[b$a] + 1
    deg[b$b] <- deg[b$b] + 1
  }
  drop <- integer(0)
  add <- numeric(na)
  for (b in q$bonds) {
    for (pr in list(c(b$a, b$b), c(b$b, b$a))) {
      h <- pr[1]
      o <- pr[2]
      if (.cs_is_h(atoms[[h]]) && deg[h] == 1 && !.cs_is_h(atoms[[o]]) &&
          (identical(b$bp, .cs_default_bond) || identical(b$bp, list(t = "single")))) {
        drop <- c(drop, h)
        add[o] <- add[o] + 1
      }
    }
  }
  .cs_env$next_id <- .cs_env$next_id + 1
  if (!length(drop)) return(list(atoms = atoms, bonds = q$bonds, starts = q$starts, id = .cs_env$next_id))
  keep <- setdiff(seq_len(na), drop)
  idx <- match(seq_len(na), keep)
  new_atoms <- lapply(keep, function(i) if (add[i] > 0) list(t = "and", x = list(atoms[[i]], list(t = "hge", v = add[i]))) else atoms[[i]])
  new_bonds <- list()
  for (b in q$bonds) if (!(b$a %in% drop) && !(b$b %in% drop)) new_bonds[[length(new_bonds) + 1]] <- list(a = idx[b$a], b = idx[b$b], bp = b$bp)
  st <- idx[q$starts]
  list(atoms = new_atoms, bonds = new_bonds, starts = sort(unique(c(st[!is.na(st)], 1))), id = .cs_env$next_id)
}

# ---------------------------------------------------------------- matching
.cs_atom_ok <- function(p, m, i, cache) {
  switch(p$t,
    and = {
      for (x in p$x) if (!.cs_atom_ok(x, m, i, cache)) return(FALSE)
      TRUE
    },
    or = {
      for (x in p$x) if (.cs_atom_ok(x, m, i, cache)) return(TRUE)
      FALSE
    },
    not = !.cs_atom_ok(p$x, m, i, cache),
    true = TRUE,
    elem = m$z[i] == p$z && m$arom[i] == p$ar,
    z = m$z[i] == p$v,
    arom = m$arom[i],
    aliph = !m$arom[i],
    h = m$h[i] + m$hn[i] == p$v,
    hge = m$h[i] + m$hn[i] >= p$v,
    D = nrow(m$nbr[[i]]) == p$v,
    X = nrow(m$nbr[[i]]) + m$h[i] == p$v,
    v = m$val2[i] == 2 * p$v,
    chg = m$chg[i] == p$v,
    inring = m$nring[i] > 0 || m$rbcount[i] > 0,
    R = if (p$v == 0) m$rbcount[i] == 0 else m$nring[i] == p$v,
    r = m$smallest[i] == p$v,
    x = m$rbcount[i] == p$v,
    xge = m$rbcount[i] >= 1,
    rec = {
      key <- paste(p$q$id, i)
      if (is.null(cache[[key]])) cache[[key]] <- .cs_embed(p$q, m, i, cache)
      cache[[key]]
    },
    stop("unknown predicate ", p$t)
  )
}

.cs_bond_ok <- function(p, m, k) {
  switch(p$t,
    and = {
      for (x in p$x) if (!.cs_bond_ok(x, m, k)) return(FALSE)
      TRUE
    },
    or = {
      for (x in p$x) if (.cs_bond_ok(x, m, k)) return(TRUE)
      FALSE
    },
    not = !.cs_bond_ok(p$x, m, k),
    true = TRUE,
    single = m$order[k] == 1,
    double = m$order[k] == 2,
    triple = m$order[k] == 3,
    aromatic = m$order[k] == 4,
    ringbond = m$ringbond[k],
    stop("unknown bond predicate ", p$t)
  )
}

.cs_embed <- function(q, m, anchor, cache) {
  na <- length(q$atoms)
  qn <- vector("list", na)
  for (b in q$bonds) {
    qn[[b$a]] <- c(qn[[b$a]], list(list(u = b$b, bp = b$bp)))
    qn[[b$b]] <- c(qn[[b$b]], list(list(u = b$a, bp = b$bp)))
  }
  parent <- vapply(seq_len(na), function(v) {
    for (e in qn[[v]]) if (e$u < v) return(e$u)
    0
  }, 0)
  st <- new.env()
  st$mp <- rep(0, na)
  ok_at <- function(v, i) {
    if (i %in% st$mp[seq_len(v - 1)]) return(FALSE)
    if (!.cs_atom_ok(q$atoms[[v]], m, i, cache)) return(FALSE)
    for (e in qn[[v]]) {
      if (e$u < v && st$mp[e$u] > 0) {
        nb <- m$nbr[[i]]
        r <- which(nb[, 1] == st$mp[e$u])
        if (!length(r) || !.cs_bond_ok(e$bp, m, nb[r[1], 2])) return(FALSE)
      }
    }
    TRUE
  }
  rec <- function(v) {
    if (v > na) return(TRUE)
    cands <- if (parent[v] > 0) m$nbr[[st$mp[parent[v]]]][, 1] else if (v == 1 && !is.null(anchor)) anchor else seq_len(m$n)
    for (i in cands) {
      if (ok_at(v, i)) {
        st$mp[v] <- i
        if (rec(v + 1)) {
          st$mp[v] <- 0
          return(TRUE)
        }
        st$mp[v] <- 0
      }
    }
    FALSE
  }
  rec(1)
}

# (type, SMARTS, logP, MR): the RDKit Crippen.cpp table (Wildman and Crippen 1999), in its matching order
.cs_crippen <- data.frame(
  type = c("C1", "C1", "C1", "C2", "C2", "C3", "C3", "C4", "C4", "C5",
           "C6", "C6", "C6", "C6", "C7", "C8", "C9", "C10", "C11", "C12",
           "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22",
           "C23", "C24", "C25", "C26", "C26", "C26", "C26", "C27", "CS", "H1",
           "H2", "H2", "H2", "H3", "H3", "H4", "H4", "HS", "N1", "N2",
           "N3", "N4", "N5", "N6", "N7", "N8", "N8", "N9", "N10", "N11",
           "N12", "N13", "N13", "N13", "N14", "N14", "N14", "NS", "O1", "O2",
           "O3", "O4", "O5", "O5", "O6", "O6", "O12", "O7", "O8", "O9",
           "O9", "O9", "O9", "O9", "O10", "O10", "O10", "O11", "OS", "F",
           "Cl", "Br", "I", "Hal", "Hal", "Hal", "P", "S2", "S2", "S1",
           "S3", "Me1", "Me1", "Me1", "Me1", "Me1", "Me1", "Me2", "Me2", "Me2"),
  smarts = c("[CH4]",
             "[CH3]C",
             "[CH2](C)C",
             "[CH](C)(C)C",
             "[C](C)(C)(C)C",
             "[CH3][N,O,P,S,F,Cl,Br,I]",
             "[CH2X4]([N,O,P,S,F,Cl,Br,I])[A;!#1]",
             "[CH1X4]([N,O,P,S,F,Cl,Br,I])([A;!#1])[A;!#1]",
             "[CH0X4]([N,O,P,S,F,Cl,Br,I])([A;!#1])([A;!#1])[A;!#1]",
             "[C]=[!C;A;!#1]",
             "[CH2]=C",
             "[CH1](=C)[A;!#1]",
             "[CH0](=C)([A;!#1])[A;!#1]",
             "[C](=C)=C",
             "[CX2]#[A;!#1]",
             "[CH3]c",
             "[CH3]a",
             "[CH2X4]a",
             "[CHX4]a",
             "[CH0X4]a",
             "[cH0]-[A;!C;!N;!O;!S;!F;!Cl;!Br;!I;!#1]",
             "[c][#9]",
             "[c][#17]",
             "[c][#35]",
             "[c][#53]",
             "[cH]",
             "[c](:a)(:a):a",
             "[c](:a)(:a)-a",
             "[c](:a)(:a)-C",
             "[c](:a)(:a)-N",
             "[c](:a)(:a)-O",
             "[c](:a)(:a)-S",
             "[c](:a)(:a)=[C,N,O]",
             "[C](=C)(a)[A;!#1]",
             "[C](=C)(c)a",
             "[CH1](=C)a",
             "[C]=c",
             "[CX4][A;!C;!N;!O;!P;!S;!F;!Cl;!Br;!I;!#1]",
             "[#6]",
             "[#1][#6,#1]",
             "[#1]O[CX4,c]",
             "[#1]O[!#6;!#7;!#8;!#16]",
             "[#1][!#6;!#7;!#8]",
             "[#1][#7]",
             "[#1]O[#7]",
             "[#1]OC=[#6,#7,O,S]",
             "[#1]O[O,S]",
             "[#1]",
             "[NH2+0][A;!#1]",
             "[NH+0]([A;!#1])[A;!#1]",
             "[NH2+0]a",
             "[NH1+0]([!#1;A,a])a",
             "[NH+0]=[!#1;A,a]",
             "[N+0](=[!#1;A,a])[!#1;A,a]",
             "[N+0]([A;!#1])([A;!#1])[A;!#1]",
             "[N+0](a)([!#1;A,a])[A;!#1]",
             "[N+0](a)(a)a",
             "[N+0]#[A;!#1]",
             "[NH3,NH2,NH;+,+2,+3]",
             "[n+0]",
             "[n;+,+2,+3]",
             "[NH0;+,+2,+3]([A;!#1])([A;!#1])([A;!#1])[A;!#1]",
             "[NH0;+,+2,+3](=[A;!#1])([A;!#1])[!#1;A,a]",
             "[NH0;+,+2,+3](=[#6])=[#7]",
             "[N;+,+2,+3]#[A;!#1]",
             "[N;-,-2,-3]",
             "[N;+,+2,+3](=[N;-,-2,-3])=N",
             "[#7]",
             "[o]",
             "[OH,OH2]",
             "[O]([A;!#1])[A;!#1]",
             "[O](a)[!#1;A,a]",
             "[O]=[#7,#8]",
             "[OX1;-,-2,-3][#7]",
             "[OX1;-,-2,-2][#16]",
             "[O;-0]=[#16;-0]",
             "[O-]C(=O)",
             "[OX1;-,-2,-3][!#1;!N;!S]",
             "[O]=c",
             "[O]=[CH]C",
             "[O]=C(C)([A;!#1])",
             "[O]=[CH][N,O]",
             "[O]=[CH2]",
             "[O]=[CX2]=O",
             "[O]=[CH]c",
             "[O]=C([C,c])[a;!#1]",
             "[O]=C(c)[A;!#1]",
             "[O]=C([!#1;!#6])[!#1;!#6]",
             "[#8]",
             "[#9-0]",
             "[#17-0]",
             "[#35-0]",
             "[#53-0]",
             "[#9,#17,#35,#53;-]",
             "[#53;+,+2,+3]",
             "[+;#3,#11,#19,#37,#55]",
             "[#15]",
             "[S;-,-2,-3,-4,+1,+2,+3,+5,+6]",
             "[S-0]=[N,O,P,S]",
             "[S;A]",
             "[s;a]",
             "[#3,#11,#19,#37,#55]",
             "[#4,#12,#20,#38,#56]",
             "[#5,#13,#31,#49,#81]",
             "[#14,#32,#50,#82]",
             "[#33,#51,#83]",
             "[#34,#52,#84]",
             "[#21,#22,#23,#24,#25,#26,#27,#28,#29,#30]",
             "[#39,#40,#41,#42,#43,#44,#45,#46,#47,#48]",
             "[#72,#73,#74,#75,#76,#77,#78,#79,#80]"),
  logp = c(0.1441, 0.1441, 0.1441, 0, 0, -0.2035, -0.2035, -0.2051, -0.2051, -0.2783,
           0.1551, 0.1551, 0.1551, 0.1551, 0.0017, 0.08452, -0.1444, -0.0516, 0.1193, -0.0967,
           -0.5443, 0, 0.245, 0.198, 0, 0.1581, 0.2955, 0.2713, 0.136, 0.4619,
           0.5437, 0.1893, -0.8186, 0.264, 0.264, 0.264, 0.264, 0.2148, 0.08129, 0.123,
           -0.2677, -0.2677, -0.2677, 0.2142, 0.2142, 0.298, 0.298, 0.1125, -1.019, -0.7096,
           -1.027, -0.5188, 0.08387, 0.1836, -0.3187, -0.4458, -0.4458, 0.01508, -1.95, -0.3239,
           -1.119, -0.3396, -0.3396, -0.3396, 0.2887, 0.2887, 0.2887, -0.4806, 0.1552, -0.2893,
           -0.0684, -0.4195, 0.0335, 0.0335, -0.3339, -0.3339, -1.326, -1.189, 0.1788, -0.1526,
           -0.1526, -0.1526, -0.1526, -0.1526, 0.1129, 0.1129, 0.1129, 0.4833, -0.1188, 0.4202,
           0.6895, 0.8456, 0.8857, -2.996, -2.996, -2.996, 0.8612, -0.0024, -0.0024, 0.6482,
           0.6237, -0.3808, -0.3808, -0.3808, -0.3808, -0.3808, -0.3808, -0.0025, -0.0025, -0.0025),
  mr = c(2.503, 2.503, 2.503, 2.433, 2.433, 2.753, 2.753, 2.731, 2.731, 5.007,
         3.513, 3.513, 3.513, 3.513, 3.888, 2.464, 2.412, 2.488, 2.582, 2.576,
         4.041, 3.257, 3.564, 3.18, 3.104, 3.35, 4.346, 3.904, 3.509, 4.067,
         3.853, 2.673, 3.135, 4.305, 4.305, 4.305, 4.305, 2.693, 3.243, 1.057,
         1.395, 1.395, 1.395, 0.9627, 0.9627, 1.805, 1.805, 1.112, 2.262, 2.173,
         2.827, 3, 1.757, 2.428, 1.839, 2.819, 2.819, 1.725, 0, 2.202,
         0, 0.2604, 0.2604, 0.2604, 3.359, 3.359, 3.359, 2.134, 1.08, 0.8238,
         1.085, 1.182, 3.367, 3.367, 0.7774, 0.7774, 0, 0, 3.135, 0,
         0, 0, 0, 0, 0.2215, 0.2215, 0.2215, 0.389, 0.6865, 1.108,
         5.853, 8.927, 14.02, 0, 0, 0, 6.92, 7.365, 7.365, 7.591,
         6.691, 5.754, 5.754, 5.754, 5.754, 5.754, 5.754, 0, 0, 0),
  stringsAsFactors = FALSE
)

#' @rdname SmartsMatch
#' @export
ClogpEstimate <- function(smiles) {
  if (is.null(.cs_env$crippen)) .cs_env$crippen <- lapply(.cs_crippen$smarts, .cs_parse)
  m <- .cs_molecule(smiles, explicit_h = TRUE)
  cache <- new.env()
  types <- character(m$n)
  contrib <- numeric(m$n)
  lp <- 0
  mr <- 0
  for (i in seq_len(m$n)) {
    t <- 0
    for (k in seq_along(.cs_env$crippen)) {
      if (.cs_embed(.cs_env$crippen[[k]], m, i, cache)) {
        t <- k
        break
      }
    }
    if (t == 0) {
      types[i] <- "unassigned"
      next
    }
    types[i] <- .cs_crippen$type[t]
    contrib[i] <- .cs_crippen$logp[t]
    lp <- lp + .cs_crippen$logp[t]
    mr <- mr + .cs_crippen$mr[t]
  }
  list(logp = lp, mr = mr, types = types, contributions = contrib)
}

#' @rdname SmartsMatch
#' @export
PainsFilter <- function(smiles) {
  if (is.null(.cs_env$pains)) .cs_env$pains <- lapply(.cs_pains$smarts, function(s) .cs_merge_hs(.cs_parse(s)))
  m <- .cs_molecule(smiles)
  cache <- new.env()
  hit <- vapply(.cs_env$pains, function(q) .cs_embed(q, m, NULL, cache), TRUE)
  nm <- .cs_pains$name[hit]
  list(flagged = length(nm) > 0, names = nm, n_alerts = length(nm))
}

# PAINS SMARTS: wehi_pains.csv of the RDKit distribution (Saubern, Guha and Baell 2011)
.cs_pains <- data.frame(
  name = c("anil_di_alk_F(14)", "hzone_anil(14)", "het_5_pyrazole_OH(14)", "het_thio_666_A(13)",
           "styrene_A(13)", "ene_rhod_C(13)", "dhp_amino_CN_A(13)", "cyano_imine_C(12)",
           "thio_urea_A(12)", "thiophene_amino_B(12)", "keto_keto_beta_B(12)", "keto_phenone_A(11)",
           "cyano_pyridone_C(11)", "thiaz_ene_C(11)", "hzone_thiophene_A(11)", "ene_quin_methide(10)",
           "het_thio_676_A(10)", "ene_five_het_G(10)", "acyl_het_A(9)", "anil_di_alk_G(9)",
           "dhp_keto_A(9)", "thio_urea_B(9)", "anil_alk_bim(9)", "imine_imine_A(9)",
           "thio_urea_C(9)", "imine_one_fives_B(9)", "dhp_amino_CN_B(9)", "anil_OC_no_alk_A(8)",
           "het_thio_66_one(8)", "styrene_B(8)", "het_thio_5_A(8)", "anil_di_alk_ene_A(8)",
           "ene_rhod_D(8)", "ene_rhod_E(8)", "anil_OH_alk_A(8)", "pyrrole_C(8)",
           "thio_urea_D(8)", "thiaz_ene_D(8)", "ene_rhod_F(8)", "thiaz_ene_E(8)",
           "het_65_B(7)", "keto_keto_beta_C(7)", "het_66_A(7)", "thio_urea_E(7)",
           "thiophene_amino_C(7)", "hzone_phenone(7)", "ene_rhod_G(7)", "ene_cyano_B(7)",
           "dhp_amino_CN_C(7)", "het_5_A(7)", "ene_five_het_H(6)", "thio_amide_A(6)",
           "ene_cyano_C(6)", "hzone_furan_A(6)", "anil_di_alk_H(6)", "het_65_C(6)",
           "thio_urea_F(6)", "ene_five_het_I(6)", "keto_keto_gamma(5)", "quinone_B(5)",
           "het_6_pyridone_OH(5)", "hzone_naphth_A(5)", "thio_ester_A(5)", "ene_misc_A(5)",
           "cyano_pyridone_D(5)", "het_65_Db(5)", "het_666_A(5)", "diazox_sulfon_B(5)",
           "anil_NH_alk_A(5)", "sulfonamide_C(5)", "het_thio_N_55(5)", "keto_keto_beta_D(5)",
           "ene_rhod_H(5)", "imine_ene_A(5)", "het_thio_656a(5)", "pyrrole_D(5)",
           "pyrrole_E(5)", "thio_urea_G(5)", "anisol_A(5)", "pyrrole_F(5)",
           "dhp_amino_CN_D(5)", "thiazole_amine_A(4)", "het_6_imidate_A(4)", "anil_OC_no_alk_B(4)",
           "styrene_C(4)", "azulene(4)", "furan_acid_A(4)", "cyano_pyridone_E(4)",
           "anil_alk_thio(4)", "anil_di_alk_I(4)", "het_thio_6_furan(4)", "anil_di_alk_ene_B(4)",
           "imine_one_B(4)", "anil_OC_alk_A(4)", "ene_five_het_J(4)", "pyrrole_G(4)",
           "ene_five_het_K(4)", "cyano_ene_amine_B(4)", "thio_ester_B(4)", "ene_five_het_L(4)",
           "hzone_thiophene_B(4)", "dhp_amino_CN_E(4)", "het_5_B(4)", "imine_imine_B(3)",
           "thiazole_amine_B(3)", "imine_ene_one_A(3)", "diazox_A(3)", "ene_one_A(3)",
           "anil_OC_no_alk_C(3)", "thiazol_SC_A(3)", "het_666_B(3)", "furan_A(3)",
           "colchicine_A(3)", "thiophene_C(3)", "anil_OC_alk_B(3)", "het_thio_66_A(3)",
           "rhod_sat_B(3)", "ene_rhod_I(3)", "keto_thiophene(3)", "imine_imine_C(3)",
           "het_65_pyridone_A(3)", "thiazole_amine_C(3)", "het_thio_pyr_A(3)", "melamine_A(3)",
           "anil_NH_alk_B(3)", "rhod_sat_C(3)", "thiophene_amino_D(3)", "anil_OC_alk_C(3)",
           "het_thio_65_A(3)", "het_thio_656b(3)", "thiazole_amine_D(3)", "thio_urea_H(3)",
           "cyano_pyridone_F(3)", "rhod_sat_D(3)", "ene_rhod_J(3)", "imine_phenol_A(3)",
           "thio_carbonate_B(3)", "het_thio_N_5A(3)", "het_thio_N_65A(3)", "anil_di_alk_J(3)",
           "pyrrole_H(3)", "ene_cyano_D(3)", "cyano_cyano_B(3)", "ene_five_het_M(3)",
           "cyano_ene_amine_C(3)", "thio_urea_I(3)", "dhp_amino_CN_F(3)", "anthranil_acid_B(3)",
           "diazox_B(3)", "thio_aldehyd_A(3)", "thio_amide_B(2)", "imidazole_B(2)",
           "thiazole_amine_E(2)", "thiazole_amine_F(2)", "thio_ester_C(2)", "ene_one_B(2)",
           "quinone_C(2)", "keto_naphthol_A(2)", "thio_amide_C(2)", "phthalimide_misc(2)",
           "sulfonamide_D(2)", "anil_NH_alk_C(2)", "het_65_E(2)", "hzide_naphth(2)",
           "anisol_B(2)", "thio_carbam_ene(2)", "thio_amide_D(2)", "het_65_Da(2)",
           "thiophene_D(2)", "het_thio_6_ene(2)", "cyano_keto_A(2)", "anthranil_acid_C(2)",
           "naphth_amino_C(2)", "naphth_amino_D(2)", "thiazole_amine_G(2)", "het_66_B(2)",
           "coumarin_A(2)", "anthranil_acid_D(2)", "het_66_C(2)", "thiophene_amino_E(2)",
           "het_6666_A(2)", "sulfonamide_E(2)", "anil_di_alk_K(2)", "het_5_C(2)",
           "ene_six_het_B(2)", "steroid_A(2)", "het_565_A(2)", "thio_imine_ium(2)",
           "anthranil_acid_E(2)", "hzone_furan_B(2)", "thiophene_E(2)", "ene_misc_B(2)",
           "het_thio_5_B(2)", "thiophene_amino_F(2)", "anil_OC_alk_D(2)", "tert_butyl_A(2)",
           "thio_urea_J(2)", "het_thio_65_B(2)", "coumarin_B(2)", "thio_urea_K(2)",
           "thiophene_amino_G(2)", "anil_NH_alk_D(2)", "het_thio_5_C(2)", "thio_keto_het(2)",
           "het_thio_N_5B(2)", "quinone_D(2)", "anil_di_alk_furan_B(2)", "ene_six_het_C(2)",
           "het_55_A(2)", "het_thio_65_C(2)", "hydroquin_A(2)", "anthranil_acid_F(2)",
           "pyrrole_I(2)", "thiophene_amino_H(2)", "imine_one_fives_C(2)", "keto_phenone_zone_A(2)",
           "dyes7A(2)", "het_pyridiniums_B(2)", "het_5_D(2)", "thiazole_amine_H(1)",
           "thiazole_amine_I(1)", "het_thio_N_5C(1)", "sulfonamide_F(1)", "thiazole_amine_J(1)",
           "het_65_F(1)", "keto_keto_beta_E(1)", "ene_five_one_B(1)", "keto_keto_beta_zone(1)",
           "thio_urea_L(1)", "het_thio_urea_ene(1)", "cyano_amino_het_A(1)", "tetrazole_hzide(1)",
           "imine_naphthol_A(1)", "misc_anisole_A(1)", "het_thio_665(1)", "anil_di_alk_L(1)",
           "colchicine_B(1)", "misc_aminoacid_A(1)", "imidazole_amino_A(1)", "phenol_sulfite_A(1)",
           "het_66_D(1)", "misc_anisole_B(1)", "tetrazole_A(1)", "het_65_G(1)",
           "misc_trityl_A(1)", "misc_pyridine_OC(1)", "het_6_hydropyridone(1)", "misc_stilbene(1)",
           "misc_imidazole(1)", "anil_NH_no_alk_A(1)", "het_6_imidate_B(1)", "anil_alk_B(1)",
           "styrene_anil_A(1)", "misc_aminal_acid(1)", "anil_no_alk_D(1)", "anil_alk_C(1)",
           "misc_anisole_C(1)", "het_465_misc(1)", "anthranil_acid_G(1)", "anil_di_alk_M(1)",
           "anthranil_acid_H(1)", "thio_urea_M(1)", "thiazole_amine_K(1)", "het_thio_5_imine_A(1)",
           "thio_amide_E(1)", "het_thio_676_B(1)", "sulfonamide_G(1)", "thio_thiomorph_Z(1)",
           "naphth_ene_one_A(1)", "naphth_ene_one_B(1)", "amino_acridine_A(1)", "keto_phenone_B(1)",
           "hzone_acid_A(1)", "sulfonamide_H(1)", "het_565_indole(1)", "pyrrole_J(1)",
           "pyrazole_amino_B(1)", "pyrrole_K(1)", "anthranil_acid_I(1)", "thio_amide_F(1)",
           "ene_one_C(1)", "het_65_H(1)", "cyano_imine_D(1)", "cyano_misc_A(1)",
           "ene_misc_C(1)", "het_66_E(1)", "keto_keto_beta_F(1)", "misc_naphthimidazole(1)",
           "naphth_ene_one_C(1)", "keto_phenone_C(1)", "coumarin_C(1)", "thio_est_cyano_A(1)",
           "het_65_imidazole(1)", "anthranil_acid_J(1)", "colchicine_het(1)", "ene_misc_D(1)",
           "indole_3yl_alk_B(1)", "anil_OH_no_alk_A(1)", "thiazole_amine_L(1)", "pyrazole_amino_A(1)",
           "het_thio_N_5D(1)", "anil_alk_indane(1)", "anil_di_alk_N(1)", "het_666_C(1)",
           "ene_one_D(1)", "anil_di_alk_indol(1)", "anil_no_alk_indol_A(1)", "dhp_amino_CN_G(1)",
           "anil_di_alk_dhp(1)", "anthranil_amide_A(1)", "hzone_anthran_Z(1)", "ene_one_amide_A(1)",
           "het_76_A(1)", "thio_urea_N(1)", "anil_di_alk_coum(1)", "ene_one_amide_B(1)",
           "het_thio_656c(1)", "het_5_ene(1)", "thio_imide_A(1)", "dhp_amidine_A(1)",
           "thio_urea_O(1)", "anil_di_alk_O(1)", "thio_urea_P(1)", "het_pyraz_misc(1)",
           "diazox_C(1)", "diazox_D(1)", "misc_cyclopropane(1)", "imine_ene_one_B(1)",
           "coumarin_D(1)", "misc_furan_A(1)", "rhod_sat_E(1)", "rhod_sat_imine_A(1)",
           "rhod_sat_F(1)", "het_thio_5_imine_B(1)", "het_thio_5_imine_C(1)", "ene_five_het_N(1)",
           "thio_carbam_A(1)", "misc_anilide_A(1)", "misc_anilide_B(1)", "mannich_B(1)",
           "mannich_catechol_A(1)", "anil_alk_D(1)", "het_65_I(1)", "misc_urea_A(1)",
           "imidazole_C(1)", "styrene_imidazole_A(1)", "thiazole_amine_M(1)", "misc_pyrrole_thiaz(1)",
           "pyrrole_L(1)", "het_thio_65_D(1)", "ene_misc_E(1)", "thio_cyano_A(1)",
           "cyano_amino_het_B(1)", "cyano_pyridone_G(1)", "het_65_J(1)", "ene_one_yne_A(1)",
           "anil_OH_no_alk_B(1)", "hzone_acyl_misc_A(1)", "thiophene_F(1)", "anil_OC_alk_E(1)",
           "anil_OC_alk_F(1)", "het_65_K(1)", "het_65_L(1)", "coumarin_E(1)",
           "coumarin_F(1)", "coumarin_G(1)", "coumarin_H(1)", "het_thio_67_A(1)",
           "sulfonamide_I(1)", "het_65_mannich(1)", "anil_alk_A(1)", "het_5_inium(1)",
           "anil_di_alk_P(1)", "thio_urea_Q(1)", "thio_pyridine_A(1)", "melamine_B(1)",
           "misc_phthal_thio_N(1)", "hzone_acyl_misc_B(1)", "tert_butyl_B(1)", "diazox_E(1)",
           "anil_NH_no_alk_B(1)", "anil_no_alk_A(1)", "anil_no_alk_B(1)", "thio_ene_amine_A(1)",
           "het_55_B(1)", "cyanamide_A(1)", "ene_one_one_A(1)", "ene_six_het_D(1)",
           "ene_cyano_E(1)", "ene_cyano_F(1)", "hzone_furan_C(1)", "anil_no_alk_C(1)",
           "hzone_acid_D(1)", "hzone_furan_E(1)", "het_6_pyridone_NH2(1)", "imine_one_fives_D(1)",
           "pyrrole_M(1)", "pyrrole_N(1)", "pyrrole_O(1)", "ene_cyano_G(1)",
           "sulfonamide_J(1)", "misc_pyrrole_benz(1)", "thio_urea_R(1)", "ene_one_one_B(1)",
           "dhp_amino_CN_H(1)", "het_66_anisole(1)", "thiazole_amine_N(1)", "het_pyridiniums_C(1)",
           "het_5_E(1)", "thiaz_ene_A(128)", "pyrrole_A(118)", "catechol_A(92)",
           "ene_five_het_B(90)", "imine_one_fives(89)", "ene_five_het_C(85)", "hzone_pipzn(79)",
           "keto_keto_beta_A(68)", "hzone_pyrrol(64)", "ene_one_ene_A(57)", "cyano_ene_amine_A(56)",
           "ene_five_one_A(55)", "cyano_pyridone_A(54)", "anil_alk_ene(51)", "amino_acridine_A(46)",
           "ene_five_het_D(46)", "thiophene_amino_Aa(45)", "ene_five_het_E(44)", "sulfonamide_A(43)",
           "thio_ketone(43)", "sulfonamide_B(41)", "anil_no_alk(40)", "thiophene_amino_Ab(40)",
           "het_pyridiniums_A(39)", "anthranil_one_A(38)", "cyano_imine_A(37)", "diazox_sulfon_A(36)",
           "hzone_anil_di_alk(35)", "rhod_sat_A(33)", "hzone_enamin(30)", "pyrrole_B(29)",
           "thiophene_hydroxy(28)", "cyano_pyridone_B(27)", "imine_one_sixes(27)", "dyes5A(27)",
           "naphth_amino_A(25)", "naphth_amino_B(25)", "ene_one_ester(24)", "thio_dibenzo(23)",
           "cyano_cyano_A(23)", "hzone_acyl_naphthol(22)", "het_65_A(21)", "imidazole_A(19)",
           "ene_cyano_A(19)", "anthranil_acid_A(19)", "dyes3A(19)", "dhp_bis_amino_CN(19)",
           "het_6_tetrazine(18)", "ene_one_hal(17)", "cyano_imine_B(17)", "thiaz_ene_B(17)",
           "ene_rhod_B(16)", "thio_carbonate_A(15)", "anil_di_alk_furan_A(15)", "ene_five_het_F(15)",
           "ene_six_het_A(483)", "hzone_phenol_A(479)", "anil_di_alk_A(478)", "indol_3yl_alk(461)",
           "quinone_A(370)", "azo_A(324)", "imine_one_A(321)", "mannich_A(296)",
           "anil_di_alk_B(251)", "anil_di_alk_C(246)", "ene_rhod_A(235)", "hzone_phenol_B(215)",
           "ene_five_het_A(201)", "anil_di_alk_D(198)", "imine_one_isatin(189)", "anil_di_alk_E(186)"),
  smarts = c("c:1:c:c(:c:c:c:1-[#6;X4]-c:2:c:c:c(:c:c:2)-[#7&H2,$([#7;!H0]-[#6;X4]),$([#7](-[#6X4])-[#6X4])])-[#7&H2,$([#7;!H0]-[#6;X4]),$([#7](-[#6X4])-[#6X4])]",
             "c:1(:c(:c(:c(:c(:c:1-[#1])-[#1])-[#7](-[#1])-[#1])-[#1])-[#1])-[#6]=[#7]-[#7]-[#1]",
             "c1(nn(c([c;!H0,$(c-[#6;!H0])]1)-[#8]-[#1])-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#1])-[#1])-[#1])-[#6;X4]",
             paste0("c:2(:c:1-[#16]-c:3:c(-[#7;!H0,$([#7]-[CH3]),$([#7]-[#6;!H0;!H1]-[#6;!H0])](-c:1:c(:c(:c:2-[#1])-[#1])-[#1])):[c;!H0,$(c~[#7](-[#1])-[#6;X4]),$(c(cc)(c",
                    "c)~[#6]:[#6])](:[c;!H0,$(c(cc)(cc)~[#6]:[#6])]:[c;!H0,$(c-[#7](-[#1])-[#1]),$(c-[#8]-[#6;X4])]:c:3-[#1]))-[#1]"),
             "[#6]-2-[#6]-c:1:c(:c:c:c:c:1)-[#6](-c:3:c:c:c:c:c-2:3)=[#6]-[#6]",
             "[#16]-1-[#6](=[#7]-[#6]:[#6])-[#7;!H0,$([#7]-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#8]),$([#7]-[#6]:[#6])]-[#6](=[#8])-[#6]-1=[#6](-[#1])-[$([#6]:[#6]:[#6]-[#17]),$([#6]:[!#6&!#1])]",
             "[#7](-[#1])(-[#1])-[#6]-1=[#6](-[#6]#[#7])-[#6](-[#1])(-[#6]:[#6])-[#6](=[#6](-[#6]=[#6])-[#8]-1)-[#6](-[#1])-[#1]",
             "[#8]=[#16](=[#8])-[#6](-[#6]#[#7])=[#7]-[#7]-[#1]",
             "c:1:c:c:c:c:c:1-[#7](-[#1])-[#6](=[#16])-[#7](-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-c:2:c:c:c:c:c:2",
             "c:1:c(:c:c:c:c:1)-[#7](-[#1])-c:2:c(:c(:c(:s:2)-[$([#6]=[#8]),$([#6]#[#7]),$([#6](-[#8]-[#1])=[#6])])-[#7])-[$([#6]#[#7]),$([#6](:[#7]):[#7])]",
             "[#6;X4]-1-[#6](=[#8])-[#7]-[#7]-[#6]-1=[#8]",
             "c:1:c-3:c(:c:c:c:1)-[#6]:2:[#7]:[!#1]:[#6]:[#6]:[#6]:2-[#6]-3=[#8]",
             "[#6]-1(-[#6](=[#6](-[#6]#[#7])-[#6](~[#8])~[#7]~[#6]-1~[#8])-[#6](-[#1])-[#1])=[#6](-[#1])-[#6]:[#6]",
             "[#6]-,:1(=,:[#6](-!@[#6]=[#7])-,:[#16]-,:[#6](-,:[#7]-,:1)=[#8])-[$([F,Cl,Br,I]),$([#7+](:[#6]):[#6])]",
             paste0("c:1:2:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1]):[!#6&!#1]:[#6;!H0,$([#6]-[OH]),$([#6]-[#6;H2,H3])](:[#6]:2-[#6](-[#1])=[#7]-[#7](-[#1])-[$([#6]:1:[#7]:[",
                    "#6]:[#6](-[#1]):[#16]:1),$([#6]:[#6](-[#1]):[#6]-[#1]),$([#6]:[#7]:[#6]:[#7]:[#6]:[#7]),$([#6]:[#7]:[#7]:[#7]:[#7])])"),
             "[!#1]:[!#1]-[#6;!H0,$([#6]-[#6]#[#7])]=[#6]-1-[#6]=,:[#6]-[#6](=[$([#8]),$([#7;!R])])-[#6]=,:[#6]-1",
             paste0("c:1:c:c-2:c(:c:c:1)-[#6]-[#6](-c:3:c(-[#16]-2):c(:c(-[#1]):[c;!H0,$(c-[#8]),$(c-[#16;X2]),$(c-[#6;X4]),$(c-[#7;H2,H3,$([#7!H0]-[#6;X4]),$([#7](-[#6;X4",
                    "])-[#6;X4])])](:c:3-[#1]))-[#1])-[#7;H2,H3,$([#7;!H0](-[#6])-[#6;X4]),$([#7](-[#6])(-[#6;X4])-[#6;X4])]"),
             "[#6]-1(=[#8])-[#6](=[#6](-[#1])-[$([#6]:1:[#6]:[#6]:[#6]:[#6]:[#6]:1),$([#6]:1:[#6]:[#6]:[#6]:[!#6&!#1]:1)])-[#7]=[#6](-[!#1]:[!#1]:[!#1])-[$([#16]),$([#7]-[!#1]:[!#1])]-1",
             "[#7+](:[!#1]:[!#1]:[!#1])-[!#1]=[#8]",
             "[#6;X4]-[#7](-[#6;X4])-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#6]2=,:[#7][#6]:[#6]:[!#1]2)-[#1])-[#1]",
             "[#7;!H0,$([#7]-[#6;X4])]-1-[#6]=,:[#6](-[#6](=[#8])-[#6]:[#6]:[#6])-[#6](-[#6])-[#6](=[#6]-1-[#6](-[#1])(-[#1])-[#1])-[$([#6]=[#8]),$([#6]#[#7])]",
             "c:1:c:c:c:c:c:1-[#7](-[#1])-[#6](=[#16])-[#7](-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-c:2:c:c:c:c:c:2",
             "c:1:3:c(:c(:c(:c(:c:1-[#1])-[#1])-[#7](-[#1])-[#6](-[#1])(-[#1])-c:2:c:c:c:c:c:2)-[#1]):n:c(-[#1]):n:3-[#6]",
             "c:1:c:c-2:c(:c:c:1)-[#7]=[#6]-[#6]-2=[#7;!R]",
             "c:1(:c:c:c:c:c:1)-[#7](-[#1])-[#6](=[#16])-[#7]-[#7](-[#1])-[#6](=[#8])-[#6]-,:2:[!#1]:[!#6&!#1]:[#6]:[#6]-,:2",
             "[#7;!R]=[#6]-2-[#6](=[#8])-c:1:c:c:c:c:c:1-[#16]-2",
             "[$([#7](-[#1])-[#1]),$([#8]-[#1])]-[#6]-2=[#6](-[#6]#[#7])-[#6](-[#1])(-[#6]:[#6])-c:1:c(:n(-[#6]):n:c:1)-[#8]-2",
             "[#7](-[#1])(-[#1])-c:1:c(:c(:c(:n:c:1-[#1])-[#8]-c:2:c:c:c:c:c:2)-[#1])-[#1]",
             "[#6](=[#8])-[#6]-1=[#6]-[#7]-c:2:c(-[#16]-1):c:c:c:c:2",
             "c:1:c:c-2:c(:c:c:1)-[#6](-c:3:c(-[$([#16;X2]),$([#6;X4])]-2):c:c:[c;!H0,$(c-[#17]),$(c-[#6;X4])](:c:3))=[#6]-[#6]",
             "[#6](-[#1])(-[#1])-[#16;X2]-c:1:n:c(:c(:n:1-!@[#6](-[#1])-[#1])-c:2:c:c:c:c:c:2)-[#1]",
             "[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-[#6]-2=[#6](-[#1])-c:1:c(:c:c:c:c:1)-[#16;X2]-c:3:c-2:c:c:c:c:3",
             "[#16]-1-[#6](=!@[#7;!H0,$([#7]-[#7](-[#1])-[#6]:[#6])])-[#7;!H0,$([#7]-[#6]:[#7]:[#6]:[#6]:[#16])]-[#6](=[#8])-[#6]-1=[#6](-[#1])-[#6]:[#6]-[$([#17]),$([#8]-[#6]-[#1])]",
             "[#16]-1-[#6](=[#8])-[#7]-[#6](=[#16])-[#6]-1=[#6](-[#1])-[#6]:[#6]",
             "c:1:c(:c:c:c:c:1)-[#6](-[#1])(-[#1])-[#7](-[#1])-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#8]-[#1])-[#1])-[#1]",
             "n1(-[#6;X4])c(c(-[#1])c(c1-[#6]:[#6])-[#1])-[#6](-[#1])-[#1]",
             "c:1(:c:c:c:c:c:1)-[#7](-[#1])-[#6](=[#16])-[#7]-[#7](-[#1])-c:2:c:c:c:c:c:2",
             "[#7](-c:1:c:c:c:c:c:1)-c2[n+]c(cs2)-c:3:c:c:c:c:c:3",
             "n:1:c:c:c(:c:1-[#6](-[#1])-[#1])-[#6](-[#1])=[#6]-2-[#6](=[#8])-[#7]-[#6](=[!#6&!#1])-[#7]-2",
             "[#6]-,:1(=,:[#6](-[#6](-[#1])(-[#6])-[#6])-,:[#16]-,:[#6](-,:[#7;!H0,$([#7]-[#6;!H0;!H1])]-,:1)=[#8])-[#16]-[#6;R]",
             "[!#1]:,-1:[!#1]-,:2:[!#1](:[!#1]:[!#1]:[!#1]:,-1)-,:[#7](-[#1])-,:[#7](-,:[#6]-,:2=[#8])-[#6]",
             "c:1:c:c-2:c(:c:c:1)-[#6](=[#6](-[#6]-2=[#8])-[#6])-[#8]-[#1]",
             "c:2:c:c:1:n:n:c(:n:c:1:c:c:2)-[#6](-[#1])(-[#1])-[#6]=[#8]",
             "c:1:c:c:c:c:c:1-[#7](-[#1])-[#6](=[#16])-[#7](-[#1])-[#6](-[#1])(-[#1])-c:2:n:c:c:c:c:2",
             "[#6](-[#1])-[#6](-[#1])(-[#1])-c:1:c(:c(:c(:s:1)-[#7](-[#1])-[#6](=[#8])-[#6]-[#6]-[#6]=[#8])-[$([#6](=[#8])-[#8]),$([#6]#[#7])])-[#6](-[#1])-[#1]",
             paste0("[#6](-c:1:c(:c(:[c;!H0,$(c-[#6;X4])]:c:c:1-[#1])-[#1])-[#1])(-c:2:c(:c(:[c;!H0,$(c-[#17])](:c(:c:2-[#1])-[#1]))-[#1])-[#1])=[$([#7]-[#8]-[#6](-[#1])(-",
                    "[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1]),$([#7]-[#8]-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7](-[#6](-[#",
                    "1])-[#1])-[#6](-[#1])-[#1]),$([#7]-[#7](-[#1])-[#6](=[#7]-[#1])-[#7](-[#1])-[#1]),$([#6](-[#1])-[#7])]"),
             "[#8](-[#1])-[#6](=[#8])-c:1:c:c(:c:c:c:1)-[#6]:[!#1]:[#6]-[#6](-[#1])=[#6]-2-[#6](=[!#6&!#1])-[#7]-[#6](=[!#6&!#1])-[!#6&!#1]-2",
             "[#6]-1(=[#6]-[#6](-c:2:c:c(:c(:n:c-1:2)-[#7](-[#1])-[#1])-[#6]#[#7])=[#6])-[#6]#[#7]",
             "[#7](-[#1])(-[#1])-[#6]-1=[#6](-[#6]#[#7])-[#6](-[#1])(-[#6]:[#6])-[#6](=[#6](-[#6]:[#6])-[#8]-1)-[#6]#[#7]",
             "[#7]-2(-c:1:c:c:c:c:c:1)-[#7]=[#6](-[#6]=[#8])-[#6;X4]-[#6]-2=[#8]",
             "[#7]-1=[#6]-[#6](-[#6](-[#7]-1)=[#16])=[#6]",
             "c1(coc(c1-[#1])-[#6](=[#16])-[#7]-2-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[!#1]-[#6](-[#1])(-[#1])-[#6]-2(-[#1])-[#1])-[#1]",
             "[#6]=[#6](-[#6]#[#7])-[#6](=[#7]-[#1])-[#7]-[#7]",
             "c:1(:c(:c(:[c;!H0,$(c-[#6;!H0;!H1])](:o:1))-[#1])-[#1])-[#6;!H0,$([#6]-[#6;!H0;!H1])]=[#7]-[#7](-[#1])-c:2:n:c:c:s:2",
             "c:1(:c(:c(:c(:c(:c:1-[#7](-[#1])-[#16](=[#8])(=[#8])-[#6]:2:[#6]:[!#1]:[#6]:[#6]:[#6]:2)-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#1])-[#1]",
             "n2c1ccccn1c(c2-[$([#6](-[!#1])=[#6](-[#1])-[#6]:[#6]),$([#6]:[#8]:[#6])])-[#7]-[#6]:[#6]",
             "[#6]-1-[#7](-[#1])-[#7](-[#1])-[#6](=[#16])-[#7]-[#7]-1-[#1]",
             "c:1(:c:c:c:o:1)-[#6](-[#1])=!@[#6]-3-[#6](=[#8])-c:2:c:c:c:c:c:2-[!#6&!#1]-3",
             "[#8]=[#6]-1-[#6;X4]-[#6]-[#6](=[#8])-c:2:c:c:c:c:c-1:2",
             "c:1:c:c-2:c(:c:c:1)-[#6](-c3cccc4noc-2c34)=[#8]",
             "[#8](-[#1])-c:1:n:c(:c:c:c:1)-[#8]-[#1]",
             "c:1:2:c(:c(:c(:c(:c:1:c(:c(:c(:c:2-[#1])-[#1])-[#6]=[#7]-[#7](-[#1])-[$([#6]:[#6]),$([#6]=[#16])])-[#1])-[#1])-[#1])-[#1])-[#1]",
             "[#6]-,:1=,:[#6](-,:[#16]-,:[#6](-,:[#6]=,:[#6]-,:1)=[#16])-,:[#7]",
             "[#6]-1=[#6]-[#6](-[#8]-[#6]-1-[#8])(-[#8])-[#6]",
             "[#8]=[#6]-,:1-,:[#6](=,:[#6]-,:[#6](=,:[#7]-,:[#7]-,:1)-,:[#6]=[#8])-[#6]#[#7]",
             "c3cn1c(nc(c1-[#7]-[#6])-c:2:c:c:c:c:n:2)cc3",
             "[#7]-2-c:1:c:c:c:c:c:1-[#6](=[#7])-c:3:c-2:c:c:c:c:3",
             "c:1:c(:c:c:c:c:1)-[#7]-2-[#6](-[#1])-[#6](-[#1])-[#7](-[#6](-[#1])-[#6]-2-[#1])-[#16](=[#8])(=[#8])-c:3:c:c:c:c:4:n:s:n:c:3:4",
             "c:1(:c(:c-,:2:c(:c(:c:1-[#1])-[#1])-,:[#7](-,:[#6](-,:[#7]-,:2-[#1])=[#8])-[#1])-[#1])-[#7](-[#1])-[#6](-[#1])-[#1]",
             "c:1(:c(:c-3:c(:c(:c:1-[#7](-[#1])-[#16](=[#8])(=[#8])-c:2:c:c:c(:c:c:2)-[!#6&!#1])-[#1])-[#8]-[#6](-[#8]-3)(-[#1])-[#1])-[#1])-[#1]",
             "[#6](-[#1])-[#6]:2:[#7]:[#7](-c:1:c:c:c:c:c:1):[#16]:3:[!#6&!#1]:[!#1]:[#6]:[#6]:2:3",
             "[#8]=[#6]-[#6]=[#6](-[#1])-[#8]-[#1]",
             "[#7]-,:1-,:2-,:[#6](=,:[#7]-,:[#6](=[#8])-,:[#6](=,:[#7]-,:1)-[#6](-[#1])-[#1])-,:[#16]-,:[#6](=[#6](-[#1])-[#6]:[#6])-,:[#6]-,:2=[#8]",
             "[#6]:[#6]-[#6](-[#1])=[#6](-[#1])-[#6](-[#1])=[#7]-[#7](-[#6;X4])-[#6;X4]",
             "c:1:3:c(:c:c:c:c:1):c:2:n:n:c(-[#16]-[#6](-[#1])(-[#1])-[#6]=[#8]):n:c:2:n:3-[#6](-[#1])(-[#1])-[#6](-[#1])=[#6](-[#1])-[#1]",
             "n1(-[#6])c(c(-[#1])c(c1-[#6](-[#1])(-[#1])-[#7](-[#1])-[#6](=[#16])-[#7]-[#1])-[#1])-[#1]",
             "n2(-[#6]:1:[!#1]:[!#6&!#1]:[!#1]:[#6]:1-[#1])c(c(-[#1])c(c2-[#6;X4])-[#1])-[#6;X4]",
             "c:1(:c:c:c:c:c:1)-[#7](-[#1])-[#6](=[#16])-[#7]-[#7](-[#1])-[#6]([#7;R])[#7;R]",
             paste0("c:1(:c(:c(:c(:c(:[c;!H0,$(c-[#6](-[#1])-[#1])]:1)-[#1])-[#8]-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[$([#7](-[#1])-[#6](=[#8])-[#6](-[#1])(-[#1])-[#6](-",
                    "[#1])(-[#1])-[#6](-[#1])-[#1]),$([#6](-[#1])(-[#6](-[#1])-[#1])-[#7](-[#1])-[#6](=[#16])-[#7]-[#1])])-[#1])-[#8]-[#6](-[#1])-[#1]"),
             "n2(-[#6]:1:[#6](-[#6]#[#7]):[#6]:[#6]:[!#6&!#1]:1)c(c(-[#1])c(c2)-[#1])-[#1]",
             "[#7](-[#1])(-[#1])-[#6]-2=[#6](-[#6]#[#7])-[#6](-[#1])(-[#6]:[#6])-c:1:c(:c:c:s:1)-[#8]-2",
             "[#7](-[#1])-c:1:n:c(:c:s:1)-c:2:c:n:c(-[#7](-[#1])-[#1]):s:2",
             "[#7]=[#6]-,:1-,:[#7](-[#1])-,:[#6](=,:[#6](-[#7]-[#1])-,:[#7]=,:[#7]-,:1)-[#7]-[#1]",
             "c:1:c(:c:2:c(:c:c:1):c:c:c:c:2)-[#8]-c:3:c(:c(:c(:c(:c:3-[#1])-[#1])-[#7]-[#1])-[#1])-[#1]",
             "c:1:c:c-2:c(:c:c:1)-[#6]-[#16]-c3c(-[#6]-2=[#6])ccs3",
             "c:2:c:c:c:1:c(:c:c:c:1):c:c:2",
             "c:1(:c(:c(:c(:o:1)-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#8]-[#6]:[#6])-[#1])-[#6](=[#8])-[#8]-[#1]",
             "[!#1]:[#6]-[#6]-,:1=,:[#6](-[#1])-,:[#6](=,:[#6](-[#6]#[#7])-,:[#6](=[#8])-,:[#7]-,:1-[#1])-[#6]:[#8]",
             "[#6]-1-3=[#6](-[#6](-[#7]-c:2:c:c:c:c:c-1:2)(-[#6])-[#6])-[#16]-[#16]-[#6]-3=[!#1]",
             "c:1(:c(:c(:c(:c(:c:1-[#7](-[#1])-[#6](=[#8])-c:2:c:c:c:c:c:2)-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#1])-[#1]",
             "[#6](-[#1])(-[#1])-[#16;X2]-c:1:n:n:c(:c(:n:1)-c:2:c(:c(:c(:o:2)-[#1])-[#1])-[#1])-c:3:c(:c(:c(:o:3)-[#1])-[#1])-[#1]",
             "[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-[#6]-2=[#6]-c:1:c(:c:c:c:c:1)-[#6]-2(-[#1])-[#1]",
             "[#7](-[#1])(-c:1:c:c:c:c:c:1)-[#7]=[#6](-[#6](=[#8])-[#6](-[#1])-[#1])-[#7](-[#1])-[$([#7]-[#1]),$([#6]:[#6])]",
             "c:1:2:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1]):o:c:3:c(-[#1]):c(:c(-[#8]-[#6](-[#1])-[#1]):c(:c:2:3)-[#1])-[#7](-[#1])-[#6](-[#1])-[#1]",
             "[#16]=[#6]-,:1-,:[#7](-[#1])-,:[#6]=,:[#6]-,:[#6]-2=,:[#6]-,:1-[#6](=[#8])-[#8]-[#6]-2=[#6]-[#1]",
             "n2(-c:1:c(:c:c(:c(:c:1)-[#1])-[$([#7](-[#1])-[#1]),$([#6]:[#7])])-[#1])c(c(-[#1])c(c2-[#1])-[#1])-[#1]",
             "n1(-[#6])c(c(-[#1])c(c1-[#6](-[#1])=[#6]-2-[#6](=[#8])-[!#6&!#1]-[#6]=,:[!#1]-2)-[#1])-[#1]",
             "[#6]=[#6]-[#6](-[#6]#[#7])(-[#6]#[#7])-[#6](-[#6]#[#7])=[#6]-[#7](-[#1])-[#1]",
             "[#6]:[#6]-[#6](=[#16;X1])-[#16;X2]-[#6](-[#1])-[$([#6](-[#1])-[#1]),$([#6]:[#6])]",
             "[#8]=[#6]-3-[#6](=!@[#6](-[#1])-c:1:c:n:c:c:1)-c:2:c:c:c:c:c:2-[#7]-3",
             "c:1(:[c;!H0,$(c-[#6;!H0;!H1])](:c(:c(:s:1)-[#1])-[#1]))-[#6](-[#1])=[#7]-[#7](-[#1])-c:2:c:c:c:c:c:2",
             "[#6](-[#1])(-[#1])-[#16;X2]-[#6]-1=[#6](-[#6]#[#7])-[#6](-[#1])(-[#6]:[#6])-[#6](-[#6]#[#7])-[#6](=[#8])-[#7]-1",
             "[#7]-2(-c:1:c:c:c:c:c:1)-[#7]=[#6](-[#7](-[#1])-[#6]=[#8])-[#6](-[#1])(-[#1])-[#6]-2=[#8]",
             "[#6]:[#6]-[#6](-[#1])=[#6](-[#1])-[#6](-[#1])=[#7]-[#7]=[#6]",
             "c:1(:c:c:c(:c:c:1)-[#6](-[#1])-[#1])-c:2:c(:s:c(:n:2)-[#7](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#1]",
             "[#6]-2(-[#6]=[#7]-c:1:c:c:c:c:c:1-[#7]-2)=[#6](-[#1])-[#6]=[#8]",
             "[#8](-c:1:c:c:c:c:c:1)-c:3:c:c:2:n:o:n:c:2:c:c:3",
             "[!#1]:1:[!#1]:[!#1]:[!#1](:[!#1]:[!#1]:1)-[#6](-[#1])=[#6](-[#1])-[#6](-[#7]-c:2:c:c:c:3:c(:c:2):c:c:c(:n:3)-[#7](-[#6])-[#6])=[#8]",
             "[#7](-[#1])(-[#1])-c:1:c(:c:c:c:n:1)-[#8]-[#6](-[#1])(-[#1])-[#6]:[#6]",
             "[#6]-[#16;X2]-c:1:n:c(:c:s:1)-[#1]",
             "c:1:c-3:c(:c:c:c:1)-[#7](-c:2:c:c:c:c:c:2-[#8]-3)-[#6](-[#1])(-[#1])-[#6](-[#1])-[#1]",
             "c:1(:c(:c(:c(:o:1)-[#6](-[#1])-[#1])-[#1])-[#1])-[#6](-[#1])(-[#8]-[#1])-[#6]#[#6]-[#6;X4]",
             "[#6]-1(-[#6](=[#6]-[#6]=[#6]-[#6]=[#6]-1)-[#7]-[#1])=[#7]-[#6]",
             "[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])=[#6]-[#6](=[#8])-c:1:c(-[#16;X2]):s:c(:c:1)-[$([#6]#[#7]),$([#6]=[#8])]",
             "c:1:c(:c:c:c:c:1)-[#7]-2-[#6](=[#8])-[#6](=[#6](-[F,Cl,Br,I])-[#6]-2=[#8])-[#7](-[#1])-[#6]:3:[#6]:[#6]:[#6](-[#8]-[#6](-[#1])-[#1]):[#6]:[#6]:3",
             "c:1-2:c(:c:c:c:c:1)-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7]=[#6]-2-[#16;X2]-[#6](-[#1])(-[#1])-[#6](=[#8])-c:3:c:c:c:c:c:3",
             "[#7]-2(-c:1:c:c:c:c:c:1-[#6](-[#1])-[#1])-[#6](=[#16])-[#7](-[#6](-[#1])(-[#1])-[!#1]:[!#1]:[!#1]:[!#1]:[!#1])-[#6](-[#1])(-[#1])-[#6]-2=[#8]",
             "[#7]-2(-[#6](-[#1])-[#1])-[#6](=[#16])-[#7](-[#1])-[#6](=[#6](-[#1])-c:1:c:c:c:c(:c:1)-[Br])-[#6]-2=[#8]",
             "c:1(:c(:c:2:c(:s:1):c:c:c:c:2)-[#6](-[#1])-[#1])-[#6](=[#8])-[#6](-[#1])(-[#1])-[#6](-[#1])-[#1]",
             "[#7](-[#6](-[#1])-[#1])(-[#6](-[#1])-[#1])-[#6](-[#1])=[#7]-[#6](-[#6](-[#1])-[#1])=[#7]-[#7](-[#6](-[#1])-[#1])-[#6]:[#6]",
             paste0("[#6]:2(:[#6](-[#6](-[#1])-[#1]):[#6]-,:1:[#6](-,:[#7]=,:[#6;!H0,$([#6]-[#16]-[#6](-[#1])-[#1])](-,:[#7](-,:[#6]-,:1=[!#6&!#1;X1])-[#6](-[#1])-[$([#6](",
                    "=[#8])-[#8]),$([#6]:[#6])])):[!#6&!#1;X2]:2)-[#6](-[#1])(-[#1])-[#6](-[#1])-[#1]"),
             "c:1(:n:c(:c(-[#1]):s:1)-[!#1]:[!#1]:[!#1](-[$([#8]-[#6](-[#1])-[#1]),$([#6](-[#1])-[#1])]):[!#1]:[!#1])-[#7](-[#1])-[#6](-[#1])(-[#1])-c:2:c(-[#1]):c(:c(-[#1]):o:2)-[#1]",
             "n:1:c(:c(:c(:c(:c:1-[#16]-[#6]-[#1])-[#6]#[#7])-c:2:c:c:c(:c:c:2)-[#8]-[#6](-[#1])-[#1])-[#1])-[#6]:[#6]",
             paste0("c:1:4:c(:n:c(:n:c:1-[#7](-[#1])-[#6](-[#1])(-[#1])-c:2:c(:c(:c(:o:2)-[#1])-[#1])-[#1])-[#7](-[#1])-c:3:c:[c;!H0,$(c-[#6](-[#1])-[#1]),$(c-[#16;X2]),$(",
                    "c-[#8]-[#6]-[#1]),$(c-[#7;X3])](:[c;!H0,$(c-[#6](-[#1])-[#1]),$(c-[#16;X2]),$(c-[#8]-[#6]-[#1]),$(c-[#7;X3])](:c:[c;!H0,$(c-[#6](-[#1])-[#1]),$(c-[#16",
                    ";X2]),$(c-[#8]-[#6]-[#1]),$(c-[#7;X3])]:3))):c:c:c:c:4"),
             "[#7](-[#1])(-[#6]:1:[#6]:[#6]:[!#1]:[#6]:[#6]:1)-c:2:c:c:c(:c:c:2)-[#7](-[#1])-[#6]-[#1]",
             "[#7]-2(-c:1:c:c:c:c:c:1)-[#6](=[#7]-[#6]=[#8])-[#16]-[#6](-[#1])(-[#1])-[#6]-2=[#8]",
             "[#6]=[#6]-[#6](=[#8])-[#7]-c:1:c(:c(:c(:s:1)-[#6](=[#8])-[#8])-[#6]-[#1])-[#6]#[#7]",
             "[#8;!H0,$([#8]-[#6](-[#1])-[#1])]-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#7](-[#1])-[#6](-[#1])(-[#1])-c:2:n:c:c:n:2",
             "[#6](-[#1])(-[#1])-[#16;X2]-c3nc1c(n(nc1-[#6](-[#1])-[#1])-c:2:c:c:c:c:c:2)nn3",
             "[#6]-[#6](=[#8])-[#6](-[#1])(-[#1])-[#16;X2]-c:3:n:n:c:2:c:1:c(:c(:c(:c(:c:1:n(:c:2:n:3)-[#1])-[#1])-[#1])-[#1])-[#1]",
             "s:1:c(:[n+](-[#6](-[#1])-[#1]):c(:c:1-[#1])-[#6])-[#7](-[#1])-c:2:c:c:c:c:c:2[$([#6](-[#1])-[#1]),$([#6]:[#6])]",
             "[#6]-,:2(=[#16])-,:[#7](-[#6](-[#1])(-[#1])-c:1:c:c:c:o:1)-,:[#6](=,:[#7]-,:[#7]-,:2-[#1])-[#6]:[#6]",
             "[#7]-,:2(-c:1:c:c:c:c:c:1)-,:[#6](=[#8])-,:[#6](=,:[#6]-,:[#6](=,:[#7]-,:2)-[#6]#[#7])-[#6]#[#7]",
             "[#7]-2(-c:1:c:c:c:c:c:1)-[#6](=[#8])-[#16]-[#6](-[#1])(-[#6](-[#1])(-[#1])-[#6](=[#8])-[#7](-[#1])-[#6]:[#6])-[#6]-2=[#8]",
             "[#6](-[#1])(-[#1])-[#7]-2-[#6](=[$([#16]),$([#7])])-[!#6&!#1]-[#6](=[#6]-1-[#6](=[#6](-[#1])-[#6]:[#6]-[#7]-1-[#6](-[#1])-[#1])-[#1])-[#6]-2=[#8]",
             "[#6]=[#7;!R]-c:1:c:c:c:c:c:1-[#8]-[#1]",
             "[#8]=[#6]-,:2-,:[#16]-,:c:1:c(:c(:c:c:c:1)-[#8]-[#6](-[#1])-[#1])-,:[#8]-,:2",
             "[#7]=,:[#6]-,:1-,:[#7]=,:[#6]-,:[#7]-,:[#16]-,:1",
             "[#7]-,:2-,:[#16]-,:[#6]-1=,:[#6](-[#6]:[#6]-[#7]-[#6]-1)-,:[#6]-,:2=[#16]",
             "[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#6](-[#1])=[#7]-[#7]=[#6](-[#6])-[#6]:[#6])-[#1])-[#1]",
             "n1-2cccc1-[#6]=[#7](-[#6])-[#6]-[#6]-2",
             "[#6](-[#6]#[#7])(-[#6]#[#7])=[#6](-[#16])-[#16]",
             "[#6]-1(-[#6]#[#7])(-[#6]#[#7])-[#6](-[#1])(-[#6](=[#8])-[#6])-[#6]-1-[#1]",
             "[#6]-1=,:[#6]-[#6](-[#6](-[$([#8]),$([#16])]-1)=[#6]-[#6]=[#8])=[#8]",
             "[#6]:[#6]-[#6](=[#8])-[#7](-[#1])-[#6](=[#8])-[#6](-[#6]#[#7])=[#6](-[#1])-[#7](-[#1])-[#6]:[#6]",
             "c:1(:c:c:c:c:c:1)-[#7](-[#1])-[#6](=[#16])-[#7](-[#1])-[#7]=[#6]-c:2:c:n:c:c:2",
             "[#7](-[#1])(-[#1])-[#6]-2=[#6](-[#6]#[#7])-[#6](-[#1])(-c:1:c:c:c:s:1)-[#6](=[#6](-[#6](-[#1])-[#1])-[#8]-2)-[#6](=[#8])-[#8]-[#6]",
             "c:1:c-3:c(:c:c(:c:1)-[#6](=[#8])-[#7](-[#1])-c:2:c(:c:c:c:c:2)-[#6](=[#8])-[#8]-[#1])-[#6](-[#7](-[#6]-3=[#8])-[#6](-[#1])-[#1])=[#8]",
             "[Cl]-c:2:c:c:1:n:o:n:c:1:c:c:2",
             "[#6]-[#6](=[#16])-[#1]",
             "[#6;X4]-[#7](-[#1])-[#6](-[#6]:[#6])=[#6](-[#1])-[#6](=[#16])-[#7](-[#1])-c:1:c:c:c:c:c:1",
             "[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#16]-[#6](-[#1])(-[#1])-c1cn(cn1)-[#1]",
             "[#8]=[#6]-[#7](-[#1])-c:1:c(-[#6]:[#6]):n:c(-[#6](-[#1])(-[#1])-[#6]#[#7]):s:1",
             "[#6](-[#1])-[#7](-[#1])-c:1:n:c(:c:s:1)-c2cnc3n2ccs3",
             "[#7]-,:1-,:[#6](=[#8])-,:[#6](=,:[#6](-[#6])-,:[#16]-,:[#6]-,:1=[#16])-[#1]",
             "[#6](-[#16])(-[#7])=[#6](-[#1])-[#6]=[#6](-[#1])-[#6]=[#8]",
             "[#8]=[#6]-3-c:1:c(:c:c:c:c:1)-[#6]-,:2=,:[#6](-[#8]-[#1])-,:[#6](=[#8])-,:[#7]-,:c:4:c-,:2:c-3:c:c:c:4",
             "c:1:2:c:c:c:c(:c:1:c(:c:c:c:2)-[$([#8]-[#1]),$([#7](-[#1])-[#1])])-[#6](-[#6])=[#8]",
             "[#6](-[#1])(-c:1:c:c:c:c:c:1)(-c:2:c:c:c:c:c:2)-[#6](=[#16])-[#7]-[#1]",
             "[#7]-2(-[#6](=[#8])-c:1:c(:c(:c(:c(:c:1-[#1])-[#6](=[#8])-[#8]-[#1])-[#1])-[#1])-[#6]-2=[#8])-c:3:c(:c:c(:c(:c:3)-[#1])-[#8])-[#1]",
             "c:1:c:c(:c:c:c:1-[#7](-[#1])-[#16](=[#8])=[#8])-[#7](-[#1])-[#16](=[#8])=[#8]",
             "[#6](-[#1])-[#7](-[#1])-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#7](-[#1])-[#6]-[#1]",
             "s1c(c(c-,:2c1-,:[#7](-[#1])-,:[#6](-,:[#6](=,:[#6]-,:2-[#1])-[#6](=[#8])-[#8]-[#1])=[#8])-[#7](-[#1])-[#1])-[#6](=[#8])-[#7]-[#1]",
             "c:2(:c:1:c(:c(:c(:c(:c:1:c(:c(:c:2-[#1])-[#1])-[#1])-[#1])-[#7](-[#1])-[#7](-[#1])-[#6]=[#8])-[#1])-[#1])-[#1]",
             "[#6](-[#1])(-[#1])-c:1:c(:c(:c(:c(:c:1-[#8]-[#6](-[#1])-[#1])-[#1])-[#1])-[#6](-[#1])(-[#1])-[#7](-[#1])-[#6;X4])-[#1]",
             "[#6]-1=[#6]-[#7]-[#6](-[#16]-[#6;X4]-1)=[#16]",
             "[#6](-[#7](-[#6]-[#1])-[#6]-[#1]):[#6]-[#7](-[#1])-[#6](=[#16])-[#6]-[#1]",
             "n2nc(c1cccc1c2-[#6])-[#6]",
             "s:1:c(:c(-[#1]):c(:c:1-[#6](=[#8])-[#7](-[#1])-[#7]-[#1])-[#8]-[#6](-[#1])-[#1])-[#1]",
             "[#6]-1:[#6]-[#7]=[#6]-[#6](=[#6]-[#7]-[#6])-[#16]-1",
             "[#6](-[#1])(-[#1])-[#6](-[#1])(-[#6]#[#7])-[#6](=[#8])-[#6]",
             "c2(c(-[#7](-[#1])-[#1])n(-c:1:c:c:c:c:c:1-[#6](=[#8])-[#8]-[#1])nc2-[#6]=[#8])-[$([#6]#[#7]),$([#6]=[#16])]",
             "c:2:c:1:c:c:c:c-,:3:c:1:c(:c:c:2)-,:[#7](-,:[#7]=,:[#6]-,:3)-[#1]",
             "c:2:c:1:c:c:c:c-,:3:c:1:c(:c:c:2)-,:[#7]-,:[#7]=,:[#7]-,:3",
             "c1csc(n1)-[#7]-[#7]-[#16](=[#8])=[#8]",
             "c:1:c:c:c:2:c(:c:1):n:c(:n:c:2)-[#7](-[#1])-[#6]-3=[#7]-[#6](-[#6]=[#6]-[#7]-3-[#1])(-[#6](-[#1])-[#1])-[#6](-[#1])-[#1]",
             "c:1-,:3:c(:c(:c(:c(:c:1)-[#8]-[#6]-[#1])-[#1])-[#1])-,:c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-,:[#6](=[#8])-,:[#8]-,:3",
             "c:12:c(:c:c:c:n:1)c(c(-[#6](=[#8])~[#8;X1])s2)-[#7](-[#1])-[#1]",
             "c:1:2:n:c(:c(:n:c:1:[#6]:[#6]:[#6]:[!#1]:2)-[#6](-[#1])=[#6](-[#8]-[#1])-[#6])-[#6](-[#1])=[#6](-[#8]-[#1])-[#6]",
             "c1csc(c1-[#7](-[#1])-[#1])-[#6](-[#1])=[#6](-[#1])-c2cccs2",
             "c:2:c:c:1:n:c:3:c(:n:c:1:c:c:2):c:c:c:4:c:3:c:c:c:c:4",
             "[#6]:[#6]-[#7](-[#1])-[#16](=[#8])(=[#8])-[#7](-[#1])-[#6]:[#6]",
             "c:1:c:c(:c:c:c:1-[#7](-[#1])-[#1])-[#7](-[#6;X3])-[#6;X3]",
             "[#7]-2=[#6](-c:1:c:c:c:c:c:1)-[#6](-[#1])(-[#1])-[#6](-[#8]-[#1])(-[#6](-[#9])(-[#9])-[#9])-[#7]-2-[$([#6]:[#6]:[#6]:[#6]:[#6]:[#6]),$([#6](=[#16])-[#6]:[#6]:[#6]:[#6]:[#6]:[#6])]",
             "c:1:c(:c:c:c:c:1)-[#6](=[#8])-[#6](-[#1])=[#6]-,:3-,:[#6](=[#8])-,:[#7](-[#1])-,:[#6](=[#8])-,:[#6](=[#6](-[#1])-c:2:c:c:c:c:c:2)-,:[#7]-,:3-[#1]",
             "[#8]=[#6]-4-[#6]-[#6]-[#6]-3-[#6]-2-[#6](=[#8])-[#6]-[#6]-1-[#6]-[#6]-[#6]-[#6]-1-[#6]-2-[#6]-[#6]-[#6]-3=[#6]-4",
             "c:1:2:c:3:c(:c(-[#8]-[#1]):c(:c:1:c(:c:n:2-[#6])-[#6]=[#8])-[#1]):n:c:n:3",
             "[#6;X4]-[#7+](-[#6;X4]-[#8]-[#1])=[#6]-[#16]-[#6]-[#1]",
             "[#6]-3(=[#8])-[#6](=[#6](-[#1])-[#7](-[#1])-c:1:c:c:c:c:c:1-[#6](=[#8])-[#8]-[#1])-[#7]=[#6](-c:2:c:c:c:c:c:2)-[#8]-3",
             "c:1(:c(:c(:[c;!H0,$(c-[#6;!H0;!H1])](:o:1))-[#1])-[#1])-[#6;!H0,$([#6]-[#6;!H0;!H1])]=[#7]-[#7](-[#1])-c:2:c:c:n:c:c:2",
             "c:1(:c(:c(:[c;!H0,$(c-[#6;!H0;!H1])](:s:1))-[#1])-[#1])-[#6;!H0,$([#6]-[#6;!H0;!H1])]-[#6](=[#8])-[#7](-[#1])-c:2:n:c:c:s:2",
             "[#6]:[#6]-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#6]=[#8])-[#7]-2-[#6](=[#8])-[#6]-1(-[#1])-[#6](-[#1])(-[#1])-[#6]=[#6]-[#6](-[#1])(-[#1])-[#6]-1(-[#1])-[#6]-2=[#8]",
             "[#6]-1(-[#6]=[#8])(-[#6]:[#6])-[#16;X2]-[#6]=[#7]-[#7]-1-[#1]",
             "[#7](-[#1])(-[#1])-c:1:c(:c(:c(:s:1)-[#7](-[#1])-[#6](=[#8])-c:2:c:c:c:c:c:2)-[#6]#[#7])-[#6]:3:[!#1]:[!#1]:[!#1]:[!#1]:[!#1]:3",
             "[#6](-[#1])(-[#1])-[#8]-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#7](-[#1])-[#6](-[#1])(-[#1])-c:2:c:c:c:c:c:2-[$([#6](-[#1])-[#1]),$([#8]-[#6](-[#1])-[#1])]",
             paste0("[#6](-[#1])(-[#1])(-[#1])-[#6](-[#6](-[#1])(-[#1])-[#1])(-[#6](-[#1])(-[#1])-[#1])-c:1:c(:c:c(:c(:c:1-[#1])-[#6](-[#6](-[#1])(-[#1])-[#1])(-[#6](-[#1]",
                    ")(-[#1])-[#1])-[#6](-[#1])(-[#1])-[#1])-[#8]-[#6](-[#1])-[#7])-[#1]"),
             "c:1(:c(:o:c:c:1)-[#6]-[#1])-[#6]=[#7]-[#7](-[#1])-[#6](=[#16])-[#7]-[#1]",
             "[#7](-[#1])-c1nc(nc2nnc(n12)-[#16]-[#6])-[#7](-[#1])-[#6]",
             "c:1-,:2:c(:c:c:c:c:1-[#6](-[#1])(-[#1])-[#6](-[#1])=[#6](-[#1])-[#1])-,:[#6](=,:[#6](-[#6](=[#8])-[#7](-[#1])-[#6]:[#6])-,:[#6](=[#8])-,:[#8]-,:2)-[#1]",
             "[#6]-,:2(=[#16])-,:[#7]-,:1-,:[#6]=,:[#6]-,:[#7]=,:[#7]-,:[#6]-,:1=,:[#7]-,:[#7]-,:2-[#1]",
             "[#6]:[#6]:[#6]:[#6]:[#6]:[#6]-c:1:c:c(:c(:s:1)-[#7](-[#1])-[#6](=[#8])-[#6])-[#6](=[#8])-[#8]-[#1]",
             "[#7](-[#1])(-[#1])-c:1:c(:c(:c(:c:c:1-[#7](-[#1])-[#6](-[#1])(-[#6])-[#6](-[#1])-[#6](-[#1])-[#1])-[#1])-[#1])-[#1]",
             "[#16]=[#6]-,:2-,:[#7](-[#1])-,:[#7]=,:[#6](-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#1])-,:[#8]-,:2",
             "[#16]=[#6]-c:1:c:c:c:2:c:c:c:c:n:1:2",
             "[#6]~1~[#6](~[#7]~[#7]~[#6](~[#6](-[#1])-[#1])~[#6](-[#1])-[#1])~[#7]~[#16]~[#6]~1",
             "[#6]-1(-[#6]=,:[#6]-[#6]=,:[#6]-[#6]-1=[!#6&!#1])=[!#6&!#1]",
             "[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-c:1:c(-[#1]):c(:c(:o:1)-[#6](-[#1])=[#6]-[#6]#[#7])-[#1]",
             "[#8]=[#6]-1-[#6]:[#6]-[#6](-[#1])(-[#1])-[#7]-[#6]-1=[#6]-[#1]",
             "[#6]:[#6]-[#7]:2:[#7]:[#6]:1-[#6](-[#1])(-[#1])-[#16;X2]-[#6](-[#1])(-[#1])-[#6]:1:[#6]:2-[#7](-[#1])-[#6](=[#8])-[#6](-[#1])=[#6]-[#1]",
             "n:1:c(:n(:c:2:c:1:c:c:c:c:2)-[#6](-[#1])-[#1])-[#16]-[#6](-[#1])(-[#1])-[#6](=[#8])-[#7](-[#1])-[#7]=[#6](-[#1])-[#6](-[#1])=[#6]-[#1]",
             "c:1(:c:c(:c(:c:c:1)-[#8]-[#1])-[#6](=!@[#6]-[#7])-[#6]=[#8])-[#8]-[#1]",
             "c:1(:c:c(:c(:c:c:1)-[#7](-[#1])-[#6](=[#8])-[#6]:[#6])-[#6](=[#8])-[#8]-[#1])-[#8]-[#1]",
             "n2(-[#6](-[#1])-[#1])c-1c(-[#6]:[#6]-[#6]-1=[#8])cc2-[#6](-[#1])-[#1]",
             "[#6](-[#1])-[#7](-[#1])-c:1:c(:c(:c(:s:1)-[#6]-[#1])-[#6]-[#1])-[#6](=[#8])-[#7](-[#1])-[#6]:[#6]",
             "[#6]:[#6]-[#7;!R]=[#6]-2-[#6](=[!#6&!#1])-c:1:c:c:c:c:c:1-[#7]-2",
             "c:1:c:c:c:c:c:1-[#6](=[#8])-[#7](-[#1])-[#7]=[#6]-3-c:2:c:c:c:c:c:2-c:4:c:c:c:c:c-3:4",
             "c:1:c(:c:c:c:c:1)-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])=[#6](-[#1])-[#6]=!@[#6](-[#1])-[#6](-[#1])=[#6]-[#6]=@[#7]-c:2:c:c:c:c:c:2",
             "[#6]:1:2:[!#1]:[#7+](:[!#1]:[#6;!H0,$([#6]-[*])](:[!#1]:1:[#6]:[#6]:[#6]:[#6]:2))~[#6]:[#6]",
             "[#7]-2(-c:1:c:c:c:c:c:1)-[#7]=[#6](-[#6](-[#1])-[#1])-[#6](-[#1])(-[#16]-[#6])-[#6]-2=[#8]",
             "c:1:c:c:c(:c:c:1-[#7](-[#1])-c2nc(c(-[#1])s2)-c:3:c:c:c(:c:c:3)-[#6](-[#1])(-[#6]-[#1])-[#6]-[#1])-[#6](=[#8])-[#8]-[#1]",
             "[#6](-[#1])(-[#1])-[#7](-[#1])-[#6]=[#7]-[#7](-[#1])-c1nc(c(-[#1])s1)-[#6]:[#6]",
             "[#6]:[#6]-[#7](-[#1])-[#6](=[#8])-c1c(snn1)-[#7](-[#1])-[#6]:[#6]",
             "[#8]=[#16](=[#8])(-[#6]:[#6])-[#7](-[#1])-c1nc(cs1)-[#6]:[#6]",
             "[#8]=[#16](=[#8])(-[#6]:[#6])-[#7](-[#1])-[#7](-[#1])-c1nc(cs1)-[#6]:[#6]",
             "s2c:1:n:c:n:c(:c:1c(c2-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#7]-[#7]=[#6]-c3ccco3",
             "[#6](=[#8])-[#6](-[#1])=[#6](-[#8]-[#1])-[#6](-[#8]-[#1])=[#6](-[#1])-[#6](=[#8])-[#6]",
             "c:2(:c:1-[#6](-[#6](-[#6](-c:1:c(:c(:c:2-[#1])-[#1])-[#1])(-[#1])-[#1])=[#8])=[#6](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1]",
             "[#6]:[#6]-[#7](-[#1])-[#7]=[#6](-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#6](-[#1])-[#1])=[#7]-[#7](-[#1])-[#6]:[#6]",
             "[#6;X4]-[#16;X2]-[#6](=[#7]-[!#1]:[!#1]:[!#1]:[!#1])-[#7](-[#1])-[#7]=[#6]",
             "[#6]-1(=[#7]-[#7](-[#6](-[#16]-1)=[#6](-[#1])-[#6]:[#6])-[#6]:[#6])-[#6]=[#8]",
             "c:1(:c(:c:2:c(:n:c:1-[#7](-[#1])-[#1]):c:c:c(:c:2-[#7](-[#1])-[#1])-[#6]#[#7])-[#6]#[#7])-[#6]#[#7]",
             "[!#1]:1:[!#1]:[!#1]:[!#1](:[!#1]:[!#1]:1)-[#6](-[#1])=[#6](-[#1])-[#6](-[#7](-[#1])-[#7](-[#1])-c2nnnn2-[#6])=[#8]",
             "c:1:2:c(:c(:c(:c(:c:1:c(:c(:c(:c:2-[#1])-[#1])-[#6](=[#7]-[#6]:[#6])-[#6](-[#1])-[#1])-[#8]-[#1])-[#1])-[#1])-[#1])-[#1]",
             paste0("c:1(:c(:c:2:c(:c(:c:1-[#8]-[#6](-[#1])-[#1])-[#1]):c(:c(:c(:c:2-[#7](-[#1])-[#6](-[#1])(-[#1])-[#1])-[#1])-c:3:c(:c(:c(:c(:c:3-[#1])-[#1])-[#8]-[#6](-",
                    "[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#1])-[#1])-[#8]-[#6](-[#1])-[#1]"),
             "c:1:c:c-2:c(:c:c:1)-[#16]-c3c(-[#7]-2)cc(s3)-[#6](-[#1])-[#1]",
             "c:1:c:c:c-2:c(:c:1)-[#6](-[#6](-[#7]-2-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7]-4-[#6](-c:3:c:c:c:c:c:3-[#6]-4=[#8])=[#8])(-[#1])-[#1])(-[#1])-[#1]",
             "c:1(:c:c:c(:c:c:1)-[#6]-,:3=,:[#6]-,:[#6](-,:c2cocc2-,:[#6](=,:[#6]-,:3)-[#8]-[#1])=[#8])-[#16]-[#6](-[#1])-[#1]",
             "[#6;X4]-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#6](=[#8])-[#7](-[#1])-[#6](-[#1])(-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#16]-[#6](-[#1])(-[#1])-[#1])-[#6](=[#8])-[#8]-[#1])-[#1])-[#1]",
             "n:1:c(:n(:c(:c:1-c:2:c:c:c:c:c:2)-c:3:c:c:c:c:c:3)-[#7]=!@[#6])-[#7](-[#1])-[#1]",
             "[#6](-c:1:c:c:c(:c:c:1)-[#8]-[#1])(-c:2:c:c:c(:c:c:2)-[#8]-[#1])-[#8]-[#16](=[#8])=[#8]",
             "c:2:c:c:1:n:c(:c(:n:c:1:c:c:2)-[#6](-[#1])(-[#1])-[#6](=[#8])-[#6]:[#6])-[#6](-[#1])(-[#1])-[#6](=[#8])-[#6]:[#6]",
             "c:1(:c(:c(:c(:c(:c:1-[#1])-[#8]-[#6](-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#1])-[#6](=[#8])-[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-c:2:c:c:c(-[#6](-[#1])-[#1])c:c:2",
             "[#6](-[#1])(-[#1])-c1nnnn1-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#8]-[#6](-[#1])(-[#1])-[#1])-[#1])-[#1]",
             "[#6]-2(=[#7]-c1c(c(nn1-[#6](-[#6]-2(-[#1])-[#1])=[#8])-[#7](-[#1])-[#1])-[#7](-[#1])-[#1])-[#6]",
             "[#6](-[#6]:[#6])(-[#6]:[#6])(-[#6]:[#6])-[#16]-[#6]:[#6]-[#6](=[#8])-[#8]-[#1]",
             "[#8]=[#6](-c:1:c(:c(:n:c(:c:1-[#1])-[#8]-[#6](-[#1])(-[#1])-[#1])-[#8]-[#6](-[#1])(-[#1])-[#1])-[#1])-[#7](-[#1])-[#6](-[#1])(-[#6](-[#1])-[#1])-[#6](-[#1])-[#1]",
             "[#7]-1=[#6](-[#7](-[#6](-[#6](-[#6]-1(-[#1])-[#6]:[#6])(-[#1])-[#1])=[#8])-[#1])-[#7]-[#1]",
             "[#6]-1(=[#6](-[#6](-[#6](-[#6](-[#6]-1(-[#1])-[#1])(-[#1])-[#6](=[#8])-[#6])(-[#1])-[#6](=[#8])-[#8]-[#1])(-[#1])-[#1])-[#6]:[#6])-[#6]:[#6]",
             paste0("[#6](-[#1])(-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[Cl])-[#1])-[#1])(-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[Cl])-[#1])-[#1])-[#8]-[#6](-[#1])(-[#1])-[#6](-[#1])(",
                    "-[#1])-[#6](-[#1])(-[#1])-c3nc(c(n3-[#6](-[#1])(-[#1])-[#1])-[#1])-[#1]"),
             "n:1:c(:c(:c(:c(:c:1-[#1])-[#7](-[#1])-[#1])-[#1])-[#1])-[#7](-[#1])-[#6]:[#6]",
             "[#7](-[#1])(-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#8]-[#1])-[#6]-,:2=,:[#6](-,:[#8]-,:[#6](-,:[#7]=,:[#7]-,:2)=[#7])-[#7](-[#1])-[#1]",
             "[#7](-[#1])(-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#1]",
             "c:1:c:c-3:c(:c:c:1)-c:2:c:c:c(:c:c:2-[#6]-3=[#6](-[#1])-[#6])-[#7](-[#1])-[#1]",
             "c:1:c:c-2:c(:c:c:1)-[#7](-[#6](-[#8]-[#6]-2)(-[#6](=[#8])-[#8]-[#1])-[#6](-[#1])-[#1])-[#6](=[#8])-[#6](-[#1])-[#1]",
             "n:1:c(:c(:c(:c(:c:1-[#7](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#6](-[#1])-[#1])-[#7](-[#1])-[#1]",
             "[#7](-[#1])(-c:1:c:c:c:c:c:1)-[#6](-[#6])(-[#6])-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#1]",
             paste0("[#7](-[#1])(-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#8]-[#6](-[#1])(-[#1])-[#1])-[#8]-[#6]-[#1])-[#1])-[#6](=[#8])-[#7](-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1]",
                    ")(-[#1])-[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])(-[#1])-[#1])-[#6]:[#6]"),
             "c:1-2:c:c-3:c(:c:c:1-[#8]-[#6]-[#8]-2)-[#6]-[#6]-3",
             "c:1(:c(:c(:c(:c(:c:1-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#6](=[#8])-[#8]-[#1])-[#7](-[#1])-[#6]:[#6]",
             "c:1(:c:4:c(:n:c(:c:1-[#6](-[#1])(-[#1])-[#7]-3-c:2:c(:c(:c(:c(:c:2-[#6](-[#1])(-[#1])-[#6]-3(-[#1])-[#1])-[#1])-[#1])-[#1])-[#1])-[#1]):c(:c(:c(:c:4-[#1])-[#1])-[#1])-[#1])-[#1]",
             "c:1:c(:c2:c(:c:c:1)c(c(n2-[#1])-[#6]:[#6])-[#6]:[#6])-[#6](=[#8])-[#8]-[#1]",
             paste0("[#6]:[#6]-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7](-[#1])-[#6](=[#16])-[#7](-[#1])-c:1:c(:c(:c(:c(:c:1-[F",
                    ",Cl,Br,I])-[#1])-[#6](-[#1])-[#1])-[#1])-[#1]"),
             "n:1:c3:c(:c:c2:c:1nc(s2)-[#7])sc(n3)-[#7]",
             "[#7]=[#6]-1-[#16]-[#6](=[#7])-[#7]=[#6]-1",
             "c:1:c(:n:c:c:c:1)-[#6](=[#16])-[#7](-[#1])-c:2:c(:c:c:c:c:2)-[#8]-[#6](-[#1])-[#1]",
             "c:1-2:c(:c(:c(:c(:c:1-[#6](-c:3:c(-[#16]-[#6]-2(-[#1])-[#1]):c(:c(-[#1]):c(:c:3-[#1])-[#1])-[#1])-[#8]-[#6]:[#6])-[#1])-[#1])-[#1])-[#1]",
             paste0("[#6](-[#1])(-[#1])(-[#1])-c:1:c(:c(:c(:c(:n:1)-[#7](-[#1])-[#16](-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#8]-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1",
                    "])-[#1])-[#1])-[#1])(=[#8])=[#8])-[#1])-[#1])-[#1]"),
             "[#6](=[#8])(-[#7]-1-[#6]-[#6]-[#16]-[#6]-[#6]-1)-c:2:c(:c(:c(:c(:c:2-[#16]-[#6](-[#1])-[#1])-[#1])-[#1])-[#1])-[#1]",
             "c:1:c:c:3:c:2:c(:c:1)-[#6](-[#6]=[#6](-c:2:c:c:c:3)-[#8]-[#6](-[#1])-[#1])=[#8]",
             "c:1-3:c:2:c(:c(:c:c:1)-[#7]):c:c:c:c:2-[#6](-[#6]=[#6]-3-[#6](-[F])(-[F])-[F])=[#8]",
             "c:1:c:c:c:c:2:c:1:c:c:3:c(:n:2):n:c:4:c(:c:3-[#7]):c:c:c:c:4",
             "c:1:c-3:c(:c:c:c:1)-[#6]-2=[#7]-[!#1]=[#6]-[#6]-[#6]-2-[#6]-3=[#8]",
             paste0("c:1-3:c(:c(:c(:c(:c:1-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#6](=[#7]-[#7](-[#1])-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#6](=[#8])-[#8]-[#1])-[#1])-[#1",
                    "])-c:4:c-3:c(:c(:c(:c:4-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#1]"),
             "c:1(:c(:c(:c(:c(:c:1-[#1])-[#1])-[#7](-[#1])-[#1])-[#1])-[#1])-[#16](=[#8])(=[#8])-[#7](-[#1])-c:2:n:n:c(:c(:c:2-[#1])-[#1])-[#1]",
             "c2(c(-[#1])n(-[#6](-[#1])-[#1])c:3:c(:c(:c:1n(c(c(c:1:c2:3)-[#1])-[#1])-[#6](-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1]",
             "c1(c-2c(c(n1-[#6](-[#8])=[#8])-[#6](-[#1])-[#1])-[#16]-[#6](-[#1])(-[#1])-[#16]-2)-[#6](-[#1])-[#1]",
             "s1ccnc1-c2c(n(nc2-[#1])-[#1])-[#7](-[#1])-[#1]",
             "c1(c(c(c(n1-[#1])-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#1])-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#6](=[#8])-[#8]-[#1]",
             "c:1(:c(:c(:c(:o:1)-[#6])-[#1])-[#1])-[#6](=[#8])-[#7](-[#1])-[#6]:2:[#6](-[#1]):[#6](-[#1]):[#6](-[#1]):[#6](-[#1]):[#6]:2-[#6](=[#8])-[#8]-[#1]",
             "[!#1]:[#6]-[#6](=[#16])-[#7](-[#1])-[#7](-[#1])-[#6]:[!#1]",
             "[#6]-1(=[#8])-[#6](-[#6](-[#6]#[#7])=[#6](-[#1])-[#7])-[#6](-[#7])-[#6]=[#6]-1",
             "c2(c-,:1n(-,:[#6](-,:[#6]=,:[#6]-,:[#7]-,:1)=[#8])nc2-c3cccn3)-[#6]#[#7]",
             "[#8]=[#6]-1-[#6](=[#7]-[#7]-[#6]-[#6]-1)-[#6]#[#7]",
             "c:2(:c:1:c:c:c:c:c:1:n:n:c:2)-[#6](-[#6]:[#6])-[#6]#[#7]",
             "c:1:c:c-2:c(:c:c:1)-[#6]=[#6]-[#6](-[#7]-2-[#6](=[#8])-[#7](-[#1])-c:3:c:c(:c(:c:c:3)-[#8]-[#6](-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])(-[#6](-[#1])-[#1])-[#6](-[#1])-[#1]",
             "c:2:c:c:1:n:c(:c(:n:c:1:c:c:2)-c:3:c:c:c:c:c:3)-c:4:c:c:c:c:c:4-[#8]-[#1]",
             "[#6](-[#1])(-[#1])-[#6](-[#8]-[#1])=[#6](-[#6](=[#8])-[#6](-[#1])-[#1])-[#6](-[#1])-[#6]#[#6]",
             "c:1:c:4:c(:c:c2:c:1nc(n2-[#1])-[#6]-[#8]-[#6](=[#8])-c:3:c:c(:c:c(:c:3)-[#7](-[#1])-[#1])-[#7](-[#1])-[#1]):c:c:c:c:4",
             "c:2(:c:1:c:c:c:c-3:c:1:c(:c:c:2)-[#6]=[#6]-[#6]-3=[#7])-[#7]",
             "c:2(:c:1:c:c:c:c:c:1:c-3:c(:c:2)-[#6](-c:4:c:c:c:c:c-3:4)=[#8])-[#8]-[#1]",
             "[#6]-,:2(-,:[#6]=,:[#7]-,:c:1:c:c(:c:c:c:1-,:[#8]-,:2)-[Cl])=[#8]",
             "[#6]-1=[#6]-[#7](-[#6](-c:2:c-1:c:c:c:c:2)(-[#6]#[#7])-[#6](=[#16])-[#16])-[#6]=[#8]",
             "c2(nc:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])n2-[#6])-[#7](-[#1])-[#6](-[#7](-[#1])-c:3:c(:c:c:c:c:3-[#1])-[#1])=[#8]",
             "[#7](-[#1])(-[#6]:[#6])-c:1:c(-[#6](=[#8])-[#8]-[#1]):c:c:c(:n:1)-,:[#6]:[#6]",
             "c:1-,:3:c(:c:c:c:c:1)-,:[#16]-,:[#6](=[#7]-[#7]=[#6]-,:2-,:[#6]=,:[#6]-,:[#6]=,:[#6]-,:[#6]=,:[#6]-,:2)-,:[#7]-,:3-[#6](-[#1])-[#1]",
             "c:1-2:c(:c(:c(:c(:c:1-[#1])-[#8]-[#6](-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#6](=[#6](-[#6])-[#16]-[#6]-2(-[#1])-[#1])-[#6]",
             "c:12:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])c(c(-[#6]:[#6])n2-!@[#6]:[#6])-[#6](-[#1])-[#1]",
             "[#7](-[#1])(-[#1])-c:1:c:c:c(:c:c:1-[#8]-[#1])-[#16](=[#8])(=[#8])-[#8]-[#1]",
             "s:1:c:c:c(:c:1-[#1])-c:2:c:s:c(:n:2)-[#7](-[#1])-[#1]",
             "c1c(-[#7](-[#1])-[#1])nnc1-c2c(-[#6](-[#1])-[#1])oc(c2-[#1])-[#1]",
             "n1nscc1-c2nc(no2)-[#6]:[#6]",
             "c:1(:c:c-3:c(:c:c:1)-[#7]-[#6]-4-c:2:c:c:c:c:c:2-[#6]-[#6]-3-4)-[#6;X4]",
             "c:1-2:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#6](=[#6](-[#1])-[#6]-3-[#6](-[#6]#[#7])-[#6](-[#1])(-[#1])-[#6](-[#1])-[#7]-2-3)-[#1]",
             "c:2-,:3:c(:c:c:1:c:c:c:c:c:1:c:2)-,:[#7](-[#6](-[#1])-[#1])-,:[#6](=[#8])-,:[#6](=,:[#7]-,:3)-[#6]:[#6]-[#7](-[#1])-[#6](-[#1])-[#1]",
             "[#6](-[#8]-[#1]):[#6]-[#6](=[#8])-[#6](-[#1])=[#6](-[#6])-[#6]",
             "c:1:2:c(:c(:c(:c(:c:1-[#1])-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1]):c(:c(-[#1]):n:2-[#1])-[#16](=[#8])=[#8]",
             "c:1:2:c(:c(:c(:c(:c:1-[#1])-[#1])-[#7](-[#1])-[#1])-[#1]):c(:c(-[#1]):n:2-[#6](-[#1])-[#1])-[#1]",
             "[#16;X2]-1-[#6]=[#6](-[#6]#[#7])-[#6](-[#6])(-[#6]=[#8])-[#6](=[#6]-1-[#7](-[#1])-[#1])-[$([#6]=[#8]),$([#6]#[#7])]",
             "[#7]-2-[#6]=[#6](-[#6]=[#8])-[#6](-c:1:c:c:c(:c:c:1)-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#6]~3=,:[#6]-2~[#7]~[#6](~[#16])~[#7]~[#6]~3~[#7]",
             "c:1:c(:c:c:c:c:1)-[#6](=[#8])-[#7](-[#1])-c:2:c(:c:c:c:c:2)-[#6](=[#8])-[#7](-[#1])-[#7](-[#1])-c:3:n:c:c:s:3",
             "c:1:c:2:c(:c:c:c:1):c(:c:3:c(:c:2):c:c:c:c:3)-[#6]=[#7]-[#7](-[#1])-c:4:c:c:c:c:c:4",
             "c:1:c(:c:c:c:c:1)-[#6](-[#1])-[#7]-[#6](=[#8])-[#6](-[#7](-[#1])-[#6](-[#1])-[#1])=[#6](-[#1])-[#6](=[#8])-c:2:c:c:c(:c:c:2)-[#8]-[#6](-[#1])-[#1]",
             "s:1:c(:c(-[#1]):c(:c:1-[#6]-3=[#7]-c:2:c:c:c:c:c:2-[#6](=[#7]-[#7]-3-[#1])-c:4:c:c:n:c:c:4)-[#1])-[#1]",
             "o:1:c(:c(-[#1]):c(:c:1-[#6](-[#1])(-[#1])-[#7](-[#1])-[#6](=[#16])-[#7](-[#6]-[#1])-[#6](-[#1])(-[#1])-c:2:c:c:c:c:c:2)-[#1])-[#1]",
             paste0("c:1:c(:c:c:c:c:1)-[#7](-[#6]-[#1])-[#6](-[#1])-[#6](-[#1])-[#6](-[#1])-[#7](-[#1])-[#6](=[#8])-[#6]-,:2=,:[#6](-,:[#8]-,:[#6](-,:[#6](=,:[#6]-,:2-[#6]",
                    "(-[#1])-[#1])-[#1])=[#8])-[#6](-[#1])-[#1]"),
             "c2-3:c:c:c:1:c:c:c:c:c:1:c2-[#6](-[#1])-[#6;X4]-[#7]-[#6]-3=[#6](-[#1])-[#6](=[#8])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1]",
             "c:1:c(:c:c:c:c:1)-[#6]-4=[#7]-[#7]:2:[#6](:[#7+]:c:3:c:2:c:c:c:c:3)-[#16]-[#6;X4]-4",
             "[#6]-2(=[#8])-[#6](=[#6](-[#6](-[#1])-[#1])-[#7](-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])-[#1])-[#7]=[#6](-c:1:c:c:c:c:c:1)-[#8]-2",
             "c:1:c(:c:c:c:c:1)-[#7]-2-[#6](=[#8])-[#6](=[#6](-[#1])-[#6]-2=[#8])-[#16]-c:3:c:c:c:c:c:3",
             "[#7]-,:1(-[#1])-,:[#7]=,:[#6](-[#7]-[#1])-,:[#16]-,:[#6](=,:[#6]-,:1-,:[#6]:[#6])-,:[#6]:[#6]",
             "c:1(:c(:c-3:c(:c(:c:1-[#7](-[#1])-[#6](=[#16])-[#7](-[#1])-[#6](-[#1])-c:2:c(:c(:c(:o:2)-[#6]-[#1])-[#1])-[#1])-[#1])-[#8]-[#6](-[#8]-3)(-[#1])-[#1])-[#1])-[#1]",
             "c:1(:c(:c(:c(:c(:c:1-[#7](-[#1])-[#6](=[#16])-[#7](-[#1])-c:2:c:c:c:c:c:2)-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#1])-[#1]",
             "[#8]=[#6]-!@n:1:c:c:c-,:2:c:1-,:[#7](-[#1])-,:[#6](=[#16])-,:[#7]-,:2-[#1]",
             "[#6](-[F])(-[F])-[#6](=[#8])-[#7](-[#1])-c:1:c(-[#1]):n(-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#8]-[#6](-[#1])(-[#1])-[#6]:[#6]):n:c:1-[#1]",
             "[#7]-2=[#7]-[#6]:1:[#7]:[!#6&!#1]:[#7]:[#6]:1-[#7]=[#7]-[#6]:[#6]-2",
             "[#6]-2(-[#1])(-[#8]-[#1])-[#6]:1:[#7]:[!#6&!#1]:[#7]:[#6]:1-[#6](-[#1])(-[#8]-[#1])-[#6]=[#6]-2",
             "[#6]-1(-[#6](-[#1])(-[#1])-[#6]-1(-[#1])-[#1])(-[#6](=[#8])-[#7](-[#1])-c:2:c:c:c(:c:c:2)-[#8]-[#6](-[#1])(-[#1])-[#8])-[#16](=[#8])(=[#8])-[#6]:[#6]",
             "[#6]-1:[#6]-[#6](=[#8])-[#6]=[#6]-1-[#7]=[#6](-[#1])-[#7](-[#6;X4])-[#6;X4]",
             "c:1:c:c(:c:c-,:2:c:1-,:[#6](=,:[#6](-[#1])-,:[#6](=[#8])-,:[#8]-,:2)-c:3:c:c:c:c:c:3)-[#8]-[#6](-[#1])(-[#1])-[#6]:[#8]:[#6]",
             paste0("c:1:c(:o:c(:c:1-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#7]-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#8]-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-",
                    "[#8]-c:2:c:c-3:c(:c:c:2)-[#8]-[#6](-[#8]-3)(-[#1])-[#1]"),
             "[#7]-4(-c:1:c:c:c:c:c:1)-[#6](=[#8])-[#16]-[#6](-[#1])(-[#7](-[#1])-c:2:c:c:c:c:3:c:c:c:c:c:2:3)-[#6]-4=[#8]",
             "[#7]-3(-[#6](=[#8])-c:1:c:c:c:c:c:1)-[#6](=[#7]-c:2:c:c:c:c:c:2)-[#16]-[#6](-[#1])(-[#1])-[#6]-3=[#8]",
             "[#7]-2(-c:1:c:c:c:c:c:1)-[#6](=[#8])-[#16]-[#6](-[#1])(-[#1])-[#6]-2=[#16]",
             "[#7]-1(-[#6](-[#1])-[#1])-[#6](=[#16])-[#7](-[#6]:[#6])-[#6](=[#7]-[#6]:[#6])-[#6]-1=[#7]-[#6]:[#6]",
             "[#16]-1-[#6](=[#7]-[#7]-[#1])-[#16]-[#6](=[#7]-[#6]:[#6])-[#6]-1=[#7]-[#6]:[#6]",
             "[#6]-2(=[#8])-[#6](=[#6](-[#1])-c:1:c(:c:c:c(:c:1)-[F,Cl,Br,I])-[#8]-[#6](-[#1])-[#1])-[#7]=[#6](-[#16]-[#6](-[#1])-[#1])-[#16]-2",
             "[#6](-[#1])(-[#1])-[#16]-[#6](=[#16])-[#7](-[#1])-[#6](-[#1])(-[#1])-[#6]:[#6]",
             paste0("c:1(:c(:c(:c(:c(:c:1-[#1])-[#1])-[#6](-[#1])-[#1])-[#7](-[#1])-[#6](=[#8])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6]:[#6])-[#1])-[#7](-[#1])-[#6](=[#",
                    "8])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6]:[#6]"),
             "c:1(:c(:c(:c(:c(:c:1-[#6](-[#1])-[#1])-[#1])-[Br])-[#1])-[#6](-[#1])-[#1])-[#7](-[#1])-[#6](=[#8])-[#7](-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])-[#1]",
             "c:1-2:c(:c:c:c(:c:1-[#8]-[#6](-[#1])(-[#1])-[#7](-[#6]:[#6]-[#8]-[#6](-[#1])-[#1])-[#6]-2(-[#1])-[#1])-[#1])-[#1]",
             "c:1-2:c(:c(:c(:c(:c:1-[#8]-[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-[#6]-2(-[#1])-[#1])-[#1])-[#8])-[#8])-[#1]",
             "[#7](-[#1])(-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#6](-[#1])(-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1]",
             "n:1:2:c:c:c(:c:c:1:c:c(:c:2-[#6](=[#8])-[#6]:[#6])-[#6]:[#6])-[#6](~[#8])~[#8]",
             paste0("c:1(:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#6](=[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#6](-[#6;X4])(-[#6;X4])-[#7](-[#1])-[#6](=[#8])-[#7](-[#6](-[#",
                    "1])(-[#1])-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])-[#6](-[#1])(-[#1])-[#6]:[#6]"),
             "[#6]-3(-[#1])(-n:1:c(:n:c(:c:1-[#1])-[#1])-[#1])-c:2:c(:c(:c(:c(:c:2-[#1])-[Br])-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-c:4:c-3:c(:c(:c(:c:4-[#1])-[#1])-[#1])-[#1]",
             "[#6](=[#6](-[#1])-[#6](-[#1])(-[#1])-n:1:c(:n:c(:c:1-[#1])-[#1])-[#1])(-[#6]:[#6])-[#6]:[#6]",
             "c:1(:n:c(:c(-[#1]):s:1)-c:2:c:c:n:c:c:2)-[#7](-[#1])-[#6]:[#6]-[#6](-[#1])-[#1]",
             "c:1(:n:c(:c(-[#1]):s:1)-c:2:c:c:c:c:c:2)-[#6](-[#1])(-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7]-[#6](-[#1])(-[#1])-c:3:c:c:c:n:3-[#1]",
             "n:1(-[#1]):c(:c(-[#6](-[#1])-[#1]):c(:c:1-[#6](-[#1])(-[#1])-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])-[#1])-[#6](=[#8])-[#8]-[#6](-[#1])-[#1]",
             "c:2(:n:c:1:c(:c(:c:c(:c:1-[#1])-[F,Cl,Br,I])-[#1]):n:2-[#1])-[#16]-[#6](-[#1])(-[#1])-[#6](=[#8])-[#7](-[#1])-[#6]:[#6]",
             "c:1(:c(:c-2:c(:c(:c:1-[#8]-[#6](-[#1])-[#1])-[#1])-[#6]=[#6]-[#6](-[#1])-[#16]-2)-[#1])-[#8]-[#6](-[#1])-[#1]",
             "[#7]-1(-[#1])-[#6](=[#16])-[#6](-[#1])(-[#6]#[#7])-[#6](-[#1])(-[#6]:[#6])-[#6](=[#6]-1-[#6]:[#6])-[#1]",
             "n:1:c(:c(:c(:c(:c:1-[#16;X2]-c:2:c:c:c:c:c:2-[#7](-[#1])-[#1])-[#6]#[#7])-c:3:c:c:c:c:c:3)-[#6]#[#7])-[#7](-[#1])-[#1]",
             "[#7]-,:2(-c:1:c:c:c(:c:c:1)-[#8]-[#6](-[#1])-[#1])-,:[#6](=[#8])-,:[#6](=,:[#6]-,:[#6](=,:[#7]-,:2)-n:3:c:n:c:c:3)-[#6]#[#7]",
             "o:1:c(:c:c:2:c:1:c(:c(:c(:c:2-[#1])-[#8]-[#6](-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#6](~[#8])~[#8]",
             "[#6]#[#6]-[#6](=[#8])-[#6]#[#6]",
             "c:2(:c:1:c(:c(:c(:c(:c:1:c(:c(:c:2-[#8]-[#1])-[#6]=[#8])-[#1])-[#1])-[#1])-[#1])-[#1])-[#7](-[#1])-[#1]",
             "c:1(:c(:c(:[c;!H0,$(c-[#6;!H0;!H1])](:o:1))-[#1])-[#1])-[#6](=[#8])-[#7](-[#1])-[#7]=[#6;!H0,$([#6]-[#6;!H0!H1])]-c:2:c:c:c:c(:c:2)-[*]-[*]-[*]-c:3:c:c:c:o:3",
             "[#16](=[#8])(=[#8])-[#7](-[#1])-c:1:c(:c(:c(:s:1)-[#6]-[#1])-[#6]-[#1])-[#6](=[#8])-[#7]-[#1]",
             "[#6](-[#1])(-[#1])-[#8]-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#7](-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#8]-[#1])-[#6](-[#1])-[#1]",
             "[#6](-[#1])(-[#1])-[#8]-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#7](-[#1])-[#6](-[#1])(-[#6]=[#8])-[#16]",
             "n1nnnc2cccc12",
             "c:1-,:2:c(-[#1]):s:c(:c:1-,:[#6](=[#8])-,:[#7]-,:[#7]=,:[#6]-,:2-[#7](-[#1])-[#1])-[#6]=[#8]",
             "c:1-,:3:c(:c:2:c(:c:c:1-[Br]):o:c:c:2)-,:[#6](=,:[#6]-,:[#6](=[#8])-,:[#8]-,:3)-[#1]",
             "c:1-,:3:c(:c:c:c:c:1)-,:[#6](=,:[#6](-[#6](=[#8])-[#7](-[#1])-c:2:n:o:c:c:2-[Br])-,:[#6](=[#8])-,:[#8]-,:3)-[#1]",
             "c:1-,:2:c(:c:c(:c:c:1-[F,Cl,Br,I])-[F,Cl,Br,I])-,:[#6](=,:[#6](-[#6](=[#8])-[#7](-[#1])-[#1])-,:[#6](=[#7]-[#1])-,:[#8]-,:2)-[#1]",
             "c:1-,:3:c(:c:c:c:c:1)-,:[#6](=,:[#6](-[#6](=[#8])-[#7](-[#1])-c:2:n:c(:c:s:2)-[#6]:[#16]:[#6]-[#1])-,:[#6](=[#8])-,:[#8]-,:3)-[#1]",
             "[#6](-[#1])(-[#1])-[#16;X2]-c:2:n:n:c:1-[#6]:[#6]-[#7]=[#6]-[#8]-c:1:n:2",
             "[#16](=[#8])(=[#8])(-c:1:c:n(-[#6](-[#1])-[#1]):c:n:1)-[#7](-[#1])-c:2:c:n(:n:c:2)-[#6](-[#1])(-[#1])-[#6]:[#6]-[#8]-[#6](-[#1])-[#1]",
             "c:1-2:c(:c(:c(:c(:c:1-[#8]-[#6](-[#1])(-[#1])-[#8]-2)-[#6](-[#1])(-[#1])-[#7]-3-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6]:[#6]-3)-[#1])-[#1])-[#1]",
             "[#6](-[#1])(-[#1])-[#8]-[#6]:[#6]-[#6](-[#1])(-[#1])-[#7](-[#1])-c:2:c(:c(:c:1:n(:c(:n:c:1:c:2-[#1])-[#1])-[#6]-[#1])-[#1])-[#1]",
             "[#7]-,:4(-c:1:c:c:c:c:c:1)-,:[#6](=,:[#7+](-c:2:c:c:c:c:c:2)-,:[#6](=[#7]-c:3:c:c:c:c:c:3)-,:[#7]-,:4)-[#1]",
             "[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-c:2:c:c:c:1:s:c(:n:c:1:c:2)-[#16]-[#6](-[#1])-[#1]",
             "c:1:2:c(:c(:c(:c(:c:1:c(:c(-[#1]):c(:c:2-[#1])-[#1])-[#6](-[#6](-[#1])-[#1])=[#7]-[#7](-[#1])-[#6](=[#16])-[#7](-[#1])-[#6]:[#6]:[#6])-[#1])-[#1])-[#1])-[#1]",
             "[#6]:1(:[#7]:[#6](:[#7]:[!#1]:[#7]:1)-c:2:c(:c(:c(:o:2)-[#1])-[#1])-[#1])-[#16]-[#6;X4]",
             "n:1:c(:n:c(:n:c:1-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#7](-[#6]-[#1])-[#6]=[#8]",
             "c:1(:n:s:c(:n:1)-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7]-[#6](=[#8])-c:2:c:c:c:c:c:2-[#6](=[#8])-[#8]-[#1])-c:3:c:c:c:c:c:3",
             "n:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#6](=[#8])-[#7](-[#1])-[#7]=[#6](-[#1])-c:2:c:c:c:c:c:2-[#8]-[#6](-[#1])(-[#1])-[#6](=[#8])-[#8]-[#1]",
             paste0("[#6](-[#1])(-[#1])(-[#1])-[#6](-[#6](-[#1])(-[#1])-[#1])(-[#6](-[#1])(-[#1])-[#1])-c:1:c(:c(:c(:c(:c:1-[#8]-[#1])-[#6](-[#6](-[#1])(-[#1])-[#1])(-[#6]",
                    "(-[#1])(-[#1])-[#1])-[#6](-[#1])(-[#1])-[#1])-[#1])-[#6](-[#1])(-[#1])-c:2:c:c:c(:c(:c:2-[#1])-[#1])-[#8]-[#1])-[#1]"),
             "[#7](-[#1])(-[#1])-c:1:c(-[#7](-[#1])-[#1]):c(:c(-[#1]):c:2:n:o:n:c:1:2)-[#1]",
             "[#7](-[#1])(-[#1])-c:1:c(:c(:c(:c(:c:1-[#7](-[#1])-[#16](=[#8])=[#8])-[#1])-[#7](-[#1])-[#6](-[#1])-[#1])-[F,Cl,Br,I])-[#1]",
             "[#7](-[#1])(-[#1])-c:1:c(:c(:c(:c(:c:1-[#7]=[#6]-2-[#6](=[#6]~[#6]~[#6]=[#6]-2)-[#1])-[#1])-[#1])-[#1])-[#1]",
             "[#7](-[#1])(-[#1])-c:1:c(:c(:c(:c(:c:1-n:2:c:c:c:c:2)-[#1])-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1]",
             "[#16]=[#6]-[#6](-[#6](-[#1])-[#1])=[#6](-[#6](-[#1])-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1]",
             "[#6]-1:[#6]-[#8]-[#6]-2-[#6](-[#1])(-[#1])-[#6](=[#8])-[#8]-[#6]-1-2",
             "[#8]-[#6](=[#8])-[#6](-[#1])(-[#1])-[#16;X2]-[#6](=[#7]-[#6]#[#7])-[#7](-[#1])-c:1:c:c:c:c:c:1",
             "[#8]=[#6]-[#6]-1=[#6](-[#16]-[#6](=[#6](-[#1])-[#6])-[#16]-1)-[#6]=[#8]",
             "[#8]=[#6]-1-[#7]-[#7]-[#6](=[#7]-[#6]-1=[#6]-[#1])-[!#1]:[!#1]",
             "[#8]=[#6]-[#6](-[#1])=[#6](-[#6]#[#7])-[#6]",
             "[#8](-[#1])-[#6](=[#8])-c:1:c(:c(:c(:c(:c:1-[#8]-[#1])-[#1])-c:2:c(-[#1]):c(:c(:o:2)-[#6](-[#1])=[#6](-[#6]#[#7])-c:3:n:c:c:n:3)-[#1])-[#1])-[#1]",
             "c:1:c(:c:c:c:c:1)-[#7](-c:2:c:c:c:c:c:2)-[#7]=[#6](-[#1])-[#6]:3:[#6](:[#6](:[#6](:[!#1]:3)-c:4:c:c:c:c(:c:4)-[#6](=[#8])-[#8]-[#1])-[#1])-[#1]",
             "[#7](-[#1])(-[#1])-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-c:2:c(-[#1]):c(:c(-[#6](-[#1])-[#1]):o:2)-[#6]=[#8])-[#1])-[#1]",
             "[#8](-[#1])-[#6](=[#8])-c:1:c:c:c(:c:c:1)-[#7]-[#7]=[#6](-[#1])-[#6]:2:[#6](:[#6](:[#6](:[!#1]:2)-c:3:c:c:c:c:c:3)-[#1])-[#1]",
             "[#8](-[#1])-[#6](=[#8])-c:1:c:c:c:c(:c:1)-[#6]:[!#1]:[#6]-[#6]=[#7]-[#7](-[#1])-[#6](=[#8])-[#6](-[#1])(-[#1])-[#8]",
             "[#8](-[#1])-[#6]:1:[#6](:[#6]:[!#1]:[#6](:[#7]:1)-[#7](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](=[#8])-[#8]",
             "[#6]-1(=[!#6&!#1])-[#6](-[#7]=[#6]-[#16]-1)=[#8]",
             "n2(-c:1:c:c:c:c:c:1)c(c(-[#1])c(c2-[#6]=[#7]-[#8]-[#1])-[#1])-[#1]",
             "n2(-[#6](-[#1])-c:1:c(:c(:c:c(:c:1-[#1])-[#1])-[#1])-[#1])c(c(-[#1])c(c2-[#6]-[#1])-[#1])-[#6]-[#1]",
             "n1(-[#6](-[#1])-[#1])c(c(-[#6](=[#8])-[#6])c(c1-[#6]:[#6])-[#6])-[#6](-[#1])-[#1]",
             "n1(-[#6])c(c(-[#1])c(c1-[#6](-[#1])=[#6](-[#6]#[#7])-c:2:n:c:c:s:2)-[#1])-[#1]",
             "n3(-c:1:c:c:c:c:c:1-[#7](-[#1])-[#16](=[#8])(=[#8])-c:2:c:c:c:s:2)c(c(-[#1])c(c3-[#1])-[#1])-[#1]",
             "n2(-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#6](=[#8])-[#7](-[#1])-[#6](-[#1])(-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#8]-[#6]:[#6])c(c(-[#1])c(c2-[#1])-[#1])-[#1]",
             "c:1(:c:c:c:c:c:1)-[#7](-[#1])-[#6](=[#16])-[#7]-[#7](-[#1])-[#6](-[#1])=[#6](-[#1])-[#6]=[#8]",
             "[#6]-1(-[#6](=[#8])-[#6](-[#1])(-[#1])-[#6]-[#6](-[#1])(-[#1])-[#6]-1=[#8])=[#6](-[#7]-[#1])-[#6]=[#8]",
             "[#7](-[#1])(-[#1])-[#6]-1=[#6](-[#6]#[#7])-[#6](-[#1])(-[#6]:[#6])-[#16]-[#6;X4]-[#16]-1",
             "[#6](-[#1])(-[#1])-[#8]-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#7](-[#1])-c:2:c:c:n:c:3:c(:c:c:c(:c:2:3)-[#8]-[#6](-[#1])-[#1])-[#8]-[#6](-[#1])-[#1]",
             "[#6](-[#1])(-[#1])-[#8]-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#7](-[#1])-c:2:n:c(:c:s:2)-c:3:c:c:c(:c:c:3)-[#8]-[#6](-[#1])-[#1]",
             "[#6]~1~3~[#7](-[#6]:[#6])~[#6]~[#6]~[#6]~[#6]~1~[#6]~2~[#7]~[#6]~[#6]~[#6]~[#7+]~2~[#7]~3",
             "[#7]-3(-c:2:c:1:c:c:c:c:c:1:c:c:c:2)-[#7]=[#6](-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6]-3=[#8]",
             "[#6]-,:1(=,:[#6;!H0,$([#6]-[#6;!H0;!H1]),$([#6]-[#6]=[#8])]-,:[#16]-,:[#6](-,:[#7;!H0,$([#7]-[#6;!H0]),$([#7]-[#6]:[#6])]-,:1)=[#7;!R])-[$([#6](-[#1])-[#1]),$([#6]:[#6])]",
             "n2(-[#6]:1:[!#1]:[#6]:[#6]:[#6]:[#6]:1)c(cc(c2-[#6;X4])-[#1])-[#6;X4]",
             "c:1:c:c(:c(:c:c:1)-[#8]-[#1])-[#8]-[#1]",
             "[#6]-1(=[#6])-[#6](-[#7]=[#6]-[#16]-1)=[#8]",
             "[#6]-1=[!#1]-[!#6&!#1]-[#6](-[#6]-1=[!#6&!#1;!R])=[#8]",
             "[#6]-1(-[#6](-[#6]=[#6]-[!#6&!#1]-1)=[#6])=[!#6&!#1]",
             "[#6]-[#7]-1-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])(-[#1])-[#6]-1(-[#1])-[#1])-[#7]=[#6](-[#1])-[#6]:[!#1]",
             "c:1-2:c(:c:c:c:c:1)-[#6](=[#8])-[#6;X4]-[#6]-2=[#8]",
             "n1(-[#6])c(c(-[#1])c(c1-[#6]=[#7]-[#7])-[#1])-[#1]",
             "[#6]=!@[#6](-[!#1])-@[#6](=!@[!#6&!#1])-@[#6](=!@[#6])-[!#1]",
             "[#6](-[#6]#[#7])(-[#6]#[#7])-[#6](-[#7](-[#1])-[#1])=[#6]-[#6]#[#7]",
             "c:1-2:c(:c:c:c:c:1)-[#6](=[#8])-[#6](=[#6])-[#6]-2=[#8]",
             "[#6]-,:1(=,:[!#1]-,:[!#1]=,:[!#1]-,:[#7](-,:[#6]-,:1=[#16])-[#1])-[#6]#[#7]",
             "c:1:c:c-2:c(:c:c:1)-[#6]-3-[#6](-[#6]-[#7]-2)-[#6]-[#6]=[#6]-3",
             "c:1:c:2:c(:c:c:c:1):n:c:3:c(:c:2-[#7]):c:c:c:c:3",
             "[#6]-1(=[#6])-[#6](=[#8])-[#7]-[#7]-[#6]-1=[#8]",
             "[#7](-[#1])(-[#1])-c:1:c(:c(:c(:s:1)-[!#1])-[!#1])-[#6]=[#8]",
             "[#7]-[#6]=!@[#6]-2-[#6](=[#8])-c:1:c:c:c:c:c:1-[!#6&!#1]-2",
             "c:1(:c(:c(:c(:c(:c:1-[#8]-[#1])-[F,Cl,Br,I])-[#1])-[F,Cl,Br,I])-[#1])-[#16](=[#8])(=[#8])-[#7]",
             "[#6]-[#6](=[#16])-[#6]",
             "c:1:c:c(:c:c:c:1-[#8]-[#1])-[#7](-[#1])-[#16](=[#8])=[#8]",
             "c:1(:c(:c(:c(:c(:c:1-[#1])-[#1])-[$([#8]),$([#7]),$([#6](-[#1])-[#1])])-[#1])-[#1])-[#7](-[#1])-[#1]",
             "[c;!H0,$(c-[#6](-[#1])-[#1]),$(c-[#6]:[#6])]:1:c(:c(:c(:s:1)-[#7](-[#1])-[#6](=[#8])-[#6])-[#6](=[#8])-[#8])-[$([#6]:1:[#6]:[#6]:[#6]:[#6]:[#6]:1),$([#6]:1:[#16]:[#6]:[#6]:[#6]:1)]",
             paste0("[#7+]:1(:[#6]:[#6]:[!#1]:c:2:c:1:c(:[c;!H0,$(c-[#7])]:c:c:2)-[#1])-[$([#6](-[#1])(-[#1])-[#1]),$([#8;X1]),$([#6](-[#1])(-[#1])-[#6](-[#1])=[#6](-[#1])",
                    "-[#1]),$([#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#8]-[#1]),$([#6](-[#1])(-[#1])-[#6](=[#8])-[#6]),$([#6](-[#1])(-[#1])-[#6](=[#8])-[#7](-[#1])-[#6]:[#6",
                    "]),$([#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#1])]"),
             "c:1:c:c:c:c(:c:1-[#7&!H0;!H1,!$([#7]-[#6]=[#8])])-[#6](-[#6]:[#6])=[#8]",
             "[#7](-[#1])-[#7]=[#6](-[#6]#[#7])-[#6]=[!#6&!#1;!R]",
             "[#7](-c:1:c:c:c:c:c:1)-[#16](=[#8])(=[#8])-[#6]:2:[#6]:[#6]:[#6]:[#6]:3:[#7]:[$([#8]),$([#16])]:[#7]:[#6]:2:3",
             paste0("[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#6](-[#1])=[#7]-[#7]-[$([#6](=[#8])-[#6](-[#1])(-[#1])-[#16]-[#6]:[#7]),$(",
                    "[#6](=[#8])-[#6](-[#1])(-[#1])-[!#1]:[!#1]:[#7]),$([#6](=[#8])-[#6]:[#6]-[#8]-[#1]),$([#6]:[#7]),$([#6](-[#1])(-[#1])-[#6](-[#1])-[#8]-[#1])])-[#1])-[",
                    "#1]"),
             "[#7]-1-[#6](=[#16])-[#16]-[#6;X4]-[#6]-1=[#8]",
             "[#7](-[#1])-[#7]=[#6]-[#6;!H0,$(*(-[#6])-[#6])]=[#6](-[#6])-!@[$([#7]),$([#8]-[#1])]",
             "n2(-[#6]:1:[!#1]:[#6]:[#6]:[#6]:[#6]:1)c(cc(c2-[#6]:[#6])-[#1])-[#6;X4]",
             "s1ccc(c1)-[#8]-[#1]",
             "[#6]-,:1(=,:[#6](-,:[#6](=[#8])-,:[#7]-,:[#6](=,:[#7]-,:1)-,:[!#6&!#1])-[#6]#[#7])-[#6]",
             "[#6]-1(-[#6](=[#8])-[#7]-[#6](=[#8])-[#7]-[#6]-1=[#8])=[#7]",
             "[#6](-[#1])(-[#1])-[#7]([#6]:[#6])~[#6][#6]=,:[#6]-[#6]~[#6][#7]",
             "c:2:c:1:c:c:c:c-,:3:c:1:c(:c:c:2)-,:[#7]-,:[#6]=,:[#7]-,:3",
             "c:2:c:1:c:c:c:c-3:c:1:c(:c:c:2)-[#7](-[#6;X4]-[#7]-3-[#1])-[#1]",
             "[#6]-[#6](=[#8])-[#6](-[#1])=[#6](-[#7](-[#1])-[#6])-[#6](=[#8])-[#8]-[#6]",
             "[#16]=[#6]-1-[#6]=,:[#6]-[!#6&!#1]-[#6]=,:[#6]-1",
             "[#6](-[#6]#[#7])(-[#6]#[#7])-[#6](-[$([#6]#[#7]),$([#6]=[#7])])-[#6]#[#7]",
             "c:1:2:c(:c(:c(:c(:c:1:c(:c(:c(:c:2-[#1])-[#8]-[#1])-[#6](=[#8])-[#7](-[#1])-[#7]=[#6])-[#1])-[#1])-[#1])-[#1])-[#1]",
             "[#8]=[#6]-c2c1nc(-[#6](-[#1])-[#1])cc(-[#8]-[#1])n1nc2",
             "n:1:c(:n(:c(:c:1-c:2:c:c:c:c:c:2)-c:3:c:c:c:c:c:3)-[#1])-[#6]:[!#1]",
             "[#6](-[#6]#[#7])(-[#6]#[#7])=[#6]-c:1:c:c:c:c:c:1",
             "c:1(:c:c:c:c:c:1-[#7](-[#1])-[#7]=[#6])-[#6](=[#8])-[#8]-[#1]",
             "[#7+]([#6]:[#6])=,:[#6]-[#6](-[#1])=[#6]-[#7](-[#6;X4])-[#6]",
             "[#7](-[#1])(-[#1])-[#6]-1=[#6](-[#6]#[#7])-[#6](-[#1])(-[#6]:[#6])-[#6](=[#6](-[#7](-[#1])-[#1])-[#16]-1)-[#6]#[#7]",
             "[#7]~[#6]:1:[#7]:[#7]:[#6](:[$([#7]),$([#6]-[#1]),$([#6]-[#7]-[#1])]:[$([#7]),$([#6]-[#7])]:1)-[$([#7]-[#1]),$([#8]-[#6](-[#1])-[#1])]",
             "[#6]-[#6]=[#6](-[F,Cl,Br,I])-[#6](=[#8])-[#6]",
             "[#6](-[#6]#[#7])(-[#6]#[#7])=[#7]-[#7](-[#1])-c:1:c:c:c:c:c:1",
             paste0("[#6]-,:1(=,:[#6](-!@[#6](=[#8])-[#7]-[#6](-[#1])-[#1])-,:[#16]-,:[#6](-,:[#7]-,:1-,:[$([#6](-[#1])(-[#1])-[#6](-[#1])=[#6](-[#1])-[#1]),$([#6]:[#6])])",
                    "=[#16])-,:[$([#7]-[#6](=[#8])-[#6]:[#6]),$([#7](-[#1])-[#1])]"),
             paste0("[#16]-1-[#6](=[#8])-[#7]-[#6](=[#8])-[#6]-1=[#6](-[#1])-[$([#6]-[#35]),$([#6]:[#6](-[#1]):[#6](-[F,Cl,Br,I]):[#6]:[#6]-[F,Cl,Br,I]),$([#6]:[#6](-[#1])",
                    ":[#6](-[#1]):[#6]-[#16]-[#6](-[#1])-[#1]),$([#6]:[#6]:[#6]:[#6]:[#6]:[#6]:[#6]:[#6]:[#6]:[#6]-[#8]-[#6](-[#1])-[#1]),$([#6]:1:[#6](-[#6](-[#1])-[#1]):",
                    "[#7](-[#6](-[#1])-[#1]):[#6](-[#6](-[#1])-[#1]):[#6]:1)]"),
             "[#8]-,:1-,:[#6](-,:[#16]-,:c:2:c-,:1:c:c:c(:c:2)-,:[$([#7]),$([#8])])=[$([#8]),$([#16])]",
             "[#7](-[#6](-[#1])-[#1])(-[#6](-[#1])-[#1])-c:1:c(:c(:c(:o:1)-[#6]=[#7]-[#7](-[#1])-[#6]=[!#6&!#1])-[#1])-[#1]",
             "c:1(:c:c:c:c:c:1)-[#6](-[#1])=!@[#6]-3-[#6](=[#8])-c:2:c:c:c:c:c:2-[#16]-3",
             "[#6]-1(-[#6](~[!#6&!#1]~[#6]-[!#6&!#1]-[#6]-1=[!#6&!#1])~[!#6&!#1])=[#6;!R]-[#1]",
             "c:1:c:c(:c(:c:c:1)-[#6]=[#7]-[#7])-[#8]-[#1]",
             "[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-c:1:c:c(:c(:[c;!H0,$(c-[#6](-[#1])-[#1]),$(c-[#8]-[#6](-[#1])(-[#1])-[#6](-[#1])-[#1])](:c:1))-[#7])-[#1]",
             paste0("[n;!H0,$(n-[#6;!H0;!H1])]:1(c(c(c:2:c:1:c:c:c:c:2-[#1])-[#6;X4]-[#1])-[$([#6](-[#1])-[#1]),$([#6]=,:[!#6&!#1]),$([#6](-[#1])-[#7]),$([#6](-[#1])(-[#6]",
                    "(-[#1])-[#1])-[#6](-[#1])(-[#1])-[#7](-[#1])-[#6](-[#1])-[#1])])"),
             "[!#6&!#1]=[#6]1[#6]=,:[#6][#6](=[!#6&!#1])[#6]=,:[#6]1",
             "[#7;!R]=[#7]",
             "[#6]-[#6](=[!#6&!#1;!R])-[#6](=[!#6&!#1;!R])-[$([#6]),$([#16](=[#8])=[#8])]",
             "[#7]-[#6;X4]-c:1:c:c:c:c:c:1-[#8]-[#1]",
             "c:1:c:c(:c:c:c:1-[#7](-[#6;X4])-[#6;X4])-[#6]=[#6]",
             "c:1:c:c(:c:c:c:1-[#8]-[#6;X4])-[#7;$([#7!H0]-[#6;X4]),$([#7](-[#6;X4])-[#6;X4])]",
             "[#7]-1-[#6](=[#16])-[#16]-[#6](=[#6])-[#6]-1=[#8]",
             "c:1(:c:c:c(:c:c:1)-[#6]=[#7]-[#7])-[#8]-[#1]",
             "[#6]-1(=[#6])-[#6]=[#7]-[!#6&!#1]-[#6]-1=[#8]",
             "c:1:c:c(:c:c:c:1-[#7](-[#6;X4])-[#6;X4])-[#6;X4]-[$([#8]-[#1]),$([#6]=[#6]-[#1]),$([#7](-[#6X4])-[#6;X4])]",
             "[#8]=[#6]-2-[#6](=!@[#7]-[#7])-c:1:c:c:c:c:c:1-[#7]-2",
             "[#6](-[#1])-[#7](-[#6](-[#1])-[#1])-c:1:c(:c(:c(:[c;!H0,$(c-[#6](-[#1])-[#1])](:c:1-[#1]))-[#6&!H0;!H1,$([#6]-[#6;!H0])])-[#1])-[#1]"),
  stringsAsFactors = FALSE
)
