.cf_symbols <- strsplit(paste(
  "H He Li Be B C N O F Ne Na Mg Al Si P S Cl Ar K Ca Sc Ti V Cr Mn Fe Co Ni Cu Zn Ga Ge As Se Br Kr",
  "Rb Sr Y Zr Nb Mo Tc Ru Rh Pd Ag Cd In Sn Sb Te I Xe Cs Ba La Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm Yb",
  "Lu Hf Ta W Re Os Ir Pt Au Hg Tl Pb Bi Po At Rn",
  "Fr Ra Ac Th Pa U Np Pu Am Cm Bk Cf Es Fm Md No Lr Rf"
), " ")[[1]]
.cf_Z <- stats::setNames(seq_along(.cf_symbols), .cf_symbols)
.cf_aromsym <- c(c = 6, n = 7, o = 8, s = 16, p = 15, b = 5, se = 34, as = 33)
.cf_val <- list("1" = 1, "5" = 3, "6" = 4, "7" = 3, "8" = 2, "9" = 1, "14" = 4, "15" = c(3, 5), "16" = c(2, 4, 6),
                "17" = 1, "35" = 1, "53" = 1)

.cf_bridges <- function(n, B) {
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

.cf_rings <- function(n, B, ringb) {
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

.cf_hcount <- function(z, arom, chg, hexp, B) {
  n <- length(z)
  used <- numeric(n)
  for (k in seq_len(nrow(B))) {
    w <- if (B[k, 3] == 4) 1 else B[k, 3]
    used[B[k, 1]] <- used[B[k, 1]] + w
    used[B[k, 2]] <- used[B[k, 2]] + w
  }
  vapply(seq_len(n), function(i) {
    if (hexp[i] >= 0) return(as.numeric(hexp[i]))
    vals <- if (z[i] %in% c(5, 6, 7, 8, 15, 16)) .cf_val[[as.character(z[i] - chg[i])]] else if (chg[i] == 0) .cf_val[[as.character(z[i])]] else NULL
    if (is.null(vals)) return(0)
    tot <- used[i] + (if (arom[i] && !(z[i] %in% c(8, 16))) 1 else 0)
    for (v in vals) if (v >= tot) return(v - tot)
    0
  }, 0)
}

.cf_more_en <- c(7, 8, 9, 15, 16, 17, 33, 34, 35, 51, 52, 53)

.cf_electrons <- function(i, z, chg, nbr, ringb, hs) {
  nb <- nbr[[i]]
  dbl <- nb[nb[, 3] == 2, , drop = FALSE]
  deg <- nrow(nb) + hs[i]
  if (any(nb[, 3] == 3) || nrow(dbl) > 1 || deg > 3) return(NA)
  if (nrow(dbl)) {
    if (ringb[dbl[1, 2]]) return(1)
    if (z[i] != 6) return(NA)
    return(if (z[dbl[1, 1]] %in% .cf_more_en) 0 else 1)
  }
  if (z[i] == 6) return(if (chg[i] == -1) 2 else if (chg[i] == 1) 0 else NA)
  if (z[i] == 7) return(if ((chg[i] == 0 && deg == 3) || chg[i] == -1) 2 else NA)
  if (z[i] %in% c(8, 16, 34)) return(if (chg[i] == 0 && deg == 2) 2 else NA)
  if (z[i] == 5) return(if (deg == 3) 0 else NA)
  NA
}

.cf_kekulize <- function(n, z, arom, chg, hs, B, ringb) {
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
    vals <- if (z[i] %in% c(5, 6, 7, 8, 15, 16)) .cf_val[[as.character(z[i] - chg[i])]] else .cf_val[[as.character(z[i])]]
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

.cf_molecule_core <- function(el, arom0, chg0, hexp, B, explicit_h = FALSE) {
  n <- length(el)
  z <- unname(.cf_Z[el])
  arom <- arom0 == 1
  chg <- as.numeric(chg0)
  hs <- .cf_hcount(z, arom, chg, hexp, B)
  ringb <- if (nrow(B)) .cf_bridges(n, B) else logical(0)
  rs <- .cf_rings(n, B, ringb)
  order <- .cf_kekulize(n, z, arom, chg, hs, B, ringb)
  kek <- order  # valences follow the Kekule form, as RDKit's
  arom <- rep(FALSE, n)
  nbr <- lapply(seq_len(n), function(i) {
    ks <- which(B[, 1] == i | B[, 2] == i)
    if (!length(ks)) return(matrix(0, 0, 3))
    cbind(ifelse(B[ks, 1] == i, B[ks, 2], B[ks, 1]), ks, order[ks])
  })
  ec <- vapply(seq_len(n), function(i) .cf_electrons(i, z, chg, nbr, ringb, hs), 0)
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
        kek <- c(kek, 1)
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
    for (k in nbr[[i]][, 2]) s <- s + 2 * kek[k]
    s
  }, 0)
  hn <- vapply(seq_len(N), function(i) sum(z[nbr[[i]][, 1]] == 1), 0)
  list(z = z, arom = arom, chg = chg, h = hs, hn = hn, nbr = nbr, order = order, ringbond = ringb, nring = nring,
       smallest = smallest, rbcount = rbcount, val2 = val2, n = N,
       ring_bonds = lapply(rs, function(r) r$edges))
}

# ---------------------------------------------------------------- SMARTS parsing
.cf_env <- new.env()
.cf_env$next_id <- 0

.cf_match_close <- function(ch, i, op, cl) {
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

.cf_bracket_end <- function(ch, i) {
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

.cf_num <- function(ch, i) {
  j <- i
  while (j <= length(ch) && grepl("^[0-9]$", ch[j])) j <- j + 1
  list(v = if (j > i) as.numeric(paste(ch[i:(j - 1)], collapse = "")) else NULL, j = j)
}

.cf_atom_prim <- function(ch, i, first) {
  c0 <- ch[i]
  nx <- if (i + 1 <= length(ch)) ch[i + 1] else ""
  if (grepl("^[A-Z]$", c0) && grepl("^[a-z]$", nx) && paste0(c0, nx) %in% names(.cf_Z)) {
    return(list(node = list(t = "elem", z = .cf_Z[[paste0(c0, nx)]], ar = FALSE), j = i + 2))
  }
  if (c0 == "h") {
    r <- .cf_num(ch, i + 1)
    return(list(node = list(t = "himp", v = r$v), j = r$j))
  }
  if (c0 == "$") {
    j <- .cf_match_close(ch, i + 1, "(", ")")
    return(list(node = list(t = "rec", q = .cf_parse(paste(ch[(i + 2):(j - 1)], collapse = ""))), j = j + 1))
  }
  if (c0 == "*") return(list(node = list(t = "true"), j = i + 1))
  if (c0 == "#") {
    r <- .cf_num(ch, i + 1)
    return(list(node = list(t = "z", v = r$v), j = r$j))
  }
  if (grepl("^[0-9]$", c0)) {
    r <- .cf_num(ch, i)
    return(list(node = list(t = "iso", v = r$v), j = r$j))
  }
  if (c0 %in% c("+", "-")) {
    sg <- if (c0 == "+") 1 else -1
    r <- .cf_num(ch, i + 1)
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
    r <- .cf_num(ch, i + 1)
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
    if (nchar(two) == 2 && two %in% names(.cf_aromsym)) return(list(node = list(t = "elem", z = .cf_aromsym[[two]], ar = TRUE), j = i + 2))
    if (c0 %in% names(.cf_aromsym)) return(list(node = list(t = "elem", z = .cf_aromsym[[c0]], ar = TRUE), j = i + 1))
    stop("unknown SMARTS primitive ", c0)
  }
  if (grepl("^[A-Z]$", c0)) {
    two <- paste0(c0, nx)
    if (grepl("^[a-z]$", nx) && two %in% names(.cf_Z)) return(list(node = list(t = "elem", z = .cf_Z[[two]], ar = FALSE), j = i + 2))
    if (c0 %in% names(.cf_Z)) return(list(node = list(t = "elem", z = .cf_Z[[c0]], ar = FALSE), j = i + 1))
  }
  stop("unknown SMARTS primitive ", c0)
}

.cf_bond_prim <- function(ch, i, first) {
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

.cf_logic <- function(s, prim) {
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

.cf_default_bond <- list(t = "or", x = list(list(t = "single"), list(t = "aromatic")))
.cf_bond_chars <- c("-", "=", "#", ":", "~", "@", "!", "/", "\\", ",", ";", "&")

.cf_parse <- function(sm) {
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
    } else if (c0 %in% .cf_bond_chars) {
      j <- i
      while (j <= length(ch) && ch[j] %in% .cf_bond_chars) j <- j + 1
      pend <- .cf_logic(paste(ch[i:(j - 1)], collapse = ""), .cf_bond_prim)
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
        bp <- if (!is.null(pend)) pend else if (!is.null(rb$bp)) rb$bp else .cf_default_bond
        bonds[[length(bonds) + 1]] <- list(a = rb$a, b = prev, bp = bp)
        rings[[lab]] <- NULL
      } else {
        rings[[lab]] <- list(a = prev, bp = pend)
      }
      pend <- NULL
    } else {
      if (c0 == "[") {
        j <- .cf_bracket_end(ch, i)
        pred <- .cf_logic(paste(ch[(i + 1):(j - 1)], collapse = ""), .cf_atom_prim)
        i <- j + 1
      } else {
        two <- if (i + 1 <= length(ch)) paste0(c0, ch[i + 1]) else ""
        if (two %in% c("Cl", "Br")) {
          pred <- list(t = "elem", z = .cf_Z[[two]], ar = FALSE)
          i <- i + 2
        } else if (c0 %in% c("B", "C", "N", "O", "S", "P", "F", "I")) {
          pred <- list(t = "elem", z = .cf_Z[[c0]], ar = FALSE)
          i <- i + 1
        } else if (c0 %in% c("c", "n", "o", "s", "p", "b")) {
          pred <- list(t = "elem", z = .cf_aromsym[[c0]], ar = TRUE)
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
        bonds[[length(bonds) + 1]] <- list(a = prev, b = cur, bp = if (!is.null(pend)) pend else .cf_default_bond)
      } else {
        starts <- c(starts, cur)
      }
      prev <- cur
      pend <- NULL
    }
  }
  if (length(rings) || length(stack)) stop("unclosed ring or branch in SMARTS")
  .cf_env$next_id <- .cf_env$next_id + 1
  list(atoms = atoms, bonds = bonds, starts = starts, id = .cf_env$next_id)
}

.cf_is_h <- function(p) identical(p, list(t = "z", v = 1)) || identical(p, list(t = "elem", z = 1, ar = FALSE))

.cf_merge_pred <- function(p) {
  if (p$t == "rec") return(list(t = "rec", q = .cf_merge_hs(p$q)))
  if (p$t %in% c("and", "or")) return(list(t = p$t, x = lapply(p$x, .cf_merge_pred)))
  if (p$t == "not") return(list(t = "not", x = .cf_merge_pred(p$x)))
  p
}

.cf_merge_hs <- function(q) {
  atoms <- lapply(q$atoms, .cf_merge_pred)
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
      if (.cf_is_h(atoms[[h]]) && deg[h] == 1 && !.cf_is_h(atoms[[o]]) &&
          (identical(b$bp, .cf_default_bond) || identical(b$bp, list(t = "single")))) {
        drop <- c(drop, h)
        add[o] <- add[o] + 1
      }
    }
  }
  .cf_env$next_id <- .cf_env$next_id + 1
  if (!length(drop)) return(list(atoms = atoms, bonds = q$bonds, starts = q$starts, id = .cf_env$next_id))
  keep <- setdiff(seq_len(na), drop)
  idx <- match(seq_len(na), keep)
  new_atoms <- lapply(keep, function(i) if (add[i] > 0) list(t = "and", x = list(atoms[[i]], list(t = "hge", v = add[i]))) else atoms[[i]])
  new_bonds <- list()
  for (b in q$bonds) if (!(b$a %in% drop) && !(b$b %in% drop)) new_bonds[[length(new_bonds) + 1]] <- list(a = idx[b$a], b = idx[b$b], bp = b$bp)
  st <- idx[q$starts]
  list(atoms = new_atoms, bonds = new_bonds, starts = sort(unique(c(st[!is.na(st)], 1))), id = .cf_env$next_id)
}

# ---------------------------------------------------------------- matching
.cf_atom_ok <- function(p, m, i, cache) {
  switch(p$t,
    and = {
      for (x in p$x) if (!.cf_atom_ok(x, m, i, cache)) return(FALSE)
      TRUE
    },
    or = {
      for (x in p$x) if (.cf_atom_ok(x, m, i, cache)) return(TRUE)
      FALSE
    },
    not = !.cf_atom_ok(p$x, m, i, cache),
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
    iso = m$iso[i] == p$v,
    himp = if (is.null(p$v)) m$himp[i] >= 1 else m$himp[i] == p$v,
    inring = m$nring[i] > 0 || m$rbcount[i] > 0,
    R = if (p$v == 0) m$rbcount[i] == 0 else m$nring[i] == p$v,
    r = m$smallest[i] == p$v,
    x = m$rbcount[i] == p$v,
    xge = m$rbcount[i] >= 1,
    rec = {
      key <- paste(p$q$id, i)
      if (is.null(cache[[key]])) cache[[key]] <- .cf_embed(p$q, m, i, cache)
      cache[[key]]
    },
    stop("unknown predicate ", p$t)
  )
}

.cf_bond_ok <- function(p, m, k) {
  switch(p$t,
    and = {
      for (x in p$x) if (!.cf_bond_ok(x, m, k)) return(FALSE)
      TRUE
    },
    or = {
      for (x in p$x) if (.cf_bond_ok(x, m, k)) return(TRUE)
      FALSE
    },
    not = !.cf_bond_ok(p$x, m, k),
    true = TRUE,
    single = m$order[k] == 1,
    double = m$order[k] == 2,
    triple = m$order[k] == 3,
    aromatic = m$order[k] == 4,
    ringbond = m$ringbond[k],
    stop("unknown bond predicate ", p$t)
  )
}

.cf_embed <- function(q, m, anchor, cache) {
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
    if (!.cf_atom_ok(q$atoms[[v]], m, i, cache)) return(FALSE)
    for (e in qn[[v]]) {
      if (e$u < v && st$mp[e$u] > 0) {
        nb <- m$nbr[[i]]
        r <- which(nb[, 1] == st$mp[e$u])
        if (!length(r) || !.cf_bond_ok(e$bp, m, nb[r[1], 2])) return(FALSE)
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
.cf_crippen <- data.frame(
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


.cf_all_matches <- function(q, m, cache, cap = 1000) {
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
  st$found <- character(0)
  st$out <- list()
  ok_at <- function(v, i) {
    if (i %in% st$mp[seq_len(v - 1)]) return(FALSE)
    if (!.cf_atom_ok(q$atoms[[v]], m, i, cache)) return(FALSE)
    for (e in qn[[v]]) {
      if (e$u < v && st$mp[e$u] > 0) {
        nb <- m$nbr[[i]]
        r <- which(nb[, 1] == st$mp[e$u])
        if (!length(r) || !.cf_bond_ok(e$bp, m, nb[r[1], 2])) return(FALSE)
      }
    }
    TRUE
  }
  rec <- function(v) {
    if (length(st$out) >= cap) return(invisible())
    if (v > na) {
      key <- paste(sort(st$mp), collapse = ",")
      if (!(key %in% st$found)) {
        st$found <- c(st$found, key)
        st$out[[length(st$out) + 1]] <- sort(st$mp)
      }
      return(invisible())
    }
    cands <- if (parent[v] > 0) m$nbr[[st$mp[parent[v]]]][, 1] else seq_len(m$n)
    for (i in cands) {
      if (ok_at(v, i)) {
        st$mp[v] <- i
        rec(v + 1)
        st$mp[v] <- 0
      }
    }
  }
  rec(1)
  st$out
}

.cf_two_arom <- list(se = c(34, "s"), te = c(52, "s"), as = c(33, "p"))

.cf_pretokenize <- function(smiles) {
  ch <- strsplit(as.character(smiles), "")[[1]]
  out <- character(0)
  iso <- numeric(0)
  brk <- logical(0)
  zfix <- list()
  i <- 1
  while (i <= length(ch)) {
    c0 <- ch[i]
    if (c0 == "[") {
      j <- i + which(ch[(i + 1):length(ch)] == "]")[1]
      body <- paste(ch[(i + 1):(j - 1)], collapse = "")
      k <- 0
      bb <- strsplit(body, "")[[1]]
      while (k < length(bb) && grepl("^[0-9]$", bb[k + 1])) k <- k + 1
      iso <- c(iso, if (k) as.numeric(substr(body, 1, k)) else 0)
      rest <- substring(body, k + 1)
      if (grepl(":", rest, fixed = TRUE)) rest <- sub(":.*$", "", rest)
      two <- substr(rest, 1, 2)
      if (!is.null(.cf_two_arom[[two]])) {
        zfix[[as.character(length(iso))]] <- as.numeric(.cf_two_arom[[two]][1])
        rest <- paste0(.cf_two_arom[[two]][2], substring(rest, 3))
      } else if (nchar(rest) > 1 && grepl("^[A-Z][a-z]", rest) && !(two %in% c("Cl", "Br"))) {
        zfix[[as.character(length(iso))]] <- .cf_Z[[two]]
        rest <- paste0("C", substring(rest, 3))
      }
      sym <- if (substr(rest, 1, 2) %in% c("Cl", "Br")) 2 else 1
      if (!grepl("H", substring(rest, sym + 1), fixed = TRUE)) {
        rest <- paste0(substr(rest, 1, sym), "H0", substring(rest, sym + 1))
      }
      out <- c(out, paste0("[", rest, "]"))
      brk <- c(brk, TRUE)
      i <- j + 1
    } else if (i + 1 <= length(ch) && paste0(c0, ch[i + 1]) %in% c("Cl", "Br")) {
      out <- c(out, paste0(c0, ch[i + 1]))
      iso <- c(iso, 0)
      brk <- c(brk, FALSE)
      i <- i + 2
    } else if (grepl("^[A-Za-z]$", c0)) {
      out <- c(out, c0)
      iso <- c(iso, 0)
      brk <- c(brk, FALSE)
      i <- i + 1
    } else {
      out <- c(out, c0)
      i <- i + 1
    }
  }
  list(smiles = paste(out, collapse = ""), iso = iso, brk = brk, zfix = zfix)
}

.cf_molecule <- function(smiles, explicit_h = FALSE) {
  pt <- .cf_pretokenize(smiles)
  p <- morie_avalon_parse(pt$smiles)
  n <- length(p$el)
  B <- if (length(p$bonds)) do.call(rbind, p$bonds) + 0 else matrix(0, 0, 3)
  if (nrow(B)) B[, 1:2] <- B[, 1:2] + 1
  hs0 <- .cf_hcount(unname(.cf_Z[p$el]), p$arom == 1, as.numeric(p$chg), p$hexp, B)
  m <- .cf_molecule_core(p$el, p$arom, p$chg, p$hexp, B, explicit_h)
  for (a in names(pt$zfix)) m$z[as.integer(a)] <- pt$zfix[[a]]
  m$iso <- c(pt$iso, rep(0, m$n - n))
  m$himp <- c(ifelse(pt$brk, 0, hs0), rep(0, m$n - n))
  m$heavy <- n
  m$bonds <- B[, 1:2, drop = FALSE]
  m
}

.cf_count <- function(m, q, cache) length(.cf_all_matches(q, m, cache))

.cf_has <- function(m, q, cache) .cf_embed(q, m, NULL, cache)

.cf_frags <- function(m) {
  n <- m$heavy
  seen <- rep(FALSE, n)
  k <- 0
  for (s in seq_len(n)) {
    if (seen[s]) next
    k <- k + 1
    stack <- s
    seen[s] <- TRUE
    while (length(stack)) {
      u <- stack[length(stack)]
      stack <- stack[-length(stack)]
      for (v in m$nbr[[u]][, 1]) {
        if (v <= n && !seen[v]) {
          seen[v] <- TRUE
          stack <- c(stack, v)
        }
      }
    }
  }
  k
}

# the public MACCS key definitions of RDKit's MACCSkeys.py (G. Landrum, updated with A. Dalke 2011):
# key, SMARTS and the number of distinct matches the key requires to be exceeded; key 1 (isotope) is
# undefined, 125 (more than one aromatic ring) and 166 (more than one fragment) are computed
.cf_maccs <- data.frame(
  key = c(
    2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,
    53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100,
    101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140,
    141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165
  ),
  count = c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0,
    0, 0, 0, 1, 0, 1, 0, 3, 2, 1, 0, 0, 1, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0
  ),
  smarts = c(
    "[#104]", "[#32,#33,#34,#50,#51,#52,#82,#83,#84]", "[Ac,Th,Pa,U,Np,Pu,Am,Cm,Bk,Cf,Es,Fm,Md,No,Lr]", "[Sc,Ti,Y,Zr,Hf]", "[La,Ce,Pr,Nd,Pm,Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu]",
    "[V,Cr,Mn,Nb,Mo,Tc,Ta,W,Re]", "[!#6;!#1]1~*~*~*~1", "[Fe,Co,Ni,Ru,Rh,Pd,Os,Ir,Pt]", "[Be,Mg,Ca,Sr,Ba,Ra]", "*1~*~*~*~1", "[Cu,Zn,Ag,Cd,Au,Hg]", "[#8]~[#7](~[#6])~[#6]", "[#16]-[#16]",
    "[#8]~[#6](~[#8])~[#8]", "[!#6;!#1]1~*~*~1", "[#6]#[#6]", "[#5,#13,#31,#49,#81]", "*1~*~*~*~*~*~*~1", "[#14]", "[#6]=[#6](~[!#6;!#1])~[!#6;!#1]", "*1~*~*~1", "[#7]~[#6](~[#8])~[#8]", "[#7]-[#8]",
    "[#7]~[#6](~[#7])~[#7]", "[#6]=;@[#6](@*)@*", "[I]", "[!#6;!#1]~[CH2]~[!#6;!#1]", "[#15]", "[#6]~[!#6;!#1](~[#6])(~[#6])~*", "[!#6;!#1]~[F,Cl,Br,I]", "[#6]~[#16]~[#7]", "[#7]~[#16]", "[CH2]=*",
    "[Li,Na,K,Rb,Cs,Fr]", "[#16R]", "[#7]~[#6](~[#8])~[#7]", "[#7]~[#6](~[#6])~[#7]", "[#8]~[#16](~[#8])~[#8]", "[#16]-[#8]", "[#6]#[#7]", "F", "[!#6;!#1;!H0]~*~[!#6;!#1;!H0]",
    "[!#1;!#6;!#7;!#8;!#9;!#14;!#15;!#16;!#17;!#35;!#53]", "[#6]=[#6]~[#7]", "Br", "[#16]~*~[#7]", "[#8]~[!#6;!#1](~[#8])(~[#8])", "[!+0]", "[#6]=[#6](~[#6])~[#6]", "[#6]~[#16]~[#8]", "[#7]~[#7]",
    "[!#6;!#1;!H0]~*~*~*~[!#6;!#1;!H0]", "[!#6;!#1;!H0]~*~*~[!#6;!#1;!H0]", "[#8]~[#16]~[#8]", "[#8]~[#7](~[#8])~[#6]", "[#8R]", "[!#6;!#1]~[#16]~[!#6;!#1]", "[#16]!:*:*", "[#16]=[#8]",
    "*~[#16](~*)~*", "*@*!@*@*", "[#7]=[#8]", "*@*!@[#16]", "c:n", "[#6]~[#6](~[#6])(~[#6])~*", "[!#6;!#1]~[#16]", "[!#6;!#1;!H0]~[!#6;!#1;!H0]", "[!#6;!#1]~[!#6;!#1;!H0]", "[!#6;!#1]~[#7]~[!#6;!#1]",
    "[#7]~[#8]", "[#8]~*~*~[#8]", "[#16]=*", "[CH3]~*~[CH3]", "*!@[#7]@*", "[#6]=[#6](~*)~*", "[#7]~*~[#7]", "[#6]=[#7]", "[#7]~*~*~[#7]", "[#7]~*~*~*~[#7]", "[#16]~*(~*)~*", "*~[CH2]~[!#6;!#1;!H0]",
    "[!#6;!#1]1~*~*~*~*~1", "[NH2]", "[#6]~[#7](~[#6])~[#6]", "[C;H2,H3][!#6;!#1][C;H2,H3]", "[F,Cl,Br,I]!@*@*", "[#16]", "[#8]~*~*~*~[#8]",
    "[$([!#6;!#1;!H0]~*~*~[CH2]~*),$([!#6;!#1;!H0;R]1@[R]@[R]@[CH2;R]1),$([!#6;!#1;!H0]~[R]1@[R]@[CH2;R]1)]",
    "[$([!#6;!#1;!H0]~*~*~*~[CH2]~*),$([!#6;!#1;!H0;R]1@[R]@[R]@[R]@[CH2;R]1),$([!#6;!#1;!H0]~[R]1@[R]@[R]@[CH2;R]1),$([!#6;!#1;!H0]~*~[R]1@[R]@[CH2;R]1)]", "[#8]~[#6](~[#7])~[#6]", "[!#6;!#1]~[CH3]",
    "[!#6;!#1]~[#7]", "[#7]~*~*~[#8]", "*1~*~*~*~*~1", "[#7]~*~*~*~[#8]", "[!#6;!#1]1~*~*~*~*~*~1", "[#6]=[#6]", "*~[CH2]~[#7]",
    paste0("[$([R]@1@[R]@[R]@[R]@[R]@[R]@[R]@[R]1),$([R]@1@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]1),$([R]@1@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]1),$([R]@1@[R]@[R]@[R]@[R]",
           "@[R]@[R]@[R]@[R]@[R]@[R]1),$([R]@1@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]1),$([R]@1@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]1),$([R]@1@[R]",
           "@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]@[R]1)]"),
    "[!#6;!#1]~[#8]", "Cl", "[!#6;!#1;!H0]~*~[CH2]~*", "*@*(@*)@*", "[!#6;!#1]~*(~[!#6;!#1])~[!#6;!#1]", "[F,Cl,Br,I]~*(~*)~*", "[CH3]~*~*~*~[CH2]~*", "*~[CH2]~[#8]", "[#7]~[#6]~[#8]",
    "[#7]~*~[CH2]~*", "*~*(~*)(~*)~*", "[#8]!:*:*", "[CH3]~[CH2]~*", "[CH3]~*~[CH2]~*", "[$([CH3]~*~*~[CH2]~*),$([CH3]~*1~*~[CH2]1)]", "[#7]~*~[#8]", "[$(*~[CH2]~[CH2]~*),$(*1~[CH2]~[CH2]1)]",
    "[#7]=*", "[!#6;R]", "[#7;R]", "*~[#7](~*)~*", "[#8]~[#6]~[#8]", "[!#6;!#1]~[!#6;!#1]", "*!@[#8]!@*", "*@*!@[#8]",
    "[$(*~[CH2]~*~*~*~[CH2]~*),$([R]1@[CH2;R]@[R]@[R]@[R]@[CH2;R]1),$(*~[CH2]~[R]1@[R]@[R]@[CH2;R]1),$(*~[CH2]~*~[R]1@[R]@[CH2;R]1)]",
    "[$(*~[CH2]~*~*~[CH2]~*),$([R]1@[CH2]@[R]@[R]@[CH2;R]1),$(*~[CH2]~[R]1@[R]@[CH2;R]1)]", "[!#6;!#1]~[!#6;!#1]", "[!#6;!#1;!H0]", "[#8]~*~[CH2]~*", "*@*!@[#7]", "[F,Cl,Br,I]", "[#7]!:*:*", "[#8]=*",
    "[!C;!c;R]", "[!#6;!#1]~[CH2]~*", "[O;!H0]", "[#8]", "[CH3]", "[#7]", "*@*!@[#8]", "*!:*:*!:*", "*1~*~*~*~*~*~1", "[#8]", "[$(*~[CH2]~[CH2]~*),$([R]1@[CH2;R]@[CH2;R]1)]", "*~[!#6;!#1](~*)~*",
    "[C;H3,H4]", "*!@*@*!@*", "[#7;!H0]", "[#8]~[#6](~[#6])~[#6]", "[!#6;!#1]~[CH2]~*", "[#6]=[#8]", "*!@[CH2]!@*", "[#7]~*(~*)~*", "[#6]-[#8]", "[#6]-[#7]", "[#8]", "[C;H3,H4]", "[#7]", "a",
    "*1~*~*~*~*~*~1", "[#8]", "[R]"
  ),
  stringsAsFactors = FALSE
)

# alert_collection.csv of rd_filters (P. Walters, MIT licence): rule set, description and SMARTS of
# 1251 alerts; every rule allows zero matches
.cf_alerts <- data.frame(
  rule_set = c(
    "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo",
    "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo",
    "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Glaxo", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee",
    "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee",
    "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee",
    "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee",
    "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee",
    "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee", "Dundee",
    "Dundee", "Dundee", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS",
    "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS",
    "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS",
    "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS",
    "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS",
    "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS",
    "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS",
    "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "PAINS", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL",
    "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL",
    "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL",
    "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL",
    "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL",
    "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL",
    "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL",
    "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL",
    "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL",
    "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL",
    "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL",
    "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL",
    "SureChEMBL", "SureChEMBL", "SureChEMBL", "SureChEMBL", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR",
    "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR",
    "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR",
    "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR",
    "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR",
    "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "MLSMR", "Inpharmatica", "Inpharmatica",
    "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica",
    "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica",
    "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica",
    "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica",
    "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica",
    "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica",
    "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica",
    "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "Inpharmatica", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT",
    "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT",
    "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT", "LINT"
  ),
  description = c(
    "R1 Reactive alkyl halides", "R2 Acid halides", "R3 Carbazides", "R4 Sulphate esters", "R5 Sulphonates", "R6 Acid anhydrides", "R7 Peroxides", "R8 Pentafluorophenyl esters",
    "R9 Paranitrophenyl esters", "R10 esters of HOBT", "R11 Isocyanates & Isothiocyanates", "R12 Triflates", "R13 lawesson's reagent and derivatives", "R14 phosphoramides", "R15 Aromatic azides",
    "R16 beta carbonyl quaternary Nitrogen", "R17 acylhydrazide", "R18 Quaternary C, Cl, I, P or S", "R19 Phosphoranes", "R20 Chloramidines", "R21 Nitroso", "R22 P/S Halides", "R23 Carbodiimide",
    "R24 Isonitrile", "R25 Triacyloximes", "R26 Cyanohydrins", "R27 Acyl cyanides", "R28 Sulfonyl cyanides", "R29 Cyanophosphonates", "R30 Azocyanamides", "R31 Azoalkanals",
    "I1 Aliphatic methylene chains 7 or more long", "I2 Compounds with 4 or more acidic groups", "I3 Crown ethers", "I4 Disulphides", "I5 Thiols", "I6 Epoxides, Thioepoxides, Aziridines",
    "I7 2,4,5 trihydroxyphenyl", "I8 2,3,4 trihydroxyphenyl", "I9 Hydrazothiourea", "I10 Thiocyanate", "I11 Benzylic quaternary Nitrogen", "I12 Thioesters", "I13 Cyanamides",
    "I14 Four membered lactones", "I15 Di and Triphosphates", "I16 Betalactams", "N1 Quinones", "N2 Polyenes", "N3 Saponin derivatives", "N4 Cytochalasin derivatives", "N5 Cycloheximide derivatives",
    "N6 Monensin derivatives", "N7 Cyanidin derivatives", "N8 Squalestatin derivatives", "> 2 ester groups", "2-halo pyridine", "acid halide", "acyclic C=C-O", "acyl cyanide", "acyl hydrazine",
    "aldehyde", "Aliphatic long chain", "alkyl halide", "amidotetrazole", "aniline", "azepane", "Azido group", "Azo group", "azocane", "benzidine", "beta-keto/anhydride", "biotin analogue",
    "Carbocation/anion", "catechol", "charged oxygen or sulfur atoms", "chinone", "chinone", "conjugated nitrile group", "crown ether", "cumarine", "cyanamide", "cyanate/aminonitrile/thiocyanate",
    "cyanohydrins", "cycloheptane", "cycloheptane", "cyclooctane", "cyclooctane", "diaminobenzene", "diaminobenzene", "diaminobenzene", "diazo group", "diketo group", "disulphide", "enamine",
    "ester of HOBT", "four member lactones", "halogenated ring", "halogenated ring", "heavy metal", "het-C-het not in ring", "hydantoin", "hydrazine", "hydroquinone", "hydroxamic acid", "imine",
    "imine", "iodine", "isocyanate", "isolated alkene", "ketene", "methylidene-1,3-dithiole", "Michael acceptor", "Michael acceptor", "Michael acceptor", "Michael acceptor", "Michael acceptor",
    "N oxide", "N-acyl-2-amino-5-mercapto-1,3,4-thiadiazole", "N-C-halo", "N-halo", "N-hydroxyl pyridine", "nitro group", "N-nitroso", "oxime", "oxime", "Oxygen-nitrogen single bond",
    "perfluorinated chain", "peroxide", "phenol ester", "phenyl carbonate", "phosphor", "phthalimide", "Polycyclic aromatic hydrocarbon", "Polycyclic aromatic hydrocarbon",
    "Polycyclic aromatic hydrocarbon", "polyene", "quaternary nitrogen", "quaternary nitrogen", "quaternary nitrogen", "saponine derivative", "silicon halogen", "stilbene", "sulfinic acid",
    "Sulfonic acid", "Sulfonic acid", "sulfonyl cyanide", "sulfur oxygen single bond", "sulphate", "Sulphur-nitrogen single bond", "Thiobenzothiazole", "thiobenzothiazole", "Thiocarbonyl group",
    "thioester", "thiol", "thiol", "Three-membered heterocycle", "triflate", "triphenyl methylsilyl", "triple bond", "2halo_pyrazine_3EWG", "2halo_pyrazine_5EWG", "2halo_pyridazine_3EWG",
    "2halo_pyridazine_5EWG", "2halo_pyridine_3EWG", "2halo_pyridine_5EWG", "2halo_pyrimidine_5EWG", "3halo_pyridazine_2EWG", "3halo_pyridazine_4EWG", "4_pyridone_3_5_EWG", "4halo_pyridine_3EWG",
    "4halo_pyrimidine_2_6EWG", "4halo_pyrimidine_5EWG", "CH2_S#O_3_ring", "HOBT_ester", "NO_phosphonate", "acrylate", "activated_4mem_ring", "activated_S#O_3_ring", "activated_acetylene",
    "activated_diazo", "activated_vinyl_ester", "activated_vinyl_sulfonate", "acyclic_imide", "acyl_123_triazole", "acyl_134_triazole", "acyl_activated_NO", "acyl_cyanide", "acyl_imidazole",
    "acyl_pyrazole", "aldehyde", "alpha_dicarbonyl", "alpha_halo_EWG", "alpha_halo_amine", "alpha_halo_carbonyl", "alpha_halo_heteroatom", "alpha_halo_heteroatom_tert", "anhydride",
    "aryl_phosphonate", "aryl_thiocarbonyl", "azide", "aziridine_diazirine", "azo_amino", "azo_aryl", "azo_filter1", "azo_filter2", "azo_filter3", "azo_filter4", "bad_boron", "bad_cations",
    "benzidine_like", "beta_lactone", "betalactam", "betalactam_EWG", "bis_activated_aryl_ester", "bis_keto_olefin", "boron_warhead", "branched_polycyclic_aromatic", "carbodiimide_iso#thio#cyanate",
    "carbonyl_halide", "contains_metal", "crown_ether", "cyano_phosphonate", "cyanohydrin", "diamino_sulfide", "diazo_carbonyl", "diazonium", "dicarbonyl_sulfonamide", "disulfide_acyclic",
    "disulfonyliminoquinone", "double_trouble_warhead", "flavanoid", "four_nitriles", "gte_10_carbon_sb_chain", "gte_2_N_quats", "gte_2_free_phos", "gte_2_sulfonic_acid", "gte_3_COOH", "gte_3_iodine",
    "gte_4_basic_N", "gte_4_nitro", "gte_5_phenolic_OH", "gte_7_aliphatic_OH", "gte_7_total_hal", "gte_8_CF2_or_CH2", "halo_5heterocycle_bis_EWG", "halo_acrylate", "halo_imino", "halo_olefin_bis_EWG",
    "halo_phenolic_carbonyl", "halo_phenolic_sulfonyl", "halogen_heteroatom", "hetero_silyl", "hydrazine", "hydrazothiourea", "hydroxamate_warhead", "hyperval_sulfur", "isonitrile",
    "keto_def_heterocycle", "linear_polycyclic_aromatic_I", "linear_polycyclic_aromatic_II", "maleimide_etc", "meldrums_acid_deriv", "monofluoroacetate", "nitrone", "nitrosamine",
    "non_ring_CH2O_acetal", "non_ring_acetal", "non_ring_ketal", "ortho_hydroiminoquinone", "ortho_hydroquinone", "ortho_nitrophenyl_carbonyl", "ortho_quinone", "oxaziridine", "oxime", "oxonium",
    "para_hydroiminoquinone", "para_hydroquinone", "para_nitrophenyl_ester", "para_quinone", "paraquat_like", "pentafluorophenylester", "perchloro_cp", "perhalo_dicarbonyl_phenyl", "perhalo_phenyl",
    "peroxide", "phenolate_bis_EWG", "phos_serine_warhead", "phos_threonine_warhead", "phos_tyrosine_warhead", "phosphite", "phosphonium", "phosphorane", "phosphorous_nitrogen_bond",
    "phosphorus_phosphorus_bond", "phosphorus_sulfur_bond", "polyene", "polyhalo_phenol_a", "polyhalo_phenol_b", "polyhalo_phenol_c", "polyhalo_phenol_d", "polyhalo_phenol_e", "polysulfide",
    "porphyrin", "primary_halide_sulfate", "quat_N_N", "quat_N_acyl", "quinone_methide", "rhodanine", "secondary_halide_sulfate", "sulf_D2_nitrogen", "sulf_D2_oxygen_D2", "sulf_D3_nitrogen",
    "sulfite_sulfate_ester", "sulfonium", "sulfonyl_anhydride", "sulfonyl_halide", "sulfonyl_heteroatom", "sulphonyl_cyanide", "tertiary_halide_sulfate", "thio_hydroxamate", "thio_xanthate",
    "thiocarbonate", "thioester", "thiol_warhead", "thiopyrylium", "thiosulfoxide", "triamide", "triaryl_phosphine_oxide", "trichloromethyl_ketone", "triflate", "trifluoroacetate_ester",
    "trifluoroacetate_thioester", "trifluoromethyl_ketone", "trihalovinyl_heteroatom", "trinitro_aromatic", "trinitromethane_derivative", "tris_activated_aryl_ester", "trisub_bis_act_olefin",
    "vinyl_carbonyl_EWG", "ene_six_het_A(483)", "hzone_phenol_A(479)", "anil_di_alk_A(478)", "indol_3yl_alk(461)", "quinone_A(370)", "azo_A(324)", "imine_one_A(321)", "mannich_A(296)",
    "anil_di_alk_B(251)", "anil_di_alk_C(246)", "ene_rhod_A(235)", "hzone_phenol_B(215)", "ene_five_hetA1(201A)", "ene_five_het_A(201)", "anil_di_alk_D(198)", "imine_one_isatin(189)",
    "anil_di_alk_E(186)", "thiaz_ene_A(128)", "pyrrole_A(118)", "catechol_A(92)", "ene_five_het_B(90)", "imine_one_fives(89)", "ene_five_het_C(85)", "hzone_pipzn(79)", "keto_keto_beta_A(68)",
    "hzone_pyrrol(64)", "ene_one_ene_A(57)", "cyano_ene_amine_A(56)", "ene_five_one_A(55)", "cyano_pyridone_A(54)", "anil_alk_ene(51)", "amino_acridine_A(46)", "ene_five_het_D(46)",
    "thiophene_amino_Aa(45)", "ene_five_het_E(44)", "sulfonamide_A(43)", "thio_ketone(43)", "sulfonamide_B(41)", "anil_no_alk(40)", "thiophene_amino_Ab(40)", "het_pyridiniums_A(39)",
    "anthranil_one_A(38)", "cyano_imine_A(37)", "diazox_sulfon_A(36)", "hzone_anil_di_alk(35)", "rhod_sat_A(33)", "hzone_enamin(30)", "pyrrole_B(29)", "thiophene_hydroxy(28)", "cyano_pyridone_B(27)",
    "imine_one_sixes(27)", "dyes5A(27)", "naphth_amino_A(25)", "naphth_amino_B(25)", "ene_one_ester(24)", "thio_dibenzo(23)", "cyano_cyano_A(23)", "hzone_acyl_naphthol(22)", "het_65_A(21)",
    "imidazole_A(19)", "ene_cyano_A(19)", "anthranil_acid_A(19)", "dyes3A(19)", "dhp_bis_amino_CN(19)", "het_6_tetrazine(18)", "ene_one_hal(17)", "cyano_imine_B(17)", "thiaz_ene_B(17)",
    "ene_rhod_B(16)", "thio_carbonate_A(15)", "anil_di_alk_furan_A(15)", "ene_five_het_F(15)", "anil_di_alk_F(14)", "hzone_anil(14)", "het_5_pyrazole_OH(14)", "het_thio_666_A(13)", "styrene_A(13)",
    "ene_rhod_C(13)", "dhp_amino_CN_A(13)", "cyano_imine_C(12)", "thio_urea_A(12)", "thiophene_amino_B(12)", "keto_keto_beta_B(12)", "keto_phenone_A(11)", "cyano_pyridone_C(11)", "thiaz_ene_C(11)",
    "hzone_thiophene_A(11)", "ene_quin_methide(10)", "het_thio_676_A(10)", "ene_five_het_G(10)", "acyl_het_A(9)", "anil_di_alk_G(9)", "dhp_keto_A(9)", "thio_urea_B(9)", "anil_alk_bim(9)",
    "imine_imine_A(9)", "thio_urea_C(9)", "imine_one_fives_B(9)", "dhp_amino_CN_B(9)", "anil_OC_no_alk_A(8)", "het_thio_66_one(8)", "styrene_B(8)", "het_thio_5_A(8)", "anil_di_alk_ene_A(8)",
    "ene_rhod_D(8)", "ene_rhod_E(8)", "anil_OH_alk_A(8)", "pyrrole_C(8)", "thio_urea_D(8)", "thiaz_ene_D(8)", "ene_rhod_F(8)", "thiaz_ene_E(8)", "het_65_B(7)", "keto_keto_beta_C(7)", "het_66_A(7)",
    "thio_urea_E(7)", "thiophene_amino_C(7)", "hzone_phenone(7)", "ene_rhod_G(7)", "ene_cyano_B(7)", "dhp_amino_CN_C(7)", "het_5_A(7)", "ene_five_het_H(6)", "thio_amide_A(6)", "ene_cyano_C(6)",
    "hzone_furan_A(6)", "anil_di_alk_H(6)", "het_65_C(6)", "thio_urea_F(6)", "ene_five_het_I(6)", "keto_keto_gamma(5)", "quinone_B(5)", "het_6_pyridone_OH(5)", "hzone_naphth_A(5)", "thio_ester_A(5)",
    "ene_misc_A(5)", "cyano_pyridone_D(5)", "het_65_Db(5)", "het_666_A(5)", "diazox_sulfon_B(5)", "anil_NH_alk_A(5)", "sulfonamide_C(5)", "het_thio_N_55(5)", "keto_keto_beta_D(5)", "ene_rhod_H(5)",
    "imine_ene_A(5)", "het_thio_656a(5)", "pyrrole_D(5)", "pyrrole_E(5)", "thio_urea_G(5)", "anisol_A(5)", "pyrrole_F(5)", "dhp_amino_CN_D(5)", "thiazole_amine_A(4)", "het_6_imidate_A(4)",
    "anil_OC_no_alk_B(4)", "styrene_C(4)", "azulene(4)", "furan_acid_A(4)", "cyano_pyridone_E(4)", "anil_alk_thio(4)", "anil_di_alk_I(4)", "het_thio_6_furan(4)", "anil_di_alk_ene_B(4)",
    "imine_one_B(4)", "anil_OC_alk_A(4)", "ene_five_het_J(4)", "pyrrole_G(4)", "ene_five_het_K(4)", "cyano_ene_amine_B(4)", "thio_ester_B(4)", "ene_five_het_L(4)", "hzone_thiophene_B(4)",
    "dhp_amino_CN_E(4)", "het_5_B(4)", "imine_imine_B(3)", "thiazole_amine_B(3)", "imine_ene_one_A(3)", "diazox_A(3)", "ene_one_A(3)", "anil_OC_no_alk_C(3)", "thiazol_SC_A(3)", "het_666_B(3)",
    "furan_A(3)", "colchicine_A(3)", "thiophene_C(3)", "anil_OC_alk_B(3)", "het_thio_66_A(3)", "rhod_sat_B(3)", "ene_rhod_I(3)", "keto_thiophene(3)", "imine_imine_C(3)", "het_65_pyridone_A(3)",
    "thiazole_amine_C(3)", "het_thio_pyr_A(3)", "melamine_A(3)", "anil_NH_alk_B(3)", "rhod_sat_C(3)", "thiophene_amino_D(3)", "anil_OC_alk_C(3)", "het_thio_65_A(3)", "het_thio_656b(3)",
    "thiazole_amine_D(3)", "thio_urea_H(3)", "cyano_pyridone_F(3)", "rhod_sat_D(3)", "ene_rhod_J(3)", "imine_phenol_A(3)", "thio_carbonate_B(3)", "het_thio_N_5A(3)", "het_thio_N_65A(3)",
    "anil_di_alk_J(3)", "pyrrole_H(3)", "ene_cyano_D(3)", "cyano_cyano_B(3)", "ene_five_het_M(3)", "cyano_ene_amine_C(3)", "thio_urea_I(3)", "dhp_amino_CN_F(3)", "anthranil_acid_B(3)", "diazox_B(3)",
    "thio_aldehyd_A(3)", "thio_amide_B(2)", "imidazole_B(2)", "thiazole_amine_E(2)", "thiazole_amine_F(2)", "thio_ester_C(2)", "ene_one_B(2)", "quinone_C(2)", "keto_naphthol_A(2)", "thio_amide_C(2)",
    "phthalimide_misc(2)", "sulfonamide_D(2)", "anil_NH_alk_C(2)", "het_65_E(2)", "hzide_naphth(2)", "anisol_B(2)", "thio_carbam_ene(2)", "thio_amide_D(2)", "het_65_Da(2)", "thiophene_D(2)",
    "het_thio_6_ene(2)", "cyano_keto_A(2)", "anthranil_acid_C(2)", "naphth_amino_C(2)", "naphth_amino_D(2)", "thiazole_amine_G(2)", "het_66_B(2)", "coumarin_A(2)", "anthranil_acid_D(2)",
    "het_66_C(2)", "thiophene_amino_E(2)", "het_6666_A(2)", "sulfonamide_E(2)", "anil_di_alk_K(2)", "het_5_C(2)", "ene_six_het_B(2)", "steroid_A(2)", "het_565_A(2)", "thio_imine_ium(2)",
    "anthranil_acid_E(2)", "hzone_furan_B(2)", "thiophene_E(2)", "ene_misc_B(2)", "het_thio_5_B(2)", "thiophene_amino_F(2)", "anil_OC_alk_D(2)", "tert_butyl_A(2)", "thio_urea_J(2)",
    "het_thio_65_B(2)", "coumarin_B(2)", "thio_urea_K(2)", "thiophene_amino_G(2)", "anil_NH_alk_D(2)", "het_thio_5_C(2)", "thio_keto_het(2)", "het_thio_N_5B(2)", "quinone_D(2)",
    "anil_di_alk_furan_B(2)", "ene_six_het_C(2)", "het_55_A(2)", "het_thio_65_C(2)", "hydroquin_A(2)", "anthranil_acid_F(2)", "pyrrole_I(2)", "thiophene_amino_H(2)", "imine_one_fives_C(2)",
    "keto_phenone_zone_A(2)", "dyes7A(2)", "het_pyridiniums_B(2)", "het_5_D(2)", "thiazole_amine_H(1)", "thiazole_amine_I(1)", "het_thio_N_5C(1)", "sulfonamide_F(1)", "thiazole_amine_J(1)",
    "het_65_F(1)", "keto_keto_beta_E(1)", "ene_five_one_B(1)", "keto_keto_beta_zone(1)", "thio_urea_L(1)", "het_thio_urea_ene(1)", "cyano_amino_het_A(1)", "tetrazole_hzide(1)", "imine_naphthol_A(1)",
    "misc_anisole_A(1)", "het_thio_665(1)", "anil_di_alk_L(1)", "colchicine_B(1)", "misc_aminoacid_A(1)", "imidazole_amino_A(1)", "phenol_sulfite_A(1)", "het_66_D(1)", "misc_anisole_B(1)",
    "tetrazole_A(1)", "het_65_G(1)", "misc_trityl_A(1)", "misc_pyridine_OC(1)", "het_6_hydropyridone(1)", "misc_stilbene(1)", "misc_imidazole(1)", "anil_NH_no_alk_A(1)", "het_6_imidate_B(1)",
    "anil_alk_B(1)", "styrene_anil_A(1)", "misc_aminal_acid(1)", "anil_no_alk_D(1)", "anil_alk_C(1)", "misc_anisole_C(1)", "het_465_misc(1)", "anthranil_acid_G(1)", "anil_di_alk_M(1)",
    "anthranil_acid_H(1)", "thio_urea_M(1)", "thiazole_amine_K(1)", "het_thio_5_imine_A(1)", "thio_amide_E(1)", "het_thio_676_B(1)", "sulfonamide_G(1)", "thio_thiomorph_Z(1)", "naphth_ene_one_A(1)",
    "naphth_ene_one_B(1)", "amino_acridine_A(1)", "keto_phenone_B(1)", "hzone_acid_A(1)", "sulfonamide_H(1)", "het_565_indole(1)", "pyrrole_J(1)", "pyrazole_amino_B(1)", "pyrrole_K(1)",
    "anthranil_acid_I(1)", "thio_amide_F(1)", "ene_one_C(1)", "het_65_H(1)", "cyano_imine_D(1)", "cyano_misc_A(1)", "ene_misc_C(1)", "het_66_E(1)", "keto_keto_beta_F(1)", "misc_naphthimidazole(1)",
    "naphth_ene_one_C(1)", "keto_phenone_C(1)", "coumarin_C(1)", "thio_est_cyano_A(1)", "het_65_imidazole(1)", "anthranil_acid_J(1)", "colchicine_het(1)", "ene_misc_D(1)", "indole_3yl_alk_B(1)",
    "anil_OH_no_alk_A(1)", "thiazole_amine_L(1)", "pyrazole_amino_A(1)", "het_thio_N_5D(1)", "anil_alk_indane(1)", "anil_di_alk_N(1)", "het_666_C(1)", "ene_one_D(1)", "anil_di_alk_indol(1)",
    "anil_no_alk_indol_A(1)", "dhp_amino_CN_G(1)", "anil_di_alk_dhp(1)", "anthranil_amide_A(1)", "hzone_anthran_Z(1)", "ene_one_amide_A(1)", "het_76_A(1)", "thio_urea_N(1)", "anil_di_alk_coum(1)",
    "ene_one_amide_B(1)", "het_thio_656c(1)", "het_5_ene(1)", "thio_imide_A(1)", "dhp_amidine_A(1)", "thio_urea_O(1)", "anil_di_alk_O(1)", "thio_urea_P(1)", "het_pyraz_misc(1)", "diazox_C(1)",
    "diazox_D(1)", "misc_cyclopropane(1)", "imine_ene_one_B(1)", "coumarin_D(1)", "misc_furan_A(1)", "rhod_sat_E(1)", "rhod_sat_imine_A(1)", "rhod_sat_F(1)", "het_thio_5_imine_B(1)",
    "het_thio_5_imine_C(1)", "ene_five_het_N(1)", "thio_carbam_A(1)", "misc_anilide_A(1)", "misc_anilide_B(1)", "mannich_B(1)", "mannich_catechol_A(1)", "anil_alk_D(1)", "het_65_I(1)",
    "misc_urea_A(1)", "imidazole_C(1)", "styrene_imidazole_A(1)", "thiazole_amine_M(1)", "misc_pyrrole_thiaz(1)", "pyrrole_L(1)", "het_thio_65_D(1)", "ene_misc_E(1)", "thio_cyano_A(1)",
    "cyano_amino_het_B(1)", "cyano_pyridone_G(1)", "het_65_J(1)", "ene_one_yne_A(1)", "anil_OH_no_alk_B(1)", "hzone_acyl_misc_A(1)", "thiophene_F(1)", "anil_OC_alk_E(1)", "anil_OC_alk_F(1)",
    "het_65_K(1)", "het_65_L(1)", "coumarin_E(1)", "coumarin_F(1)", "coumarin_G(1)", "coumarin_H(1)", "het_thio_67_A(1)", "sulfonamide_I(1)", "het_65_mannich(1)", "anil_alk_A(1)", "het_5_inium(1)",
    "anil_di_alk_P(1)", "thio_urea_Q(1)", "thio_pyridine_A(1)", "melamine_B(1)", "misc_phthal_thio_N(1)", "hzone_acyl_misc_B(1)", "tert_butyl_B(1)", "diazox_E(1)", "anil_NH_no_alk_B(1)",
    "anil_no_alk_A(1)", "anil_no_alk_B(1)", "thio_ene_amine_A(1)", "het_55_B(1)", "cyanamide_A(1)", "ene_one_one_A(1)", "ene_six_het_D(1)", "ene_cyano_E(1)", "ene_cyano_F(1)", "hzone_furan_C(1)",
    "anil_no_alk_C(1)", "hzone_acid_D(1)", "hzone_furan_E(1)", "het_6_pyridone_NH2(1)", "imine_one_fives_D(1)", "pyrrole_M(1)", "pyrrole_N(1)", "pyrrole_O(1)", "ene_cyano_G(1)", "sulfonamide_J(1)",
    "misc_pyrrole_benz(1)", "thio_urea_R(1)", "ene_one_one_B(1)", "dhp_amino_CN_H(1)", "het_66_anisole(1)", "thiazole_amine_N(1)", "het_pyridiniums_C(1)", "het_5_E(1)",
    "2,2-dimethyl-4,5-dicarboxy-dithiole", "2,3,4_trihydroxyphenyl", "2,3,5_trihydroxyphenyl", "acid_anhydrides", "acid_halides", "Acridine", "Active_Phosphate", "acyl_cyanide",
    "Adjacent_Ring_Double_Bonds", "Aldehyde", "Aliphatic_Triflate", "alkyl_halides", "AlkylEnamine", "Allene", "Alpha_Halo_Carbonyl", "amidotetrazole", "Amino_Naphtalimide", "Aminonitrile",
    "Anhydride", "Any_Carbazide", "aromatic_azides", "Azanitrone", "azoalkanals", "Azobenzene", "Azocyanamide", "b-Carbonyl_Quaternary_Nitrogen", "benzylic_quaternary_nitrogen",
    "beta-carbonyl_quaternary_nitrogen", "Beta-Fluoro-ethyl-ON", "biotin_analogue", "carbazides", "carbodiimides", "CCl3-CHO_releasing", "Chloramidine", "Conjugated_Dithioether", "Crown_Ether_12CRO4",
    "Crown_Ether_15CRO5", "Crown_Ether_16CRO6", "crown_ethers", "cyanamide", "Cyanophosphonate", "Cyanohydrin", "di_and_triphosphates", "Diacetylene", "Diazoalkane", "Diazonium_Salt", "Diene",
    "Dinitrobenzene_1", "Dinitrobenzene_2", "Dinitrobenzene_3", "disulfides", "Dithiocarbamate", "Dithiole-2-thione", "Dithiole-3-thione", "Dithiomethylene_acetal", "Enyne",
    "epoxides,_thioepoxides,_aziridines", "ester_of_HOBT", "Flavin", "Fluorescein", "Fluorinated_Carbon_1", "Fluorinated_Carbon_2", "four_member_lactones", "geminal_amines", "geminal_dinitriles",
    "halo-pyridine,_-diazoles_and_-triazoles", "hydrazothiourea", "Imidazolium", "Imine2", "imines_(not_ring)", "isocyanates_and_isothiocyanates", "isonitrile", "ketene",
    "Lawesson_Reagent_Derivatives", "methylidene-1,3-dithiole", "Michael_Phenyl_Ketone", "N-halo", "Nitrobenz-azadiazole_1", "Nitrobenz-azadiazole_2", "nitrosamine", "nitroso", "noname",
    "N-Oxide_aliphatic", "N-S_(not_sulfonamides)", "Orthoester", "o-tertbutylphenol", "Oxobenzothiepine", "P_or_S_Halides", "p-Aminoaryl_diazo", "PCP", "paranitrophenyl_esters", "pentahalophenyl",
    "pentafluorophenyl_esters", "peroxide", "Phenanthrene", "Phenylester", "phosphonate_esters", "phosphoramides", "phosphorane", "Phosphorus_Halide", "Polyene", "polyenes",
    "polyene_chain_between_aromatics", "polyines", "Polynuclear_Aromatic_1", "Polynuclear_Aromatic_2", "Polysulfide", "Sulphur_Halide", "pyrene_fragments", "Pyrylium", "reactive_carbonyls",
    "reactive_carbonyls", "Ring_Triple_Bond", "S=N_(not_ring)", "Sulfonate_Ester", "sulfonyl_cyanide", "Sulphate_Ester", "sulphonates", "Sulphur_Nitrogen_single_bond", "Tetraazinane", "Thiocyanate",
    "thioesters", "thioles_(not_aromatic)", "Thiophosphothionate", "thiourea", "Three_Membered_Heterocycle", "Tri_Pentavalent_S", "Triacyloxime", "Triazole", "triflate", "Triphenyl_Boranyl",
    "triphenylphosphines", "Triphenyl_Silyl", "Vinyl_Halide", "Vinyl_Sulphone", "sulphates", "tropone", "Oxime", "hydrazone", "Nitrosone_not_nitro", "Thiocarbonyl_group", "enamine_like", "analine",
    "acid_anhydrides_2", "trifluroacetate_amide", "triple_bond", "Allene", "thiatetrazolidine", "glycol", "oxy-amide", "formate_formide", "pyranone", "Coumarin", "aminothiazole", "Thiazolidinone",
    "Thiomorpholinedione", "oxepine", "phenylethene", "Ethene", "cyclobutene", "poly_sub_atomatic", "Isotopes", "Undesirable_Elements_Salts", "Metal_Carbon_bond", "Aromatic_N-Oxide_more_than_one",
    "Nitro_more_than_one", "anhydride", "pentafluorophenyl ester", "p-nitrophenyl ester", "any carbazide", "HOBT ester", "aromatic azide", "imine2", "sulfonyl cyanide", "azocyanamide", "cyanohydrin",
    "acyl cyanide", "acid halide", "chloramidine", "P/S halide", "quaternary", "unacceptable atoms", "triacyloxime", "b-carbonyl quaternary nitrogen", "benzylic quaternary nitrogen", "phosphorane",
    "Lawesson reagent derivatives", "cyanophosphonate", "sulfonate", "Heteroaryl sulfonate", "sulfate ester", "triflate", "polyacidic", "Sulfonic acid", "thiol", "benzhydrol", "dihydroxybenzene",
    "2,3,4trihydroxyphenyl", "2,4,5trihydroxyphenyl", "allene", "Azide", "azoalkanal", "hydrazothiourea", "Azo", "aldehyde", "hemiacetal", "acetal", "Ketone", "Ester", "imine 1", "Imine 3",
    "thioketone", "thioester", "thionoester", "thioamide", "thiourea", "nitroso", "long chain hydrocarbon", "Long aliphatic chain", "Unbranched chain", "polyene", "Dye 25", "isonitrile",
    "thiocyanate", "cyanamide", "Dye 16 (1)", "nitro aromatic 2+", "Dye 29", "Dye 1 (1)", "Dye 7", "Dye 11", "Dye 9", "Dye 32", "Dye 6", "Dye 22", "Dye 2", "Dye 26", "alkyl halide", "Perhalo_ketone",
    "Beta halo carbonyl", "4-halopyridine", "2-halopyridine", "Hetero_hetero", "peroxide", "disulfide", "hydrazine", "acyl hydrazine", "vinyl michael acceptor1", "vinyl michael acceptor2",
    "michael acceptor 5", "Michael acceptor 6", "alkynyl michael acceptor1", "alkynyl michael acceptor2", "nitroalkane", "crown ether", "nitrate", "Oxalyl", "Dipeptide", "quaternary nitroxy",
    "Triphenylphosphine", "Phosphoric acid", "Phosphoric ester", "di/triphosphate", "tri phosphoric esters", "phosphoramide", "Phenalene", "(poly(azo(anthracene))", "(poly(azo(phenanthrene))",
    "Dye 31", "Dye 4", "Dye 8", "epoxide, aziridine, thioepoxide", "propiolactone", "b-lactam", "cycloheximide", "aromatic Sulfonic ester", "quinone", "saponin", "monensin", "squalestatin",
    "cyanidin", "cytochalasin", "Filter1_2_halo_ether", "Filter2_acyl_phosphyl_sulfonyl_halide", "Filter3_allyl_halide", "Filter4_alpha_halo_carbonyl", "Filter5_azo", "Filter6_benzyl_halide",
    "Filter7_diazo", "Filter8_thio_isocyanat_diimin", "Filter9_metal", "Filter10_Terminal_vinyl", "Filter11_nitrosamin", "Filter12_nitroso", "Filter13_PS_double_bond",
    "Filter14_thio_oxopyrylium_salt", "Filter15_thiosulfate", "Filter16_trialkyl_phosphin", "Filter17_trialkyl_phosphin2", "Filter18_oxime_ester", "Filter19_hydroxyimide_ester", "Filter20_hydrazine",
    "Filter21_cyanhydrin", "Filter22_sulfonium_salt", "Filter23_ortho_quinone", "Filter24_react_imide", "Filter25_sulfonyl_halide", "Filter26_alkyl_halide", "Filter27_anhydride",
    "Filter28_halo_pyrimidine", "Filter29_thioester", "Filter30_beta_halo_carbonyl", "Filter31_so_bond", "Filter32_oo_bond", "Filter33_c10_alkyl", "Filter34_isotope", "Filter35_pp_bond",
    "Filter36_ss_double_bond", "Filter37_silicate", "Filter38_aldehyde", "Filter39_imine", "Filter40_epoxide_aziridine", "Filter41_12_dicarbonyl", "Filter42_12_dicarbonyl_tautomer",
    "Filter43_michael_acceptor_sp1", "Filter44_michael_acceptor2", "Filter45_allyl_halide2", "Filter46_nhalide", "Filter47_so2f", "Filter48_foso", "Filter49_halogen", "Filter50_grignard",
    "Filter51_pn3", "Filter52_NC_haloamine", "Filter53_para_quinones", "Filter56_SS_bond", "Filter57_polyphenol1", "Filter58_polyphenol2", "Filter59_phoshorous_ylide", "Filter60_Acyclic_N-S",
    "Filter61_phosphor_halide_and_P_S_bond", "Filter62_oxo_thio_halide", "Filter63_polyaromatic", "Filter64_halo_ketone_sulfone", "Filter65_alkyl_sulfonate", "Filter66_c4_perfluoralkyl",
    "Filter67_S_or_O_C_triplebond_N", "Filter68_anthracene_acridine", "Filter69_thio_carbonate", "Filter70_AlkylCN2", "Filter71_thio_anhydride", "Filter72_hydrated_di_ketone", "Filter73_thio_ketone",
    "Filter74_thiol", "Filter75_alkyl_Br_I", "Filter76_S_ester", "Filter77_alkyl_NO2", "Filter78_bicyclic_Imide", "Filter79_maleimide", "Filter80_Thioepoxide_aziridone", "Filter81_Thiocarbamate",
    "Filter82_pyridinium", "Filter83_per_halo_chain", "Filter84_nitrogen_mustard", "Filter85_keto_acrylonitrile", "Filter86_cyanamide", "Filter87_crowns", "Filter88_ene_sulfone",
    "Filter89_hydroxylamine", "Filter90_N_double_bond_S", "Filter92_trityl", "Filter93_acetyl_urea", "Filter94_2_halo_pyridine", "aromatic NO2", "deuterium", "C13", "2-chloropyridine", "aniline",
    "Si,B,Se atoms", "hetero imides", "poly ethers", "acyclic imines", "alkyl esters of S or P", "ugly P compounds", "acyclic N-,=N and not N bound to carbonyl or sulfone", "acyclic N-C-N",
    "acyclic N-S", "mustards", "aldehyde", "1,2-dicarbonyl not in ring", "carbamate, T-boc Protected", "carbamate, CBZ Protected", "carbamate include di-substitued N", "acyl halide", "alkyl halide",
    "alpha halo carbonyl", "sufonyl halide", "N:C-SCH2 groups", "26", "terminal vinyl", "28", "thio cyanates", "thiols", "thionyl", "n-haloamines", "N-C-Hal or cyano methyl",
    "alpha beta-unsaturated ketones; center of Michael reactivity", "aliphatic ketone not ring and not di-carbonyl", "aliphatic ester, not lactones", "long aliphatic chain, 6+", "quinones",
    "acyclic C=C-O", "acyclic C=N-H", "acyclic NO not nitro", "42", "43", "thioester", "aziridine-like N in 3-membered ring", "epoxides", "aryl iodide", "aryl bromide", "multiple aromatic rings",
    "multiple aromatic rings", "S/PO3 groups", "adamantyl", "too many cyano Groups (>1)", "too many COOH groups (>1)", "amino acid", "chlorates", "high halogen content (>3)"
  ),
  smarts = c(
    "[Br,Cl,I][CX4;CH,CH2]", "[S,C](=[O,S])[F,Br,Cl,I]", "O=CN=[N+]=[N-]", "COS(=O)O[C,c]", "COS(=O)(=O)[C,c]", "C(=O)OC(=O)", "OO", "C(=O)Oc1c(F)c(F)c(F)c(F)c1(F)", "C(=O)Oc1ccc(N(=O)~[OX1])cc1",
    "C(=O)Onnn", "N=C=[S,O]", "OS(=O)(=O)C(F)(F)F", "P(=S)(S)S", "NP(=O)(N)N", "cN=[N+]=[N-]", "C(=O)C[N+,n+]", "[N;R0][N;R0]C(=O)", "[C+,Cl+,I+,P+,S+]", "C=P", "[Cl]C([C&R0])=N", "[N&D2](=O)",
    "[P,S][Cl,Br,F,I]", "N=C=N", "[N+]#[C-]", "C(=O)N(C(=O))OC(=O)", "N#CC[OH]", "N#CC(=O)", "S(=O)(=O)C#N", "P(OCC)(OCC)(=O)C#N", "[N;R0]=[N;R0]C#N", "[N;R0]=[N;R0]CC=O",
    "[CD2;R0][CD2;R0][CD2;R0][CD2;R0][CD2;R0][CD2;R0][CD2;R0]", "[C,S,P](=O)[OH].[C,S,P](=O)[OH].[C,S,P](=O)[OH].[C,S,P](=O)[OH]", "[O;R1][C;R1][C;R1][O;R1][C;R1][C;R1][O;R1]", "SS", "[SH]",
    "C1[O,S,N]C1", "c([OH])c([OH])c([OH])", "c([OH])c([OH])cc([OH])", "N=NC(=S)N", "SC#N", "cC[N+]", "C[O,S;R0][C;R0](=S)", "N[CH2]C#N", "C1(=O)OCC1", "P(=O)([OH])OP(=O)[OH]", "N1CCC1=O",
    "O=C1[#6]~[#6]C(=O)[#6]~[#6]1", "C=CC=CC=CC=C", "O1CCCCC1OC2CCC3CCCCC3C2", "O=C1NCC2CCCCC21", "O=C1CCCC(N1)=O", "O1CCCCC1C2CCCO2", "[OH]c1cc([OH])cc2=[O+]C(=C([OH])Cc21)c3cc([OH])c([OH])cc3",
    "C12OCCC(O1)CC2", "C(=O)O[C,H1].C(=O)O[C,H1].C(=O)O[C,H1]", "n1c([F,Cl,Br,I])cccc1", "C(=O)[Cl,Br,I,F]", "C=[C!r]O", "N#CC(=O)", "C(=O)N[NH2]", "[CH1](=O)", "[R0;D2][R0;D2][R0;D2][R0;D2]",
    "[CX4][Cl,Br,I]", "c1nnnn1C=O", "c1cc([NH2])ccc1", "[CH2R2]1N[CH2R2][CH2R2][CH2R2][CH2R2][CH2R2]1", "N=[N+]=[N-]", "N#N", "[CH2R2]1N[CH2R2][CH2R2][CH2R2][CH2R2][CH2R2][CH2R2]1",
    "[cR2]1[cR2][cR2]([Nv3X3,Nv4X4])[cR2][cR2][cR2]1[cR2]2[cR2][cR2][cR2]([Nv3X3,Nv4X4])[cR2][cR2]2", "[C,c](=O)[CX4,CR0X3,O][C,c](=O)", "C12C(NC(N1)=O)CSC2", "[C+,c+,C-,c-]",
    "c1c([OH])c([OH,NH2,NH])ccc1", "[O+,o+,S+,s+]", "C1(=[O,N])C=CC(=[O,N])C=C1", "C1(=[O,N])C(=[O,N])C=CC=C1", "C=[C!r]C#N", "[OR2,NR2]@[CR2]@[CR2]@[OR2,NR2]@[CR2]@[CR2]@[OR2,NR2]",
    "c1ccc2c(c1)ccc(=O)o2", "N[CH2]C#N", "[N,O,S]C#N", "N#CC[OH]", "[CR2]1[CR2][CR2][CR2][CR2][CR2][CR2]1", "[CR2]1[CR2][CR2]cc[CR2][CR2]1", "[CR2]1[CR2][CR2][CR2][CR2][CR2][CR2][CR2]1",
    "[CR2]1[CR2][CR2]cc[CR2][CR2][CR2]1", "[cR2]1[cR2]c([N+0X3R0,nX3R0])c([N+0X3R0,nX3R0])[cR2][cR2]1", "[cR2]1[cR2]c([N+0X3R0,nX3R0])[cR2]c([N+0X3R0,nX3R0])[cR2]1",
    "[cR2]1[cR2]c([N+0X3R0,nX3R0])[cR2][cR2]c1([N+0X3R0,nX3R0])", "[N!R]=[N!R]", "[C,c](=O)[C,c](=O)", "SS", "[CX2R0][NX3R0]", "C(=O)Onnn", "C1(=O)OCC1",
    "c1cc([Cl,Br,I,F])cc([Cl,Br,I,F])c1[Cl,Br,I,F]", "c1ccc([Cl,Br,I,F])c([Cl,Br,I,F])c1[Cl,Br,I,F]", "[Hg,Fe,As,Sb,Zn,Se,se,Te,B,Si]", "[NX3R0,NX4R0,OR0,SX2R0][CX4][NX3R0,NX4R0,OR0,SX2R0]",
    "C1NC(=O)NC(=O)1", "N[NH2]", "[OH]c1ccc([OH,NH2,NH])cc1", "C(=O)N[OH]", "C=[N!R]", "N=[CR0][N,n,O,S]", "I", "N=C=O",
    "[$([CH2]),$([CH][CX4]),$(C([CX4])[CX4])]=[$([CH2]),$([CH][CX4]),$(C([CX4])[CX4])]", "C=C=O", "S1C=CSC1=S", "C=!@CC=[O,S]", "[$([CH]),$(CC)]#CC(=O)[C,c]", "[$([CH]),$(CC)]#CS(=O)(=O)[C,c]",
    "C=C(C=O)C=O", "[$([CH]),$(CC)]#CC(=O)O[C,c]", "[NX2,nX3][OX1]", "s1c(S)nnc1NC=O", "NC[F,Cl,Br,I]", "[NX3,NX4][F,Cl,Br,I]", "n[OH]", "[N+](=O)[O-]", "[#7]-N=O", "[C,c]=N[OH]", "[C,c]=NOC=O",
    "[OR0,NR0][OR0,NR0]", "[CX4](F)(F)[CX4](F)F", "OO", "c1ccccc1OC(=O)[#6]", "c1ccccc1OC(=O)O", "P", "[cR,CR]~C(=O)NC(=O)~[cR,CR]", "a1aa2a3a(a1)A=AA=A3=AA=A2", "a21aa3a(aa1aaaa2)aaaa3",
    "a31a(a2a(aa1)aaaa2)aaaa3", "[CR0]=[CR0][CR0]=[CR0]", "[s,S,c,C,n,N,o,O]~[nX3+,NX3+](~[s,S,c,C,n,N])~[s,S,c,C,n,N]",
    "[s,S,c,C,n,N,o,O]~[n+,N+](~[s,S,c,C,n,N,o,O])(~[s,S,c,C,n,N,o,O])~[s,S,c,C,n,N,o,O]", "[*]=[N+]=[*]", "O1CCCCC1OC2CCC3CCCCC3C2", "[Si][F,Cl,Br,I]", "c1ccccc1C=Cc2ccccc2", "[SX3](=O)[O-,OH]",
    "[C,c]S(=O)(=O)O[C,c]", "S(=O)(=O)[O-,OH]", "S(=O)(=O)C#N", "[SX2]O", "OS(=O)(=O)[O-]", "[SX2H0][N]", "c12ccccc1(SC(S)=N2)", "c12ccccc1(SC(=S)N2)", "[C,c]=S", "SC=O", "[S-]", "[SH]",
    "*1[O,S,N]*1", "OS(=O)(=O)C(F)(F)F", "[SiR0,CR0](c1ccccc1)(c2ccccc2)(c3ccccc3)", "C#C",
    "[#7;R1]1[#6]([F,Cl,Br,I])[#6]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])[#7][#6][#6]1",
    "[#7;R1]1[#6]([F,Cl,Br,I])[#6;!$(c-N)][#7][#6]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])[#6;!$(c-N)]1",
    "[#7;R1]1[#6]([F,Cl,Br,I])[#6]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])[#6][#6][#7]1",
    "[#7;R1]1[#6]([F,Cl,Br,I])[#6][#6][#6]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])[#7]1",
    "[#7;R1]1[#6;!$(c=O)]([F,Cl,Br,I])[#6]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])[#6;!$(c-N)][#6][#6;!$(c-N)]1",
    "[#7;R1]1[#6;!$(c=O)]([F,Cl,Br,I])[#6][#6;!$(c-N)][#6]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])[#6;!$(c=O);!$(c-N)]1",
    "[#7;R1]1[#6]([F,Cl,Br,I])[#7][#6][#6]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])[#6]1",
    "[#7;R1]1[#6]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])[#6]([F,Cl,Br,I])[#6][#6][#7]1",
    "[#7;R1]1[#6][#6]([F,Cl,Br,I])[#6]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])[#6][#7]1",
    "[#7,#8,#16]1~[#6;H]~[#6]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])~[#6](=O)~[#6]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])~[#6;H]1",
    "[#7;R1]1[#6;!$(c=O);!$(c-N)][#6]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])[#6]([F,Cl,Br,I])[#6][#6;!$(c=O);!$(c-N)]1",
    "[#7]1[#6]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])[#7;R1][#6]([F,Cl,Br,I])[#6][#6]1([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])",
    "[#7]1[#6][#7;R1][#6]([F,Cl,Br,I])[#6]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])[#6]1", "[CH2]1[O,S]C1", "O=C(-[!N])O[$(nnn),$([#7]-[#7]=[#7])]", "P(=O)ON",
    "[CH2]=[C;!$(C-N);!$(C-O)]C(=O)", "[#6]1~[$(C(=O)),$(S(=O))]~[O,S,N]~[$(C(=O)),$(S(=O))]1", "C1~[O,S]~[C,N,O,S]1[a,N,O,S]",
    "[$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O))]C#[C;!$(C-N);!$(C-n)]",
    "[N;!R]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O))])=[N;!R]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O))])",
    "O=COC=[$(C(S(=O)(=O))),$(C(C(F)(F)(F))),$(C(C#N)),$(C(N(=O)(=O))),$(C([N+](=O)[O-])),$(C(C(=O)));!$(C(N))]",
    "O(-S(=O)(=O))C=[$(C(S(=O)(=O))),$(C(C(F)(F)(F))),$(C(C#N)),$(C(N(=O)(=O))),$(C([N+](=O)[O-])),$(C(C(=O)));!$(C(N))]", "[C,c][C;!R](=O)[N;!R][C;!R](=O)[C,c]",
    "[#7;R1]1~[#7;R1]~[#7;R1](-C(=O))~[#6]~[#6]1", "[#7]1~[#7]~[#6]~[#7](-C(=O)[!N])~[#6]1", "O=C(-[!N])O[$([#7;+]),$(N(C=[O,S,N])(C=[O,S,N]))]", "C(=O)-C#N",
    "[C;!$(C-N)](=O)[#7]1[#6;H1,$([#6]([*;!R]))][#7][#6;H1,$([#6]([*;!R]))][#6;H1,$([#6]([*;!R]))]1", "[C;!$(C-N)](=O)[#7]1[#7][#6;H1,$([#6]([*;!R]))][#6;H1,$([#6]([*;!R]))][#6;H1,$([#6]([*;!R]))]1",
    "[C,c][C;H1](=O)", "C(=O)!@C(=O)", "[$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-])]-[CH,CH2]-[Cl,Br,I,$(O(S(=O)(=O)))]", "[F,Cl,Br,I,$(O(S(=O)(=O)))]-[CH,CH2;!$(C(F)F)]-[N,n]",
    "C(=O)([CH,CH2][Cl,Br,I,$(O(S(=O)(=O)))])", "[N,n,O,S;!$(S(=O)(=O))]-[CH,CH2;!$(C(F)(F))][F,Cl,Br,I,$(O(S(=O)(=O)))]", "[N,n,O,S;!$(S(=O)(=O))]-C([Cl,Br,I,$(O(S(=O)(=O)))])(C)(C)",
    "[$(C(=O)),$(C(=S))]-[O,S]-[$(C(=O)),$(C(=S)),$(C(=[N;!R])),$(C(=N(-[C;X4])))]", "P(=O)-[O;!R]-a", "a-[S;X2;!R]-[C;!R](=O)", "[$(N#[N+]-[N-]),$([N-]=[N+]=N)]", "[C,N]1~[C,N]~N~1",
    "[N]=[N;!R]-[N]", "c[N;!R;!+]=[N;!R;!+]-c", "[N;!R]=[N;!R]-[N]=[*]", "[N;!$(N-S(=O)(=O));!$(N-C=O)]-[N;!r3;!$(N-S(=O)(=O));!$(N-C=O)]-[N;!$(N-S(=O)(=O));!$(N-C=O)]", "[N;!R]-[N;!R]-[N;!R]",
    "a-N=N-[N;H2]", "[B-,BH2,BH3,$(B(F)(F))]", "[C+,F+,Cl+,Br+,I+,Se+]", "c([N;!+])1ccc(c2ccc([N;!+])cc2)cc1", "[#6,#15,#16]1(=O)~[#6]~[#6]~[#8,#16]1", "C1(=O)~[#6]~[#6]N1",
    "C1(=O)~[#6]~[#6]N1([$(S(=O)(=O)[C,c,O&D2]),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O)[C,c,O&D2])])",
    "O=[C,S]Oc1aaa([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O)O),$(C(=O)N)])aa([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O)O),$(C(=O)N)])1",
    "CC(=O)[$([C&H1]),$(C-F),$(C-Cl),$(C-Br),$(C-I)]=[$([C&H1]),$(C-F),$(C-Cl),$(C-Br),$(C-I)]C(=O)C", "[C,c]~[#5]", "a1(a2aa(a3aaaaa3)aa(a4aaaaa4)a2)aaaaa1", "N=C=[N,O,S]", "O=C[F,Cl,Br,I]",
    paste0("[$([Ru]),$([Rh]),$([#34]),$([Pd]),$([Sc]),$([Bi]),$([Sb]),$([Ag]),$([Ti]),$([Al]),$([Cd]),$([V]),$([In]),$([Cr]),$([Sn]),$([Mn]),$([La]),$([Fe]),$([Er",
           "]),$([Tm]),$([Yb]),$([Lu]),$([Hf]),$([Ta]),$([W]),$([Re]),$([Co]),$([Os]),$([Ni]),$([Ir]),$([Cu]),$([Zn]),$([Ga]),$([Ge]),$([#33]),$([Y]),$([Zr]),$([N",
           "b]),$([Ce]),$([Pr]),$([Nd]),$([Sm]),$([Eu]),$([Gd]),$([Tb]),$([Dy]),$([Ho]),$([Pt]),$([Au]),$([Hg]),$([Tl]),$([Pb]),$([Ac]),$([Th]),$([Pa]),$([Mo]),$(",
           "[U]),$([Tc]),$([Te]),$([Po]),$([At])]"),
    paste0("[$([O,S,#7;R1;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][CH,CH2;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][CH,CH2;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][O,",
           "S,#7;R1;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][CH,CH2;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][CH,CH2;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][O,S,#7;R",
           "1;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18]),$([O,S,#7;R1;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][CH,CH2;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][CH,CH2;",
           "r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][CH,CH2;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][O,S,#7;R1;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][CH,CH2;r9,r10",
           ",r11,r12,r13,r14,r15,r16,r17,r18][CH,CH2;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][CH,CH2;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][O,S,#7;R1;r9,r10,r11,r",
           "12,r13,r14,r15,r16,r17,r18]),$([O,S,#7;R1;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][CH,CH2;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][CH,CH2;r9,r10,r11,r12",
           ",r13,r14,r15,r16,r17,r18][O,S,#7;R1;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][CH,CH2;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][CH,CH2;r9,r10,r11,r12,r13,r",
           "14,r15,r16,r17,r18][CH,CH2;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18][O,S,#7;R1;r9,r10,r11,r12,r13,r14,r15,r16,r17,r18])]"),
    "P(O[A,a])(O[A,a])(=O)C#N", "[C;X4](-[OH,NH1,NH2,SH])(-C#N)", "[N,n]~[S;!R;D2]~[N,n]", "[$(N=N=C~C=O),$(N#N-C~C=O)]", "a[N+]#N",
    "[$(N(-C(=O))(-C(=O))(-S(=O))),$(n([#6](=O))([#6](=O))([#16](=O)))]", "[S;!R;X2]-[S;!R;X2]", "S(=O)(=O)N=C1C=CC(=NS(=O)(=O))C=C1", "NC(C[S;D1])C([N;H1]([O;D1]))=O", "O=C2CC(a3aaaaa3)Oa1aaaaa12",
    "C#N.C#N.C#N.C#N", "[C;!R]-[C;!R]-[C;!R]-[C;!R]-[C;!R]-[C;!R]-[C;!R]-[C;!R]-[C;!R]-[C;!R]", "[N,n;H0;+;!$(N~O);!$(n~O)].[N,n;H0;+;!$(N~O);!$(n~O)]", "P([O;D1])=O.P([O;D1])=O",
    "[C,c]S(=O)(=O)[O;D1].[C,c]S(=O)(=O)[O;D1]", "C(=O)[O;D1].C(=O)[O;D1].C(=O)[O;D1]", "[#53].[#53].[#53]",
    paste0("[N;!$(N(=[N,O,S,C]));!$(N(S(=O)(=O)));!$(N(C(F)(F)(F)));!$(N(C#N));!$(N(C(=O)));!$(N(C(=S)));!$(N(C(=N)));!$(N(#C));!$(Nc)].[N;!$(N(=[N,O,S,C]));!$(N(",
           "S(=O)(=O)));!$(N(C(F)(F)(F)));!$(N(C#N));!$(N(C(=O)));!$(N(C(=S)));!$(N(C(=N)));!$(N(#C));!$(Nc)].[N;!$(N(=[N,O,S,C]));!$(N(S(=O)(=O)));!$(N(C(F)(F)(F",
           ")));!$(N(C#N));!$(N(C(=O)));!$(N(C(=S)));!$(N(C(=N)));!$(N(#C));!$(Nc)].[N;!$(N(=[N,O,S,C]));!$(N(S(=O)(=O)));!$(N(C(F)(F)(F)));!$(N(C#N));!$(N(C(=O))",
           ");!$(N(C(=S)));!$(N(C(=N)));!$(N(#C));!$(N-c)]"),
    "[$([N+](=O)[O-]),$(N(=O)=O)].[$([N+](=O)[O-]),$(N(=O)=O)].[$([N+](=O)[O-]),$(N(=O)=O)].[$([N+](=O)[O-]),$(N(=O)=O)]", "a[O;D1].a[O;D1].a[O;D1].a[O;D1].a[O;D1]",
    "C[O;D1].C[O;D1].C[O;D1].C[O;D1].C[O;D1].C[O;D1].C[O;D1]", "[Cl,Br,I].[Cl,Br,I].[Cl,Br,I].[Cl,Br,I].[Cl,Br,I].[Cl,Br,I].[Cl,Br,I]",
    "[CH2,$(C(F)(F));R0][CH2,$(C(F)(F));R0][CH2,$(C(F)(F));R0][CH2,$(C(F)(F));R0][CH2,$(C(F)(F));R0][CH2,$(C(F)(F));R0][CH2,$(C(F)(F));R0][CH2,$(C(F)(F));R0]",
    paste0("[#7,#8,#16]1[#6]([$(S(=O)(=O)),$([F,Cl]),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O))])[#6]([$(S(=O)(=O)),$([F,Cl]),$(C(F)(F)(F)),$(C#N)",
           ",$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O))])[#7][#6]1([Cl,Br,I])"),
    paste0("[$([C;H2]),$([C&H1;$(C-F)]),$([C&H1;$(C-Cl)]),$([C&H1;$(C-Br)]),$([C&H1;$(CI)]),$(C(F)F),$(C(Cl)Cl),$(C(Br)Br),$(C(I)I),$(C(F)Cl),$(C(F)Br),$(C(F)I),$",
           "(C(Cl)Br),$(C(Br)I)](=[$([C&H1;$(C(-C(=O)))]),$(C(F)(C(=O))),$(C(Cl)(C(=O))),$(C(Br)(C(=O))),$(C(I)(C(=O))),$(C(C)(C(=O))),$(C(c)(C(=O)))])"),
    "C(=[#7])([Cl,Br,I,$(O(S(=O)(=O)))])",
    "C([Cl,Br,I,$(O(S(=O)(=O)))])=C([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])",
    "C(=O)Oc1c([Cl,F])[cH1,$(c[F,Cl])]c([F,Cl])[cH1,$(c[F,Cl])]c1([F,Cl])", "S(=O)Oc1c([Cl,F])[cH1,$(c[F,Cl])]c([F,Cl])[cH1,$(c[F,Cl])]c1([F,Cl])", "[!C;!c;!H][F,Cl,Br,I]", "[Si]~[!#6]",
    "[N;X3;!$(N-S(=O)(=O));!$(N-C(F)(F)(F));!$(N-C#N);!$(N-C(=O));!$(N-C(=S));!$(N-C(=N))]-[N;X3;!$(N-S(=O)(=O));!$(N-C(F)(F)(F));!$(N-C#N);!$(N-C(=O));!$(N-C(=S));!$(N-C(=N))]", "[N;!R]=NC(=S)N",
    "C([N;H1]([O;D1]))=O", "[$([#16&D3]),$([#16&D4])]=,:[#6]", "[N+]#[C-]", "[$(c([C;!R;!$(C-[N,O,S]);!$(C-[H])](=O))1naaaa1),$(c([C;!R;!$(C-[N,O,S]);!$(C-[H])](=O))1naa[n,s,o]1)]",
    "[$(a12aaaaa1aa3a(aa(aaaa4)a4a3)a2),$(a12aaaaa1aa3a(aaa4a3aaaa4)a2),$(a12aaaaa1a(aa5)a3a(aaa4a3a5aaa4)a2)]",
    "[$(a12aaaa4a1a3a(aaaa3aa4)aa2),$(a12aaaaa1a3a(aaa4a3aaaa4)aa2),$(a1(a(aaaa4)a4a3a2aaaa3)a2aaaa1)]", "[$([C;H1]),$(C(-[F,Cl,Br,I]))]1=[$([C;H1]),$(C(-[F,Cl,Br,I]))]C(=O)[N,O,S]C(=O)1",
    "O=C1OC(C)(C)OC(C1)=O", "[C;H2](F)C(=O)[O,N,S]", "[C;!R]=[N+][O;D1]", "N-[N;X2](=O)", "[O,N,S;!$(S~O)]!@[CH2]!@[O,S,N;!$(S~O)]", "[O,N,S;!$(S~O)]!@[C;H1;X4]!@[O,N,S;!$(S~O)]",
    "[O,N,S;!$(S~O)]!@[C;H0;X4](!@[O,N,S;!$(S~O)])(C)", "c1c([N;D1])c([N;D1])c[cH1][cH1]1", "a1c([O,S;D1])c([O,S;D1])a[cH1][cH1]1",
    "[#6]1(-O-[C;!R](=[O,N;!R]))[#6]([$(N(=O)(=O)),$([N+](=O)[O-])])[#6][#6][#6][#6]1", "[CH1,$(C(-[Cl,Br,I]))]1=CC(=[O,N,S;!R])C(=[O,N,S])C=[CH1,$(C(-[Cl,Br,I]))]1", "C1~[O,S]~N1",
    "[$(C=N[O;D1]);!$(C=[N+])][#6][#6]", "[o+,O+]", "a1[cH1]c([N;D1])[cH1]ac([N;D1])1", "a1[cH1]c([O,S;D1])[cH1]ac([O,S;D1])1",
    "[#6]1(-O(-[C;!R](-[!N])(=[O,N;!R])))[#6][#6][#6]([$(N(=O)(=O)),$([N+](=O)[O-])])[#6][#6]1",
    "[CH1,$(C(-[Cl,Br,I]))]1=[CH1,$(C(-[Cl,Br,I]))]C(=[O,N,S])[CH1,$(C(-[Cl,Br,I]))]=[CH1,$(C(-[Cl,Br,I]))]C1(=[O,N,S])", "[#6]1[#6][#6]([#6]2[#6][#6][#7;+][#6][#6]2)[#6][#6][#7;+]1",
    "C(=O)Oc1c(F)c(F)c(F)c(F)c1(F)", "C1(Cl)(Cl)C(Cl)C(Cl)=C(Cl)C1(Cl)", "c1(C=O)c([Br,Cl,I])c([Br,Cl,I])c([Br,Cl,I])c([Br,Cl,I])c1(C=O)",
    "c1c([Br,Cl,I])c([Br,Cl,I])c([Br,Cl,I])c([Br,Cl,I])c1([Br,Cl,I])", "[#8]~[#8]",
    "O=[C,S]Oc1aaa([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O)O),$(C(=O)N)])aa([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O)O),$(C(=O)N)])1",
    "NC(COP(O)(O)=O)C(O)=O", "NC(C(C)OP(O)(O)=O)C(O)=O", "NC(Cc1ccc(OP(O)(O)=O)cc1)C(O)=O", "[c,C]-[P;v3]", "[#15;+]~[!O]", "C=P", "[#15]~[N,n]", "P~P", "P~S", "C=[C;!R][C;!R]=[C;!R][C;!R]=[C;!R]",
    "c1c([O;D1])c(-[Cl,Br,I])c(-[Cl,Br,I])cc1.c1c([O;D1])c(-[Cl,Br,I])c(-[Cl,Br,I])cc1", "c1c([O;D1])c(-[Cl,Br,I])cc(-[Cl,Br,I])c1.c1c([O;D1])c(-[Cl,Br,I])cc(-[Cl,Br,I])c1",
    "c1c([O;D1])ccc(-[Cl,Br,I])c(-[Cl,Br,I])1.c1c([O;D1])ccc(-[Cl,Br,I])c(-[Cl,Br,I])1", "c(-[Cl,Br,I])1c([O;D1])c(-[Cl,Br,I])ccc1.c(-[Cl,Br,I])1c([O;D1])c(-[Cl,Br,I])ccc1",
    "c1c([O;D1])ccc(-[Cl,Br,I])c(-[Cl,Br,I])1.c1c([O;D1])ccc(-[Cl,Br,I])c(-[Cl,Br,I])1", "[S;D2]-[S;D2]-[S;D2]", "[#6;r16,r17,r18]~[#6]1~[#6]~[#6]~[#6](~[#6])~[#7]1",
    "[CH2][Cl,Br,I,$(O(S(=O)(=O)[!$(N);!$([O&D1])]))]", "[N,n;R;+]!@[N,n]", "[N,n;+]!@C(=O)",
    "[#6;!$([#6](-[N,O,S]))]1=[#6;!$([#6](-[N,O,S]))][#6](=[#6])[#6;!$([#6](-[N,O,S]))]=[#6;!$([#6](-[N,O,S]))][#6]1(=[O,N,S])", "C(=C)1SC(=S)NC(=O)1",
    "[CH;!$(C=C)][Cl,Br,I,$(O(S(=O)(=O)[!$(N);!$([O&D1])]))]", "[S;D2](-[N;!$(N(=C));!$(N(-S(=O)(=O)));!$(N(-C(=O)))])", "[S;D2][O;D2]", "[S;D3](-N)(-[c,C])(-[c,C])", "[C,c]OS(=O)O[C,c]",
    "[S+;X3;$(S-C);!$(S-[O;D1])]", "[$(C(=O)),$(S(=O)(=O))][O,S](S(=O)(=O))", "S(=O)(=O)[F,Cl,Br,I]", "[!#6;!#1;!#11;!#19]O(S(=O)(=O)(-[C,c]))", "S(=O)(=O)C#N",
    "[C;X4](-[Cl,Br,I,$(O(S(=O)(=O)[!$(N);!$([O&D1])]))])(-[c,C])(-[c,C])(-[c,C])", "[S;D2]([$(N(=C)),$(N(-S(=O)(=O))),$(N(-C(=O)))])", "[S;!R]-[C;!R](=[S;!R])(-[S;!R])", "SC(=O)[O,S]",
    "[S;!R;H0]C(=[S,O;!R])([!O;!S;!N])", "NC(C[S;D1])C(O)=O", "c1[S,s;+]cccc1", "[C,c][S;X3](~O)-S", "[$(N(-C(=O))(-C(=O))(-C(=O))),$(n([#6](=O))([#6](=O))([#6](=O)))]", "P(=O)(a)(a)(a)",
    "[$(C(=O));!$(C-N);!$(C-O);!$(C-S)]C(Cl)(Cl)(Cl)", "OS(=O)(=O)(C(F)(F)(F))", "C(F)(F)(F)C(=O)O", "C(F)(F)(F)C(=O)S", "[$(C(=O));!$(C-N);!$(C-O);!$(C-S)]C(F)(F)(F)",
    "C(-[Cl,Br,I])(-[Cl,Br,I])=C(-[Cl,Br,I])(-[N,O,S])",
    paste0("[$(a1aaa([$(N(=O)(=O)),$([N+](=O)[O-])])a([$(N(=O)(=O)),$([N+](=O)[O-])])a1([$(N(=O)(=O)),$([N+](=O)[O-])])),$(a1aa([$(N(=O)(=O)),$([N+](=O)[O-])])a([",
           "$(N(=O)(=O)),$([N+](=O)[O-])])aa1([$(N(=O)(=O)),$([N+](=O)[O-])])),$(a1a([$(N(=O)(=O)),$([N+](=O)[O-])])aa([$(N(=O)(=O)),$([N+](=O)[O-])])aa1([$(N(=O)",
           "(=O)),$([N+](=O)[O-])]))]"),
    "C([$([N+](=O)[O-]),$(N(=O)=O)])([$([N+](=O)[O-]),$(N(=O)=O)])([$([N+](=O)[O-]),$(N(=O)=O)])",
    paste0("[$(O=[C,S]Oc1a([$(S(=O)(=O)),F,$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O)O),$(C(=O)N)])a([$(S(=O)(=O)),F,$(C(F)(F)(F)),$(C#N),$(N(=O)(=",
           "O)),$([N+](=O)[O-]),$(C(=O)O),$(C(=O)N)])a([$(S(=O)(=O)),F,$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O)O),$(C(=O)N)])aa1),$(O=[C,S]Oc1a([",
           "$(S(=O)(=O)),F,$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O)O),$(C(=O)N)])a([$(S(=O)(=O)),F,$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O",
           "-]),$(C(=O)O),$(C(=O)N)])aaa([$(S(=O)(=O)),F,$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O)O),$(C(=O)N)])1),$(O=[C,S]Oc1a([$(S(=O)(=O)),F,$",
           "(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O)O),$(C(=O)N)])aa([$(S(=O)(=O)),F,$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O)O),$",
           "(C(=O)N)])a([$(S(=O)(=O)),F,$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O)O),$(C(=O)N)])a1),$(O=[C,S]Oc1a([$(S(=O)(=O)),F,$(C(F)(F)(F)),$(C",
           "#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O)O),$(C(=O)N)])aa([$(S(=O)(=O)),F,$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O)O),$(C(=O)N)])aa([$(",
           "S(=O)(=O)),F,$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O)O),$(C(=O)N)])1)]"),
    "[CH;!R;!$(C-N)]=C([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O))])([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C(=O))])",
    "[C;!R]([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])([$(S(=O)(=O)),$(C(F)(F)(F)),$(C#N),$(N(=O)(=O)),$([N+](=O)[O-]),$(C=O)])=[C;!R]([C;!R](=O))([!$([#8]);!$([#7])])",
    "[#6]-1(-[#6](~[!#6&!#1]~[#6]-[!#6&!#1]-[#6]-1=[!#6&!#1])~[!#6&!#1])=[#6;!R]", "c:1:c:c(:c(:c:c:1)-[#6]=[#7]-[#7])-[O;H1]",
    "[C;H2]N([C;H2])c1cc([$([H]),$([C;H2]),$([O][C;H2][C;H2])])c(N)c([H])c1",
    "n:1(c(c(c:2:c:1:c:c:c:c:2-[H])-[C;D4]-[H])-[$([C;H2]),$([C]=,:[!C]),$([C;H1][N]),$([C;H1]([C;H2])[N;H1][C;H2]),$([C;H1]([C;H2])[C;H2][N;H1][C;H2])])-[$([H]),$([C;H2])]",
    "[!#6&!#1]=[#6]-1-[#6]=,:[#6]-[#6](=[!#6&!#1])-[#6]=,:[#6]-1", "[#7;!R]=[#7]", "[#6]-[#6](=[!#6&!#1;!R])-[#6](=[!#6&!#1;!R])-[$([#6]),$([#16](=[#8])=[#8])]", "[#7]-[C;X4]-c1ccccc1-[O;H1]",
    "c:1:c:c(:c:c:c:1-[#7](-[#6;X4])-[#6;X4])-[#6]=[#6]", "c:1:c:c(:c:c:c:1-[#8]-[#6;X4])-[#7](-[#6;X4])-[$([#1]),$([#6;X4])]", "[#7]-1-[#6](=[#16])-[#16]-[#6](=[#6])-[#6]-1=[#8]",
    "c:1(:c:c:c(:c:c:1)-[#6]=[#7]-[#7])-[#8]-[#1]", "[#6]-1(=[#6])-[#6]=[#7]-[#7,#8,#16]-[#6]-1=[#8]", "[#6]-1(=[#6])-[#6]=[#7]-[!#6&!#1]-[#6]-1=[#8]",
    "c:1:c:c(:c:c:c:1-[#7](-[#6;X4])-[#6;X4])-[#6;X4]-[$([#8]-[#1]),$([#6]=[#6]-[#1]),$([#7]-[#6;X4])]", "[#8]=[#6]-2-[#6](=!@[#7]-[#7])-c:1:c:c:c:c:c:1-[#7]-2",
    "[#6](-[#1])-[#7](-[#6](-[#1])-[#1])-c:1:c(:c(:c(:c(:c:1-[#1])-[$([#1]),$([#6](-[#1])-[#1])])-[#6](-[#1])-[$([#1]),$([#6]-[#1])])-[#1])-[#1]",
    "[#6]-1(=[#6](-[$([#1]),$([#6](-[#1])-[#1]),$([#6]=[#8])])-[#16]-[#6](-[#7]-1-[$([#1]),$([#6]-[#1]),$([#6]:[#6])])=[#7;!R])-[$([#6](-[#1])-[#1]),$([#6]:[#6])]",
    "n2(-[#6]:1:[!#1]:[#6]:[#6]:[#6]:[#6]:1)c(cc(c2-[#6;X4])-[#1])-[#6;X4]", "c:1:c:c(:c(:c:c:1)-[#8;H1])-[#8;H1]", "[#6]-1(=[#6])-[#6](-[#7]=[#6]-[#16]-1)=[#8]",
    "[#6]-1=[!#1]-[!#6&!#1]-[#6](-[#6]-1=[!#6&!#1;!R])=[#8]", "[#6]-1(-[#6](-[#6]=[#6]-[!#6&!#1]-1)=[#6])=[!#6&!#1]", "CN1[C;H2][C;H2]N(N=[C;H1][#6]=,:[#6])[C;H2][C;H2]1",
    "c:1-2:c(:c:c:c:c:1)-[#6](=[#8])-[#6;X4]-[#6]-2=[#8]", "Cn1cccc1C=NN", "[#6]=!@[#6](-[!#1])-@[#6](=!@[!#6&!#1])-@[#6](=!@[#6])-[!#1]", "N#CC=C(N)C(C#N)C#N",
    "c:1-2:c(:c:c:c:c:1)-[#6](=[#8])-[#6](=[#6])-[#6]-2=[#8]", "N#Cc1ccc[#7;H1]c1=S", "c:1:c:c-2:c(:c:c:1)-[#6]-3-[#6](-[#6]-[#7]-2)-[#6]-[#6]=[#6]-3",
    "c:1:c:2:c(:c:c:c:1):n:c:3:c(:c:2-[#7]):c:c:c:c:3", "[#6]-1(=[#6])-[#6](=[#8])-[#7]-[#7]-[#6]-1=[#8]", "[H]N([H])c1sc([!#1])c([!#1])c1C=O",
    "[#7]-[#6]=!@[#6]-2-[#6](=[#8])-c:1:c:c:c:c:c:1-[!#6&!#1]-2", "NS(=O)(=O)c1cc([F,Cl,Br,I])cc([F,Cl,Br,I])c1O", "[#6]-[#6](=[#16])-[#6]", "[H]N(c1ccc([O;H1])cc1)S(=O)=O",
    "c:1(:c(:c(:c(:c(:c:1-[#1])-[#1])-[$([#8]),$([#7]),$([#6](-[#1])-[#1])])-[#1])-[#1])-[#7](-[#1])-[#1]",
    "[$([#1]),$([#6](-[#1])-[#1]),$([#6]:[#6])]-c:1:c(:c(:c(:s:1)-[#7](-[#1])-[#6](=[#8])-[#6])-[#6](=[#8])-[#8])-[$([#6]:1:[#6]:[#6]:[#6]:[#6]:[#6]:1),$([#6]:1:[#16]:[#6]:[#6]:[#6]:1)]",
    "[H]c1c([$([N]),$([H])])ccc2ccc[n+]([$([O;X1]),$([C;H3]),$([#6][#6]:[#6]),$([#6][#6][#8]),$([#6][#6](C)=[#8]),$([#6][#6](N)=[#8]),$([#6][#6][#6])])c12", "CC(=O)c1ccccc1[#7;H1][!$([#6]=[#8])]",
    "[#7;H1][#7]=[#6](-[#6]#[#7])-[#6]=[!#6&!#1;!R]", "[#7](-c:1:c:c:c:c:c:1)-[#16](=[#8])(=[#8])-[#6]:2:[#6]:[#6]:[#6]:[#6]:3:[#7]:[$([#8]),$([#16])]:[#7]:[#6]:2:3",
    paste0("[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#6](-[#1])=[#7]-[#7]-[$([#6](=[#8])-[#6](-[#1])(-[#1])-[#16]-[#6]:[#7]),$(",
           "[#6](=[#8])-[#6](-[#1])(-[#1])-[!#1]:[!#1]:[#7]),$([#6](=[#8])-[#6]:[#6]-[#8]-[#1]),$([#6]:[#7]),$([#6](-[#1])(-[#1])-[#6](-[#1])-[#8]-[#1])])-[#1])-[",
           "#1]"),
    "[#7]-1-[#6](=[#16])-[#16]-[#6;X4]-[#6]-1=[#8]", "[#7][#7]=[#6][#6](-[$([#1]),$([#6])])=[#6]([#6])-!@[$([#7]),$([#8])]", "[#6;X4]c1ccc([#6]:[#6])n1c2ccccc2", "s1ccc(c1)-[#8;H1]",
    "[!#6][#6]1=,:[#7][#6]([#6])=,:[#6](C#N)[#6](=O)[#7]1", "[#6]-1(-[#6](=[#8])-[#7]-[#6](=[#8])-[#7]-[#6]-1=[#8])=[#7]", "[#6]=,:[#6]:[#7]([#6])~[#6]:[#6]=,:[#6][#6]~[#6]:[#7]",
    "c1cc2cccc3[#7][#6]=,:[#7]c(c1)c23", "[C;X4]1[N;H1]c3cccc2cccc([N;H1]1)c23", "[#6]-[#8]-[#6](=[#8])-[#6](-[#7][#6])=[#6]-[#6](-[#6])=[#8]", "S=[#6]1[#6]=,:[#6][!#6,!#6][#6]=,:[#6]1",
    "[#6](-[#6]#[#7])(-[#6]#[#7])-[#6](-[$([#6]#[#7]),$([#6]=[#7])])-[#6]#[#7]", "[H]c2c([H])c([H])c1c([H])c(C(=O)NN=C)c(O)c([H])c1c2[H]", "O=Cc1cnn2c([#8;H1])ccnc12",
    "n:1:c(:n(:c(:c:1-c:2:c:c:c:c:c:2)-c:3:c:c:c:c:c:3)-[#1])-[#6]:[!#1]", "[#6](-[#6]#[#7])(-[#6]#[#7])=[#6]-c:1:c:c:c:c:c:1", "C=NNc1ccccc1C(=O)[#8;H1]",
    "[#6]-,:[#6]:[#7+]=,:[#6][#6]=[#6][#7][#6;X4]", "[#6]=,:[#6]C1C(C#N)=C(N)SC(N)=C1C#N",
    "[#7]~[#6]:1:[#7]:[#7]:[#6](:[$([#7]),$([#6]-[#1]),$([#6]-[#7]-[#1])]:[$([#7]),$([#6]-[#7])]:1)-[$([#7]-[#1]),$([#8]-[#6](-[#1])-[#1])]", "[#6]-[#6]=[#6](-[F,Cl,Br,I])-[#6](=[#8])-[#6]",
    "N#CC(C#N)=NNc1ccccc1", "[#6]NC(=O)-!@[#6]1=,:[#6]([$([N]),$(NC(=O)[#6]:[#6])])[#7]([$([#6;H2]-[#6;H1]=[#6;H2]),$([#6]=,:[#6])])[#6](=S)[#16]1",
    paste0("[H]C([$([#6]-[#35]),$([#6]:[#6](-[#1]):[#6](-[F,Cl,Br,I]):[#6]:[#6]-[F,Cl,Br,I]),$([#6]:[#6](-[#1]):[#6](-[#1]):[#6]-[#16]-[#6](-[#1])-[#1]),$([#6]:[#",
           "6]:[#6]:[#6]:[#6]:[#6]:[#6]:[#6]:[#6]:[#6]-[#8]-[#6;H2]),$([#6]:1:[#6](-[#6;H2]):[#7](-[#6;H2]):[#6](-[#6;H2]):[#6]:1)])=C1SC(=O)[N]C1=O"),
    "[#7,#8]c2ccc1oc(=[#8,#16])sc1c2", "[#7](-[#6](-[#1])-[#1])(-[#6](-[#1])-[#1])-c:1:c(:c(:c(:o:1)-[#6]=[#7]-[#7](-[#1])-[#6]=[!#6&!#1])-[#1])-[#1]", "O=[#6]2[#6](=!@[#6]c1ccccc1)Sc3ccccc23",
    "c:1:c:c(:c:c:c:1-[#6;X4]-c:2:c:c:c(:c:c:2)-[#7](-[$([#1]),$([#6;X4])])-[$([#1]),$([#6;X4])])-[#7](-[$([#1]),$([#6;X4])])-[$([#1]),$([#6;X4])]",
    "c:1(:c(:c(:c(:c(:c:1-[#1])-[#1])-[#7](-[#1])-[#1])-[#1])-[#1])-[#6]=[#7]-[#7]-[#1]", "c1(nn(c(c1-[$([#1]),$([#6]-[#1])])-[#8]-[#1])-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#1])-[#1])-[#1])-[#6;X4]",
    paste0("c:2(:c:1-[#16]-c:3:c(-[#7](-c:1:c(:c(:c:2-[#1])-[#1])-[#1])-[$([#1]),$([#6](-[#1])(-[#1])-[#1]),$([#6](-[#1])(-[#1])-[#6]-[#1])]):c(:c(~[$([#1]),$([#6",
           "]:[#6])]):c(:c:3-[#1])-[$([#1]),$([#7](-[#1])-[#1]),$([#8]-[#6;X4])])~[$([#1]),$([#7](-[#1])-[#6;X4]),$([#6]:[#6])])-[#1]"),
    "[#6]-2-[#6]-c:1:c(:c:c:c:c:1)-[#6](-c:3:c:c:c:c:c-2:3)=[#6]-[#6]",
    "[#16]-1-[#6](=[#7]-[#6]:[#6])-[#7](-[$([#1]),$([#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#8]),$([#6]:[#6])])-[#6](=[#8])-[#6]-1=[#6](-[#1])-[$([#6]:[#6]:[#6]-[#17]),$([#6]:[!#6&!#1])]",
    "[#7](-[#1])(-[#1])-[#6]-1=[#6](-[#6]#[#7])-[#6](-[#1])(-[#6]:[#6])-[#6](=[#6](-[#6]=[#6])-[#8]-1)-[#6](-[#1])-[#1]", "[#8]=[#16](=[#8])-[#6](-[#6]#[#7])=[#7]-[#7]-[#1]",
    "c:1:c:c:c:c:c:1-[#7](-[#1])-[#6](=[#16])-[#7](-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-c:2:c:c:c:c:c:2",
    "c:1:c(:c:c:c:c:1)-[#7](-[#1])-c:2:c(:c(:c(:s:2)-[$([#6]=[#8]),$([#6]#[#7]),$([#6](-[#8]-[#1])=[#6])])-[#7])-[$([#6]#[#7]),$([#6](:[#7]):[#7])]", "[#6;X4]-1-[#6](=[#8])-[#7]-[#7]-[#6]-1=[#8]",
    "c:1:c-3:c(:c:c:c:1)-[#6]:2:[#7]:[!#1]:[#6]:[#6]:[#6]:2-[#6]-3=[#8]", "[#6]-1(-[#6](=[#6](-[#6]#[#7])-[#6](~[#8])~[#7]~[#6]-1~[#8])-[#6](-[#1])-[#1])=[#6](-[#1])-[#6]:[#6]",
    "[#6]-1(=[#6](-!@[#6]=[#7])-[#16]-[#6](-[#7]-1)=[#8])-[$([F,Cl,Br,I]),$([#7+](:[#6]):[#6])]",
    paste0("c:1:2:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1]):[!#6&!#1]:[#6](:[#6]:2-[#6](-[#1])=[#7]-[#7](-[#1])-[$([#6]:1:[#7]:[#6]:[#6](-[#1]):[#16]:1),$([#6]:[#6]",
           "(-[#1]):[#6]-[#1]),$([#6]:[#7]:[#6]:[#7]:[#6]:[#7]),$([#6]:[#7]:[#7]:[#7]:[#7])])-[$([#1]),$([#8]-[#1]),$([#6](-[#1])-[#1])]"),
    "[!#1]:[!#1]-[#6](-[$([#1]),$([#6]#[#7])])=[#6]-1-[#6]=:[#6]-[#6](=[$([#8]),$([#7;!R])])-[#6]=:[#6]-1",
    paste0("c:1:c:c-2:c(:c:c:1)-[#6]-[#6](-c:3:c(-[#16]-2):c(:c(-[#1]):c(:c:3-[#1])-[$([#1]),$([#8]),$([#16;X2]),$([#6;X4]),$([#7](-[$([#1]),$([#6;X4])])-[$([#1])",
           ",$([#6;X4])])])-[#1])-[#7](-[$([#1]),$([#6;X4])])-[$([#1]),$([#6;X4])]"),
    "[#6]-1(=[#6])-[#6](-[#7,#16,#8][#6](-[!#1])=[#7]-1)=[#8]", "[#7+](:[!#1]:[!#1]:[!#1])-[!#1]=[#8]",
    "[#6;X4]-[#7](-[#6;X4])-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#6]2=:[#7][#6]:[#6]:[!#1]2)-[#1])-[#1]",
    "[#7]-1(-[$([#6;X4]),$([#1])])-[#6]=:[#6](-[#6](=[#8])-[#6]:[#6]:[#6])-[#6](-[#6])-[#6](=[#6]-1-[#6](-[#1])(-[#1])-[#1])-[$([#6]=[#8]),$([#6]#[#7])]",
    "c:1:c:c:c:c:c:1-[#7](-[#1])-[#6](=[#16])-[#7](-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-c:2:c:c:c:c:c:2",
    "c:1:3:c(:c(:c(:c(:c:1-[#1])-[#1])-[#7](-[#1])-[#6](-[#1])(-[#1])-c:2:c:c:c:c:c:2)-[#1]):n:c(-[#1]):n:3-[#6]", "c:1:c:c-2:c(:c:c:1)-[#7]=[#6]-[#6]-2=[#7;!R]",
    "c:1(:c:c:c:c:c:1)-[#7](-[#1])-[#6](=[#16])-[#7]-[#7](-[#1])-[#6](=[#8])-[#6]-2:[!#1]:[!#6&!#1]:[#6]:[#6]-2", "[#7;!R]=[#6]-2-[#6](=[#8])-c:1:c:c:c:c:c:1-[#16]-2",
    "[$([#7](-[#1])-[#1]),$([#8]-[#1])]-[#6]-2=[#6](-[#6]#[#7])-[#6](-[#1])(-[#6]:[#6])-c:1:c(:n(-[#6]):n:c:1)-[#8]-2", "[#7](-[#1])(-[#1])-c:1:c(:c(:c(:n:c:1-[#1])-[#8]-c:2:c:c:c:c:c:2)-[#1])-[#1]",
    "[#6](=[#8])-[#6]-1=[#6]-[#7]-c:2:c(-[#16]-1):c:c:c:c:2", "c:1:c:c-2:c(:c:c:1)-[#6](-c:3:c(-[$([#16;X2]),$([#6;X4])]-2):c:c:c(:c:3)-[$([#1]),$([#17]),$([#6;X4])])=[#6]-[#6]",
    "[#6](-[#1])(-[#1])-[#16;X2]-c:1:n:c(:c(:n:1-!@[#6](-[#1])-[#1])-c:2:c:c:c:c:c:2)-[#1]",
    "[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-[#6]-2=[#6](-[#1])-c:1:c(:c:c:c:c:1)-[#16;X2]-c:3:c-2:c:c:c:c:3",
    "[#16]-1-[#6](=!@[#7]-[$([#1]),$([#7](-[#1])-[#6]:[#6])])-[#7](-[$([#1]),$([#6]:[#7]:[#6]:[#6]:[#16])])-[#6](=[#8])-[#6]-1=[#6](-[#1])-[#6]:[#6]-[$([#17]),$([#8]-[#6]-[#1])]",
    "[#16]-1-[#6](=[#8])-[#7]-[#6](=[#16])-[#6]-1=[#6](-[#1])-[#6]:[#6]", "c:1:c(:c:c:c:c:1)-[#6](-[#1])(-[#1])-[#7](-[#1])-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#8]-[#1])-[#1])-[#1]",
    "n1(-[#6;X4])c(c(-[#1])c(c1-[#6]:[#6])-[#1])-[#6](-[#1])-[#1]", "c:1(:c:c:c:c:c:1)-[#7](-[#1])-[#6](=[#16])-[#7]-[#7](-[#1])-c:2:c:c:c:c:c:2",
    "[#7](-c:1:c:c:c:c:c:1)-c2[n+]c(cs2)-c:3:c:c:c:c:c:3", "n:1:c:c:c(:c:1-[#6](-[#1])-[#1])-[#6](-[#1])=[#6]-2-[#6](=[#8])-[#7]-[#6](=[!#6&!#1])-[#7]-2",
    "[#6]-1(=[#6](-[#6](-[#1])(-[#6])-[#6])-[#16]-[#6](-[#7]-1-[$([#1]),$([#6](-[#1])-[#1])])=[#8])-[#16]-[#6;R]", "[!#1]:1:[!#1]-2:[!#1](:[!#1]:[!#1]:[!#1]:1)-[#7](-[#1])-[#7](-[#6]-2=[#8])-[#6]",
    "c:1:c:c-2:c(:c:c:1)-[#6](=[#6](-[#6]-2=[#8])-[#6])-[#8]-[#1]", "c:2:c:c:1:n:n:c(:n:c:1:c:c:2)-[#6](-[#1])(-[#1])-[#6]=[#8]",
    "c:1:c:c:c:c:c:1-[#7](-[#1])-[#6](=[#16])-[#7](-[#1])-[#6](-[#1])(-[#1])-c:2:n:c:c:c:c:2",
    "[#6](-[#1])-[#6](-[#1])(-[#1])-c:1:c(:c(:c(:s:1)-[#7](-[#1])-[#6](=[#8])-[#6]-[#6]-[#6]=[#8])-[$([#6](=[#8])-[#8]),$([#6]#[#7])])-[#6](-[#1])-[#1]",
    paste0("[#6](-c:1:c(:c(:c(:c:c:1-[#1])-[$([#6;X4]),$([#1])])-[#1])-[#1])(-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[$([#1]),$([#17])])-[#1])-[#1])=[$([#7]-[#8]-[#6](-[",
           "#1])(-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1]),$([#7]-[#8]-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7](-[#",
           "6](-[#1])-[#1])-[#6](-[#1])-[#1]),$([#7]-[#7](-[#1])-[#6](=[#7]-[#1])-[#7](-[#1])-[#1]),$([#6](-[#1])-[#7])]"),
    "[#8](-[#1])-[#6](=[#8])-c:1:c:c(:c:c:c:1)-[#6]:[!#1]:[#6]-[#6](-[#1])=[#6]-2-[#6](=[!#6&!#1])-[#7]-[#6](=[!#6&!#1])-[!#6&!#1]-2",
    "[#6]-1(=[#6]-[#6](-c:2:c:c(:c(:n:c-1:2)-[#7](-[#1])-[#1])-[#6]#[#7])=[#6])-[#6]#[#7]",
    "[#7](-[#1])(-[#1])-[#6]-1=[#6](-[#6]#[#7])-[#6](-[#1])(-[#6]:[#6])-[#6](=[#6](-[#6]:[#6])-[#8]-1)-[#6]#[#7]", "[#7]-2(-c:1:c:c:c:c:c:1)-[#7]=[#6](-[#6]=[#8])-[#6;X4]-[#6]-2=[#8]",
    "[#7]-1=[#6]-[#6](-[#6](-[#7]-1)=[#16])=[#6]", "c1(coc(c1-[#1])-[#6](=[#16])-[#7]-2-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[!#1]-[#6](-[#1])(-[#1])-[#6]-2(-[#1])-[#1])-[#1]",
    "[#6]=[#6](-[#6]#[#7])-[#6](=[#7]-[#1])-[#7]-[#7]", "c:1(:c(:c(:c(:o:1)-[$([#1]),$([#6](-[#1])-[#1])])-[#1])-[#1])-[#6](-[$([#1]),$([#6](-[#1])-[#1])])=[#7]-[#7](-[#1])-c:2:n:c:c:s:2",
    "c:1(:c(:c(:c(:c(:c:1-[#7](-[#1])-[#16](=[#8])(=[#8])-[#6]:2:[#6]:[!#1]:[#6]:[#6]:[#6]:2)-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#1])-[#1]",
    "n2c1ccccn1c(c2-[$([#6](-[!#1])=[#6](-[#1])-[#6]:[#6]),$([#6]:[#8]:[#6])])-[#7]-[#6]:[#6]", "[#6]-1-[#7](-[#1])-[#7](-[#1])-[#6](=[#16])-[#7]-[#7]-1-[#1]",
    "c:1(:c:c:c:o:1)-[#6](-[#1])=!@[#6]-3-[#6](=[#8])-c:2:c:c:c:c:c:2-[!#6&!#1]-3", "[#8]=[#6]-1-[#6;X4]-[#6]-[#6](=[#8])-c:2:c:c:c:c:c-1:2", "c:1:c:c-2:c(:c:c:1)-[#6](-c3cccc4noc-2c34)=[#8]",
    "[#8](-[#1])-c:1:n:c(:c:c:c:1)-[#8]-[#1]", "c:1:2:c(:c(:c(:c(:c:1:c(:c(:c(:c:2-[#1])-[#1])-[#6]=[#7]-[#7](-[#1])-[$([#6]:[#6]),$([#6]=[#16])])-[#1])-[#1])-[#1])-[#1])-[#1]",
    "[#6]-1=[#6](-[#16]-[#6](-[#6]=[#6]-1)=[#16])-[#7]", "[#6]-1=[#6]-[#6](-[#8]-[#6]-1-[#8])(-[#8])-[#6]", "[#8]=[#6]-1-[#6](=[#6]-[#6](=[#7]-[#7]-1)-[#6]=[#8])-[#6]#[#7]",
    "C3=CN1C(=NC(=C1-[#7]-[#6])-c:2:c:c:c:c:n:2)C=C3", "[#7]N-2-c:1:c:c:c:c:c:1-[#6](=[#7])-c:3:c-2:c:c:c:c:3",
    "c:1:c(:c:c:c:c:1)-[#7]-2-[#6](-[#1])-[#6](-[#1])-[#7](-[#6](-[#1])-[#6]-2-[#1])-[#16](=[#8])(=[#8])-c:3:c:c:c:c:4:n:s:n:c:3:4",
    "c:1(:c(:c-2:c(:c(:c:1-[#1])-[#1])-[#7](-[#6](-[#7]-2-[#1])=[#8])-[#1])-[#1])-[#7](-[#1])-[#6](-[#1])-[#1]",
    "c:1(:c(:c-3:c(:c(:c:1-[#7](-[#1])-[#16](=[#8])(=[#8])-c:2:c:c:c(:c:c:2)-[!#6&!#1])-[#1])-[#8]-[#6](-[#8]-3)(-[#1])-[#1])-[#1])-[#1]",
    "[#6](-[#1])-[#6]:2:[#7]:[#7](-c:1:c:c:c:c:c:1):[#16]:3:[!#6&!#1]:[!#1]:[#6]:[#6]:2:3", "[#8]=[#6]-[#6]=[#6](-[#1])-[#8]-[#1]",
    "[#7]-1-2-[#6](=[#7]-[#6](=[#8])-[#6](=[#7]-1)-[#6](-[#1])-[#1])-[#16]-[#6](=[#6](-[#1])-[#6]:[#6])-[#6]-2=[#8]", "[#6]:[#6]-[#6](-[#1])=[#6](-[#1])-[#6](-[#1])=[#7]-[#7](-[#6;X4])-[#6;X4]",
    "c:1:3:c(:c:c:c:c:1):c:2:n:n:c(-[#16]-[#6](-[#1])(-[#1])-[#6]=[#8]):n:c:2:n:3-[#6](-[#1])(-[#1])-[#6](-[#1])=[#6](-[#1])-[#1]",
    "n1(-[#6])c(c(-[#1])c(c1-[#6](-[#1])(-[#1])-[#7](-[#1])-[#6](=[#16])-[#7]-[#1])-[#1])-[#1]", "n2(-[#6]:1:[!#1]:[!#6&!#1]:[!#1]:[#6]:1-[#1])c(c(-[#1])c(c2-[#6;X4])-[#1])-[#6;X4]",
    "c:1(:c:c:c:c:c:1)-[#7](-[#1])-[#6](=[#16])-[#7]-[#7](-[#1])-[#6]([#7;R])[#7;R]",
    paste0("c:1(:c(:c(:c(:c(:c:1-[$([#1]),$([#6](-[#1])-[#1])])-[#1])-[#8]-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[$([#7](-[#1])-[#6](=[#8])-[#6](-[#1])(-[#1])-[#6]",
           "(-[#1])(-[#1])-[#6](-[#1])-[#1]),$([#6](-[#1])(-[#6](-[#1])-[#1])-[#7](-[#1])-[#6](=[#16])-[#7]-[#1])])-[#1])-[#8]-[#6](-[#1])-[#1]"),
    "n2(-[#6]:1:[#6](-[#6]#[#7]):[#6]:[#6]:[!#6&!#1]:1)c(c(-[#1])c(c2)-[#1])-[#1]", "[#7](-[#1])(-[#1])-[#6]-2=[#6](-[#6]#[#7])-[#6](-[#1])(-[#6]:[#6])-c:1:c(:c:c:s:1)-[#8]-2",
    "[#7](-[#1])-c:1:n:c(:c:s:1)-c:2:c:n:c(-[#7](-[#1])-[#1]):s:2", "[#7]=[#6]-1-[#7](-[#1])-[#6](=[#6](-[#7]-[#1])-[#7]=[#7]-1)-[#7]-[#1]",
    "c:1:c(:c:2:c(:c:c:1):c:c:c:c:2)-[#8]-c:3:c(:c(:c(:c(:c:3-[#1])-[#1])-[#7]-[#1])-[#1])-[#1]", "c:1:c:c-2:c(:c:c:1)-[#6]-[#16]-c3c(-[#6]-2=[#6])ccs3", "c:2:c:c:c:1:c(:c:c:c:1):c:c:2",
    "c:1(:c(:c(:c(:o:1)-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#8]-[#6]:[#6])-[#1])-[#6](=[#8])-[#8]-[#1]", "[!#1]:[#6]-[#6]-1=[#6](-[#1])-[#6](=[#6](-[#6]#[#7])-[#6](=[#8])-[#7]-1-[#1])-[#6]:[#8]",
    "[#6]-1-3=[#6](-[#6](-[#7]-c:2:c:c:c:c:c-1:2)(-[#6])-[#6])-[#16]-[#16]-[#6]-3=[!#1]",
    "c:1(:c(:c(:c(:c(:c:1-[#7](-[#1])-[#6](=[#8])-c:2:c:c:c:c:c:2)-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#1])-[#1]",
    "[#6](-[#1])(-[#1])-[#16;X2]-c:1:n:n:c(:c(:n:1)-c:2:c(:c(:c(:o:2)-[#1])-[#1])-[#1])-c:3:c(:c(:c(:o:3)-[#1])-[#1])-[#1]",
    "[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-[#6]-2=[#6]-c:1:c(:c:c:c:c:1)-[#6]-2(-[#1])-[#1]",
    "[#7](-[#1])(-c:1:c:c:c:c:c:1)-[#7]=[#6](-[#6](=[#8])-[#6](-[#1])-[#1])-[#7](-[#1])-[$([#7]-[#1]),$([#6]:[#6])]",
    "c:1:2:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1]):o:c:3:c(-[#1]):c(:c(-[#8]-[#6](-[#1])-[#1]):c(:c:2:3)-[#1])-[#7](-[#1])-[#6](-[#1])-[#1]",
    "[#16]=[#6]-1-[#7](-[#1])-[#6]=[#6]-[#6]-2=[#6]-1-[#6](=[#8])-[#8]-[#6]-2=[#6]-[#1]", "n2(-c:1:c(:c:c(:c(:c:1)-[#1])-[$([#7](-[#1])-[#1]),$([#6]:[#7])])-[#1])c(c(-[#1])c(c2-[#1])-[#1])-[#1]",
    "n1(-[#6])c(c(-[#1])c(c1-[#6](-[#1])=[#6]-2-[#6](=[#8])-[!#6&!#1]-[#6]=:[!#1]-2)-[#1])-[#1]", "[#6]=[#6]-[#6](-[#6]#[#7])(-[#6]#[#7])-[#6](-[#6]#[#7])=[#6]-[#7](-[#1])-[#1]",
    "[#6]:[#6]-[#6](=[#16;X1])-[#16;X2]-[#6](-[#1])-[$([#6](-[#1])-[#1]),$([#6]:[#6])]", "[#8]=[#6]-3-[#6](=!@[#6](-[#1])-c:1:c:n:c:c:1)-c:2:c:c:c:c:c:2-[#7]-3",
    "c:1(:c(:c(:c(:s:1)-[#1])-[#1])-[$([#1]),$([#6](-[#1])-[#1])])-[#6](-[#1])=[#7]-[#7](-[#1])-c:2:c:c:c:c:c:2",
    "[#6](-[#1])(-[#1])-[#16;X2]-[#6]-1=[#6](-[#6]#[#7])-[#6](-[#1])(-[#6]:[#6])-[#6](-[#6]#[#7])-[#6](=[#8])-[#7]-1",
    "[#7]-2(-c:1:c:c:c:c:c:1)-[#7]=[#6](-[#7](-[#1])-[#6]=[#8])-[#6](-[#1])(-[#1])-[#6]-2=[#8]", "[#6]:[#6]-[#6](-[#1])=[#6](-[#1])-[#6](-[#1])=[#7]-[#7]=[#6]",
    "c:1(:c:c:c(:c:c:1)-[#6](-[#1])-[#1])-c:2:c(:s:c(:n:2)-[#7](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#1]", "[#6]-2(-[#6]=[#7]-c:1:c:c:c:c:c:1-[#7]-2)=[#6](-[#1])-[#6]=[#8]",
    "[#8](-c:1:c:c:c:c:c:1)-c:3:c:c:2:n:o:n:c:2:c:c:3", "[!#1]:1:[!#1]:[!#1]:[!#1](:[!#1]:[!#1]:1)-[#6](-[#1])=[#6](-[#1])-[#6](-[#7]-c:2:c:c:c:3:c(:c:2):c:c:c(:n:3)-[#7](-[#6])-[#6])=[#8]",
    "[#7](-[#1])(-[#1])-c:1:c(:c:c:c:n:1)-[#8]-[#6](-[#1])(-[#1])-[#6]:[#6]", "[#6]-[#16;X2]-c:1:n:c(:c:s:1)-[#1]",
    "c:1:c-3:c(:c:c:c:1)-[#7](-c:2:c:c:c:c:c:2-[#8]-3)-[#6](-[#1])(-[#1])-[#6](-[#1])-[#1]", "c:1(:c(:c(:c(:o:1)-[#6](-[#1])-[#1])-[#1])-[#1])-[#6](-[#1])(-[#8]-[#1])-[#6]#[#6]-[#6;X4]",
    "[#6]-1(-[#6](=[#6]-[#6]=[#6]-[#6]=[#6]-1)-[#7]-[#1])=[#7]-[#6]", "[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])=[#6]-[#6](=[#8])-c:1:c(-[#16;X2]):s:c(:c:1)-[$([#6]#[#7]),$([#6]=[#8])]",
    "c:1:3:c(:c:c:c:c:1)-[#7]-2-[#6](=[#8])-[#6](=[#6](-[F,Cl,Br,I])-[#6]-2=[#8])-[#7](-[#1])-[#6]:[#6]:[#6]:[#6](-[#8]-[#6](-[#1])-[#1]):[#6]:[#6]:3",
    "c:1-2:c(:c:c:c:c:1)-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7]=[#6]-2-[#16;X2]-[#6](-[#1])(-[#1])-[#6](=[#8])-c:3:c:c:c:c:c:3",
    "[#7]-2(-c:1:c:c:c:c:c:1-[#6](-[#1])-[#1])-[#6](=[#16])-[#7](-[#6](-[#1])(-[#1])-[!#1]:[!#1]:[!#1]:[!#1]:[!#1])-[#6](-[#1])(-[#1])-[#6]-2=[#8]",
    "[#7]-2(-[#6](-[#1])-[#1])-[#6](=[#16])-[#7](-[#1])-[#6](=[#6](-[#1])-c:1:c:c:c:c(:c:1)-[Br])-[#6]-2=[#8]",
    "c:1(:c(:c:2:c(:s:1):c:c:c:c:2)-[#6](-[#1])-[#1])-[#6](=[#8])-[#6](-[#1])(-[#1])-[#6](-[#1])-[#1]",
    "[#7](-[#6](-[#1])-[#1])(-[#6](-[#1])-[#1])-[#6](-[#1])=[#7]-[#6](-[#6](-[#1])-[#1])=[#7]-[#7](-[#6](-[#1])-[#1])-[#6]:[#6]",
    paste0("[#6]:2(:[#6](-[#6](-[#1])-[#1]):[#6]-1:[#6](-[#7]=[#6](-[#7](-[#6]-1=[!#6&!#1;X1])-[#6](-[#1])-[$([#6](=[#8])-[#8]),$([#6]:[#6])])-[$([#1]),$([#16]-[#",
           "6](-[#1])-[#1])]):[!#6&!#1;X2]:2)-[#6](-[#1])(-[#1])-[#6](-[#1])-[#1]"),
    "c:1(:n:c(:c(-[#1]):s:1)-[!#1]:[!#1]:[!#1](-[$([#8]-[#6](-[#1])-[#1]),$([#6](-[#1])-[#1])]):[!#1]:[!#1])-[#7](-[#1])-[#6](-[#1])(-[#1])-c:2:c(-[#1]):c(:c(-[#1]):o:2)-[#1]",
    "n:1:c(:c(:c(:c(:c:1-[#16]-[#6]-[#1])-[#6]#[#7])-c:2:c:c:c(:c:c:2)-[#8]-[#6](-[#1])-[#1])-[#1])-[#6]:[#6]",
    paste0("c:1:4:c(:n:c(:n:c:1-[#7](-[#1])-[#6](-[#1])(-[#1])-c:2:c(:c(:c(:o:2)-[#1])-[#1])-[#1])-[#7](-[#1])-c:3:c:c(:c(:c:c:3-[$([#1]),$([#6](-[#1])-[#1]),$([#",
           "16;X2]),$([#8]-[#6]-[#1]),$([#7;X3])])-[$([#1]),$([#6](-[#1])-[#1]),$([#16;X2]),$([#8]-[#6]-[#1]),$([#7;X3])])-[$([#1]),$([#6](-[#1])-[#1]),$([#16;X2]",
           "),$([#8]-[#6]-[#1]),$([#7;X3])]):c:c:c:c:4"),
    "[#7](-[#1])(-[#6]:1:[#6]:[#6]:[!#1]:[#6]:[#6]:1)-c:2:c:c:c(:c:c:2)-[#7](-[#1])-[#6]-[#1]", "[#7]-2(-c:1:c:c:c:c:c:1)-[#6](=[#7]-[#6]=[#8])-[#16]-[#6](-[#1])(-[#1])-[#6]-2=[#8]",
    "[#6]=[#6]-[#6](=[#8])-[#7]-c:1:c(:c(:c(:s:1)-[#6](=[#8])-[#8])-[#6]-[#1])-[#6]#[#7]",
    "[$([#1]),$([#6](-[#1])-[#1])]-[#8]-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#7](-[#1])-[#6](-[#1])(-[#1])-c:2:n:c:c:n:2",
    "[#6](-[#1])(-[#1])-[#16;X2]-c3nc1c(n(nc1-[#6](-[#1])-[#1])-c:2:c:c:c:c:c:2)nn3",
    "[#6]-[#6](=[#8])-[#6](-[#1])(-[#1])-[#16;X2]-c:3:n:n:c:2:c:1:c(:c(:c(:c(:c:1:n(:c:2:n:3)-[#1])-[#1])-[#1])-[#1])-[#1]",
    "s:1:c(:[n+](-[#6](-[#1])-[#1]):c(:c:1-[#1])-[#6])-[#7](-[#1])-c:2:c:c:c:c:c:2[$([#6](-[#1])-[#1]),$([#6]:[#6])]",
    "[#6]-2(=[#16])-[#7](-[#6](-[#1])(-[#1])-c:1:c:c:c:o:1)-[#6](=[#7]-[#7]-2-[#1])-[#6]:[#6]", "[#7]-2(-c:1:c:c:c:c:c:1)-[#6](=[#8])-[#6](=[#6]-[#6](=[#7]-2)-[#6]#[#7])-[#6]#[#7]",
    "[#7]-2(-c:1:c:c:c:c:c:1)-[#6](=[#8])-[#16]-[#6](-[#1])(-[#6](-[#1])(-[#1])-[#6](=[#8])-[#7](-[#1])-[#6]:[#6])-[#6]-2=[#8]",
    "[#6](-[#1])(-[#1])-[#7]-2-[#6](=[$([#16]),$([#7])])-[!#6&!#1]-[#6](=[#6]-1-[#6](=[#6](-[#1])-[#6]:[#6]-[#7]-1-[#6](-[#1])-[#1])-[#1])-[#6]-2=[#8]", "[#6]=[#7;!R]-c:1:c:c:c:c:c:1-[#8]-[#1]",
    "[#8]=[#6]-2-[#16]-c:1:c(:c(:c:c:c:1)-[#8]-[#6](-[#1])-[#1])-[#8]-2", "[#7]=[#6]-1-[#7]=[#6]-[#7]-[#16]-1", "[#7]-2-[#16]-[#6]-1=[#6](-[#6]:[#6]-[#7]-[#6]-1)-[#6]-2=[#16]",
    "[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#6](-[#1])=[#7]-[#7]=[#6](-[#6])-[#6]:[#6])-[#1])-[#1]", "n1-2cccc1-[#6]=[#7](-[#6])-[#6]-[#6]-2",
    "[#6](-[#6]#[#7])(-[#6]#[#7])=[#6](-[#16])-[#16]", "[#6]-1(-[#6]#[#7])(-[#6]#[#7])-[#6](-[#1])(-[#6](=[#8])-[#6])-[#6]-1-[#1]",
    "[#6]-1=:[#6]-[#6](-[#6](-[$([#8]),$([#16])]-1)=[#6]-[#6]=[#8])=[#8]", "[#6]:[#6]-[#6](=[#8])-[#7](-[#1])-[#6](=[#8])-[#6](-[#6]#[#7])=[#6](-[#1])-[#7](-[#1])-[#6]:[#6]",
    "c:1(:c:c:c:c:c:1)-[#7](-[#1])-[#6](=[#16])-[#7](-[#1])-[#7]=[#6]-c:2:c:n:c:c:2",
    "[#7](-[#1])(-[#1])-[#6]-2=[#6](-[#6]#[#7])-[#6](-[#1])(-c:1:c:c:c:s:1)-[#6](=[#6](-[#6](-[#1])-[#1])-[#8]-2)-[#6](=[#8])-[#8]-[#6]",
    "c:1:c-3:c(:c:c(:c:1)-[#6](=[#8])-[#7](-[#1])-c:2:c(:c:c:c:c:2)-[#6](=[#8])-[#8]-[#1])-[#6](-[#7](-[#6]-3=[#8])-[#6](-[#1])-[#1])=[#8]", "[Cl]-c:2:c:c:1:n:o:n:c:1:c:c:2", "[#6]-[#6](=[#16])-[#1]",
    "[#6;X4]-[#7](-[#1])-[#6](-[#6]:[#6])=[#6](-[#1])-[#6](=[#16])-[#7](-[#1])-c:1:c:c:c:c:c:1", "[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#16]-[#6](-[#1])(-[#1])-c1cn(cn1)-[#1]",
    "[#8]=[#6]-[#7](-[#1])-c:1:c(-[#6]:[#6]):n:c(-[#6](-[#1])(-[#1])-[#6]#[#7]):s:1", "[#6](-[#1])-[#7](-[#1])-c:1:n:c(:c:s:1)-c2cnc3n2ccs3",
    "[#7]-1-[#6](=[#8])-[#6](=[#6](-[#6])-[#16]-[#6]-1=[#16])-[#1]", "[#6](-[#16])(-[#7])=[#6](-[#1])-[#6]=[#6](-[#1])-[#6]=[#8]",
    "[#8]=[#6]-3-c:1:c(:c:c:c:c:1)-[#6]-2=[#6](-[#8]-[#1])-[#6](=[#8])-[#7]-c:4:c-2:c-3:c:c:c:4", "c:1:2:c:c:c:c(:c:1:c(:c:c:c:2)-[$([#8]-[#1]),$([#7](-[#1])-[#1])])-[#6](-[#6])=[#8]",
    "[#6](-[#1])(-c:1:c:c:c:c:c:1)(-c:2:c:c:c:c:c:2)-[#6](=[#16])-[#7]-[#1]",
    "[#7]-2(-[#6](=[#8])-c:1:c(:c(:c(:c(:c:1-[#1])-[#6](=[#8])-[#8]-[#1])-[#1])-[#1])-[#6]-2=[#8])-c:3:c(:c:c(:c(:c:3)-[#1])-[#8])-[#1]",
    "c:1:c:c(:c:c:c:1-[#7](-[#1])-[#16](=[#8])=[#8])-[#7](-[#1])-[#16](=[#8])=[#8]", "[#6](-[#1])-[#7](-[#1])-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#7](-[#1])-[#6]-[#1]",
    "s1c(c(c-2c1-[#7](-[#1])-[#6](-[#6](=[#6]-2-[#1])-[#6](=[#8])-[#8]-[#1])=[#8])-[#7](-[#1])-[#1])-[#6](=[#8])-[#7]-[#1]",
    "c:2(:c:1:c(:c(:c(:c(:c:1:c(:c(:c:2-[#1])-[#1])-[#1])-[#1])-[#7](-[#1])-[#7](-[#1])-[#6]=[#8])-[#1])-[#1])-[#1]",
    "[#6](-[#1])(-[#1])-c:1:c(:c(:c(:c(:c:1-[#8]-[#6](-[#1])-[#1])-[#1])-[#1])-[#6](-[#1])(-[#1])-[#7](-[#1])-[#6;X4])-[#1]", "[#6]-1=[#6]-[#7]-[#6](-[#16]-[#6;X4]-1)=[#16]",
    "[#6](-[#7](-[#6]-[#1])-[#6]-[#1]):[#6]-[#7](-[#1])-[#6](=[#16])-[#6]-[#1]", "n2nc(c1cccc1c2-[#6])-[#6]", "s:1:c(:c(-[#1]):c(:c:1-[#6](=[#8])-[#7](-[#1])-[#7]-[#1])-[#8]-[#6](-[#1])-[#1])-[#1]",
    "[#6]-1:[#6]-[#7]=[#6]-[#6](=[#6]-[#7]-[#6])-[#16]-1", "[#6](-[#1])(-[#1])-[#6](-[#1])(-[#6]#[#7])-[#6](=[#8])-[#6]",
    "c2(c(-[#7](-[#1])-[#1])n(-c:1:c:c:c:c:c:1-[#6](=[#8])-[#8]-[#1])nc2-[#6]=[#8])-[$([#6]#[#7]),$([#6]=[#16])]", "c:2:c:1:c:c:c:c-3:c:1:c(:c:c:2)-[#7](-[#7]=[#6]-3)-[#1]",
    "c:2:c:1:c:c:c:c-3:c:1:c(:c:c:2)-[#7]-[#7]=[#7]-3", "c1csc(n1)-[#7]-[#7]-[#16](=[#8])=[#8]",
    "c:1:c:c:c:2:c(:c:1):n:c(:n:c:2)-[#7](-[#1])-[#6]-3=[#7]-[#6](-[#6]=[#6]-[#7]-3-[#1])(-[#6](-[#1])-[#1])-[#6](-[#1])-[#1]",
    "c:1-3:c(:c(:c(:c(:c:1)-[#8]-[#6]-[#1])-[#1])-[#1])-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#6](=[#8])-[#8]-3",
    "c:12:c(:c:c:c:n:1)c(c(-[#6](=[#8])~[#8;X1])s2)-[#7](-[#1])-[#1]", "c:1:2:n:c(:c(:n:c:1:[#6]:[#6]:[#6]:[!#1]:2)-[#6](-[#1])=[#6](-[#8]-[#1])-[#6])-[#6](-[#1])=[#6](-[#8]-[#1])-[#6]",
    "c1csc(c1-[#7](-[#1])-[#1])-[#6](-[#1])=[#6](-[#1])-c2cccs2", "c:2:c:c:1:n:c:3:c(:n:c:1:c:c:2):c:c:c:4:c:3:c:c:c:c:4", "[#6]:[#6]-[#7](-[#1])-[#16](=[#8])(=[#8])-[#7](-[#1])-[#6]:[#6]",
    "c:1:c:c(:c:c:c:1-[#7](-[#1])-[#1])-[#7](-[#6;X3])-[#6;X3]",
    "[#7]-2=[#6](-c:1:c:c:c:c:c:1)-[#6](-[#1])(-[#1])-[#6](-[#8]-[#1])(-[#6](-[#9])(-[#9])-[#9])-[#7]-2-[$([#6]:[#6]:[#6]:[#6]:[#6]:[#6]),$([#6](=[#16])-[#6]:[#6]:[#6]:[#6]:[#6]:[#6])]",
    "c:1:c(:c:c:c:c:1)-[#6](=[#8])-[#6](-[#1])=[#6]-3-[#6](=[#8])-[#7](-[#1])-[#6](=[#8])-[#6](=[#6](-[#1])-c:2:c:c:c:c:c:2)-[#7]-3-[#1]",
    "[#8]=[#6]-4-[#6]-[#6]-[#6]-3-[#6]-2-[#6](=[#8])-[#6]-[#6]-1-[#6]-[#6]-[#6]-[#6]-1-[#6]-2-[#6]-[#6]-[#6]-3=[#6]-4", "c:1:2:c:3:c(:c(-[#8]-[#1]):c(:c:1:c(:c:n:2-[#6])-[#6]=[#8])-[#1]):n:c:n:3",
    "[#6;X4]-[#7+](-[#6;X4]-[#8]-[#1])=[#6]-[#16]-[#6]-[#1]", "[#6]-3(=[#8])-[#6](=[#6](-[#1])-[#7](-[#1])-c:1:c:c:c:c:c:1-[#6](=[#8])-[#8]-[#1])-[#7]=[#6](-c:2:c:c:c:c:c:2)-[#8]-3",
    "c:1(:c(:c(:c(:o:1)-[$([#1]),$([#6](-[#1])-[#1])])-[#1])-[#1])-[#6](-[$([#1]),$([#6](-[#1])-[#1])])=[#7]-[#7](-[#1])-c:2:c:c:n:c:c:2",
    "c:1(:c(:c(:c(:s:1)-[$([#1]),$([#6](-[#1])-[#1])])-[#1])-[#1])-[#6](-[$([#1]),$([#6](-[#1])-[#1])])-[#6](=[#8])-[#7](-[#1])-c:2:n:c:c:s:2",
    "[#6]:[#6]-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#6]=[#8])-[#7]-2-[#6](=[#8])-[#6]-1(-[#1])-[#6](-[#1])(-[#1])-[#6]=[#6]-[#6](-[#1])(-[#1])-[#6]-1(-[#1])-[#6]-2=[#8]",
    "[#6]-1(-[#6]=[#8])(-[#6]:[#6])-[#16;X2]-[#6]=[#7]-[#7]-1-[#1]", "[#7](-[#1])(-[#1])-c:1:c(:c(:c(:s:1)-[#7](-[#1])-[#6](=[#8])-c:2:c:c:c:c:c:2)-[#6]#[#7])-[#6]:3:[!#1]:[!#1]:[!#1]:[!#1]:[!#1]:3",
    "[#6](-[#1])(-[#1])-[#8]-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#7](-[#1])-[#6](-[#1])(-[#1])-c:2:c:c:c:c:c:2-[$([#6](-[#1])-[#1]),$([#8]-[#6](-[#1])-[#1])]",
    paste0("[#6](-[#1])(-[#1])(-[#1])-[#6](-[#6](-[#1])(-[#1])-[#1])(-[#6](-[#1])(-[#1])-[#1])-c:1:c(:c:c(:c(:c:1-[#1])-[#6](-[#6](-[#1])(-[#1])-[#1])(-[#6](-[#1]",
           ")(-[#1])-[#1])-[#6](-[#1])(-[#1])-[#1])-[#8]-[#6](-[#1])-[#7])-[#1]"),
    "c:1(:c(:o:c:c:1)-[#6]-[#1])-[#6]=[#7]-[#7](-[#1])-[#6](=[#16])-[#7]-[#1]", "[#7](-[#1])-c1nc(nc2nnc(n12)-[#16]-[#6])-[#7](-[#1])-[#6]",
    "c:1-2:c(:c:c:c:c:1-[#6](-[#1])(-[#1])-[#6](-[#1])=[#6](-[#1])-[#1])-[#6](=[#6](-[#6](=[#8])-[#7](-[#1])-[#6]:[#6])-[#6](=[#8])-[#8]-2)-[#1]",
    "[#6]-2(=[#16])-[#7]-1-[#6]:[#6]-[#7]=[#7]-[#6]-1=[#7]-[#7]-2-[#1]", "[#6]:[#6]:[#6]:[#6]:[#6]:[#6]-c:1:c:c(:c(:s:1)-[#7](-[#1])-[#6](=[#8])-[#6])-[#6](=[#8])-[#8]-[#1]",
    "[#7](-[#1])(-[#1])-c:1:c(:c(:c(:c:c:1-[#7](-[#1])-[#6](-[#1])(-[#6])-[#6](-[#1])-[#6](-[#1])-[#1])-[#1])-[#1])-[#1]",
    "[#16]=[#6]-2-[#7](-[#1])-[#7]=[#6](-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#1])-[#8]-2", "[#16]=[#6]-c:1:c:c:c:2:c:c:c:c:n:1:2",
    "[#6]~1~[#6](~[#7]~[#7]~[#6](~[#6](-[#1])-[#1])~[#6](-[#1])-[#1])~[#7]~[#16]~[#6]~1", "[#6]-1(-[#6]=:[#6]-[#6]=:[#6]-[#6]-1=[!#6&!#1])=[!#6&!#1]",
    "[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-c:1:c(-[#1]):c(:c(:o:1)-[#6](-[#1])=[#6]-[#6]#[#7])-[#1]", "[#8]=[#6]-1-[#6]:[#6]-[#6](-[#1])(-[#1])-[#7]-[#6]-1=[#6]-[#1]",
    "[#6]:[#6]-[#7]:2:[#7]:[#6]:1-[#6](-[#1])(-[#1])-[#16;X2]-[#6](-[#1])(-[#1])-[#6]:1-[#6]:2-[#7](-[#1])-[#6](=[#8])-[#6](-[#1])=[#6]-[#1]",
    "n:1:c(:n(:c:2:c:1:c:c:c:c:2)-[#6](-[#1])-[#1])-[#16]-[#6](-[#1])(-[#1])-[#6](=[#8])-[#7](-[#1])-[#7]=[#6](-[#1])-[#6](-[#1])=[#6]-[#1]",
    "c:1(:c:c(:c(:c:c:1)-[#8]-[#1])-[#6](=!@[#6]-[#7])-[#6]=[#8])-[#8]-[#1]", "c:1(:c:c(:c(:c:c:1)-[#7](-[#1])-[#6](=[#8])-[#6]:[#6])-[#6](=[#8])-[#8]-[#1])-[#8]-[#1]",
    "n2(-[#6](-[#1])-[#1])c-1c(-[#6]:[#6]-[#6]-1=[#8])cc2-[#6](-[#1])-[#1]", "[#6](-[#1])-[#7](-[#1])-c:1:c(:c(:c(:s:1)-[#6]-[#1])-[#6]-[#1])-[#6](=[#8])-[#7](-[#1])-[#6]:[#6]",
    "[#6]:[#6]-[#7;!R]=[#6]-2-[#6](=[!#6&!#1])-c:1:c:c:c:c:c:1-[#7]-2", "c:1:c:c:c:c:c:1-[#6](=[#8])-[#7](-[#1])-[#7]=[#6]-3-c:2:c:c:c:c:c:2-c:4:c:c:c:c:c-3:4",
    "c:1:c(:c:c:c:c:1)-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])=[#6](-[#1])-[#6]=!@[#6](-[#1])-[#6](-[#1])=[#6]-[#6]=@[#7]-c:2:c:c:c:c:c:2",
    "[#6]:1:2:[!#1]:[#7+](:[!#1]:[#6](:[!#1]:1:[#6]:[#6]:[#6]:[#6]:2)-[*])~[#6]:[#6]", "[#7]-2(-c:1:c:c:c:c:c:1)-[#7]=[#6](-[#6](-[#1])-[#1])-[#6](-[#1])(-[#16]-[#6])-[#6]-2=[#8]",
    "c:1:c:c:c(:c:c:1-[#7](-[#1])-c2nc(c(-[#1])s2)-c:3:c:c:c(:c:c:3)-[#6](-[#1])(-[#6]-[#1])-[#6]-[#1])-[#6](=[#8])-[#8]-[#1]",
    "[#6](-[#1])(-[#1])-[#7](-[#1])-[#6]=[#7]-[#7](-[#1])-c1nc(c(-[#1])s1)-[#6]:[#6]", "[#6]:[#6]-[#7](-[#1])-[#6](=[#8])-c1c(snn1)-[#7](-[#1])-[#6]:[#6]",
    "[#8]=[#16](=[#8])(-[#6]:[#6])-[#7](-[#1])-c1nc(cs1)-[#6]:[#6]", "[#8]=[#16](=[#8])(-[#6]:[#6])-[#7](-[#1])-[#7](-[#1])-c1nc(cs1)-[#6]:[#6]",
    "s2c:1:n:c:n:c(:c:1c(c2-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#7]-[#7]=[#6]-c3ccco3", "[#6](=[#8])-[#6](-[#1])=[#6](-[#8]-[#1])-[#6](-[#8]-[#1])=[#6](-[#1])-[#6](=[#8])-[#6]",
    "c:2(:c:1-[#6](-[#6](-[#6](-c:1:c(:c(:c:2-[#1])-[#1])-[#1])(-[#1])-[#1])=[#8])=[#6](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1]",
    "[#6]:[#6]-[#7](-[#1])-[#7]=[#6](-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#6](-[#1])-[#1])=[#7]-[#7](-[#1])-[#6]:[#6]",
    "[#6;X4]-[#16;X2]-[#6](=[#7]-[!#1]:[!#1]:[!#1]:[!#1])-[#7](-[#1])-[#7]=[#6]", "[#6]-1(=[#7]-[#7](-[#6](-[#16]-1)=[#6](-[#1])-[#6]:[#6])-[#6]:[#6])-[#6]=[#8]",
    "c:1(:c(:c:2:c(:n:c:1-[#7](-[#1])-[#1]):c:c:c(:c:2-[#7](-[#1])-[#1])-[#6]#[#7])-[#6]#[#7])-[#6]#[#7]",
    "[!#1]:1:[!#1]:[!#1]:[!#1](:[!#1]:[!#1]:1)-[#6](-[#1])=[#6](-[#1])-[#6](-[#7](-[#1])-[#7](-[#1])-c2nnnn2-[#6])=[#8]",
    "c:1:2:c(:c(:c(:c(:c:1:c(:c(:c(:c:2-[#1])-[#1])-[#6](=[#7]-[#6]:[#6])-[#6](-[#1])-[#1])-[#8]-[#1])-[#1])-[#1])-[#1])-[#1]",
    paste0("c:1(:c(:c:2:c(:c(:c:1-[#8]-[#6](-[#1])-[#1])-[#1]):c(:c(:c(:c:2-[#7](-[#1])-[#6](-[#1])(-[#1])-[#1])-[#1])-c:3:c(:c(:c(:c(:c:3-[#1])-[#1])-[#8]-[#6](-",
           "[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#1])-[#1])-[#8]-[#6](-[#1])-[#1]"),
    "c:1:c:c-2:c(:c:c:1)-[#16]-c3c(-[#7]-2)cc(s3)-[#6](-[#1])-[#1]",
    "c:1:c:c:c-2:c(:c:1)-[#6](-[#6](-[#7]-2-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7]-4-[#6](-c:3:c:c:c:c:c:3-[#6]-4=[#8])=[#8])(-[#1])-[#1])(-[#1])-[#1]",
    "c:1(:c:c:c(:c:c:1)-[#6]-3=[#6]-[#6](-c2cocc2-[#6](=[#6]-3)-[#8]-[#1])=[#8])-[#16]-[#6](-[#1])-[#1]",
    "[#6;X4]-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#6](=[#8])-[#7](-[#1])-[#6](-[#1])(-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#16]-[#6](-[#1])(-[#1])-[#1])-[#6](=[#8])-[#8]-[#1])-[#1])-[#1]",
    "n:1:c(:n(:c(:c:1-c:2:c:c:c:c:c:2)-c:3:c:c:c:c:c:3)-[#7]=!@[#6])-[#7](-[#1])-[#1]", "[#6](-c:1:c:c:c(:c:c:1)-[#8]-[#1])(-c:2:c:c:c(:c:c:2)-[#8]-[#1])-[#8]-[#16](=[#8])=[#8]",
    "c:2:c:c:1:n:c(:c(:n:c:1:c:c:2)-[#6](-[#1])(-[#1])-[#6](=[#8])-[#6]:[#6])-[#6](-[#1])(-[#1])-[#6](=[#8])-[#6]:[#6]",
    "c:1(:c(:c(:c(:c(:c:1-[#1])-[#8]-[#6](-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#1])-[#6](=[#8])-[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-c:2:c:c:c(-[#6](-[#1])-[#1])c:c:2",
    "[#6](-[#1])(-[#1])-c1nnnn1-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#8]-[#6](-[#1])(-[#1])-[#1])-[#1])-[#1]",
    "[#6]-2(=[#7]-c1c(c(nn1-[#6](-[#6]-2(-[#1])-[#1])=[#8])-[#7](-[#1])-[#1])-[#7](-[#1])-[#1])-[#6]", "[#6](-[#6]:[#6])(-[#6]:[#6])(-[#6]:[#6])-[#16]-[#6]:[#6]-[#6](=[#8])-[#8]-[#1]",
    "[#8]=[#6](-c:1:c(:c(:n:c(:c:1-[#1])-[#8]-[#6](-[#1])(-[#1])-[#1])-[#8]-[#6](-[#1])(-[#1])-[#1])-[#1])-[#7](-[#1])-[#6](-[#1])(-[#6](-[#1])-[#1])-[#6](-[#1])-[#1]",
    "[#7]-1=[#6](-[#7](-[#6](-[#6](-[#6]-1(-[#1])-[#6]:[#6])(-[#1])-[#1])=[#8])-[#1])-[#7]-[#1]",
    "[#6]-1(=[#6](-[#6](-[#6](-[#6](-[#6]-1(-[#1])-[#1])(-[#1])-[#6](=[#8])-[#6])(-[#1])-[#6](=[#8])-[#8]-[#1])(-[#1])-[#1])-[#6]:[#6])-[#6]:[#6]",
    paste0("[#6](-[#1])(-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[Cl])-[#1])-[#1])(-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[Cl])-[#1])-[#1])-[#8]-[#6](-[#1])(-[#1])-[#6](-[#1])(",
           "-[#1])-[#6](-[#1])(-[#1])-c3nc(c(n3-[#6](-[#1])(-[#1])-[#1])-[#1])-[#1]"),
    "n:1:c(:c(:c(:c(:c:1-[#1])-[#7](-[#1])-[#1])-[#1])-[#1])-[#7](-[#1])-[#6]:[#6]",
    "[#7](-[#1])(-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#8]-[#1])-[#6]-2=[#6](-[#8]-[#6](-[#7]=[#7]-2)=[#7])-[#7](-[#1])-[#1]",
    "[#7](-[#1])(-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#1]",
    "c:1:c:c-3:c(:c:c:1)-c:2:c:c:c(:c:c:2-[#6]-3=[#6](-[#1])-[#6])-[#7](-[#1])-[#1]",
    "c:1:c:c-2:c(:c:c:1)-[#7](-[#6](-[#8]-[#6]-2)(-[#6](=[#8])-[#8]-[#1])-[#6](-[#1])-[#1])-[#6](=[#8])-[#6](-[#1])-[#1]",
    "n:1:c(:c(:c(:c(:c:1-[#7](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#6](-[#1])-[#1])-[#7](-[#1])-[#1]",
    "[#7](-[#1])(-c:1:c:c:c:c:c:1)-[#6](-[#6])(-[#6])-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#1]",
    paste0("[#7](-[#1])(-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#8]-[#6](-[#1])(-[#1])-[#1])-[#8]-[#6]-[#1])-[#1])-[#6](=[#8])-[#7](-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1]",
           ")(-[#1])-[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])(-[#1])-[#1])-[#6]:[#6]"),
    "c:1-2:c:c-3:c(:c:c:1-[#8]-[#6]-[#8]-2)-[#6]-[#6]-3", "c:1(:c(:c(:c(:c(:c:1-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#6](=[#8])-[#8]-[#1])-[#7](-[#1])-[#6]:[#6]",
    "c:1(:c:4:c(:n:c(:c:1-[#6](-[#1])(-[#1])-[#7]-3-c:2:c(:c(:c(:c(:c:2-[#6](-[#1])(-[#1])-[#6]-3(-[#1])-[#1])-[#1])-[#1])-[#1])-[#1])-[#1]):c(:c(:c(:c:4-[#1])-[#1])-[#1])-[#1])-[#1]",
    "c:1:c(:c2:c(:c:c:1)c(c(n2-[#1])-[#6]:[#6])-[#6]:[#6])-[#6](=[#8])-[#8]-[#1]",
    "[#6]:[#6]-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7](-[#1])-[#6](=[#16])-[#7](-[#1])-c:1:c(:c(:c(:c(:c:1-[F,Cl,Br,I])-[#1])-[#6](-[#1])-[#1])-[#1])-[#1]",
    "n:1:c3:c(:c:c2:c:1nc(s2)-[#7])sc(n3)-[#7]", "[#7]=[#6]-1-[#16]-[#6](=[#7])-[#7]=[#6]-1", "c:1:c(:n:c:c:c:1)-[#6](=[#16])-[#7](-[#1])-c:2:c(:c:c:c:c:2)-[#8]-[#6](-[#1])-[#1]",
    "c:1-2:c(:c(:c(:c(:c:1-[#6](-c:3:c(-[#16]-[#6]-2(-[#1])-[#1]):c(:c(-[#1]):c(:c:3-[#1])-[#1])-[#1])-[#8]-[#6]:[#6])-[#1])-[#1])-[#1])-[#1]",
    "[#6](-[#1])(-[#1])(-[#1])-c:1:c(:c(:c(:c(:n:1)-[#7](-[#1])-[#16](-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#8]-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])-[#1])-[#1])-[#1])(=[#8])=[#8])-[#1])-[#1])-[#1]",
    "[#6](=[#8])(-[#7]-1-[#6]-[#6]-[#16]-[#6]-[#6]-1)-c:2:c(:c(:c(:c(:c:2-[#16]-[#6](-[#1])-[#1])-[#1])-[#1])-[#1])-[#1]",
    "c:1:c:c:3:c:2:c(:c:1)-[#6](-[#6]=[#6](-c:2:c:c:c:3)-[#8]-[#6](-[#1])-[#1])=[#8]", "c:1-3:c:2:c(:c(:c:c:1)-[#7]):c:c:c:c:2-[#6](-[#6]=[#6]-3-[#6](-[F])(-[F])-[F])=[#8]",
    "c:1:c:c:c:c:2:c:1:c:c:3:c(:n:2):n:c:4:c(:c:3-[#7]):c:c:c:c:4", "c:1:c-3:c(:c:c:c:1)-[#6]-2=[#7]-[!#1]=[#6]-[#6]-[#6]-2-[#6]-3=[#8]",
    paste0("c:1-3:c(:c(:c(:c(:c:1-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#6](=[#7]-[#7](-[#1])-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#6](=[#8])-[#8]-[#1])-[#1])-[#1",
           "])-c:4:c-3:c(:c(:c(:c:4-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#1]"),
    "c:1(:c(:c(:c(:c(:c:1-[#1])-[#1])-[#7](-[#1])-[#1])-[#1])-[#1])-[#16](=[#8])(=[#8])-[#7](-[#1])-c:2:n:n:c(:c(:c:2-[#1])-[#1])-[#1]",
    "c2(c(-[#1])n(-[#6](-[#1])-[#1])c:3:c(:c(:c:1n(c(c(c:1:c2:3)-[#1])-[#1])-[#6](-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1]",
    "c1(c-2c(c(n1-[#6](-[#8])=[#8])-[#6](-[#1])-[#1])-[#16]-[#6](-[#1])(-[#1])-[#16]-2)-[#6](-[#1])-[#1]", "s1ccnc1-c2c(n(nc2-[#1])-[#1])-[#7](-[#1])-[#1]",
    "c1(c(c(c(n1-[#1])-c:2:c(:c(:c(:c(:c:2-[#1])-[#1])-[#1])-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#6](=[#8])-[#8]-[#1]",
    "c:1:2(:c(:c(:c(:o:1)-[#6])-[#1])-[#1])-[#6](=[#8])-[#7](-[#1])-[#6]:[#6](-[#1]):[#6](-[#1]):[#6](-[#1]):[#6](-[#1]):[#6]:2-[#6](=[#8])-[#8]-[#1]",
    "[!#1]:[#6]-[#6](=[#16])-[#7](-[#1])-[#7](-[#1])-[#6]:[!#1]", "[#6]-1(=[#8])-[#6](-[#6](-[#6]#[#7])=[#6](-[#1])-[#7])-[#6](-[#7])-[#6]=[#6]-1",
    "c2(c-1n(-[#6](-[#6]=[#6]-[#7]-1)=[#8])nc2-c3cccn3)-[#6]#[#7]", "[#8]=[#6]-1-[#6](=[#7]-[#7]-[#6]-[#6]-1)-[#6]#[#7]", "c:2(:c:1:c:c:c:c:c:1:n:n:c:2)-[#6](-[#6]:[#6])-[#6]#[#7]",
    "c:1:c:c-2:c(:c:c:1)-[#6]=[#6]-[#6](-[#7]-2-[#6](=[#8])-[#7](-[#1])-c:3:c:c(:c(:c:c:3)-[#8]-[#6](-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])(-[#6](-[#1])-[#1])-[#6](-[#1])-[#1]",
    "c:2:c:c:1:n:c(:c(:n:c:1:c:c:2)-c:3:c:c:c:c:c:3)-c:4:c:c:c:c:c:4-[#8]-[#1]", "[#6](-[#1])(-[#1])-[#6](-[#8]-[#1])=[#6](-[#6](=[#8])-[#6](-[#1])-[#1])-[#6](-[#1])-[#6]#[#6]",
    "c:1:c:4:c(:c:c2:c:1nc(n2-[#1])-[#6]-[#8]-[#6](=[#8])-c:3:c:c(:c:c(:c:3)-[#7](-[#1])-[#1])-[#7](-[#1])-[#1]):c:c:c:c:4", "c:2(:c:1:c:c:c:c-3:c:1:c(:c:c:2)-[#6]=[#6]-[#6]-3=[#7])-[#7]",
    "c:2(:c:1:c:c:c:c:c:1:c-3:c(:c:2)-[#6](-c:4:c:c:c:c:c-3:4)=[#8])-[#8]-[#1]", "[#6]-2(-[#6]=[#7]-c:1:c:c(:c:c:c:1-[#8]-2)-[Cl])=[#8]",
    "[#6]-1=[#6]-[#7](-[#6](-c:2:c-1:c:c:c:c:2)(-[#6]#[#7])-[#6](=[#16])-[#16])-[#6]=[#8]",
    "c2(nc:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])n2-[#6])-[#7](-[#1])-[#6](-[#7](-[#1])-c:3:c(:c:c:c:c:3-[#1])-[#1])=[#8]",
    "[#7](-[#1])(-[#6]:[#6])-c:1:c(-[#6](=[#8])-[#8]-[#1]):c:c:c(:n:1)-[#6]:[#6]", "c:1-3:c(:c:c:c:c:1)-[#16]-[#6](=[#7]-[#7]=[#6]-2-[#6]=[#6]-[#6]=[#6]-[#6]=[#6]-2)-[#7]-3-[#6](-[#1])-[#1]",
    "c:1-2:c(:c(:c(:c(:c:1-[#1])-[#8]-[#6](-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#6](=[#6](-[#6])-[#16]-[#6]-2(-[#1])-[#1])-[#6]",
    "c:12:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])c(c(-[#6]:[#6])n2-!@[#6]:[#6])-[#6](-[#1])-[#1]", "[#7](-[#1])(-[#1])-c:1:c:c:c(:c:c:1-[#8]-[#1])-[#16](=[#8])(=[#8])-[#8]-[#1]",
    "s:1:c:c:c(:c:1-[#1])-c:2:c:s:c(:n:2)-[#7](-[#1])-[#1]", "c1c(-[#7](-[#1])-[#1])nnc1-c2c(-[#6](-[#1])-[#1])oc(c2-[#1])-[#1]", "n1nscc1-c2nc(no2)-[#6]:[#6]",
    "c:1(:c:c-3:c(:c:c:1)-[#7]-[#6]-4-c:2:c:c:c:c:c:2-[#6]-[#6]-3-4)-[#6;X4]",
    "c:1-2:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#6](=[#6](-[#1])-[#6]-3-[#6](-[#6]#[#7])-[#6](-[#1])(-[#1])-[#6](-[#1])-[#7]-2-3)-[#1]",
    "c:2-3:c(:c:c:1:c:c:c:c:c:1:c:2)-[#7](-[#6](-[#1])-[#1])-[#6](=[#8])-[#6](=[#7]-3)-[#6]:[#6]-[#7](-[#1])-[#6](-[#1])-[#1]", "[#6](-[#8]-[#1]):[#6]-[#6](=[#8])-[#6](-[#1])=[#6](-[#6])-[#6]",
    "c:1:2:c(:c(:c(:c(:c:1-[#1])-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1]):c(:c(-[#1]):n:2-[#1])-[#16](=[#8])=[#8]",
    "c:1:2:c(:c(:c(:c(:c:1-[#1])-[#1])-[#7](-[#1])-[#1])-[#1]):c(:c(-[#1]):n:2-[#6](-[#1])-[#1])-[#1]",
    "[#16;X2]-1-[#6]=[#6](-[#6]#[#7])-[#6](-[#6])(-[#6]=[#8])-[#6](=[#6]-1-[#7](-[#1])-[#1])-[$([#6]=[#8]),$([#6]#[#7])]",
    "[#7]-2-[#6]=[#6](-[#6]=[#8])-[#6](-c:1:c:c:c(:c:c:1)-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#6]~3=[#6]-2~[#7]~[#6](~[#16])~[#7]~[#6]~3~[#7]",
    "c:1:c(:c:c:c:c:1)-[#6](=[#8])-[#7](-[#1])-c:2:c(:c:c:c:c:2)-[#6](=[#8])-[#7](-[#1])-[#7](-[#1])-c:3:n:c:c:s:3",
    "c:1:c:2:c(:c:c:c:1):c(:c:3:c(:c:2):c:c:c:c:3)-[#6]=[#7]-[#7](-[#1])-c:4:c:c:c:c:c:4",
    "c:1:c(:c:c:c:c:1)-[#6](-[#1])-[#7]-[#6](=[#8])-[#6](-[#7](-[#1])-[#6](-[#1])-[#1])=[#6](-[#1])-[#6](=[#8])-c:2:c:c:c(:c:c:2)-[#8]-[#6](-[#1])-[#1]",
    "s:1:c(:c(-[#1]):c(:c:1-[#6]-3=[#7]-c:2:c:c:c:c:c:2-[#6](=[#7]-[#7]-3-[#1])-c:4:c:c:n:c:c:4)-[#1])-[#1]",
    "o:1:c(:c(-[#1]):c(:c:1-[#6](-[#1])(-[#1])-[#7](-[#1])-[#6](=[#16])-[#7](-[#6]-[#1])-[#6](-[#1])(-[#1])-c:2:c:c:c:c:c:2)-[#1])-[#1]",
    "c:1:c(:c:c:c:c:1)-[#7](-[#6]-[#1])-[#6](-[#1])-[#6](-[#1])-[#6](-[#1])-[#7](-[#1])-[#6](=[#8])-[#6]-2=[#6](-[#8]-[#6](-[#6](=[#6]-2-[#6](-[#1])-[#1])-[#1])=[#8])-[#6](-[#1])-[#1]",
    "c2-3:c:c:c:1:c:c:c:c:c:1:c2-[#6](-[#1])-[#6;X4]-[#7]-[#6]-3=[#6](-[#1])-[#6](=[#8])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1]",
    "c:1:c(:c:c:c:c:1)-[#6]-4=[#7]-[#7]:2:[#6](:[#7+]:c:3:c:2:c:c:c:c:3)-[#16]-[#6;X4]-4",
    "[#6]-2(=[#8])-[#6](=[#6](-[#6](-[#1])-[#1])-[#7](-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])-[#1])-[#7]=[#6](-c:1:c:c:c:c:c:1)-[#8]-2",
    "c:1:c(:c:c:c:c:1)-[#7]-2-[#6](=[#8])-[#6](=[#6](-[#1])-[#6]-2=[#8])-[#16]-c:3:c:c:c:c:c:3", "[#7]-1(-[#1])-[#7]=[#6](-[#7]-[#1])-[#16]-[#6](=[#6]-1-[#6]:[#6])-[#6]:[#6]",
    "c:1(:c(:c-3:c(:c(:c:1-[#7](-[#1])-[#6](=[#16])-[#7](-[#1])-[#6](-[#1])-c:2:c(:c(:c(:o:2)-[#6]-[#1])-[#1])-[#1])-[#1])-[#8]-[#6](-[#8]-3)(-[#1])-[#1])-[#1])-[#1]",
    "c:1(:c(:c(:c(:c(:c:1-[#7](-[#1])-[#6](=[#16])-[#7](-[#1])-c:2:c:c:c:c:c:2)-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#1])-[#1]",
    "[#8]=[#6]-!@n:1:c:c:c-2:c:1-[#7](-[#1])-[#6](=[#16])-[#7]-2-[#1]",
    "[#6](-[F])(-[F])-[#6](=[#8])-[#7](-[#1])-c:1:c(-[#1]):n(-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#8]-[#6](-[#1])(-[#1])-[#6]:[#6]):n:c:1-[#1]",
    "[#7]-2=[#7]-[#6]:1:[#7]:[!#6&!#1]:[#7]:[#6]:1-[#7]=[#7]-[#6]:[#6]-2", "[#6]-2(-[#1])(-[#8]-[#1])-[#6]:1:[#7]:[!#6&!#1]:[#7]:[#6]:1-[#6](-[#1])(-[#8]-[#1])-[#6]=[#6]-2",
    "[#6]-1(-[#6](-[#1])(-[#1])-[#6]-1(-[#1])-[#1])(-[#6](=[#8])-[#7](-[#1])-c:2:c:c:c(:c:c:2)-[#8]-[#6](-[#1])(-[#1])-[#8])-[#16](=[#8])(=[#8])-[#6]:[#6]",
    "[#6]-1:[#6]-[#6](=[#8])-[#6]=[#6]-1-[#7]=[#6](-[#1])-[#7](-[#6;X4])-[#6;X4]", "c:1:c:c(:c:c-2:c:1-[#6](=[#6](-[#1])-[#6](=[#8])-[#8]-2)-c:3:c:c:c:c:c:3)-[#8]-[#6](-[#1])(-[#1])-[#6]:[#8]:[#6]",
    paste0("c:1:c(:o:c(:c:1-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#7]-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#8]-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-",
           "[#8]-c:2:c:c-3:c(:c:c:2)-[#8]-[#6](-[#8]-3)(-[#1])-[#1]"),
    "[#7]-4(-c:1:c:c:c:c:c:1)-[#6](=[#8])-[#16]-[#6](-[#1])(-[#7](-[#1])-c:2:c:c:c:c:3:c:c:c:c:c:2:3)-[#6]-4=[#8]",
    "[#7]-3(-[#6](=[#8])-c:1:c:c:c:c:c:1)-[#6](=[#7]-c:2:c:c:c:c:c:2)-[#16]-[#6](-[#1])(-[#1])-[#6]-3=[#8]", "[#7]-2(-c:1:c:c:c:c:c:1)-[#6](=[#8])-[#16]-[#6](-[#1])(-[#1])-[#6]-2=[#16]",
    "[#7]-1(-[#6](-[#1])-[#1])-[#6](=[#16])-[#7](-[#6]:[#6])-[#6](=[#7]-[#6]:[#6])-[#6]-1=[#7]-[#6]:[#6]", "[#16]-1-[#6](=[#7]-[#7]-[#1])-[#16]-[#6](=[#7]-[#6]:[#6])-[#6]-1=[#7]-[#6]:[#6]",
    "[#6]-2(=[#8])-[#6](=[#6](-[#1])-c:1:c(:c:c:c(:c:1)-[F,Cl,Br,I])-[#8]-[#6](-[#1])-[#1])-[#7]=[#6](-[#16]-[#6](-[#1])-[#1])-[#16]-2",
    "[#6](-[#1])(-[#1])-[#16]-[#6](=[#16])-[#7](-[#1])-[#6](-[#1])(-[#1])-[#6]:[#6]",
    paste0("c:1(:c(:c(:c(:c(:c:1-[#1])-[#1])-[#6](-[#1])-[#1])-[#7](-[#1])-[#6](=[#8])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6]:[#6])-[#1])-[#7](-[#1])-[#6](=[#",
           "8])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6]:[#6]"),
    "c:1(:c(:c:c(:c:c:1-[#6])-[Br])-[#6])-[#7](-[#1])-[#6](=[#8])-[#7](-[#1])-[#6]-[#6]-[#6]",
    "c:1-2:c(:c:c:c(:c:1-[#8]-[#6](-[#1])(-[#1])-[#7](-[#6]:[#6]-[#8]-[#6](-[#1])-[#1])-[#6]-2(-[#1])-[#1])-[#1])-[#1]",
    "c:1-2:c(:c(:c(:c(:c:1-[#8]-[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-[#6]-2(-[#1])-[#1])-[#1])-[#8])-[#8])-[#1]",
    "[#7](-[#1])(-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#6](-[#1])(-[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1]",
    "n:1:2:c:c:c(:c:c:1:c:c(:c:2-[#6](=[#8])-[#6]:[#6])-[#6]:[#6])-[#6](~[#8])~[#8]",
    paste0("c:1(:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#6](=[#6](-[#1])-[#1])-[#6](-[#1])-[#1])-[#1])-[#6](-[#6;X4])(-[#6;X4])-[#7](-[#1])-[#6](=[#8])-[#7](-[#6](-[#",
           "1])(-[#1])-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])-[#6](-[#1])(-[#1])-[#6]:[#6]"),
    "[#6]-3(-[#1])(-n:1:c(:n:c(:c:1-[#1])-[#1])-[#1])-c:2:c(:c(:c(:c(:c:2-[#1])-[Br])-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-c:4:c-3:c(:c(:c(:c:4-[#1])-[#1])-[#1])-[#1]",
    "[#6](=[#6](-[#1])-[#6](-[#1])(-[#1])-n:1:c(:n:c(:c:1-[#1])-[#1])-[#1])(-[#6]:[#6])-[#6]:[#6]", "c:1(:n:c(:c(-[#1]):s:1)-c:2:c:c:n:c:c:2)-[#7](-[#1])-[#6]:[#6]-[#6](-[#1])-[#1]",
    "c:1(:n:c(:c(-[#1]):s:1)-c:2:c:c:c:c:c:2)-[#6](-[#1])(-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#7]-[#6](-[#1])(-[#1])-c:3:c:c:c:n:3-[#1]",
    "n:1(-[#1]):c(:c(-[#6](-[#1])-[#1]):c(:c:1-[#6](-[#1])(-[#1])-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])-[#1])-[#6](=[#8])-[#8]-[#6](-[#1])-[#1]",
    "c:2(:n:c:1:c(:c(:c:c(:c:1-[#1])-[F,Cl,Br,I])-[#1]):n:2-[#1])-[#16]-[#6](-[#1])(-[#1])-[#6](=[#8])-[#7](-[#1])-[#6]:[#6]",
    "c:1(:c(:c-2:c(:c(:c:1-[#8]-[#6](-[#1])-[#1])-[#1])-[#6]=[#6]-[#6](-[#1])-[#16]-2)-[#1])-[#8]-[#6](-[#1])-[#1]",
    "[#7]-1(-[#1])-[#6](=[#16])-[#6](-[#1])(-[#6]#[#7])-[#6](-[#1])(-[#6]:[#6])-[#6](=[#6]-1-[#6]:[#6])-[#1]",
    "n:1:c(:c(:c(:c(:c:1-[#16;X2]-c:2:c:c:c:c:c:2-[#7](-[#1])-[#1])-[#6]#[#7])-c:3:c:c:c:c:c:3)-[#6]#[#7])-[#7](-[#1])-[#1]",
    "[#7]-2(-c:1:c:c:c(:c:c:1)-[#8]-[#6](-[#1])-[#1])-[#6](=[#8])-[#6](=[#6]-[#6](=[#7]-2)-n:3:c:n:c:c:3)-[#6]#[#7]",
    "o:1:c(:c:c:2:c:1:c(:c(:c(:c:2-[#1])-[#8]-[#6](-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#6](~[#8])~[#8]", "[#6]#[#6]-[#6](=[#8])-[#6]#[#6]",
    "c:2(:c:1:c(:c(:c(:c(:c:1:c(:c(:c:2-[#8]-[#1])-[#6]=[#8])-[#1])-[#1])-[#1])-[#1])-[#1])-[#7](-[#1])-[#1]",
    "c:1(:c(:c(:c(:o:1)-[$([#1]),$([#6](-[#1])-[#1])])-[#1])-[#1])-[#6](=[#8])-[#7](-[#1])-[#7]=[#6](-[$([#1]),$([#6](-[#1])-[#1])])-c:2:c:c:c:c(:c:2)-[*]-[*]-[*]-c:3:c:c:c:o:3",
    "[#16](=[#8])(=[#8])-[#7](-[#1])-c:1:c(:c(:c(:s:1)-[#6]-[#1])-[#6]-[#1])-[#6](=[#8])-[#7]-[#1]",
    "[#6](-[#1])(-[#1])-[#8]-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#7](-[#1])-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#8]-[#1])-[#6](-[#1])-[#1]",
    "[#6](-[#1])(-[#1])-[#8]-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#7](-[#1])-[#6](-[#1])(-[#6]=[#8])-[#16]", "n1nnnc2cccc12",
    "c:1-2:c(-[#1]):s:c(:c:1-[#6](=[#8])-[#7]-[#7]=[#6]-2-[#7](-[#1])-[#1])-[#6]=[#8]", "c:1-3:c(:c:2:c(:c:c:1-[Br]):o:c:c:2)-[#6](=[#6]-[#6](=[#8])-[#8]-3)-[#1]",
    "c:1-3:c(:c:c:c:c:1)-[#6](=[#6](-[#6](=[#8])-[#7](-[#1])-c:2:n:o:c:c:2-[Br])-[#6](=[#8])-[#8]-3)-[#1]",
    "c:1-2:c(:c:c(:c:c:1-[F,Cl,Br,I])-[F,Cl,Br,I])-[#6](=[#6](-[#6](=[#8])-[#7](-[#1])-[#1])-[#6](=[#7]-[#1])-[#8]-2)-[#1]",
    "c:1-3:c(:c:c:c:c:1)-[#6](=[#6](-[#6](=[#8])-[#7](-[#1])-c:2:n:c(:c:s:2)-[#6]:[#16]:[#6]-[#1])-[#6](=[#8])-[#8]-3)-[#1]",
    "[#6](-[#1])(-[#1])-[#16;X2]-c:2:n:n:c:1-[#6]:[#6]-[#7]=[#6]-[#8]-c:1:n:2",
    "[#16](=[#8])(=[#8])(-c:1:c:n(-[#6](-[#1])-[#1]):c:n:1)-[#7](-[#1])-c:2:c:n(:n:c:2)-[#6](-[#1])(-[#1])-[#6]:[#6]-[#8]-[#6](-[#1])-[#1]",
    "c:1-2:c(:c(:c(:c(:c:1-[#8]-[#6](-[#1])(-[#1])-[#8]-2)-[#6](-[#1])(-[#1])-[#7]-3-[#6](-[#1])(-[#1])-[#6](-[#1])(-[#1])-[#6]:[#6]-3)-[#1])-[#1])-[#1]",
    "[#6](-[#1])(-[#1])-[#8]-[#6]:[#6]-[#6](-[#1])(-[#1])-[#7](-[#1])-c:2:c(:c(:c:1:n(:c(:n:c:1:c:2-[#1])-[#1])-[#6]-[#1])-[#1])-[#1]",
    "[#7]-4(-c:1:c:c:c:c:c:1)-[#6](=[#7+](-c:2:c:c:c:c:c:2)-[#6](=[#7]-c:3:c:c:c:c:c:3)-[#7]-4)-[#1]", "[#6](-[#1])(-[#1])-[#7](-[#6](-[#1])-[#1])-c:2:c:c:c:1:s:c(:n:c:1:c:2)-[#16]-[#6](-[#1])-[#1]",
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
    "[#16]=[#6]-[#6](-[#6](-[#1])-[#1])=[#6](-[#6](-[#1])-[#1])-[#7](-[#6](-[#1])-[#1])-[#6](-[#1])-[#1]", "[#6]-1:[#6]-[#8]-[#6]-2-[#6](-[#1])(-[#1])-[#6](=[#8])-[#8]-[#6]-1-2",
    "[#8]-[#6](=[#8])-[#6](-[#1])(-[#1])-[#16;X2]-[#6](=[#7]-[#6]#[#7])-[#7](-[#1])-c:1:c:c:c:c:c:1", "[#8]=[#6]-[#6]-1=[#6](-[#16]-[#6](=[#6](-[#1])-[#6])-[#16]-1)-[#6]=[#8]",
    "[#8]=[#6]-1-[#7]-[#7]-[#6](=[#7]-[#6]-1=[#6]-[#1])-[!#1]:[!#1]", "[#8]=[#6]-[#6](-[#1])=[#6](-[#6]#[#7])-[#6]",
    "[#8](-[#1])-[#6](=[#8])-c:1:c(:c(:c(:c(:c:1-[#8]-[#1])-[#1])-c:2:c(-[#1]):c(:c(:o:2)-[#6](-[#1])=[#6](-[#6]#[#7])-c:3:n:c:c:n:3)-[#1])-[#1])-[#1]",
    "c:1:c(:c:c:c:c:1)-[#7](-c:2:c:c:c:c:c:2)-[#7]=[#6](-[#1])-[#6]:3:[#6](:[#6](:[#6](:[!#1]:3)-c:4:c:c:c:c(:c:4)-[#6](=[#8])-[#8]-[#1])-[#1])-[#1]",
    "[#7](-[#1])(-[#1])-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-c:2:c(-[#1]):c(:c(-[#6](-[#1])-[#1]):o:2)-[#6]=[#8])-[#1])-[#1]",
    "[#8](-[#1])-[#6](=[#8])-c:1:c:c:c(:c:c:1)-[#7]-[#7]=[#6](-[#1])-[#6]:2:[#6](:[#6](:[#6](:[!#1]:2)-c:3:c:c:c:c:c:3)-[#1])-[#1]",
    "[#8](-[#1])-[#6](=[#8])-c:1:c:c:c:c(:c:1)-[#6]:[!#1]:[#6]-[#6]=[#7]-[#7](-[#1])-[#6](=[#8])-[#6](-[#1])(-[#1])-[#8]",
    "[#8](-[#1])-[#6]:1:[#6](:[#6]:[!#1]:[#6](:[#7]:1)-[#7](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6](=[#8])-[#8]", "[#6]-1(=[!#6&!#1])-[#6](-[#7]=[#6]-[#16]-1)=[#8]",
    "n2(-c:1:c:c:c:c:c:1)c(c(-[#1])c(c2-[#6]=[#7]-[#8]-[#1])-[#1])-[#1]", "n2(-[#6](-[#1])-c:1:c(:c(:c:c(:c:1-[#1])-[#1])-[#1])-[#1])c(c(-[#1])c(c2-[#6]-[#1])-[#1])-[#6]-[#1]",
    "n1(-[#6](-[#1])-[#1])c(c(-[#6](=[#8])-[#6])c(c1-[#6]:[#6])-[#6])-[#6](-[#1])-[#1]", "n1(-[#6])c(c(-[#1])c(c1-[#6](-[#1])=[#6](-[#6]#[#7])-c:2:n:c:c:s:2)-[#1])-[#1]",
    "n3(-c:1:c:c:c:c:c:1-[#7](-[#1])-[#16](=[#8])(=[#8])-c:2:c:c:c:s:2)c(c(-[#1])c(c3-[#1])-[#1])-[#1]",
    "n2(-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#6](=[#8])-[#7](-[#1])-[#6](-[#1])(-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#8]-[#6]:[#6])c(c(-[#1])c(c2-[#1])-[#1])-[#1]",
    "c:1(:c:c:c:c:c:1)-[#7](-[#1])-[#6](=[#16])-[#7]-[#7](-[#1])-[#6](-[#1])=[#6](-[#1])-[#6]=[#8]",
    "[#6]-1(-[#6](=[#8])-[#6](-[#1])(-[#1])-[#6]-[#6](-[#1])(-[#1])-[#6]-1=[#8])=[#6](-[#7]-[#1])-[#6]=[#8]",
    "[#7](-[#1])(-[#1])-[#6]-1=[#6](-[#6]#[#7])-[#6](-[#1])(-[#6]:[#6])-[#16]-[#6;X4]-[#16]-1",
    "[#6](-[#1])(-[#1])-[#8]-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#1])-[#1])-[#7](-[#1])-c:2:c:c:n:c:3:c(:c:c:c(:c:2:3)-[#8]-[#6](-[#1])-[#1])-[#8]-[#6](-[#1])-[#1]",
    "[#6](-[#1])(-[#1])-[#8]-c:1:c(:c(:c(:c(:c:1-[#1])-[#1])-[#8]-[#6](-[#1])-[#1])-[#1])-[#7](-[#1])-c:2:n:c(:c:s:2)-c:3:c:c:c(:c:c:3)-[#8]-[#6](-[#1])-[#1]",
    "[#6]~1~3~[#7](-[#6]:[#6])~[#6]~[#6]~[#6]~[#6]~1~[#6]~2~[#7]~[#6]~[#6]~[#6]~[#7+]~2~[#7]~3", "[#7]-3(-c:2:c:1:c:c:c:c:c:1:c:c:c:2)-[#7]=[#6](-[#6](-[#1])-[#1])-[#6](-[#1])(-[#1])-[#6]-3=[#8]",
    "C1(C)(C)SC(C(=O)O)=C(C(=O)O)S1", "c([OH])c([OH])c([OH])", "c([OH])c([OH])cc([OH])", "C(=O)OC(=O)", "[S,C](=[O,S])[F,Br,Cl,I]", "c1c2cc4ccccc4nc2ccc1",
    "P(=S)([OH1,O$(O[#6])])([OH1,O$(O[#6])])[S,O]", "C(=O)-C#N", "[*;R]=[*;R]=[*;R]", "[#6][C!H0]=O", "COS(=O)(=O)C(F)(F)F", "[Br,Cl,I][CX4;CH,CH2]",
    "[C;H1$(C([#6;!$(C=O)])),H0$(C([#6;!$(C=O)])[#6;!$(C=O)])]=[CH1]!@N([#6;!$(C(=O))])[#6;!$(C(=O))]", "*=C=*", "[C;!$(C[N])](=O)!@[C;h1,h2;H1,H2][F,Cl,Br,I]", "c1nnnn1C=O",
    "c1(N)ccc(C(=O)NC3(=O))c(c3ccc2)c21", "NC#N", "[#6]C(=O)OC(=O)[#6]", "O=*N=[N+]=[N-]", "cN=N=N", "N=[N+]([O-])C", "[N;R0]=[N;R0]CC=O", "c1ccccc1[N!r]=[N!r]c2ccccc2", "[N;R0]=[N;R0]C#N",
    "C(=O)CC[N+,n+]", "cC[N+,NX4]", "C(=O)C[N+,n+,NX4,nX4]", "[C;H2$(CF),H1$(C(F)F)]!@[CH2][N,O]", "C12C(NC(N1)=O)CSC2", "C(=O)N=N=N", "N=C=N", "C(Cl)(Cl)(Cl)C([O,S])[NX3]", "[Cl]C([C&R0])=N",
    "SC(=[!r])S", "O1CCOCCOCCOCC1", "O1CCOCCOCCOCCOCC1", "O1CCOCCOCCOCCOCCOCC1", "[O;R1][C;R1][C;R1][O;R1][C;R1][C;R1][O;R1]", "N[CH2]C#N", "P(OCC)(OCC)(=O)C#N", "N#CC[OH1]", "P(=O)([OH])OP(=O)[OH]",
    "C#CC#C", "C=[N+]=[N-]", "[N+]#N", "C!@=[CH1]-C!@=[CH1]-[CX3](=O)", "c1c([N+](=O)[O-])c([N+](=O)[O-])ccc1", "c1c([N+](=O)[O-])ccc([N+](=O)[O-])c1", "c1c([N+](=O)[O-])cc([N+](=O)[O-])cc1",
    "[SX2][SX2]", "NC(=S)S", "S1SC=CC1=S", "S1C=CSC1=C", "S[C;!$(C=*)]S", "C=!@CC#C", "C1[O,S,N]C1", "C(=O)Onnn", "c1cccc(NC(=NC(=[N,S,O])NC(=O)3)C3=N2)c12", "c1cc(O)cc(OC(=CC(=O)C=C3)C3=C2)c12",
    "C(C(CF)F)F", "C(C(F)F)(F)F", "C1(=O)OCC1", "[NH1;!r][CX4][NH1;!r]", "N#CCC#N", "[Cl,Br,I]c1[c,n][c,n][c,n][c,n]n1", "N=NC(S)N", "c1[n+]([#6])ccn1([#6])", "[#6,#8,#16]-[CH1]=[NH1]",
    "[#6][C;R0](=[N;R0,O0])[#6]", "N=C=[S,O]", "[N+]#[C-]", "C=C=O", "P(=S)(S)S", "S1C=CSC1=S", "c1ccccc1C(=O)C=!@CC(=O)!@*", "[NX3,NX4][F,Cl,Br,I]", "c1ccc(n[o,s]n2)c2c1[N+](=O)[O-]",
    "c1c([N+](=O)[O-])cc(n[o,s]n2)c2c1", "N-[N;X2](=O)", "[N&D2](=O)", "N1=C[S,NH1]C(=[C,N,P][C,N,O,P])C1(=O)", "[N+!$(N=O)][O-X1]", "[#6][S!$(S(~[OD1])~[OD1])][N;H0]", "C(O)(O)[OH]",
    "c1c([OH1])c(C(C)(C)C)ccc1", "C1(=O)C=CCSC=C1", "[P,S][Cl,Br,F,I]", "Nc1aaa(N!@=N)aa1", "PCP", "C(=O)Oc1ccc([N+](=O)[O-])cc1", "c1c([F,Cl])c([F,Cl])c([F,Cl])c([F,Cl])c1([F,Cl])",
    "C(=O)Oc1c(F)c(F)c(F)c(F)c1(F)", "[#8]~[#8]", "c12cccc3c1c4c(cc3)cccc4cc2", "C(=O)!@Oc1ccccc1", "[#6]P(=O)(~O)O[#6]", "NP(=O)(N)N", "C=P", "[S,P][F,Cl,Br,I]", "C=!@CC=!@C", "C=CC=CC=CC=C",
    "cC=CC=CC=Cc", "CC#CC#CC", "c1cccc(cc(cccc2)c2c3)c13", "c1cccc(c(cccc2)c2cc3)c13", "*[SX2][SX2][SX2]*", "[#16][F,Cl,Br,I]", "c1c2cccc3c2c4c(cc3)cccc4c1", "c1ccc[o+]c1", "[C;!r](=[O,S])[S;!r]",
    "[C;!r](=[O,S])[CD2;!r][F,Br,Cl]", "[C,c;R]#[C,c;R]", "[S;R0]=[N;R0]", "O=[SX4](=O)OC", "S(=O)(=O)C#N", "COS(=O)O[C,c]", "COS(=O)(=O)[C,c]", "[SX2H0]!@[N]", "C1NNC=NN1", "SC#N",
    "C[O,S;R0][C;R0](=S)", "[!a][SX2;H1]", "P(=S)(-[S;H1,H0$(S(P)C)])(-[O;H1,H0$(O(P)C)])(-N(C)C)", "[N;!r][C;!r](=S)[N;!r]", "*1[O,S]*1", "[#16v3,#16v5]", "C(=O)N(C(=O))OC(=O)",
    "c1cnnn1!@C!@[NH1][#6]", "OS(=O)(=O)(C(F)(F)(F))", "B(c1ccccc1)(c2ccccc2)c3ccccc3", "P(c1aaaaa1)(c1aaaaa1)(c1aaaaa1)", "[Si](c1ccccc1)(c2ccccc2)(c3ccccc3)", "[Cl,Br,I]C=[!O!R]",
    "[#6][CH1]!@=[CH1][S;H1,H0$(S(C)C)](=O)(=O)", "[#6]S(=O)(=O)O", "C1C(=O)C=CC=CC=1", "[#6]C(=!@N[$(OC),$([OH])])[#6]", "[#6]C(=!@NNa)[#6]", "[$(N(~!@[#6])!@O);!$([N+]([O-])=O)]", "C=S", "C=[NH]",
    "c1ccccc1[NH2,NH3+]", "[#6]C(=O)!@OC(=!@[N,O])[#6]", "FC(F)(F)C(=O)N", "[#6]C#[CH]", "C=C=C", "[$(Sc1nnn[nH,n-]1),$(Sc1nn[nH,n-]n1)]", "[#6][O;R0][$(C([#6])[#6]),$([CH][#6]),$([CH2])][O;R0][#6]",
    "[#6]C(=O)!@C!@C(=O)N", "O=[CH][O,N][#6]", "O=C1C=COC=C1", "c1cc2C=CC(=O)Oc2cc1", "s1ccnc1[N!H0]", "O=C1CSCN1", "N1C(=O)CSCC1=O", "O1C=CC=CC=C1", "c1ccccc1!@[CH]=!@[C!H0]", "C=[CH2]", "C1CC=C1",
    "*!@c1c(!@*)c(!@*)c(!@*)c(!@*)c1", "[2#1,3#1,11C,11c,14C,14c,125I,32P,33P,35S]",
    paste0("[Ac,Ag,Am,Ar,As,At,Au,Ba,Be,Bi,Bk,Cd,Ce,Cf,Cm,Cr,Cs,Dy,Er,Eu,Fr,Ga,Gd,Ge,He,Hf,Ho,In,Ir,Kr,La,Lu,Mo,Nb,Nd,Ne,Ni,Np,Os,Pa,Pb,Pd,Pm,Po,Pr,Pt,Pu,Ra,Rb,Re",
           ",Rh,Rn,Ru,Sb,Sc,Se,Sm,Sr,Ta,Tb,Tc,Te,Th,Ti,Tl,Tm,U,V,W,Xe,Y,Yb,Zr]"),
    "[#6;$([#6]~[#3,#11,#12,#13,#19,#20,#26,#27,#28,#29,#30])]", "[n+][O-X1].[n+][O-X1]", "[N+](=O)[O-].[N+](=O)[O-]", "C(=O)OC(=O)", "C(=O)Oc1c(F)c(F)c(F)c(F)c1(F)", "C(=O)Oc1ccc([N+]([O-])=O)cc1",
    "O=*N=[N+]=[N-]", "C(=O)Onnn", "cN=[N+]=[N-]", "[#6,#8,#16]-[CH1]=[NH1]", "S(=O)(=O)C#N", "[N;R0]=[N;R0]C#N", "N#CC[OH]", "N#CC(=O)", "[S,C](=[O,S])[F,Br,Cl,I]", "[Cl]C([C&R0])=N",
    "[P,S][F,Cl,Br,I]", "[C+,Cl+,I+,P+,S+]", "[!#6;!#7;!#8;!#16;!#1;!#3;!#9;!#11;!#12;!#15;!#17;!#19;!#20;!#30;!#35]", "C(=O)N(C(=O))OC(=O)", "C(=O)CC[N+,n+]", "cC[N+]", "C=P", "P(=S)(S)S",
    "P(OCC)(OCC)(=O)C#N", "COS(=O)(=O)[C,c]", "a-S(=O)(=O)-O[$([a&!#6]),$(c[a&!#6]),$(cc[a&!#6]),$(ccc[a&!#6]),$(cccc[a&!#6]),$(ccccc[a&!#6])]", "COS(=O)O[C,c]", "OS(=O)(=O)C(F)(F)F",
    "[C,S,P](=O)[OH].[C,S,P](=O)[OH].[C,S,P](=O)[OH].[C,S,P](=O)[OH]", "[OH]-S(=O)(=O)-*", "[SH]", "[OH1]-C(-c1ccccc1)c2ccccc2", "[OH1]c1ccc([OH1])cc1", "c([OH])c([OH])c([OH])",
    "c([OH])c([OH])cc([OH])", "*=C=*", "N=N=N", "[N;R0]=[N;R0]CC=O", "N=NC(=S)N", "N=N", "[#6]-[CH1]=O", "[#6]-O[CH1](-[#6])[OH1]", "[#6]-O[CH1](-[#6])O-[#6]", "[#6]-C(=O)-[#6]", "[#6]-C(=O)O-[#6]",
    "[#6,#8,#16]-C(=[NH1])[#6,#8,#16]", "C=[NH]", "CC(=S)C", "C[O,S;R0][C;R0](=S)", "COC(=S)C", "CC(=S)N", "NC(=S)N", "[N&D2](=O)", "[CD2;R0][CD2;R0][CD2;R0][CD2;R0][CD2;R0][CD2;R0]",
    "[N,C,S,O]-&!@[N,C,S,O]-&!@[N,C,S,O]-&!@[N,C,S,O]-&!@[N,C,S,O]-&!@[N,C,S,O]-&!@[N,C,S,O]", "[$([A&D2]),$([A&D1])]!@[A&D2]!@[A&D2]!@[A&D2]!@[A&D2]!@[$([A&D2]),$([A&D1])]", "C=CC=CC=CC=C",
    "acC=&!@Cca", "[N+]#[C-]", "SC#N", "N[CH2]C#N", "c([N+](=O)[O-])", "a-[N+](=O)[O-].a-[N+](=O)[O-]", "O=[N+](-[O-])-caac-[$(N(C)C),$([NH]C),$([NH2])]",
    "c1cccc(C(=O)[C,c]([#7])=,:[C,c]([#7])C2(=O))c12", "N=C1[#6]:,=[#6]C(=[C,N])[#6]:,=[#6]1", "*=,:[#6]C([#6]=,:*)[#6]=,:*", "ac-*=&!@*-&!@C(=O)-&!@ca", "c1cccc2C(=O)C(C:,=*)C(=O)c12",
    "c12cccc(C(=O)C(ca)C(=O)3)c2c3ccc1", "NS(=O)(=O)c1cccc([#7])c1", "OCccCO", "c1ccccc1-n2nnnc2[CH2]*", "[Br,Cl,I][CX4,CH,CH2,CH3]", "O=CC(-[F,Cl,Br,I])([F,Cl,Br,I])-[F,Cl,Br,I]", "O=CCC[F,Cl,Br,I]",
    "[F,Cl,Br][c]1:[c,n]:[c,n]:[n]:[c,n]:[c,n]1", "[F,Cl,Br][c]1:[c,n]:[c,n]:[c,n]:[c,n]:[n]1", "*[N,S,O]-&!@[N,S,O][#6]", "OO", "SS", "[#6]-[NH]-[NH]-[#6]", "[N;R0][N;R0]C(=O)",
    "[#6]-[CH1]=C-C(=O)[#6,#7,#8]", "[CH2]=C-C(=O)[#6,#7,#8]", "N#CC(=C)C#N", "[#6,#7]-&!@[#6](=&!@[CH])-&!@C(=O)-&!@[C,N,O,S]", "[#6]-C#CC(=O)[#6,#7,#8]", "[CH1]#CC(=O)-[#6,#7,#8]", "C[N+](=O)[O-]",
    "[O;R1][C;R1][C;R1][O;R1][C;R1][C;R1][O;R1]", "[#6]-O-[N+](=O)[O-]", "O=C-&!@C=O", "*-C(=O)-&!@[NH]-C-&!@C(=O)-&!@[NH]-*", "C[N+](-[O-])(C)C", "a-P(-a)-a", "[OH]-P(=O)(-O)-*", "COP(=O)(-*)O",
    "P(=O)([OH])OP(=O)[OH]", "[#6]OP(=O)(*)O[#6].[#6]OP(=O)(*)O[#6].[#6]OP(=O)(*)O[#6]", "NP(=O)(N)N", "c1(c)c2c(c)cccc2ccc1", "c12:[c,n]:[c,n]:[c,n]:[c,n]:c1[c,n]c3:[c,n]:[c,n]:[c,n]:[c,n]:c3[c,n]2",
    "c12:[c,n]:[c,n]:[c,n]:[c,n]:c1:[c,n]:[c,n]:c3:[c,n]:[c,n]:[c,n]:[c,n]:c23", "a1aaac2ac3acac4aaac(c34)c12", "c12ccccc1C(=O)c3ccccc3C2=O", "c12cccc(C(=O)N(-&!@C)C(=O)3)c2c3ccc1", "C1[O,S,N]C1",
    "C1(=O)OCC1", "N1CCC1=O", "O=C1CCCC(N1)=O", "[#6,#7]-S(=O)(=O)Oc", "[$([o,n]=c1ccc(=[o,n])cc1),$([O,N]=C1C=CC(=[O,N])C=C1),$([O,N]=C1[#6]:,=[#6]C(=[O,N])[#6]:,=[#6]1)]", "O1CCCCC1OC2CCC3CCCCC3C2",
    "O1CCCCC1C2CCCO2", "C12OCCC(O1)CC2", "[OH]c1cc([OH])cc2=[O+]C(=C([OH])Cc21)c3cc([OH])c([OH])cc3", "O=C1NCC2CCCCC21", "[Cl,Br,I][CX4][CX4][$([O,S,N]*),Cl,Br,I]", "[C,S,P](=O)[F,Cl,Br,I]",
    "[F,Cl,Br,I][CX4]C=C", "[Br,Cl,I][C!H0]C=[O,S]", "[$([NX2R0]-[!#7]),$([NX2R0H1])]=[$([NX2R0]-[!#7]),$([NX2R0H1])]", "[Br,Cl,I][CX4]c", "[!#7]~[NX2]~[NX1]", "N=C=[N,O,P,S]",
    "[$([!#1!#6!#7!#8!#9!#15!#16!#17!#35!#53]~[*]),$([!#1!#6!#7!#8!#9!#15!#16!#17!#35!#53;h])]", "[CH2]=[CH][N,O,P,S;R0]", "[NR0]~[NX2]~[OX1]", "[$([NX2]~*),$([NX2H])]~[OX1]",
    "S=[$([PX2]~*),$([PX2H])]", "c1ccc[s,o;X2]c1", "[SX4](~S)(~O)(~O)~*", "[PX3]([#6])([#6])[#6]", "[PX4](~[!O!S])([#6])([#6])[#6]", "[$([CX3H1]-[!#7]),$([CX3H2])]=NO[C,S,P](=O)*",
    "O=C[NX3](C=O)OC(=O)*", "[Nv3X3][Nv3X3!H0]", "[NX1]#C[CX4][OH]", "*[SX3](*)*", "C1(C=CC=CC1=O)=O", "*C([F,Cl,Br,I,$(OS(=O)(=O)*)])=[NX2]*", "*S(=O)(=O)[F,Cl,Br,I]",
    "AA[CH2][F,Cl,Br,I,$(OS(=O)(=O)*)]", "*[C,S](=O)O[C,S](=O)*", "c1nc(ncc1)[Br,I]", "CC(=[OX1,SX1,NH])[$([SX2]-*),$([SX2H])]", "[$(C(-C)(-C)(=O)),$([CH](=O)-C)](=O)CC[Br,I]", "[SX2R0][OX2]",
    "[OX2R0][OX2]", "[CH3][CH2][CH2][CH2][CH2][CH2][CH2][CH2][CH2][CH2]", "[2#1,3#1,13C,14C,15N,125I,23F,22Na,32P,33P,35S,45Ca,57Co,103Ru,141Ce]", "P-P", "S=S", "[Si]~O", "O=[CH]*",
    "C[CR0]=[NR0][*!O]", "C1C[N,S,O]1", "*C(=O)C(=O)*", "[*!$(C[OH])]=C([OH])C(=O)*", "C#CC=O", "[$([C!$(C1(=O)C=CC(=O)C=C1)]C),$([Ch]),$(C[OH0])](=O)C=[C!$(C[Nv3X3,OH])]", "[Br,Cl,I][CX4]C=[C,N,P]",
    "N[F,Cl,Br,I]", "O=S(=O)(*)F", "O=S(*)OF", "[FX2,ClX2,BrX2,IX2,IX3,IX4,IX5]", "C[Mg][F,Cl,Br,I]", "N[PX3](N)N", "NC[F,Cl,Br,I]", "O=C1C=CC(=O)C=C1", "S-S", "Oc1cc(O)cc(O)c1", "Oc1c(O)cc(O)cc1",
    "C=P", "N!@[SX2]", "[#15]~[F,Cl,Br,I,#16]", "[O,S]~[Cl,Br,I]", "a1aaaa2ccc3ccccc3c12", "[#6][C,S](=[O,S])C[F,Cl,Br,I]", "[#6][P,S](~[OX1])(~[OX1])O[C!H0]", "C(F)(F)C(F)(F)C(F)(F)C(F)(F)",
    "[O,S]C#N", "a1aaac2cc3ccccc3cc12", "[O,S]C(=[O,S])[O,S]", "*(C#N)C#N", "*C(=[O,S])[O,S]C(=[O,S])*", "C(=O)C([OH])[OH]", "CC(=S)C", "*[SX2H]", "[C!H0][Br,I]", "CC(=S)O",
    "[*!#7]~[#6]-,=CN(~O)(~O)", "*~@C1C(=O)NC(=O)C(~@*)1", "[CH]1C(=O)NC(=O)[CH]=1", "[N,S]1[C,N,S][C,N,S]1", "S!@C(=!@[O,S])!@[#7]", "[c,n]1[c,n][c,n][c,n][c,n]n(C)1",
    "[F,Cl,Br,I]C-,=;!@C([F,Cl,Br,I])-,=;!@C([F,Cl,Br,I])", "[N,P,Se,S][C!H0]C[Br,Cl,I]", "*C(=O)C(=!@[C!H0])C#N", "N#C[#7!$(N(C#N)=C(N)NC)]",
    "[N,O,S]-@[#6!$(*(~@*)(~@*)~@*)]@[#6!$(*(~@*)(~@*)~@*)]-@[N,O,S]-@[#6!$(*(~@*)(~@*)~@*)]@[#6!$(*(~@*)(~@*)~@*)]-@[O,N]-@[#6!$(*(~@*)(~@*)~@*)]@[#6!$(*(~@*)(~@*)~@*)]-@[N,O,S]-@C",
    "[C!H0]=CS(=O)(=O)*", "[*!$(C=O)]!@N!@[$([OX2])]", "N=!@S", "*(c1ccccc1)(c1ccccc1)(c1ccccc1)", "C(=O)!@N!@C(=O)[#7]", "c1nc(ccc1)[Br,I,Cl,F]", "O~N(=O)-c(:*):*", "[2H]", "[13#6]", "n1c(cccc1)Cl",
    "[NH2D1]-c(:*):*", "[Si,B,Se]", "[!#6]-[CH2]-N1C(=O)CCC(=O)1", "O[CH2][CH2]O-!@[CH2][CH2]O", "[$([CX3R0]([#6])[#6]),$([CX3HR0][#6])]=[$([NX2R0][#6]),$([NX2HR0])]", "[S,P](=O)OC",
    "P(=[O,S])[C,N]([C,N])[C,N]", "[N;!$(N-[C,S]=*)]-,=;!@[N;!$(N-[C,S]=*)]", "N-!@[CX4]-!@N", "N-!@[SX2]-*", "[N,S,O][CH2][CH2]-[F,Cl,Br,I]", "O=[C!H0]", "O=[CX3]-!@[CX3]=O",
    "NC(OC([CH3])([CH3])[CH3])=O", "NC(O[CH2]c1ccccc1)=O", "OC(=O)-!@[NX3]", "O=C-[F,Cl,Br,I]", "[CH2]-[Cl,Br,I]", "O=C-C-[F,Cl,Br,I]", "S(=O)(=O)-[F,Cl,Br,I]", "[ND1]=C-!@[SX2]-[CH2D2]", "N-C(=S)-N",
    "[CH2D1]=[CD2]-!@*", "S=P~*", "S-C#N", "*-[S!H0]", "[Sv4](=O)(-!@[!#1])-!@[!#1]", "N-[F,Cl,Br,I]", "N-C-[F,Cl,Br,I,$(C#N)]", "[$(C#N),$(N(~O)~O),$(C=O),$(S(=O)=O),$(C(F)(F)F),Cl][C!H0]=[C!H0]",
    "[C;!$(C=*)][C!R](=O)[CH2D2]", "C[C!R](=O)[O!R][CH2D2]", "[CH2][CH2][CH2]-!@[CH2][CH2][CH2]", "O=[#6]1[#6]:,=[#6][#6](=O)[#6]:,=[#6]1", "C=C-!@O-*", "[NH1X2]=[C!R;!$(C(-N)(=[NH1])-N)]",
    "O-!@[N;!$(N(=O)=O);!$([N+](=O)[O-])]", "S-S", "O~O", "C-C(=O)[SD2]", "N~1~*~*1", "O~1C~*1", "I-c:*", "Br-c:*", "a1aaa2a(a1)aaa(a2):a", "[!#1]-,:1-:a2a(-:[!#1]:3:a1aaaa3)aaaa2", "[P,S](~O)(~O)~O",
    "C12CC3CC(C1)CC(C2)C3", "C#N.C#N", "[CX3](=O)[OH1].[CX3](=O)[OH1]", "[NH2][CX4]C(=O)O", "Cl~O", "[F,Cl,Br,I].[F,Cl,Br,I].[F,Cl,Br,I].[F,Cl,Br,I]"
  ),
  stringsAsFactors = FALSE
)

#' Cheminformatics filters on SMILES: MACCS keys, REOS alerts and synthetic accessibility
#'
#' \code{MaccsFingerprint}: the 166 public MACCS structural keys as SMARTS
#' indicators (the definitions of RDKit's \code{MACCSkeys.py}). Key k is on
#' when its SMARTS has more than the listed number of distinct matches; key
#' 125 counts SSSR rings whose bonds are all aromatic (on above one), key 166
#' counts fragments (on above one), and key 1 (isotope) is undefined as in
#' RDKit.
#'
#' \code{MolecularProperties}: the rd_filters descriptor set, namely average
#' molecular weight, Wildman-Crippen logP, the distinct matches of RDKit's
#' NumHBD and NumHBA patterns, Ertl's topological polar surface area over N
#' and O, RDKit's strict rotatable-bond count, heavy atoms and total charge.
#'
#' \code{ReosFilter}: a REOS-style nuisance filter (Walters and Murcko 2002)
#' as implemented in rd_filters: a molecule passes when no substructure alert
#' of the chosen rule sets matches and every property lies in its closed
#' range. Defaults are rd_filters' rules.json: MW 0 to 500, logP -5 to 5, HBD
#' 0 to 5, HBA 0 to 10, TPSA 0 to 200, rotatable bonds 0 to 10, Inpharmatica
#' alerts. Rule sets: Glaxo, Dundee, BMS, PAINS, SureChEMBL, MLSMR,
#' Inpharmatica, LINT (1251 alerts in all).
#'
#' \code{MorganEnvironments}: unfolded Morgan (ECFP-like) environment
#' identifiers as RDKit's generator computes them, from invariants hashing
#' atomic number, total degree, total hydrogens, charge, mass shift and ring
#' membership with RDKit's 32-bit hash_combine, dropping an environment whose
#' bond set was already seen and retiring its atom.
#'
#' \code{SaScore}: the synthetic accessibility score of Ertl and Schuffenhauer
#' (2009), 1 (easy) to 10 (hard), as RDKit's sascorer: the count-weighted mean
#' fragment contribution of the radius-2 Morgan environments (unknown ones
#' score -4) plus complexity penalties for size, stereocentres, spiro and
#' bridgehead atoms and macrocycles, a symmetry correction, and the mapping to
#' 1 to 10 with smoothing above 8. Fragment contributions are supplied by the
#' caller (RDKit's fpscores data are not bundled), so with none given every
#' environment counts as unknown and only the complexity terms inform the
#' score. Identical to the Python arm \code{morie.fn.chemfilt} (atom indices
#' 1-based here, 0-based there).
#'
#' @param smiles SMILES string.
#' @param rule_sets Character vector of alert rule sets to apply.
#' @param mw,logp,hbd,hba,tpsa,rot Length-two inclusive property ranges.
#' @param radius Morgan radius.
#' @param fragment_scores Named numeric vector or list of environment
#'   identifier to contribution (names are the identifiers as strings).
#' @return \code{MaccsFingerprint}: list with on_bits and bits (length 167,
#'   index 1 unused). \code{MolecularProperties}: list with MW, LogP, HBD,
#'   HBA, TPSA, Rot, heavy_atoms, charge. \code{ReosFilter}: list with passed,
#'   filter, alerts, properties, violations. \code{MorganEnvironments}: list
#'   with counts (named by identifier) and environments (identifier, atom,
#'   radius). \code{SaScore}: list with score, fragment_score, complexity,
#'   symmetry, n_stereo, n_spiro, n_bridgehead, macrocycle.
#' @references Durant, J. L., Leland, B. A., Henry, D. R. and Nourse, J. G.
#'   (2002). Reoptimization of MDL keys for use in drug discovery. Journal of
#'   Chemical Information and Computer Sciences 42, 1273-1280.
#'
#'   Walters, W. P. and Murcko, M. A. (2002). Prediction of drug-likeness.
#'   Advanced Drug Delivery Reviews 54, 255-271.
#'
#'   Rogers, D. and Hahn, M. (2010). Extended-connectivity fingerprints.
#'   Journal of Chemical Information and Modeling 50, 742-754.
#'
#'   Ertl, P. and Schuffenhauer, A. (2009). Estimation of synthetic
#'   accessibility score of drug-like molecules based on molecular complexity
#'   and fragment contributions. Journal of Cheminformatics 1, 8.
#'
#'   Ertl, P., Rohde, B. and Selzer, P. (2000). Fast calculation of molecular
#'   polar surface area as a sum of fragment-based contributions. Journal of
#'   Medicinal Chemistry 43, 3714-3717.
#' @examples
#' MaccsFingerprint("CNO")$on_bits
#' MolecularProperties("CC(=O)Oc1ccccc1C(=O)O")$TPSA
#' ReosFilter("CC(=O)Oc1ccccc1C(=O)O")$filter
#' MorganEnvironments("CC", radius = 1)$counts
#' SaScore("c1ccccc1")$complexity
#' @export
MaccsFingerprint <- function(smiles) {
  if (is.null(.cf_env$maccs)) .cf_env$maccs <- lapply(.cf_maccs$smarts, .cf_parse)
  m <- .cf_molecule(smiles)
  cache <- new.env()
  bits <- numeric(167)
  for (r in seq_along(.cf_env$maccs)) {
    key <- .cf_maccs$key[r]
    cnt <- .cf_maccs$count[r]
    q <- .cf_env$maccs[[r]]
    hit <- if (cnt == 0) .cf_has(m, q, cache) else .cf_count(m, q, cache) > cnt
    bits[key] <- if (hit) 1 else 0
  }
  n_arom <- 0
  for (r in m$ring_bonds) if (all(m$order[r] == 4)) n_arom <- n_arom + 1
  bits[125] <- if (n_arom > 1) 1 else 0
  bits[166] <- if (.cf_frags(m) > 1) 1 else 0
  list(on_bits = which(bits > 0), bits = bits)
}

# average atomic weights of elements 1 to 104 as in RDKit's periodic table
.cf_weight <- c(
  1.008, 4.003, 6.941, 9.012, 10.812, 12.011, 14.007, 15.999, 18.998, 20.18, 22.99, 24.305, 26.982, 28.086, 30.974,
  32.067, 35.453, 39.948, 39.098, 40.078, 44.956, 47.867, 50.944, 51.996, 54.938, 55.845, 58.933, 58.693, 63.546,
  65.39, 69.723, 72.61, 74.922, 78.96, 79.904, 83.8, 85.468, 87.62, 88.906, 91.224, 92.906, 95.94, 98, 101.07,
  102.906, 106.42, 107.868, 112.412, 114.818, 118.711, 121.76, 127.6, 126.904, 131.29, 132.905, 137.328, 138.906,
  140.116, 140.908, 144.24, 145, 150.36, 151.964, 157.25, 158.925, 162.5, 164.93, 167.26, 168.934, 173.04,
  174.967, 178.49, 180.948, 183.84, 186.207, 190.23, 192.217, 195.078, 196.967, 200.59, 204.383, 207.2, 208.98,
  209, 210, 222, 223, 226, 227, 232.038, 231.036, 238.029, 237, 244, 243, 247, 247, 251,
  252, 257, 258, 259, 262, 267
)
# RDKit Lipinski.cpp: NumHBD, NumHBA and the strict rotatable-bond pattern
.cf_hbd <- "[N&!H0&v3,N&!H0&+1&v4,O&H1&+0,S&H1&+0,n&H1&+0]"
.cf_hba <- paste0("[$([O,S;H1;v2]-[!$(*=[O,N,P,S])]),$([O,S;H0;v2]),$([O,S;-]),$([N;v3;!$(N-*=!@[O,N,P,S])]),",
                  "$([nH0X2,o,s;+0])]")
.cf_rotb <- paste0("[!$(*#*)&!D1&!$(C(F)(F)F)&!$(C(Cl)(Cl)Cl)&!$(C(Br)(Br)Br)&!$(C([CH3])([CH3])[CH3])&!$([CH3])",
                   "&!$([CD3](=[N,O,S])-!@[#7,O,S!D1])&!$([#7,O,S!D1]-!@[CD3]=[N,O,S])&!$([CD3](=[N+])-!@[#7!D1])",
                   "&!$([#7!D1]-!@[CD3]=[N+])]-,:;!@[!$(*#*)&!D1&!$(C(F)(F)F)&!$(C(Cl)(Cl)Cl)&!$(C(Br)(Br)Br)",
                   "&!$(C([CH3])([CH3])[CH3])&!$([CH3])]")

.cf_q <- function(sm) {
  key <- paste0("q", sm)
  if (is.null(.cf_env[[key]])) .cf_env[[key]] <- .cf_parse(sm)
  .cf_env[[key]]
}

.cf_tpsa <- function(m) {
  total <- 0
  for (i in seq_len(m$heavy)) {
    z <- m$z[i]
    if (!(z %in% c(7, 8))) next
    nb <- 0
    ns <- 0
    nd <- 0
    nt <- 0
    na <- 0
    nh <- m$h[i]
    for (r in seq_len(nrow(m$nbr[[i]]))) {
      j <- m$nbr[[i]][r, 1]
      k <- m$nbr[[i]][r, 2]
      if (m$z[j] == 1) {
        nh <- nh + 1
        next
      }
      nb <- nb + 1
      o <- m$order[k]
      if (o == 4) na <- na + 1 else if (o == 1) ns <- ns + 1 else if (o == 2) nd <- nd + 1 else if (o == 3) nt <- nt + 1
    }
    q <- m$chg[i]
    r3 <- m$smallest[i] == 3
    t <- -1
    if (z == 7) {
      if (nb == 1) {
        if (nh == 0 && q == 0 && nt == 1) t <- 23.79
        else if (nh == 1 && q == 0 && nd == 1) t <- 23.85
        else if (nh == 2 && q == 0 && ns == 1) t <- 26.02
        else if (nh == 2 && q == 1 && nd == 1) t <- 25.59
        else if (nh == 3 && q == 1 && ns == 1) t <- 27.64
      } else if (nb == 2) {
        if (nh == 0 && q == 0 && ns == 1 && nd == 1) t <- 12.36
        else if (nh == 0 && q == 0 && nt == 1 && nd == 1) t <- 13.60
        else if (nh == 1 && q == 0 && ns == 2 && r3) t <- 21.94
        else if (nh == 1 && q == 0 && ns == 2) t <- 12.03
        else if (nh == 0 && q == 1 && nt == 1 && ns == 1) t <- 4.36
        else if (nh == 1 && q == 1 && nd == 1 && ns == 1) t <- 13.97
        else if (nh == 2 && q == 1 && ns == 2) t <- 16.61
        else if (nh == 0 && q == 0 && na == 2) t <- 12.89
        else if (nh == 1 && q == 0 && na == 2) t <- 15.79
        else if (nh == 1 && q == 1 && na == 2) t <- 14.14
      } else if (nb == 3) {
        if (nh == 0 && q == 0 && ns == 3 && r3) t <- 3.01
        else if (nh == 0 && q == 0 && ns == 3) t <- 3.24
        else if (nh == 0 && q == 0 && ns == 1 && nd == 2) t <- 11.68
        else if (nh == 0 && q == 1 && ns == 2 && nd == 1) t <- 3.01
        else if (nh == 1 && q == 1 && ns == 3) t <- 4.44
        else if (nh == 0 && q == 0 && na == 3) t <- 4.41
        else if (nh == 0 && q == 0 && ns == 1 && na == 2) t <- 4.93
        else if (nh == 0 && q == 0 && nd == 1 && na == 2) t <- 8.39
        else if (nh == 0 && q == 1 && na == 3) t <- 4.10
        else if (nh == 0 && q == 1 && ns == 1 && na == 2) t <- 3.88
      } else if (nb == 4 && nh == 0 && ns == 4 && q == 1) {
        t <- 0
      }
      if (t < 0) t <- max(30.5 - nb * 8.2 + nh * 1.5, 0)
    } else {
      if (nb == 1) {
        if (nh == 0 && q == 0 && nd == 1) t <- 17.07
        else if (nh == 1 && q == 0 && ns == 1) t <- 20.23
        else if (nh == 0 && q == -1 && ns == 1) t <- 23.06
      } else if (nb == 2) {
        if (nh == 0 && q == 0 && ns == 2 && r3) t <- 12.53
        else if (nh == 0 && q == 0 && ns == 2) t <- 9.23
        else if (nh == 0 && q == 0 && na == 2) t <- 13.14
      }
      if (t < 0) t <- max(28.5 - nb * 8.6 + nh * 1.5, 0)
    }
    total <- total + t
  }
  total
}

.cf_crippen_logp <- function(smiles) {
  if (is.null(.cf_env$crippen)) .cf_env$crippen <- lapply(.cf_crippen$smarts, .cf_parse)
  m <- .cf_molecule(smiles, explicit_h = TRUE)
  cache <- new.env()
  lp <- 0
  for (i in seq_len(m$n)) {
    for (k in seq_along(.cf_env$crippen)) {
      if (.cf_embed(.cf_env$crippen[[k]], m, i, cache)) {
        lp <- lp + .cf_crippen$logp[k]
        break
      }
    }
  }
  lp
}

#' @rdname MaccsFingerprint
#' @export
MolecularProperties <- function(smiles) {
  m <- .cf_molecule(smiles)
  cache <- new.env()
  mw <- 0
  for (i in seq_len(m$heavy)) mw <- mw + .cf_weight[m$z[i]] + m$h[i] * .cf_weight[1]
  list(MW = mw, LogP = .cf_crippen_logp(smiles), HBD = .cf_count(m, .cf_q(.cf_hbd), cache),
       HBA = .cf_count(m, .cf_q(.cf_hba), cache), TPSA = .cf_tpsa(m), Rot = .cf_count(m, .cf_q(.cf_rotb), cache),
       heavy_atoms = sum(m$z[seq_len(m$heavy)] != 1), charge = sum(m$chg[seq_len(m$heavy)]))
}

.cf_rule_sets <- c("Glaxo", "Dundee", "BMS", "PAINS", "SureChEMBL", "MLSMR", "Inpharmatica", "LINT")

#' @rdname MaccsFingerprint
#' @export
ReosFilter <- function(smiles, rule_sets = "Inpharmatica", mw = c(0, 500), logp = c(-5, 5), hbd = c(0, 5),
                       hba = c(0, 10), tpsa = c(0, 200), rot = c(0, 10)) {
  bad <- setdiff(rule_sets, .cf_rule_sets)
  if (length(bad)) stop("unknown rule set: ", paste(bad, collapse = ", "))
  rows <- which(.cf_alerts$rule_set %in% rule_sets)
  if (is.null(.cf_env$alerts)) .cf_env$alerts <- new.env()
  m <- .cf_molecule(smiles)
  cache <- new.env()
  alerts <- list()
  for (r in rows) {
    key <- paste0("a", r)
    if (is.null(.cf_env$alerts[[key]])) .cf_env$alerts[[key]] <- .cf_parse(.cf_alerts$smarts[r])
    if (.cf_has(m, .cf_env$alerts[[key]], cache)) {
      alerts[[length(alerts) + 1]] <- c(.cf_alerts$rule_set[r], .cf_alerts$description[r])
    }
  }
  props <- MolecularProperties(smiles)
  ranges <- list(MW = mw, LogP = logp, HBD = hbd, HBA = hba, TPSA = tpsa, Rot = rot)
  viol <- names(ranges)[vapply(names(ranges), function(k) {
    !(props[[k]] >= ranges[[k]][1] && props[[k]] <= ranges[[k]][2])
  }, TRUE)]
  list(passed = length(alerts) == 0 && length(viol) == 0,
       filter = if (length(alerts)) paste(alerts[[1]][2], "> 0") else "OK",
       alerts = alerts, properties = props, violations = viol)
}

# exact masses of the isotopes most often written in SMILES (others use their mass number)
.cf_iso_mass <- list("1-2" = 2.014101778, "1-3" = 3.016049268, "6-11" = 11.011433, "6-13" = 13.00335484,
                     "6-14" = 14.003241989, "7-15" = 15.0001089, "8-18" = 17.9991610, "9-18" = 18.0009380,
                     "15-32" = 31.97390727, "16-35" = 34.96903216, "53-123" = 122.905589, "53-125" = 124.9046302,
                     "53-131" = 130.9061246)
.cf_2p32 <- 4294967296

.cf_xor32 <- function(a, b) {
  bitwXor(a %/% 65536, b %/% 65536) * 65536 + bitwXor(a %% 65536, b %% 65536)
}

.cf_combine <- function(seed, v) {
  add <- (v %% .cf_2p32 + 2654435769 + (seed * 64) %% .cf_2p32 + seed %/% 4) %% .cf_2p32
  .cf_xor32(seed, add)
}

.cf_hash_pair <- function(a, b) .cf_combine(.cf_combine(0, a %% .cf_2p32), b)

#' @rdname MaccsFingerprint
#' @export
MorganEnvironments <- function(smiles, radius = 2) {
  m <- .cf_molecule(smiles)
  n <- m$heavy
  inring <- rep(FALSE, m$n)
  for (r in m$ring_bonds) for (k in r) inring[m$bonds[k, ]] <- TRUE
  cur <- numeric(n)
  for (i in seq_len(n)) {
    z <- m$z[i]
    hs <- m$h[i] + sum(m$z[m$nbr[[i]][, 1]] == 1)
    deg <- nrow(m$nbr[[i]]) + m$h[i]
    mass <- if (m$iso[i]) {
      mm <- .cf_iso_mass[[paste0(z, "-", m$iso[i])]]
      if (is.null(mm)) m$iso[i] else mm
    } else {
      .cf_weight[z]
    }
    comp <- c(z, deg, hs, m$chg[i] %% .cf_2p32, trunc(mass - .cf_weight[z]) %% .cf_2p32)
    if (inring[i]) comp <- c(comp, 1)
    seed <- 0
    for (cc in comp) seed <- .cf_combine(seed, cc)
    cur[i] <- seed
  }
  nbrs <- lapply(seq_len(n), function(i) m$nbr[[i]][m$nbr[[i]][, 1] <= n, , drop = FALSE])
  bt <- ifelse(m$order == 4, 12, m$order)
  envs <- lapply(seq_len(n), function(i) c(cur[i], i, 0))
  nbhd <- lapply(seq_len(n), function(i) integer(0))
  seen <- character(0)
  dead <- rep(FALSE, n)
  for (layer in seq_len(radius) - 1) {
    nxt <- numeric(n)
    rnd <- list()
    new_nbhd <- nbhd
    for (i in seq_len(n)) {
      if (dead[i]) next
      if (!nrow(nbrs[[i]])) {
        dead[i] <- TRUE
        next
      }
      s <- nbhd[[i]]
      pairs <- matrix(0, 0, 2)
      for (r in seq_len(nrow(nbrs[[i]]))) {
        j <- nbrs[[i]][r, 1]
        k <- nbrs[[i]][r, 2]
        s <- c(s, k, nbhd[[j]])
        pairs <- rbind(pairs, c(bt[k], cur[j]))
      }
      pairs <- pairs[order(pairs[, 1], pairs[, 2]), , drop = FALSE]
      invar <- .cf_combine(layer, cur[i])
      for (r in seq_len(nrow(pairs))) invar <- .cf_combine(invar, .cf_hash_pair(pairs[r, 1], pairs[r, 2]))
      nxt[i] <- invar
      new_nbhd[[i]] <- sort(unique(s))
      rnd[[length(rnd) + 1]] <- list(key = paste(new_nbhd[[i]], collapse = ","), invar = invar, i = i)
    }
    if (length(rnd)) {
      ord <- order(vapply(rnd, function(e) e$key, ""), vapply(rnd, function(e) e$invar, 0),
                   vapply(rnd, function(e) e$i, 0), method = "radix")
      for (e in rnd[ord]) {
        if (!(e$key %in% seen)) {
          envs[[length(envs) + 1]] <- c(e$invar, e$i, layer + 1)
          seen <- c(seen, e$key)
        } else {
          dead[e$i] <- TRUE
        }
      }
    }
    cur <- nxt
    nbhd <- new_nbhd
  }
  codes <- vapply(envs, function(e) e[1], 0)
  tb <- table(codes)
  counts <- as.numeric(tb)
  names(counts) <- names(tb)
  list(counts = counts, environments = envs)
}

.cf_relabel <- function(keys) match(keys, sort(unique(keys)))

.cf_ranks <- function(m) {
  n <- m$heavy
  keys <- vapply(seq_len(n), function(i) paste(m$z[i], nrow(m$nbr[[i]]), m$h[i], m$chg[i], m$iso[i]), "")
  cls <- .cf_relabel(keys)
  for (it in seq_len(n)) {
    keys <- vapply(seq_len(n), function(i) {
      nb <- m$nbr[[i]][m$nbr[[i]][, 1] <= n, , drop = FALSE]
      paste(cls[i], paste(sort(paste(m$order[nb[, 2]], cls[nb[, 1]])), collapse = "|"))
    }, "")
    new <- .cf_relabel(keys)
    if (length(unique(new)) == length(unique(cls))) break
    cls <- new
  }
  cls
}

.cf_stereo_centres <- function(m) {
  cls <- .cf_ranks(m)
  out <- integer(0)
  for (i in seq_len(m$heavy)) {
    z <- m$z[i]
    nb <- m$nbr[[i]][m$nbr[[i]][, 1] <= m$heavy, , drop = FALSE]
    conn <- nrow(nb) + m$h[i]
    if (m$h[i] > 1) next
    ok <- if (z %in% c(6, 14, 32, 50)) {
      conn == 4 && all(m$order[nb[, 2]] == 1)
    } else if (z %in% c(7, 15, 33)) {
      conn == 4 && (z != 7 || m$chg[i] == 1)
    } else if (z %in% c(16, 34)) {
      conn == 3
    } else {
      FALSE
    }
    if (!ok) next
    sub <- c(cls[nb[, 1]], if (m$h[i]) -1 else NULL)
    if (length(unique(sub)) == length(sub)) out <- c(out, i)
  }
  out
}

#' @rdname MaccsFingerprint
#' @export
SaScore <- function(smiles, fragment_scores = NULL) {
  me <- MorganEnvironments(smiles, radius = 2)
  m <- .cf_molecule(smiles)
  nf <- sum(me$counts)
  s1 <- 0
  for (nm in names(me$counts)) {
    v <- if (!is.null(fragment_scores) && nm %in% names(fragment_scores)) as.numeric(fragment_scores[[nm]]) else -4
    s1 <- s1 + v * me$counts[[nm]]
  }
  s1 <- s1 / nf
  n <- sum(m$z[seq_len(m$heavy)] != 1)
  ring_atoms <- lapply(m$ring_bonds, function(r) sort(unique(as.vector(m$bonds[r, , drop = FALSE]))))
  spiro <- integer(0)
  bridge <- integer(0)
  nr <- length(ring_atoms)
  for (a in seq_len(nr)) {
    for (b in seq_len(nr)) {
      if (b <= a) next
      inter <- intersect(ring_atoms[[a]], ring_atoms[[b]])
      if (length(inter) == 1) spiro <- union(spiro, inter)
      shared <- intersect(m$ring_bonds[[a]], m$ring_bonds[[b]])
      if (length(shared) > 1) {
        tb <- table(as.vector(m$bonds[shared, , drop = FALSE]))
        bridge <- union(bridge, as.integer(names(tb)[tb == 1]))
      }
    }
  }
  stereo <- length(.cf_stereo_centres(m))
  macro <- any(vapply(ring_atoms, function(at) length(at) > 8, TRUE))
  s2 <- 0 - (n^1.005 - n) - log10(stereo + 1) - log10(length(spiro) + 1) - log10(length(bridge) + 1) -
    (if (macro) log10(2) else 0)
  s3 <- if (n > length(me$counts)) 0.5 * log(n / length(me$counts)) else 0
  raw <- s1 + s2 + s3
  sa <- 11 - (raw + 4 + 1) / 6.5 * 9
  if (sa > 8) sa <- 8 + log(sa + 1 - 9)
  sa <- min(max(sa, 1), 10)
  list(score = sa, fragment_score = s1, complexity = s2, symmetry = s3, n_stereo = stereo, n_spiro = length(spiro),
       n_bridgehead = length(bridge), macrocycle = macro)
}
