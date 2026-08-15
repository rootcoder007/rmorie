# benRea -- Named-entity recognition with BIO tagging.
# Ramshaw & Marcus (1995); Tjong Kim Sang & De Meulder (2003); Viterbi (1967).
# Base R only.

.benRea_NEG <- -Inf

bio_labels <- function(types) {
  ts <- as.character(types)
  if (length(ts) == 0) stop("benRea: no entity types given")
  if (any(duplicated(ts))) stop("benRea: duplicate entity types")
  out <- c("O")
  for (t in ts) {
    out <- c(out, paste("B-", t, sep = ""))
    out <- c(out, paste("I-", t, sep = ""))
  }
  out
}

.parts <- function(label) {
  if (label == "O") return(c("O", NA))
  substr(label, 1, 1)
}

.parts_full <- function(label) {
  if (label == "O") return(list(p = "O", t = NA))
  list(p = substr(label, 1, 1), t = substr(label, 3, nchar(label)))
}

valid_transitions <- function(labels) {
  n <- length(labels)
  T <- matrix(TRUE, n, n)
  for (a in 1:n) {
    pa <- substr(labels[a], 1, 1)
    ta <- if (pa == "O") NA else substr(labels[a], 3, nchar(labels[a]))
    for (b in 1:n) {
      pb <- substr(labels[b], 1, 1)
      tb <- if (pb == "O") NA else substr(labels[b], 3, nchar(labels[b]))
      if (pb == "I") T[a, b] <- (pa %in% c("B", "I")) && !is.na(ta) &&
                                  !is.na(tb) && ta == tb
    }
  }
  T
}

start_allowed <- function(labels) {
  substr(labels, 1, 1) != "I"
}

is_valid_bio <- function(path) {
  prev <- "O"
  prev_t <- NA
  for (lab in path) {
    p <- substr(lab, 1, 1)
    t_ <- if (p == "O") NA else substr(lab, 3, nchar(lab))
    if (p == "I" && !(prev %in% c("B", "I") && !is.na(prev_t) &&
                      !is.na(t_) && prev_t == t_)) return(FALSE)
    prev <- p; prev_t <- t_
  }
  TRUE
}

greedy_decode <- function(emissions, labels) {
  em <- as.matrix(emissions)
  storage.mode(em) <- "double"
  apply(em, 1, function(r) labels[which.max(r)])
}

viterbi_decode <- function(emissions, labels, transitions = NULL,
                           transition_scores = NULL) {
  em <- as.matrix(emissions)
  storage.mode(em) <- "double"
  L <- nrow(em); n <- ncol(em)
  if (L == 0) stop("benRea: empty emission sequence")
  if (n != length(labels))
    stop("benRea: emissions must have one score per label")
  T <- if (is.null(transitions)) valid_transitions(labels) else transitions
  S <- if (is.null(transition_scores)) matrix(0, n, n) else transition_scores
  ok0 <- start_allowed(labels)
  dp <- matrix(.benRea_NEG, L, n)
  bk <- matrix(-1L, L, n)
  for (j in 1:n) if (ok0[j]) dp[1, j] <- em[1, j]
  for (t in 2:L) {
    for (j in 1:n) {
      best <- .benRea_NEG; arg <- -1L
      for (i in 1:n) {
        if (!T[i, j] || dp[t - 1, i] == .benRea_NEG) next
        v <- dp[t - 1, i] + S[i, j]
        if (v > best) { best <- v; arg <- i }
      }
      if (arg >= 0) {
        dp[t, j] <- best + em[t, j]
        bk[t, j] <- arg
      }
    }
  }
  end <- which.max(dp[L, ])
  if (dp[L, end] == .benRea_NEG) stop("benRea: no valid path exists")
  path_idx <- end
  for (t in L:2) {
    path_idx <- c(bk[t, path_idx[1]], path_idx)
  }
  list(path = labels[path_idx], score = dp[L, end])
}

extract_spans <- function(path) {
  spans <- list()
  cur_t <- NA; cur_s <- NA
  for (t in seq_along(path)) {
    lab <- path[t]
    p <- substr(lab, 1, 1)
    ty <- if (p == "O") NA else substr(lab, 3, nchar(lab))
    if (p == "B") {
      if (!is.na(cur_t))
        spans[[length(spans) + 1L]] <- c(type = cur_t, start = cur_s,
                                          end = t - 1)
      cur_t <- ty; cur_s <- t
    } else if (p == "I") {
      if (is.na(cur_t) || !is.na(cur_t) && cur_t != ty) {
        if (!is.na(cur_t))
          spans[[length(spans) + 1L]] <- c(type = cur_t, start = cur_s,
                                            end = t - 1)
        cur_t <- ty; cur_s <- t
      }
    } else {
      if (!is.na(cur_t))
        spans[[length(spans) + 1L]] <- c(type = cur_t, start = cur_s,
                                          end = t - 1)
      cur_t <- NA; cur_s <- NA
    }
  }
  if (!is.na(cur_t))
    spans[[length(spans) + 1L]] <- c(type = cur_t, start = cur_s,
                                      end = length(path))
  spans
}

span_f1 <- function(pred, gold) {
  p <- extract_spans(pred); g <- extract_spans(gold)
  pk <- sapply(p, function(s) paste(s, collapse = ":"))
  gk <- sapply(g, function(s) paste(s, collapse = ":"))
  tp <- length(intersect(pk, gk))
  prec <- if (length(p) > 0) tp / length(p) else 0
  rec  <- if (length(g) > 0) tp / length(g) else 0
  f1 <- if ((prec + rec) > 0) 2 * prec * rec / (prec + rec) else 0
  list(precision = prec, recall = rec, f1 = f1,
       true_positives = tp, n_pred = length(p), n_gold = length(g))
}

ner_decode <- function(emissions, types, decoder = "viterbi",
                       transition_scores = NULL, gold = NULL) {
  if (!(decoder %in% c("viterbi", "greedy")))
    stop("benRea: decoder must be viterbi or greedy")
  labels <- bio_labels(types)
  if (decoder == "viterbi") {
    vd <- viterbi_decode(emissions, labels, transition_scores = transition_scores)
    path <- vd$path; score <- vd$score
  } else {
    path <- greedy_decode(emissions, labels)
    score <- sum(vapply(seq_along(path), function(t) {
      emissions[[t]][match(path[t], labels)]
    }, numeric(1)))
  }
  spans <- extract_spans(path)
  out <- list(estimate = path, path = path, score = score, spans = spans,
              valid = is_valid_bio(path), labels = labels, decoder = decoder,
              n_tokens = nrow(as.matrix(emissions)),
              n_spans = length(spans),
              method = paste("BIO named-entity decoding, Ramshaw & Marcus",
                             "(1995) scheme, Viterbi (1967) constrained",
                             "decoding"))
  if (!is.null(gold))
    out <- c(out, span_f1(path, as.character(gold)))
  out
}

nerdecode <- ner_decode
named_entity <- ner_decode
namedentity <- ner_decode

# house entry point: the package exports one morie_<module>
morie_benRea <- ner_decode
