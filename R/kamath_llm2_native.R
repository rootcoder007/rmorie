# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Kamath LLM shelf tranche 2 (DL shelf W3). Mirrors morie.fn km042-km150
# plus the named kamath_* modules (km3h, kmadal .. kmyarn): 200 Python
# modules, 197 exported R functions.
#
# Kamath, Keenan, Somers and Sorenson (2024) Large Language Models: A
# Deep Dive, Springer, Ch 3-9 and the named-module shelf.
#
# REUSE, never redefine. This file leans on:
#   kamath_ch2_native.R -- morie_kamath_mlm_loss (km022), morie_kamath_
#     moe_output (km039), morie_kamath_moe_topk_gating (km040) and the
#     .morie_km_* helpers.
#   alammar_llm_native.R -- morie_alammar_sdp_attention (Python attsdp),
#     morie_alammar_reward_model_bt (Python alrmt), .morie_al_lcg,
#     .morie_al_softmax_rows.
#   burkov_lm_native.R -- morie_burkov_ngram_mle (Python bkngr).
# Python's kmclm, toppd and rmsnr cores have no R sibling yet, so their
# arithmetic lives here as .morie_km2_causal_lm_loss / _top_p / _rms.
#
# ONE R function serves several Python modules where the Python modules
# themselves delegate to one core. The mapping is recorded on each
# function's @details.
#
# Payload index conventions are Python's and are preserved: every index
# reported (mask_index, argmax, positions_scored, selected_experts,
# top_k_indices, kept, argmin, best_index, retrieved) is 0-BASED.
# np.std / np.var are POPULATION moments unless ddof says otherwise.

# ---------------------------------------------------------------- helpers

.morie_km2_soft <- function(z) {
  z <- as.numeric(z)
  e <- exp(z - max(z))
  e / sum(e)
}

.morie_km2_sig <- function(z) {
  ifelse(z >= 0, 1 / (1 + exp(-abs(z))), exp(-abs(z)) / (1 + exp(-abs(z))))
}

.morie_km2_lse_rows <- function(M) {
  m <- apply(M, 1, max)
  m + log(rowSums(exp(M - m)))
}

.morie_km2_rowce <- function(logits) {
  .morie_km2_lse_rows(logits) - diag(logits)
}

.morie_km2_dist <- function(p, name) {
  p <- as.numeric(p)
  if (length(p) == 0L) stop(sprintf("%s is empty.", name), call. = FALSE)
  if (any(p < 0)) stop(sprintf("%s holds a negative probability.", name),
                       call. = FALSE)
  if (abs(sum(p) - 1) > 1e-8) {
    stop(sprintf("%s must sum to 1; it sums to %.6g.", name, sum(p)),
         call. = FALSE)
  }
  p
}

.morie_km2_ord <- function(v) order(v, decreasing = TRUE)

# Stable descending order matching numpy argsort(-v, kind="stable").
.morie_km2_stable_desc <- function(v) order(-v, seq_along(v))

.morie_km2_ngrams <- function(tokens, n) {
  L <- length(tokens)
  if (L < n) return(character(0))
  vapply(seq_len(L - n + 1L),
         function(i) paste(tokens[i:(i + n - 1L)], collapse = "\r"),
         character(1))
}

.morie_km2_counts <- function(keys) {
  if (length(keys) == 0L) return(integer(0))
  tb <- table(keys)
  stats::setNames(as.integer(tb), names(tb))
}

.morie_km2_layer_norm <- function(x, eps = 1e-5) {
  x <- as.matrix(x)
  mu <- rowMeans(x)
  v <- rowMeans((x - mu)^2)
  (x - mu) / sqrt(v + eps)
}

# kmclm's core: causal-LM cross entropy over non-ignored positions.
.morie_km2_causal_lm_loss <- function(logits, targets, ignore_index = -100L) {
  logits <- as.matrix(logits)
  tgt <- as.integer(targets)
  if (length(tgt) != nrow(logits)) {
    stop("one target per position is required.", call. = FALSE)
  }
  keep <- which(tgt != ignore_index)
  if (length(keep) == 0L) stop("every position is ignored.", call. = FALSE)
  if (any(tgt[keep] < 0L | tgt[keep] >= ncol(logits))) {
    stop("a target id lies outside the vocabulary.", call. = FALSE)
  }
  per <- vapply(keep, function(t) {
    z <- logits[t, ]
    m <- max(z)
    -(z[tgt[t] + 1L] - (m + log(sum(exp(z - m)))))
  }, numeric(1))
  loss <- mean(per)
  list(loss = loss, perplexity = exp(loss), token_losses = per,
       n_tokens = length(keep), vocab_size = ncol(logits))
}

# rmsnr's core.
.morie_km2_rms <- function(x, gamma = NULL, eps = 1e-6) {
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), nrow = 1L)
  r <- sqrt(rowMeans(X^2) + eps)
  Y <- X / r
  if (!is.null(gamma)) Y <- t(t(Y) * as.numeric(gamma))
  list(tensor = if (is.matrix(x)) Y else as.numeric(Y), rms = r)
}

# toppd's core: nucleus truncation of temperature-scaled logits.
.morie_km2_top_p <- function(z, p, T = 1) {
  q <- .morie_km2_soft(as.numeric(z) / T)
  ord <- order(q, decreasing = TRUE)
  cum <- cumsum(q[ord])
  n_keep <- which(cum >= p - 1e-12)[1]
  if (is.na(n_keep)) n_keep <- length(q)
  keep <- rep(FALSE, length(q))
  keep[ord[seq_len(n_keep)]] <- TRUE
  out <- ifelse(keep, q, 0)
  list(tensor = out / sum(out), keep_mask = keep, n_kept = n_keep)
}

# Exact min-cost transport (successive shortest augmenting path with
# Bellman-Ford potentials); ports km122's _solve_transport, and km/kmmsc's
# WMD reads its objective off the same core.
.morie_km2_transport <- function(a, b, C) {
  m <- length(a); n <- length(b)
  Fm <- matrix(0, m, n)
  supply <- as.numeric(a); demand <- as.numeric(b)
  tol <- 1e-12
  guard <- (m + 1) * (n + 1) + m + n + 10
  ok <- FALSE
  for (g in seq_len(guard)) {
    if (sum(supply) <= tol) { ok <- TRUE; break }
    dist <- rep(Inf, m + n)
    prevk <- rep("", m + n); previ <- rep(0L, m + n); prevj <- rep(0L, m + n)
    dist[seq_len(m)] <- ifelse(supply > tol, 0, Inf)
    for (it in seq_len(m + n)) {
      changed <- FALSE
      for (i in seq_len(m)) {
        if (!is.finite(dist[i])) next
        for (j in seq_len(n)) {
          if (dist[i] + C[i, j] < dist[m + j] - 1e-15) {
            dist[m + j] <- dist[i] + C[i, j]
            prevk[m + j] <- "f"; previ[m + j] <- i; prevj[m + j] <- j
            changed <- TRUE
          }
        }
      }
      for (j in seq_len(n)) {
        if (!is.finite(dist[m + j])) next
        for (i in seq_len(m)) {
          if (Fm[i, j] > tol && dist[m + j] - C[i, j] < dist[i] - 1e-15) {
            dist[i] <- dist[m + j] - C[i, j]
            prevk[i] <- "b"; previ[i] <- i; prevj[i] <- j
            changed <- TRUE
          }
        }
      }
      if (!changed) break
    }
    cand <- which(demand > tol & is.finite(dist[m + seq_len(n)]))
    if (length(cand) == 0L) {
      stop("the transport problem is infeasible.", call. = FALSE)
    }
    j <- cand[which.min(dist[m + cand])]
    path <- list()
    node <- m + j
    while (nzchar(prevk[node])) {
      kind <- prevk[node]; pi <- previ[node]; pj <- prevj[node]
      path[[length(path) + 1L]] <- c(kind, pi, pj)
      node <- if (kind == "f") pi else m + pj
    }
    push <- min(demand[j], supply[node])
    for (st in path) {
      if (st[1] == "b") push <- min(push, Fm[as.integer(st[2]), as.integer(st[3])])
    }
    if (push <= tol) stop("the transport solver stalled.", call. = FALSE)
    for (st in path) {
      i2 <- as.integer(st[2]); j2 <- as.integer(st[3])
      Fm[i2, j2] <- Fm[i2, j2] + if (st[1] == "f") push else -push
    }
    supply[node] <- supply[node] - push
    demand[j] <- demand[j] - push
  }
  if (!ok) stop("the transport solver did not converge.", call. = FALSE)
  if (sum(supply) > 1e-9 || sum(demand) > 1e-9) {
    stop("the transport problem was left unbalanced.", call. = FALSE)
  }
  d <- rep(0, m + n)
  conv <- FALSE
  for (it in seq_len(m + n)) {
    changed <- FALSE
    for (i in seq_len(m)) for (j in seq_len(n)) {
      if (d[i] + C[i, j] < d[m + j] - 1e-12) {
        d[m + j] <- d[i] + C[i, j]; changed <- TRUE
      }
      if (Fm[i, j] > tol && d[m + j] - C[i, j] < d[i] - 1e-12) {
        d[i] <- d[m + j] - C[i, j]; changed <- TRUE
      }
    }
    if (!changed) { conv <- TRUE; break }
  }
  if (!conv) stop("the residual graph still has a negative cycle.",
                  call. = FALSE)
  list(flow = Fm, u = -d[seq_len(m)], v = d[m + seq_len(n)])
}

# ------------------------------------------------------- Ch 3: prompting

#' Prompt-based classification through a label word map (Kamath Eq 3.1)
#'
#' p(y|x) = p(z = M(y) | x'). `x` is a named numeric vector of answer-word
#' probabilities (validated to sum to 1); `M` a named character vector
#' label -> answer word.
#'
#' @param x Named answer-word distribution. @param y Class label.
#' @param M Named label -> answer-word map.
#' @return List with `estimate`, `label_probs`, `label_mass`.
#' @export
morie_kamath_ch3_prompt_label_mapping <- function(x, y, M) {
  .morie_km2_dist(x, "x")
  if (length(M) == 0L) stop("M must be a non-empty label map.", call. = FALSE)
  if (!(y %in% names(M))) stop("label is not in the label word map M.",
                               call. = FALSE)
  Mv <- unlist(M)
  if (!all(Mv %in% names(x))) {
    stop("an answer word carries no probability in x.", call. = FALSE)
  }
  lp <- stats::setNames(as.numeric(x[Mv]), names(Mv))
  list(estimate = as.numeric(x[M[[y]]]), label = y, answer_word = M[[y]],
       label_probs = lp, label_mass = sum(lp), n = length(x),
       method = "prompt label word mapping (Kamath Eq 3.1)")
}

#' Label-word softmax (Kamath Eq 3.2)
#'
#' p(y|x) = exp(w_{M(y)}.h_z) / sum over the LABEL words only.
#'
#' @param w Named list of answer-word vectors. @param h_z Answer-slot state.
#' @param M Named label -> answer-word map.
#' @return List with `estimate`, `label`, `label_probs`, `logits`.
#' @export
morie_kamath_ch3_prompt_softmax_label <- function(w, h_z, M) {
  if (length(M) == 0L) stop("M is empty.", call. = FALSE)
  if (length(w) == 0L) stop("w is empty.", call. = FALSE)
  h <- as.numeric(h_z)
  if (length(h) == 0L) stop("h_z is empty.", call. = FALSE)
  labels <- names(M)
  z <- vapply(labels, function(lab) {
    word <- M[[lab]]
    if (!(word %in% names(w))) stop("an answer word has no vector in w.",
                                    call. = FALSE)
    v <- as.numeric(w[[word]])
    if (length(v) != length(h)) stop("w[word] and h_z differ in length.",
                                     call. = FALSE)
    sum(v * h)
  }, numeric(1))
  p <- .morie_km2_soft(z)
  best <- which.max(p)
  list(estimate = p[[best]], label = labels[best],
       label_probs = stats::setNames(p, labels),
       logits = stats::setNames(z, labels), n = length(labels),
       method = "label-word softmax (Kamath Eq 3.2)")
}

#' Answer search over filled prompts (Kamath Eq 3.3)
#'
#' z_hat = argmax_{z in Z} score(f_fill(x, z)). `theta` scores the FILLED
#' prompt; argmax is invariant to any monotone map.
#'
#' @param x Template. @param z Candidate answers. @param theta Scorer.
#' @param f_fill Optional filler; default substitutes `\[z\]`, else appends.
#' @return List with `estimate`, `z_hat`, `filled_prompt`, `scores`.
#' @export
morie_kamath_ch3_prompt_search_argmax <- function(x, z, theta, f_fill = NULL) {
  if (!is.character(x) || !nzchar(x)) stop("x must be a non-empty prompt.",
                                           call. = FALSE)
  cands <- as.character(z)
  if (length(cands) == 0L) stop("the candidate answer set Z is empty.",
                                call. = FALSE)
  if (!is.function(theta)) stop("theta must be a scorer function.",
                                call. = FALSE)
  fill <- if (is.null(f_fill)) function(a, b) {
    if (grepl("[z]", a, fixed = TRUE)) gsub("[z]", b, a, fixed = TRUE)
    else paste0(a, b)
  } else f_fill
  filled <- vapply(cands, function(c) fill(x, c), character(1),
                   USE.NAMES = FALSE)
  scores <- vapply(filled, function(s) as.numeric(theta(s)), numeric(1),
                   USE.NAMES = FALSE)
  if (any(!is.finite(scores))) stop("theta returned a non-finite score.",
                                    call. = FALSE)
  best <- which.max(scores)
  list(estimate = scores[best], z_hat = cands[best],
       filled_prompt = filled[best],
       scores = stats::setNames(scores, cands), n = length(cands),
       method = "argmax answer search over filled prompts (Kamath Eq 3.3)")
}

#' Cloze knowledge probe (Kamath Eq 3.4)
#'
#' Exactly one mask must be present; `mask_index` is 0-BASED.
#'
#' @param prompt Cloze prompt. @param mask Mask token.
#' @return List with `mask_index` (0-based), `tokens`, `estimate`.
#' @export
morie_kamath_ch3_dante_cloze <- function(prompt = "Dante was born in [MASK]",
                                         mask = "[MASK]") {
  if (!is.character(prompt) || !nzchar(trimws(prompt))) {
    stop("prompt must be a non-empty string.", call. = FALSE)
  }
  n_mask <- length(gregexpr(mask, prompt, fixed = TRUE)[[1]])
  if (!grepl(mask, prompt, fixed = TRUE)) n_mask <- 0L
  if (n_mask != 1L) stop("a cloze prompt needs exactly one mask.",
                         call. = FALSE)
  tokens <- strsplit(trimws(prompt), "\\s+")[[1]]
  idx <- which(vapply(tokens, function(t) grepl(mask, t, fixed = TRUE),
                      logical(1)))[1] - 1L
  list(prompt = prompt, mask = mask, mask_index = as.integer(idx),
       tokens = tokens, estimate = as.numeric(idx), n = length(tokens),
       method = "cloze knowledge probe (Kamath Eq 3.4)")
}

.morie_km2_fill_template <- function(template, x, z) {
  if (!is.character(template) || !grepl("[x]", template, fixed = TRUE)) {
    stop("the template must be a string with an [x] slot.", call. = FALSE)
  }
  if (!is.character(x) || !nzchar(trimws(x))) {
    stop("x must be a non-empty input string.", call. = FALSE)
  }
  out <- gsub("[x]", x, template, fixed = TRUE)
  if (is.null(z)) return(list(out, FALSE))
  if (!is.character(z)) stop("z must be a string or NULL.", call. = FALSE)
  if (!grepl("[z]", template, fixed = TRUE)) {
    stop("the template has no [z] answer slot to fill.", call. = FALSE)
  }
  list(gsub("[z]", z, out, fixed = TRUE), TRUE)
}

.morie_km2_tmpl_result <- function(prompt, filled, eq, template) {
  tokens <- strsplit(trimws(prompt), "\\s+")[[1]]
  list(prompt = prompt, slot_filled = filled, template = template,
       tokens = tokens, estimate = as.numeric(length(tokens)),
       n = length(tokens),
       method = sprintf("prompt template (Kamath Eq %s)", eq))
}

#' Ch 3 prompt templates (Kamath Eq 3.5-3.7)
#'
#' @details One filler serves all three Python modules exactly as
#' km046/km047/km048 share `_fill_template`: km046 ->
#' `morie_kamath_ch3_prefix_prompt_template`, km047 ->
#' `morie_kamath_ch3_translate_prefix_prompt`, km048 ->
#' `morie_kamath_ch3_cloze_prompt_template`.
#'
#' @param x Input text. @param z Answer, or NULL to leave the slot open.
#' @param template Template string.
#' @return List with `prompt`, `slot_filled`, `tokens`.
#' @export
morie_kamath_ch3_prefix_prompt_template <- function(
    x, z = NULL, template = "[x] This movie is [z]") {
  r <- .morie_km2_fill_template(template, x, z)
  .morie_km2_tmpl_result(r[[1]], r[[2]], "3.5", template)
}

#' @rdname morie_kamath_ch3_prefix_prompt_template
#' @export
morie_kamath_ch3_translate_prefix_prompt <- function(
    x, z = NULL,
    template = "Translate the following English sentence to French: [x] [z]") {
  r <- .morie_km2_fill_template(template, x, z)
  .morie_km2_tmpl_result(r[[1]], r[[2]], "3.6", template)
}

#' @rdname morie_kamath_ch3_prefix_prompt_template
#' @export
morie_kamath_ch3_cloze_prompt_template <- function(
    x, z = NULL, template = "[x] This is a [z] movie.") {
  r <- .morie_km2_fill_template(template, x, z)
  .morie_km2_tmpl_result(r[[1]], r[[2]], "3.7", template)
}

#' Top-1 prompt selection accuracy (Kamath Eq 3.8)
#'
#' A(t) = mean over (x, y) of 1\[y = argmax_y' P_LM(y'|x, t)\]. A proportion,
#' never a mean of the inputs.
#'
#' @param R List of `list(x, y)` pairs. @param t Template.
#' @param P_LM Function (x, t) -> named probability vector.
#' @return List with `estimate`, `n_correct`, `correct`.
#' @export
morie_kamath_ch3_top1_prompt_metric <- function(R, t, P_LM) {
  if (length(R) == 0L) stop("R is empty.", call. = FALSE)
  if (!is.function(P_LM)) stop("P_LM must be a function.", call. = FALSE)
  hits <- vapply(R, function(pair) {
    x <- pair[[1]]; y <- pair[[2]]
    dist <- P_LM(x, t)
    p <- .morie_km2_dist(dist, "P_LM's distribution")
    if (!(y %in% names(dist))) stop("the gold label is absent.", call. = FALSE)
    as.numeric(names(dist)[which.max(p)] == y)
  }, numeric(1))
  list(estimate = mean(hits), n_correct = as.integer(sum(hits)),
       correct = as.integer(hits), template = t, n = length(R),
       method = "top-1 prompt selection accuracy (Kamath Eq 3.8)")
}

#' Round-trip back-translation prompt score (Kamath Eq 3.9)
#'
#' P(t) = P_forward(t_hat|t) . P_backward(t|t_hat). The printed equation
#' cannot be a round trip; the coherent reading is implemented, as in Python.
#'
#' @param t Candidate prompt. @param thatt Pivot translation.
#' @param p_forward,p_backward Leg probabilities in \[0, 1\].
#' @return List with `estimate`.
#' @export
morie_kamath_ch3_back_translation_prob <- function(t, thatt, p_forward = NULL,
                                                   p_backward = NULL) {
  if (is.null(p_forward) || is.null(p_backward)) {
    stop("both p_forward and p_backward are required.", call. = FALSE)
  }
  pf <- as.numeric(p_forward); pb <- as.numeric(p_backward)
  if (pf < 0 || pf > 1 || pb < 0 || pb > 1) {
    stop("the leg probabilities must lie in [0, 1].", call. = FALSE)
  }
  list(estimate = pf * pb, p_forward = pf, p_backward = pb,
       candidate = t, pivot = thatt, n = 2L,
       method = "round-trip back-translation score (Kamath Eq 3.9)")
}

#' Adversarial-trigger QA prompt (Kamath Eq 3.10)
#'
#' @param x Question. @param y Context. @param T Trigger token.
#' @param z_adv Adversarial answer. @param n_triggers Repeats (3 in the book).
#' @return List with `prompt`, `tokens`, `n_triggers`.
#' @export
morie_kamath_ch3_qa_trigger_template <- function(x, y, T, z_adv,
                                                 n_triggers = 3) {
  for (v in list(x, y, T, z_adv)) {
    if (!is.character(v) || !nzchar(trimws(v))) {
      stop("x, y, T and z_adv must be non-empty strings.", call. = FALSE)
    }
  }
  k <- as.integer(n_triggers)
  if (k < 1L) stop("n_triggers must be at least 1.", call. = FALSE)
  triggers <- paste(rep(T, k), collapse = " ")
  prompt <- sprintf("Question: %s Context: %s Answer: %s %s", x, y,
                    triggers, z_adv)
  tokens <- strsplit(trimws(prompt), "\\s+")[[1]]
  list(prompt = prompt, trigger = T, n_triggers = k,
       adversarial_answer = z_adv, tokens = tokens,
       estimate = as.numeric(length(tokens)), n = length(tokens),
       method = "adversarial-trigger QA prompt (Kamath Eq 3.10)")
}

#' T5 template generation objective (Kamath Eq 3.11)
#'
#' sum over D_train of log P_T5(T | T(x_in, y)). A probability of 0 is
#' refused rather than returned as -Inf.
#'
#' @param D_train List of `list(x_in, y)` pairs. @param T Template
#'   (function of (x_in, y) or a sprintf-free string with `{x}`/`{y}`).
#' @param T5 Function (T, filled) -> probability in (0, 1\].
#' @return List with `estimate`, `per_example`, `filled_inputs`.
#' @export
morie_kamath_ch3_t5_template_obj <- function(D_train, T, T5) {
  if (length(D_train) == 0L) stop("D_train is empty.", call. = FALSE)
  if (!is.function(T5)) stop("T5 must be a function.", call. = FALSE)
  filled <- vapply(D_train, function(pr) {
    if (is.function(T)) T(pr[[1]], pr[[2]])
    else gsub("{y}", pr[[2]], gsub("{x}", pr[[1]], T, fixed = TRUE),
              fixed = TRUE)
  }, character(1))
  logs <- vapply(filled, function(s) {
    p <- as.numeric(T5(T, s))
    if (!(p > 0 && p <= 1)) stop("P_T5 must lie in (0, 1].", call. = FALSE)
    log(p)
  }, numeric(1), USE.NAMES = FALSE)
  list(estimate = sum(logs), per_example = logs, filled_inputs = filled,
       n = length(D_train),
       method = "T5 template generation objective (Kamath Eq 3.11)")
}

#' Prefix-tuning objective (Kamath Eq 3.12)
#'
#' A SUM of log p_phi(z_i | h_<i) over the target index set. `Y_idx` is
#' 0-BASED, as in Python.
#'
#' @param phi Function (z_i, h_prefix) -> probability. @param x Source.
#' @param y Target tokens. @param h Per-position activations.
#' @param Y_idx Optional 0-based positions.
#' @return List with `estimate`, `per_position`, `positions_scored`.
#' @export
morie_kamath_ch3_prefix_tuning_obj <- function(phi, x, y, h, Y_idx = NULL) {
  if (!is.function(phi)) stop("phi must be a function.", call. = FALSE)
  toks <- as.list(y); states <- as.list(h)
  if (length(toks) == 0L) stop("the target sequence y is empty.",
                               call. = FALSE)
  if (length(states) != length(toks)) {
    stop("h must exist at every scored position.", call. = FALSE)
  }
  idx <- if (is.null(Y_idx)) seq_along(toks) - 1L else as.integer(Y_idx)
  if (length(idx) == 0L) stop("Y_idx is empty.", call. = FALSE)
  if (anyDuplicated(idx)) stop("Y_idx contains duplicates.", call. = FALSE)
  if (any(idx < 0L | idx >= length(toks))) {
    stop("an index in Y_idx lies outside the target.", call. = FALSE)
  }
  logs <- vapply(idx, function(i) {
    p <- as.numeric(phi(toks[[i + 1L]],
                        if (i == 0L) list() else states[seq_len(i)]))
    if (!(p > 0 && p <= 1)) stop("phi must lie in (0, 1].", call. = FALSE)
    log(p)
  }, numeric(1))
  list(estimate = sum(logs), per_position = logs, positions_scored = idx,
       prompt = x, n = length(toks),
       method = "prefix-tuning objective (Kamath Eq 3.12)")
}

# ------------------------------------------------------------ Ch 4: PEFT

.morie_km2_adapter_core <- function(H_o, H_in, W_down, W_up, f) {
  H_o <- as.matrix(H_o); H_in <- as.matrix(H_in)
  Wd <- as.matrix(W_down); Wu <- as.matrix(W_up)
  if (ncol(H_in) != nrow(Wd)) stop("W_down does not fit the adapter input.",
                                   call. = FALSE)
  if (ncol(Wd) != nrow(Wu)) stop("the bottleneck disagrees.", call. = FALSE)
  if (ncol(Wu) != ncol(H_o)) stop("W_up does not return H_o's width.",
                                  call. = FALSE)
  if (nrow(H_in) != nrow(H_o)) stop("row counts differ.", call. = FALSE)
  if (!is.function(f)) stop("f must be a function.", call. = FALSE)
  delta <- f(H_in %*% Wd) %*% Wu
  list(H_o + delta, delta, ncol(Wd))
}

#' Bottleneck adapters (Kamath Eq 4.1-4.2)
#'
#' @details One core serves both Python modules exactly as km055 imports
#' km054's `_adapter_core`: km054 -> `morie_kamath_ch4_series_adapter`
#' (bottleneck fed by H_o), km055 -> `morie_kamath_ch4_parallel_adapter`
#' (fed by the layer input H_i). Passing H_i = H_o reproduces the series
#' adapter exactly.
#'
#' @param H_o Layer output. @param W_down,W_up Bottleneck factors.
#' @param f Activation; default ReLU.
#' @return List with `output`, `delta`, `bottleneck_rank`.
#' @export
morie_kamath_ch4_series_adapter <- function(H_o, W_down, W_up, f = NULL) {
  r <- .morie_km2_adapter_core(H_o, H_o, W_down, W_up,
                               if (is.null(f)) function(a) pmax(a, 0) else f)
  list(output = r[[1]], delta = r[[2]], bottleneck_rank = as.integer(r[[3]]),
       estimate = r[[1]][1, 1], n = nrow(r[[1]]),
       method = "series adapter (Kamath Eq 4.1)")
}

#' @rdname morie_kamath_ch4_series_adapter
#' @param H_i Layer input feeding the parallel branch.
#' @export
morie_kamath_ch4_parallel_adapter <- function(H_o, H_i, W_down, W_up,
                                              f = NULL) {
  r <- .morie_km2_adapter_core(H_o, H_i, W_down, W_up,
                               if (is.null(f)) function(a) pmax(a, 0) else f)
  list(output = r[[1]], delta = r[[2]], bottleneck_rank = as.integer(r[[3]]),
       estimate = r[[1]][1, 1], n = nrow(r[[1]]),
       method = "parallel adapter (Kamath Eq 4.2)")
}

.morie_km2_seq_obj <- function(model, x, y) {
  xs <- as.list(x); ys <- lapply(y, as.list)
  if (length(xs) == 0L) stop("Z is empty.", call. = FALSE)
  if (length(xs) != length(ys)) stop("contexts and targets differ.",
                                     call. = FALSE)
  if (!is.function(model)) stop("the model must be a function.", call. = FALSE)
  per <- vapply(seq_along(xs), function(i) {
    yi <- ys[[i]]
    if (length(yi) == 0L) stop("a target sequence is empty.", call. = FALSE)
    s <- 0
    for (t in seq_along(yi)) {
      p <- as.numeric(model(xs[[i]],
                            if (t == 1L) list() else yi[seq_len(t - 1L)],
                            yi[[t]]))
      if (!(p > 0 && p <= 1)) stop("probabilities must lie in (0, 1].",
                                   call. = FALSE)
      s <- s + log(p)
    }
    s
  }, numeric(1))
  list(sum(per), per)
}

#' Fine-tuning objectives (Kamath Eq 4.3-4.4)
#'
#' @details One core serves both Python modules exactly as km057 imports
#' km056's `_sequence_objective`: km056 ->
#' `morie_kamath_ch4_full_finetune_obj`, km057 ->
#' `morie_kamath_ch4_lora_obj` (the same sum scored under Phi_0 + dPhi).
#' Larger is better -- this is the objective, not a loss.
#'
#' @param Phi Function (x_i, y_prefix, y_t) -> probability.
#' @param x Contexts. @param y Target sequences.
#' @return List with `estimate`, `per_pair`, `n_tokens`.
#' @export
morie_kamath_ch4_full_finetune_obj <- function(Phi, x, y) {
  r <- .morie_km2_seq_obj(Phi, x, y)
  list(estimate = r[[1]], per_pair = r[[2]],
       n_tokens = as.integer(sum(lengths(lapply(y, as.list)))),
       n = length(r[[2]]),
       method = "full-parameter fine-tuning objective (Kamath Eq 4.3)")
}

#' @rdname morie_kamath_ch4_full_finetune_obj
#' @param Theta Adapted model. @param Phi_0 Frozen base model.
#' @export
morie_kamath_ch4_lora_obj <- function(Theta, Phi_0, x, y) {
  a <- .morie_km2_seq_obj(Theta, x, y)
  b <- .morie_km2_seq_obj(Phi_0, x, y)
  list(estimate = a[[1]], base_objective = b[[1]],
       improvement = a[[1]] - b[[1]], per_pair = a[[2]],
       base_per_pair = b[[2]], n = length(a[[2]]),
       method = "LoRA objective over Theta (Kamath Eq 4.4)")
}

#' LoRA forward pass (Kamath Eq 4.5)
#'
#' h = W_0 x + B A x; dW = BA has rank at most r, and that rank is reported.
#'
#' @param W_0 Frozen (d, k) weight. @param B (d, r). @param A (r, k).
#' @param x Input vector of length k.
#' @return List with `h`, `base`, `delta_h`, `r`, `delta_W_rank`.
#' @export
morie_kamath_ch4_lora_forward <- function(W_0, B, A, x) {
  W0 <- as.matrix(W_0); Bm <- as.matrix(B); Am <- as.matrix(A)
  xv <- as.numeric(x)
  if (ncol(W0) != length(xv)) stop("W_0 and x disagree.", call. = FALSE)
  if (ncol(Am) != length(xv)) stop("A and x disagree.", call. = FALSE)
  if (ncol(Bm) != nrow(Am)) stop("the rank dimension r must match.",
                                 call. = FALSE)
  if (nrow(Bm) != nrow(W0)) stop("B and W_0 disagree in outputs.",
                                 call. = FALSE)
  base <- as.numeric(W0 %*% xv)
  delta <- as.numeric(Bm %*% (Am %*% xv))
  list(h = base + delta, base = base, delta_h = delta,
       r = ncol(Bm), delta_W_rank = qr(Bm %*% Am)$rank,
       estimate = (base + delta)[1], n = length(xv),
       method = "LoRA forward pass (Kamath Eq 4.5)")
}

#' Kronecker product (Kamath Eq 4.6)
#'
#' W = A (x) B. rank(A (x) B) = rank(A) rank(B), so KronA can be full rank
#' at LoRA's parameter count; the rank is reported.
#'
#' @param A,B Matrices.
#' @return List with `W`, `shape`, `rank`, `n_params`.
#' @export
morie_kamath_ch4_kronecker_product <- function(A, B) {
  Am <- as.matrix(A); Bm <- as.matrix(B)
  if (length(Am) == 0L || length(Bm) == 0L) {
    stop("a Kronecker factor is empty.", call. = FALSE)
  }
  W <- kronecker(Am, Bm)
  list(W = W, shape = c(nrow(W), ncol(W)), rank = qr(W)$rank,
       n_params = length(Am) + length(Bm), estimate = W[1, 1],
       n = length(W), method = "Kronecker product (Kamath Eq 4.6)")
}

#' KronA matrix-free product (Kamath Eq 4.7)
#'
#' (A (x) B) x = gamma(B eta(x) A^T). Both folds are COLUMN-MAJOR, which is
#' R's native `matrix()` layout and what makes the identity hold.
#'
#' @param A,B Factors. @param x Vector of length ncol(A) * ncol(B).
#' @return List with `y`, `folded_shape`, `products_avoided`.
#' @export
morie_kamath_ch4_krona_efficient <- function(A, B, x) {
  Am <- as.matrix(A); Bm <- as.matrix(B); xv <- as.numeric(x)
  a1 <- nrow(Am); a2 <- ncol(Am); b1 <- nrow(Bm); b2 <- ncol(Bm)
  if (length(xv) != a2 * b2) stop("x does not match A (x) B's columns.",
                                  call. = FALSE)
  X <- matrix(xv, nrow = b2, ncol = a2)
  Y <- Bm %*% X %*% t(Am)
  list(y = as.numeric(Y), folded_shape = c(b2, a2), estimate = as.numeric(Y)[1],
       n = length(Y), products_avoided = a1 * a2 * b1 * b2,
       method = "KronA matrix-free product (Kamath Eq 4.7)")
}

.morie_km2_tuned <- function(W, A_k, B_k, s) {
  Wm <- as.matrix(W)
  K <- kronecker(as.matrix(A_k), as.matrix(B_k))
  if (!all(dim(K) == dim(Wm))) stop("the adapter cannot be merged.",
                                    call. = FALSE)
  s <- as.numeric(s)
  if (!is.finite(s)) stop("s must be finite.", call. = FALSE)
  list(Wm + s * K, Wm, K, s)
}

#' KronA layer output and merged weights (Kamath Eq 4.8-4.9)
#'
#' @details One merge serves both Python modules exactly as km061 imports
#' km062's `_tuned`: km062 -> `morie_kamath_ch4_krona_tuned_weights`
#' (W + s \[A_k (x) B_k\]), km061 -> `morie_kamath_ch4_krona_output`
#' (that merged weight applied to X). s = 0 returns W unchanged.
#'
#' @param W Base weight. @param A_k,B_k Kronecker factors. @param s Scale.
#' @return List with `W_tuned`, `delta`, `shape`.
#' @export
morie_kamath_ch4_krona_tuned_weights <- function(W, A_k, B_k, s) {
  r <- .morie_km2_tuned(W, A_k, B_k, s)
  list(W_tuned = r[[1]], delta = r[[4]] * r[[3]], s = r[[4]],
       shape = c(nrow(r[[1]]), ncol(r[[1]])), estimate = r[[1]][1, 1],
       n = length(r[[1]]),
       method = "merged KronA weights (Kamath Eq 4.9)")
}

#' @rdname morie_kamath_ch4_krona_tuned_weights
#' @param X Input rows.
#' @export
morie_kamath_ch4_krona_output <- function(X, W, A_k, B_k, s) {
  Xm <- as.matrix(X)
  r <- .morie_km2_tuned(W, A_k, B_k, s)
  if (ncol(Xm) != nrow(r[[2]])) stop("X and W disagree.", call. = FALSE)
  Y <- Xm %*% r[[1]]
  list(Y = Y, base = Xm %*% r[[2]], adapter_term = r[[4]] * (Xm %*% r[[3]]),
       s = r[[4]], estimate = Y[1, 1], n = nrow(Xm),
       method = "KronA layer output (Kamath Eq 4.8)")
}

.morie_km2_diag <- function(v, name, size) {
  v <- if (is.matrix(v)) {
    if (nrow(v) != ncol(v) || !isTRUE(all.equal(v, diag(diag(v)),
                                                check.attributes = FALSE))) {
      stop(sprintf("%s must be diagonal.", name), call. = FALSE)
    }
    diag(v)
  } else as.numeric(v)
  if (length(v) != size) stop(sprintf("%s has the wrong length.", name),
                              call. = FALSE)
  v
}

#' VeRA forward pass (Kamath Eq 4.10)
#'
#' h = W_0 x + Lambda_b B Lambda_d A x; only the two diagonals train.
#'
#' @param W_0 Frozen weight. @param Lambda_b,Lambda_d Diagonals.
#' @param A,B Frozen shared factors. @param x Input.
#' @return List with `h`, `delta_h`, `r`, `n_trainable`.
#' @export
morie_kamath_ch4_vera_forward <- function(W_0, Lambda_b, Lambda_d, A, B, x) {
  W0 <- as.matrix(W_0); Am <- as.matrix(A); Bm <- as.matrix(B)
  xv <- as.numeric(x)
  d <- nrow(W0); k <- ncol(W0)
  if (k != length(xv)) stop("W_0 and x disagree.", call. = FALSE)
  if (ncol(Am) != k) stop("A and W_0 disagree.", call. = FALSE)
  if (ncol(Bm) != nrow(Am)) stop("r must match.", call. = FALSE)
  if (nrow(Bm) != d) stop("B and W_0 disagree.", call. = FALSE)
  r <- nrow(Am)
  lb <- .morie_km2_diag(Lambda_b, "Lambda_b", d)
  ld <- .morie_km2_diag(Lambda_d, "Lambda_d", r)
  base <- as.numeric(W0 %*% xv)
  delta <- lb * as.numeric(Bm %*% (ld * as.numeric(Am %*% xv)))
  list(h = base + delta, base = base, delta_h = delta, r = r,
       n_trainable = d + r, n_trainable_lora = r * (d + k),
       estimate = (base + delta)[1], n = length(xv),
       method = "VeRA forward pass (Kamath Eq 4.10)")
}

#' LoftQ Frobenius objective (Kamath Eq 4.11)
#'
#' ||W - Q - A B^T||_F; smaller is better.
#'
#' @param W Target weight. @param Q Quantised weight. @param A,B Factors.
#' @return List with `estimate`, `residual`, `quantisation_error`, `r`.
#' @export
morie_kamath_ch4_loftq_objective <- function(W, Q, A, B) {
  Wm <- as.matrix(W); Qm <- as.matrix(Q)
  Am <- as.matrix(A); Bm <- as.matrix(B)
  if (!all(dim(Qm) == dim(Wm))) stop("Q and W differ in shape.", call. = FALSE)
  if (ncol(Am) != ncol(Bm)) stop("A B^T needs a shared rank dimension.",
                                 call. = FALSE)
  low <- Am %*% t(Bm)
  if (!all(dim(low) == dim(Wm))) stop("A B^T and W differ in shape.",
                                      call. = FALSE)
  resid <- Wm - Qm - low
  list(estimate = sqrt(sum(resid^2)), residual = resid,
       quantisation_error = sqrt(sum((Wm - Qm)^2)), r = ncol(Am),
       n = length(Wm), method = "LoftQ Frobenius objective (Kamath Eq 4.11)")
}

# ------------------------------------------------------- Ch 5: alignment

.morie_km2_bt_loss <- function(margins) {
  m <- as.numeric(margins)
  if (length(m) == 0L) stop("no preference pairs.", call. = FALSE)
  if (any(!is.finite(m))) stop("a reward margin is not finite.", call. = FALSE)
  per <- log1p(exp(-abs(m))) + pmax(-m, 0)      # logaddexp(0, -m)
  list(mean(per), per)
}

#' Pairwise reward-model losses (Kamath Eq 5.1, 5.3)
#'
#' -mean log sigmoid(r_chosen - r_rejected).
#'
#' @details One core serves both Python modules exactly as km067 calls
#' km065: km065 -> `morie_kamath_ch5_reward_loss_pairwise` (choice vector
#' `i`, 0 = y_0 wins), km067 -> `morie_kamath_ch5_rm_bradley_terry` (winner
#' fixed, so i is all zeros).
#'
#' @param r_theta Function (x, y) -> reward. @param x Prompts.
#' @param y_0,y_1 Response pairs. @param i Choice, 0 or 1 per pair.
#' @return List with `estimate`, `margins`, `per_pair`.
#' @export
morie_kamath_ch5_reward_loss_pairwise <- function(r_theta, x, y_0, y_1, i) {
  if (!is.function(r_theta)) stop("r_theta must be a function.", call. = FALSE)
  xs <- as.list(x); a <- as.list(y_0); b <- as.list(y_1)
  ch <- as.integer(i)
  if (!(length(xs) == length(a) && length(a) == length(b) &&
        length(b) == length(ch))) {
    stop("x, y_0, y_1 and i must have equal length.", call. = FALSE)
  }
  if (any(!(ch %in% c(0L, 1L)))) stop("every entry of i must be 0 or 1.",
                                      call. = FALSE)
  margins <- vapply(seq_along(xs), function(j) {
    chosen <- if (ch[j] == 0L) a[[j]] else b[[j]]
    rejected <- if (ch[j] == 0L) b[[j]] else a[[j]]
    as.numeric(r_theta(xs[[j]], chosen)) - as.numeric(r_theta(xs[[j]], rejected))
  }, numeric(1))
  r <- .morie_km2_bt_loss(margins)
  list(estimate = r[[1]], margins = margins, per_pair = r[[2]],
       n = length(xs),
       method = "pairwise reward-model loss (Kamath Eq 5.1)")
}

#' @rdname morie_kamath_ch5_reward_loss_pairwise
#' @param y_w,y_l Winning and losing responses.
#' @export
morie_kamath_ch5_rm_bradley_terry <- function(x, y_w, y_l, r_theta) {
  inner <- morie_kamath_ch5_reward_loss_pairwise(
    r_theta, x, y_w, y_l, rep(0L, length(as.list(x))))
  list(estimate = inner$estimate, margins = inner$margins,
       per_pair = inner$per_pair, n = inner$n,
       method = "Bradley-Terry reward-model loss (Kamath Eq 5.3)")
}

#' KL-penalised RLHF reward (Kamath Eq 5.2)
#'
#' R = r_theta(x, y) - beta log(pi_RL / pi_SFT).
#'
#' @param x,y Prompt and response. @param pi_RL,pi_SFT Probabilities in (0, 1\].
#' @param beta Penalty weight. @param r_theta Reward or function (x, y).
#' @return List with `estimate`, `penalised_reward`, `penalty`.
#' @export
morie_kamath_ch5_reward_kl_penalty <- function(x, y, pi_RL, pi_SFT, beta,
                                               r_theta = NULL) {
  if (is.null(r_theta)) stop("r_theta is required.", call. = FALSE)
  beta <- as.numeric(beta)
  if (beta < 0) stop("beta must be non-negative.", call. = FALSE)
  pos <- function(v, nm) {
    p <- as.numeric(v)
    if (length(p) == 0L) stop(sprintf("%s is empty.", nm), call. = FALSE)
    if (any(p <= 0 | p > 1)) stop(sprintf("%s must lie in (0, 1].", nm),
                                  call. = FALSE)
    p
  }
  p_rl <- pos(pi_RL, "pi_RL"); p_sft <- pos(pi_SFT, "pi_SFT")
  if (length(p_rl) != length(p_sft)) stop("pi_RL and pi_SFT differ.",
                                          call. = FALSE)
  r <- if (is.function(r_theta)) as.numeric(r_theta(x, y)) else as.numeric(r_theta)
  penalty <- beta * log(p_rl / p_sft)
  R <- r - penalty
  list(estimate = if (length(R) == 1L) R[1] else mean(R),
       penalised_reward = R, raw_reward = r, penalty = penalty,
       beta = beta, n = length(R),
       method = "KL-penalised RLHF reward (Kamath Eq 5.2)")
}

#' RLHF objective and PPO loss (Kamath Eq 5.4-5.5)
#'
#' E\[r\] - beta KL(pi_theta || pi_ref), maximised.
#'
#' @details One core serves both Python modules exactly as km068 calls
#' km069 per prompt and negates the mean: km069 ->
#' `morie_kamath_ch5_rlhf_objective`, km068 -> `morie_kamath_ch5_ppo_loss`.
#'
#' @param pi_theta,pi_ref Response distributions. @param r_phi Rewards.
#' @param beta KL weight.
#' @return List with `estimate`, `expected_reward`, `kl`, `penalty`.
#' @export
morie_kamath_ch5_rlhf_objective <- function(pi_theta, pi_ref, r_phi, beta) {
  p <- .morie_km2_dist(pi_theta, "pi_theta")
  q <- .morie_km2_dist(pi_ref, "pi_ref")
  if (length(p) != length(q)) stop("pi_theta and pi_ref differ in size.",
                                   call. = FALSE)
  r <- if (is.function(r_phi)) {
    if (is.null(names(pi_theta))) {
      stop("a function r_phi needs named responses.", call. = FALSE)
    }
    vapply(names(pi_theta), function(k) as.numeric(r_phi(k)), numeric(1))
  } else as.numeric(r_phi)
  if (length(r) != length(p)) stop("r_phi and pi_theta differ.", call. = FALSE)
  beta <- as.numeric(beta)
  if (beta < 0) stop("beta must be non-negative.", call. = FALSE)
  if (any(q <= 0 & p > 0)) {
    stop("pi_ref assigns zero probability where pi_theta does not.",
         call. = FALSE)
  }
  nz <- p > 0
  kl <- sum(p[nz] * log(p[nz] / q[nz]))
  exp_r <- sum(p * r)
  list(estimate = exp_r - beta * kl, expected_reward = exp_r, kl = kl,
       penalty = beta * kl, beta = beta, n = length(p),
       method = "RLHF objective (Kamath Eq 5.5)")
}

#' @rdname morie_kamath_ch5_rlhf_objective
#' @param phi List of per-prompt policy distributions.
#' @param x Prompts. @param y Per-prompt response name vectors.
#' @param r_theta Function (x, y) -> reward.
#' @export
morie_kamath_ch5_ppo_loss <- function(phi, x, y, r_theta, beta,
                                      pi_ref = NULL) {
  if (is.null(pi_ref)) stop("pi_ref is required.", call. = FALSE)
  pol <- as.list(phi); refs <- as.list(pi_ref)
  xs <- as.list(x); ys <- as.list(y)
  if (length(xs) == 0L) stop("no prompts.", call. = FALSE)
  if (!(length(pol) == length(refs) && length(refs) == length(xs) &&
        length(xs) == length(ys))) {
    stop("phi, pi_ref, x and y must have equal length.", call. = FALSE)
  }
  if (!is.function(r_theta)) stop("r_theta must be a function.", call. = FALSE)
  per <- lapply(seq_along(xs), function(i) {
    rewards <- vapply(as.list(ys[[i]]),
                      function(resp) as.numeric(r_theta(xs[[i]], resp)),
                      numeric(1))
    morie_kamath_ch5_rlhf_objective(pol[[i]], refs[[i]], rewards, beta)
  })
  obj <- vapply(per, function(o) o$estimate, numeric(1))
  list(estimate = -mean(obj), per_prompt_objective = obj,
       kl = vapply(per, function(o) o$kl, numeric(1)),
       expected_reward = vapply(per, function(o) o$expected_reward,
                                numeric(1)),
       beta = as.numeric(beta), n = length(xs),
       method = "PPO loss = -mean RLHF objective (Kamath Eq 5.4)")
}

#' Optimal KL-regularised policy (Kamath Eq 5.6)
#'
#' pi_r = pi_ref exp(r/beta) / Z. Z is computed, and a supplied Z is checked
#' rather than trusted.
#'
#' @param pi_ref Reference distribution. @param r Rewards.
#' @param beta Strictly positive. @param Z Optional partition function.
#' @return List with `pi`, `Z`, `argmax` (0-based).
#' @export
morie_kamath_ch5_rlhf_optimal_policy <- function(pi_ref, r, beta, Z = NULL) {
  q <- as.numeric(pi_ref); rv <- as.numeric(r)
  if (length(q) == 0L) stop("pi_ref is empty.", call. = FALSE)
  if (any(q < 0) || abs(sum(q) - 1) > 1e-8) {
    stop("pi_ref must be a distribution.", call. = FALSE)
  }
  if (length(rv) != length(q)) stop("r and pi_ref differ.", call. = FALSE)
  beta <- as.numeric(beta)
  if (beta <= 0) stop("beta must be strictly positive.", call. = FALSE)
  w <- q * exp(rv / beta)
  Zh <- sum(w)
  if (Zh <= 0) stop("the partition function is 0.", call. = FALSE)
  if (!is.null(Z) && abs(as.numeric(Z) - Zh) > 1e-8 * max(1, Zh)) {
    stop("the supplied Z does not normalise.", call. = FALSE)
  }
  p <- w / Zh
  list(pi = p, Z = Zh, beta = beta, estimate = max(p),
       argmax = which.max(p) - 1L, n = length(p),
       method = "optimal KL-regularised policy (Kamath Eq 5.6)")
}

#' Reward implied by an optimal policy (Kamath Eq 5.7)
#'
#' r* = beta log(pi*/pi_ref) + beta log Z.
#'
#' @param pi_star,pi_ref Distributions with entries in (0, 1\].
#' @param beta Strictly positive. @param Z Partition function; default 1.
#' @return List with `r`, `log_ratio`, `offset`.
#' @export
morie_kamath_ch5_dpo_reward_optimal <- function(pi_star, pi_ref, beta,
                                                Z = NULL) {
  beta <- as.numeric(beta)
  if (beta <= 0) stop("beta must be strictly positive.", call. = FALSE)
  p <- as.numeric(pi_star); q <- as.numeric(pi_ref)
  if (length(p) == 0L || length(q) == 0L) stop("empty policy.", call. = FALSE)
  if (length(p) != length(q)) stop("pi_star and pi_ref differ.", call. = FALSE)
  if (any(p <= 0 | q <= 0 | p > 1 | q > 1)) {
    stop("every probability must lie in (0, 1].", call. = FALSE)
  }
  Zv <- if (is.null(Z)) 1 else as.numeric(Z)
  if (Zv <= 0) stop("Z must be strictly positive.", call. = FALSE)
  logs <- log(p / q)
  r <- beta * logs + beta * log(Zv)
  list(r = r, log_ratio = logs, beta = beta, Z = Zv,
       offset = beta * log(Zv), estimate = r[1], n = length(r),
       method = "reward implied by an optimal policy (Kamath Eq 5.7)")
}

#' Bradley-Terry preference probability (Kamath Eq 5.8-5.9)
#'
#' @details One sigmoid serves both Python modules exactly as km073 calls
#' km072: km072 -> `morie_kamath_ch5_bradley_terry_pref` (named or function
#' rewards), km073 -> `morie_kamath_ch5_pref_sigmoid_form` (a bare pair).
#'
#' @param r_star Named reward vector or function. @param y_w,y_l Responses.
#' @return List with `estimate`, `margin`, `r_w`, `r_l`.
#' @export
morie_kamath_ch5_bradley_terry_pref <- function(r_star, y_w, y_l) {
  if (is.function(r_star)) {
    rw <- as.numeric(r_star(y_w)); rl <- as.numeric(r_star(y_l))
  } else {
    if (!all(c(y_w, y_l) %in% names(r_star))) {
      stop("a response has no reward in r_star.", call. = FALSE)
    }
    rw <- as.numeric(r_star[[y_w]]); rl <- as.numeric(r_star[[y_l]])
  }
  if (!is.finite(rw) || !is.finite(rl)) stop("a reward is not finite.",
                                             call. = FALSE)
  p <- .morie_km2_sig(rw - rl)
  list(estimate = p, margin = rw - rl, r_w = rw, r_l = rl,
       p_reversed = 1 - p, n = 2L,
       method = "Bradley-Terry preference probability (Kamath Eq 5.8)")
}

#' @rdname morie_kamath_ch5_bradley_terry_pref
#' @export
morie_kamath_ch5_pref_sigmoid_form <- function(r_star) {
  vals <- if (!is.null(names(r_star)) && all(c("y_w", "y_l") %in% names(r_star)))
    c(as.numeric(r_star[["y_w"]]), as.numeric(r_star[["y_l"]]))
  else as.numeric(unlist(r_star))
  if (length(vals) != 2L) stop("r_star must hold exactly two rewards.",
                               call. = FALSE)
  inner <- morie_kamath_ch5_bradley_terry_pref(
    c(y_w = vals[1], y_l = vals[2]), "y_w", "y_l")
  list(estimate = inner$estimate, margin = inner$margin,
       r_w = vals[1], r_l = vals[2], n = 2L,
       method = "preference as sigmoid of the margin (Kamath Eq 5.9)")
}

.morie_km2_implicit_rewards <- function(pi_star, pi_ref, beta) {
  beta <- as.numeric(beta)
  if (beta <= 0) stop("beta must be strictly positive.", call. = FALSE)
  p <- as.numeric(pi_star); q <- as.numeric(pi_ref)
  if (length(p) != 2L || length(q) != 2L) {
    stop("pi_star and pi_ref must each hold exactly two probabilities.",
         call. = FALSE)
  }
  if (any(c(p, q) <= 0 | c(p, q) > 1)) {
    stop("every probability must lie in (0, 1].", call. = FALSE)
  }
  c(beta * log(p[1] / q[1]), beta * log(p[2] / q[2]), beta)
}

#' DPO preference probability (Kamath Eq 5.10-5.11)
#'
#' @details km075 -> `morie_kamath_ch5_dpo_pref_simplified` (Z cancelled),
#' km074 -> `morie_kamath_ch5_dpo_pref_substituted`, which carries Z
#' explicitly and refuses to return unless the beta log Z terms cancel --
#' the same assertion the Python module makes.
#'
#' @param pi_star,pi_ref Two-response policies. @param beta Positive.
#' @return List with `estimate`, `margin`, implicit rewards.
#' @export
morie_kamath_ch5_dpo_pref_simplified <- function(pi_star, pi_ref, beta) {
  r <- .morie_km2_implicit_rewards(pi_star, pi_ref, beta)
  inner <- morie_kamath_ch5_pref_sigmoid_form(c(r[1], r[2]))
  list(estimate = inner$estimate, margin = inner$margin,
       implicit_reward_w = r[1], implicit_reward_l = r[2], beta = r[3],
       n = 2L, method = "DPO preference, Z cancelled (Kamath Eq 5.11)")
}

#' @rdname morie_kamath_ch5_dpo_pref_simplified
#' @param Z Partition function carried through; default 1.
#' @export
morie_kamath_ch5_dpo_pref_substituted <- function(pi_star, pi_ref, beta,
                                                  Z = NULL) {
  r <- .morie_km2_implicit_rewards(pi_star, pi_ref, beta)
  Zv <- if (is.null(Z)) 1 else as.numeric(Z)
  if (Zv <= 0) stop("Z must be strictly positive.", call. = FALSE)
  off <- r[3] * log(Zv)
  inner <- morie_kamath_ch5_pref_sigmoid_form(c(r[1] + off, r[2] + off))
  simple <- morie_kamath_ch5_dpo_pref_simplified(pi_star, pi_ref, beta)
  cancels <- abs(inner$estimate - simple$estimate) < 1e-12
  if (!cancels) stop("the beta log Z terms failed to cancel.", call. = FALSE)
  list(estimate = inner$estimate, margin = inner$margin, z_offset = off,
       z_terms_cancel = cancels, simplified = simple$estimate,
       beta = r[3], Z = Zv, n = 2L,
       method = "DPO preference with Z carried explicitly (Kamath Eq 5.10)")
}

#' DPO loss (Kamath Eq 5.12)
#'
#' -mean log sigmoid of the beta-scaled log-ratio margin.
#'
#' @param pi_theta,pi_ref Lists of two-response policies. @param beta Positive.
#' @return List with `estimate`, `margins`, implicit rewards.
#' @export
morie_kamath_ch5_dpo_loss <- function(pi_theta, pi_ref, beta) {
  pt <- as.list(pi_theta); pr <- as.list(pi_ref)
  if (length(pt) == 0L) stop("no preference pairs.", call. = FALSE)
  if (length(pt) != length(pr)) stop("pi_theta and pi_ref differ.",
                                     call. = FALSE)
  rs <- lapply(seq_along(pt), function(i)
    .morie_km2_implicit_rewards(pt[[i]], pr[[i]], beta))
  rw <- vapply(rs, function(v) v[1], numeric(1))
  rl <- vapply(rs, function(v) v[2], numeric(1))
  r <- .morie_km2_bt_loss(rw - rl)
  list(estimate = r[[1]], margins = rw - rl, per_pair = r[[2]],
       implicit_reward_w = rw, implicit_reward_l = rl,
       beta = as.numeric(beta), n = length(rw),
       method = "DPO loss (Kamath Eq 5.12)")
}

# --------------------------------------------- Ch 6: bias, toxicity, PII

#' FActScore (Kamath Eq 6.1)
#'
#' Mean supported-fact fraction over the prompts the model ANSWERED; a NULL
#' response is skipped, not scored 0.
#'
#' @param M Function prompt -> response or NULL. @param X Prompts.
#' @param A_y Function response -> atomic facts.
#' @param C Support predicate or a vector of supported facts.
#' @return List with `estimate`, `per_prompt`, `response_rate`.
#' @export
morie_kamath_ch6_factscore <- function(M, X, A_y, C) {
  prompts <- as.list(X)
  if (length(prompts) == 0L) stop("X is empty.", call. = FALSE)
  if (!is.function(M)) stop("M must be a function.", call. = FALSE)
  if (!is.function(A_y)) stop("A_y must be a function.", call. = FALSE)
  supported <- if (is.function(C)) C else function(a) a %in% C
  per <- c()
  for (x in prompts) {
    resp <- M(x)
    if (is.null(resp)) next
    facts <- as.list(A_y(resp))
    if (length(facts) == 0L) stop("a response yielded no atomic facts.",
                                  call. = FALSE)
    per <- c(per, mean(vapply(facts,
                              function(a) as.numeric(isTRUE(supported(a))),
                              numeric(1))))
  }
  if (length(per) == 0L) stop("the model responded to no prompt.",
                              call. = FALSE)
  list(estimate = mean(per), per_prompt = per, n_responded = length(per),
       response_rate = length(per) / length(prompts), n = length(prompts),
       method = "FActScore (Kamath Eq 6.1)")
}

#' AlignScore alignment function (Kamath Eq 6.2)
#'
#' f: (a, b) -> y with the label space ENFORCED. `y` is "bin", "3way" or "reg".
#'
#' @param a,b Context and claim. @param y Output space. @param f Head.
#' @return List with `estimate`, `label`, `space`, `labels`.
#' @export
morie_kamath_ch6_alignment_function <- function(a, b, y, f = NULL) {
  SPACES <- list(bin = c("ALIGNED", "NOT ALIGNED"),
                 `3way` = c("ALIGNED", "CONTRADICT", "NEUTRAL"))
  if (is.null(f)) stop("f is required; Eq 6.2 defines the shape only.",
                       call. = FALSE)
  if (!is.function(f)) stop("f must be a function.", call. = FALSE)
  if (!(y %in% names(SPACES)) && y != "reg") {
    stop("unknown output space; use 'bin', '3way' or 'reg'.", call. = FALSE)
  }
  out <- f(a, b)
  if (y == "reg") {
    v <- as.numeric(out)
    if (v < 0 || v > 1) stop("y_reg lies in [0, 1].", call. = FALSE)
    label <- NULL; est <- v
  } else {
    if (!(out %in% SPACES[[y]])) stop("the label is outside the space.",
                                      call. = FALSE)
    label <- out; est <- as.numeric(out == "ALIGNED")
  }
  list(estimate = est, label = label, space = y,
       labels = if (y %in% names(SPACES)) SPACES[[y]] else NULL,
       a = a, b = b, n = 2L,
       method = "AlignScore alignment function (Kamath Eq 6.2)")
}

#' AlignScore joint loss (Kamath Eq 6.3)
#'
#' lam1 L_3way + lam2 L_bin + lam3 L_reg.
#'
#' @param L_3way,L_bin,L_reg Component losses. @param lambdas Three weights.
#' @return List with `estimate`, `contributions`.
#' @export
morie_kamath_ch6_alignscore_total_loss <- function(L_3way, L_bin, L_reg,
                                                   lambdas) {
  lam <- as.numeric(lambdas)
  if (length(lam) != 3L) stop("lambdas must hold three weights.",
                              call. = FALSE)
  if (any(lam < 0) || any(!is.finite(lam))) {
    stop("every weight must be finite and non-negative.", call. = FALSE)
  }
  losses <- c(as.numeric(L_3way), as.numeric(L_bin), as.numeric(L_reg))
  if (any(!is.finite(losses))) stop("every loss must be finite.",
                                    call. = FALSE)
  list(estimate = sum(lam * losses), contributions = lam * losses,
       losses = losses, lambdas = lam, n = 3L,
       method = "AlignScore joint loss (Kamath Eq 6.3)")
}

.morie_km2_cos_mean <- function(a, W, name) {
  A <- as.matrix(W)
  if (nrow(A) == 0L) stop(sprintf("%s is empty.", name), call. = FALSE)
  nw <- sqrt(rowSums(A^2))
  if (any(nw == 0)) stop(sprintf("%s contains a zero vector.", name),
                         call. = FALSE)
  na <- sqrt(sum(a^2))
  if (na == 0) stop("a is a zero vector.", call. = FALSE)
  if (ncol(A) != length(a)) stop(sprintf("%s and a differ in width.", name),
                                 call. = FALSE)
  mean(as.numeric(A %*% a) / (nw * na))
}

.morie_km2_weat_s <- function(a, W_1, W_2) {
  a <- as.numeric(a)
  .morie_km2_cos_mean(a, W_1, "W_1") - .morie_km2_cos_mean(a, W_2, "W_2")
}

.morie_km2_weat_sums <- function(A_1, A_2, W_1, W_2) {
  a1 <- as.matrix(A_1); a2 <- as.matrix(A_2)
  if (nrow(a1) == 0L || nrow(a2) == 0L) {
    stop("A_1 and A_2 must each contain an attribute word.", call. = FALSE)
  }
  if (ncol(a1) != ncol(a2)) stop("A_1 and A_2 differ in width.", call. = FALSE)
  list(apply(a1, 1, .morie_km2_weat_s, W_1 = W_1, W_2 = W_2),
       apply(a2, 1, .morie_km2_weat_s, W_1 = W_1, W_2 = W_2))
}

#' WEAT differential association (Kamath Eq 6.5)
#'
#' s(a) = mean cos(a, W_1) - mean cos(a, W_2).
#'
#' @param a Attribute vector. @param W_1,W_2 Target word matrices.
#' @return List with `estimate`, `mean_cos_W1`, `mean_cos_W2`.
#' @export
morie_kamath_ch6_weat_similarity <- function(a, W_1, W_2) {
  av <- as.numeric(a)
  m1 <- .morie_km2_cos_mean(av, W_1, "W_1")
  m2 <- .morie_km2_cos_mean(av, W_2, "W_2")
  list(estimate = m1 - m2, mean_cos_W1 = m1, mean_cos_W2 = m2,
       n = length(av),
       method = "WEAT differential association s (Kamath Eq 6.5)")
}

#' WEAT test statistic (Kamath Eq 6.4)
#'
#' sum s over A_1 minus sum s over A_2.
#'
#' @param A_1,A_2 Attribute-word matrices. @param W_1,W_2 Target matrices.
#' @return List with `estimate`, `s_A1`, `s_A2`.
#' @export
morie_kamath_ch6_weat_function <- function(A_1, A_2, W_1, W_2) {
  s <- .morie_km2_weat_sums(A_1, A_2, W_1, W_2)
  list(estimate = sum(s[[1]]) - sum(s[[2]]), s_A1 = s[[1]], s_A2 = s[[2]],
       n = length(s[[1]]) + length(s[[2]]),
       method = "WEAT test statistic (Kamath Eq 6.4)")
}

#' WEAT effect size (Kamath Eq 6.6)
#'
#' (mean s over A_1 - mean s over A_2) / sd over the union. `ddof = 0` is
#' numpy's default, i.e. the POPULATION standard deviation, not R's `sd()`.
#'
#' @param A_1,A_2 Attribute matrices. @param W_1,W_2 Target matrices.
#' @param ddof Degrees of freedom subtracted; 0 = population.
#' @return List with `estimate`, `numerator`, `std`.
#' @export
morie_kamath_ch6_weat_effect_size <- function(A_1, A_2, W_1, W_2, ddof = 0) {
  s <- .morie_km2_weat_sums(A_1, A_2, W_1, W_2)
  union <- c(s[[1]], s[[2]])
  ddof <- as.integer(ddof)
  if (length(union) - ddof <= 0L) stop("too few attribute words for ddof.",
                                       call. = FALSE)
  sdv <- sqrt(sum((union - mean(union))^2) / (length(union) - ddof))
  if (sdv == 0) stop("every attribute word has the same association.",
                     call. = FALSE)
  num <- mean(s[[1]]) - mean(s[[2]])
  list(estimate = num / sdv, numerator = num, std = sdv, ddof = ddof,
       s_A1 = s[[1]], s_A2 = s[[2]], n = length(union),
       method = "WEAT effect size (Kamath Eq 6.6)")
}

#' CEAT random-effects pooled WEAT (Kamath Eq 6.7)
#'
#' Weighted mean of per-sample WEAT effect sizes.
#'
#' @param S_A1,S_A2,S_W1,S_W2 Lists of per-sample matrices. @param v Weights.
#' @param ddof Passed to the effect size.
#' @return List with `estimate`, `weat`, `weights`.
#' @export
morie_kamath_ch6_ceat_random_effects <- function(S_A1, S_A2, S_W1, S_W2, v,
                                                 ddof = 0) {
  N <- length(S_A1)
  if (N == 0L) stop("no samples.", call. = FALSE)
  if (length(S_A2) != N || length(S_W1) != N || length(S_W2) != N) {
    stop("the four sample lists must hold the same number of samples.",
         call. = FALSE)
  }
  w <- as.numeric(v)
  if (length(w) != N) stop("v has the wrong length.", call. = FALSE)
  if (any(w < 0) || any(!is.finite(w))) {
    stop("every weight must be finite and non-negative.", call. = FALSE)
  }
  if (sum(w) == 0) stop("the weights sum to 0.", call. = FALSE)
  eff <- vapply(seq_len(N), function(i)
    morie_kamath_ch6_weat_effect_size(S_A1[[i]], S_A2[[i]], S_W1[[i]],
                                      S_W2[[i]], ddof = ddof)$estimate,
    numeric(1))
  list(estimate = sum(w * eff) / sum(w), weat = eff, weights = w, n = N,
       method = "CEAT random-effects pooled WEAT (Kamath Eq 6.7)")
}

#' Log-Probability Bias Score (Kamath Eq 6.8)
#'
#' Difference of prior-normalised log probabilities.
#'
#' @param p_a,p_prior Two-entry probability vectors in (0, 1\].
#' @return List with `estimate`, `normalised_log_i`, `normalised_log_j`.
#' @export
morie_kamath_ch6_lpbs_bias <- function(p_a, p_prior) {
  pair <- function(p, nm) {
    v <- as.numeric(p)
    if (length(v) != 2L) stop(sprintf("%s must hold two probabilities.", nm),
                              call. = FALSE)
    if (any(v <= 0 | v > 1)) stop(sprintf("%s must lie in (0, 1].", nm),
                                  call. = FALSE)
    v
  }
  logs <- log(pair(p_a, "p_a") / pair(p_prior, "p_prior"))
  list(estimate = logs[1] - logs[2], normalised_log_i = logs[1],
       normalised_log_j = logs[2], n = 2L,
       method = "Log-Probability Bias Score (Kamath Eq 6.8)")
}

#' Categorical Bias Score (Kamath Eq 6.9)
#'
#' Mean over template words of the variance over groups of log(p / prior).
#' `ddof = 0` is numpy's POPULATION variance.
#'
#' @param W Template words. @param A Social groups.
#' @param p_a,p_prior |W| x |A| probability matrices. @param ddof Default 0.
#' @return List with `estimate`, `per_word`, `log_ratios`.
#' @export
morie_kamath_ch6_cbs_variance <- function(W, A, p_a, p_prior, ddof = 0) {
  words <- as.list(W); attrs <- as.list(A)
  if (length(words) == 0L) stop("W is empty.", call. = FALSE)
  if (length(attrs) < 2L) stop("A needs at least two social groups.",
                               call. = FALSE)
  pa <- as.matrix(p_a); pp <- as.matrix(p_prior)
  if (!all(dim(pa) == c(length(words), length(attrs)))) {
    stop("p_a is not |W| x |A|.", call. = FALSE)
  }
  if (!all(dim(pp) == dim(pa))) stop("p_prior and p_a differ.", call. = FALSE)
  if (any(pa <= 0 | pp <= 0 | pa > 1 | pp > 1)) {
    stop("every probability must lie in (0, 1].", call. = FALSE)
  }
  ddof <- as.integer(ddof)
  if (length(attrs) - ddof <= 0L) stop("ddof leaves no degrees of freedom.",
                                       call. = FALSE)
  logs <- log(pa / pp)
  per_word <- apply(logs, 1, function(r) sum((r - mean(r))^2) /
                      (length(r) - ddof))
  list(estimate = mean(per_word), per_word = per_word, log_ratios = logs,
       ddof = ddof, n = length(words),
       method = "Categorical Bias Score (Kamath Eq 6.9)")
}

.morie_km2_log_probs <- function(items, scorer, name) {
  seqv <- as.list(items)
  if (length(seqv) == 0L) stop(sprintf("%s is empty.", name), call. = FALSE)
  vapply(seq_along(seqv), function(i) {
    p <- if (is.null(scorer)) as.numeric(seqv[[i]]) else as.numeric(scorer(i - 1L))
    if (!(p > 0 && p <= 1)) stop("a conditional probability is outside (0, 1].",
                                 call. = FALSE)
    log(p)
  }, numeric(1))
}

#' Pseudo-log-likelihood and CrowS-Pairs (Kamath Eq 6.10-6.11)
#'
#' @details One core serves both Python modules exactly as km087 calls
#' km086: km086 -> `morie_kamath_ch6_pll` (sum of log P(token | rest)),
#' km087 -> `morie_kamath_ch6_cps_metric` (the same sum over the unmodified
#' tokens, conditioned on the modified ones). `theta` receives 0-BASED
#' positions.
#'
#' @param S Token probabilities, or tokens when `theta` is given.
#' @param theta Optional function of a 0-based index.
#' @return List with `estimate`, `per_token`.
#' @export
morie_kamath_ch6_pll <- function(S, theta = NULL) {
  if (!is.null(theta) && !is.function(theta)) {
    stop("theta must be a function.", call. = FALSE)
  }
  logs <- .morie_km2_log_probs(S, theta, "S")
  list(estimate = sum(logs), per_token = logs, n = length(logs),
       method = "pseudo-log-likelihood (Kamath Eq 6.10)")
}

#' @rdname morie_kamath_ch6_pll
#' @param U Unmodified tokens. @param M Modified tokens.
#' @export
morie_kamath_ch6_cps_metric <- function(U, M, theta = NULL) {
  toks <- as.list(U); mod <- as.list(M)
  if (length(mod) == 0L) stop("M is empty.", call. = FALSE)
  probs <- if (is.null(theta)) toks else {
    if (!is.function(theta)) stop("theta must be a function.", call. = FALSE)
    lapply(seq_along(toks) - 1L, function(i) theta(toks, mod, i))
  }
  inner <- morie_kamath_ch6_pll(probs)
  list(estimate = inner$estimate, per_token = inner$per_token,
       n_modified = length(mod), n = inner$n,
       method = "CrowS-Pairs Score (Kamath Eq 6.11)")
}

#' Context Association Test score (Kamath Eq 6.12)
#'
#' MEAN log P(modified token | unmodified context) -- a mean, not the sum
#' that Eq 6.10 takes.
#'
#' @param M Modified tokens. @param U Unmodified context.
#' @param theta Optional function (M, U, i) with i 0-BASED.
#' @return List with `estimate`, `per_token`, `n_context`.
#' @export
morie_kamath_ch6_cat_metric <- function(M, U, theta = NULL) {
  ctx <- as.list(U)
  if (length(ctx) == 0L) stop("U is empty.", call. = FALSE)
  if (!is.null(theta) && !is.function(theta)) {
    stop("theta must be a function.", call. = FALSE)
  }
  toks <- as.list(M)
  scorer <- if (is.null(theta)) NULL else function(i) theta(toks, ctx, i)
  logs <- .morie_km2_log_probs(toks, scorer, "M")
  list(estimate = mean(logs), per_token = logs, n_context = length(ctx),
       n = length(logs),
       method = "Context Association Test score (Kamath Eq 6.12)")
}

#' Social Group Substitution invariance (Kamath Eq 6.13)
#'
#' psi(original, counterfactual); 1 means unchanged.
#'
#' @param Yhat_i,Yhat_j Paired outputs. @param psi Optional comparator.
#' @return List with `estimate`, `per_pair`, `n_invariant`.
#' @export
morie_kamath_ch6_sgs_invariance <- function(Yhat_i, Yhat_j, psi = NULL) {
  match_fn <- if (is.null(psi)) function(u, v) as.numeric(identical(u, v)) else psi
  if (!is.function(match_fn)) stop("psi must be a function.", call. = FALSE)
  pairs <- if (is.character(Yhat_i) && length(Yhat_i) == 1L) {
    list(list(Yhat_i, Yhat_j))
  } else {
    a <- as.list(Yhat_i); b <- as.list(Yhat_j)
    if (length(a) == 0L) stop("no outputs to compare.", call. = FALSE)
    if (length(a) != length(b)) stop("the outputs must be paired.",
                                     call. = FALSE)
    lapply(seq_along(a), function(i) list(a[[i]], b[[i]]))
  }
  vals <- vapply(pairs, function(p) {
    r <- as.numeric(match_fn(p[[1]], p[[2]]))
    if (r < 0 || r > 1) stop("an invariance metric lies in [0, 1].",
                             call. = FALSE)
    r
  }, numeric(1))
  list(estimate = mean(vals), per_pair = vals,
       n_invariant = as.integer(sum(vals == 1)), n = length(vals),
       method = "Social Group Substitution invariance (Kamath Eq 6.13)")
}

#' Co-Occurrence Bias Score (Kamath Eq 6.14)
#'
#' log P(w|A_i) / P(w|A_j) over generated text.
#'
#' @param w Word. @param A_i,A_j Named count vectors or token corpora.
#' @return List with `estimate`, `p_given_Ai`, `p_given_Aj`.
#' @export
morie_kamath_ch6_co_occurrence_bias <- function(w, A_i, A_j) {
  cond <- function(A, name) {
    counts <- if (!is.null(names(A))) {
      stats::setNames(as.numeric(A), names(A))
    } else {
      toks <- unlist(lapply(as.list(A), function(it)
        if (is.character(it)) strsplit(trimws(it), "\\s+")[[1]] else it))
      tb <- table(toks)
      stats::setNames(as.numeric(tb), names(tb))
    }
    total <- sum(counts)
    if (total <= 0) stop(sprintf("%s contains no tokens.", name),
                         call. = FALSE)
    c_w <- if (w %in% names(counts)) as.numeric(counts[[w]]) else 0
    if (c_w <= 0) stop("the word never co-occurs; log 0 is undefined.",
                       call. = FALSE)
    c(c_w / total, c_w, total)
  }
  i <- cond(A_i, "A_i"); j <- cond(A_j, "A_j")
  list(estimate = log(i[1] / j[1]), p_given_Ai = i[1], p_given_Aj = j[1],
       count_Ai = i[2], count_Aj = j[2], n = as.integer(i[3] + j[3]),
       method = "Co-Occurrence Bias Score (Kamath Eq 6.14)")
}

.morie_km2_tokens <- function(Y) {
  if (is.character(Y) && length(Y) == 1L) strsplit(trimws(Y), "\\s+")[[1]]
  else unlist(Y)
}

.morie_km2_count_word <- function(word, outputs) {
  sum(vapply(outputs, function(Y) sum(.morie_km2_tokens(Y) == word),
             numeric(1)))
}

#' Demographic representation and stereotypical association (Eq 6.15-6.16)
#'
#' @details One counter serves both Python modules exactly as km092 imports
#' km091's `_count`/`_tokens`: km091 ->
#' `morie_kamath_ch6_demographic_representation` (counts in every output),
#' km092 -> `morie_kamath_ch6_stereotypical_assoc` (counts only in outputs
#' that mention w).
#'
#' @param G_i Group label. @param A_i Attribute words. @param Yhat Outputs.
#' @return List with `estimate`, `per_word`, `share_of_tokens`.
#' @export
morie_kamath_ch6_demographic_representation <- function(G_i, A_i, Yhat) {
  words <- as.character(A_i); outs <- as.list(Yhat)
  if (length(words) == 0L) stop("A_i is empty.", call. = FALSE)
  if (length(outs) == 0L) stop("Yhat is empty.", call. = FALSE)
  per <- stats::setNames(vapply(words, .morie_km2_count_word, numeric(1),
                                outputs = outs), words)
  n_tokens <- sum(vapply(outs, function(Y) length(.morie_km2_tokens(Y)),
                         numeric(1)))
  list(estimate = sum(per), group = G_i, per_word = per,
       share_of_tokens = if (n_tokens) sum(per) / n_tokens else 0,
       n_outputs = length(outs), n = as.integer(n_tokens),
       method = "Demographic Representation count (Kamath Eq 6.15)")
}

#' @rdname morie_kamath_ch6_demographic_representation
#' @param w Gating word.
#' @export
morie_kamath_ch6_stereotypical_assoc <- function(w, A_i, Yhat) {
  words <- as.character(A_i); outs <- as.list(Yhat)
  if (length(words) == 0L) stop("A_i is empty.", call. = FALSE)
  if (length(outs) == 0L) stop("Yhat is empty.", call. = FALSE)
  gated <- Filter(function(Y) sum(.morie_km2_tokens(Y) == w) > 0, outs)
  per <- stats::setNames(vapply(words, .morie_km2_count_word, numeric(1),
                                outputs = gated), words)
  list(estimate = sum(per), word = w, per_attribute = per,
       n_outputs_with_w = length(gated), n = length(outs),
       method = "Stereotypical Associations count (Kamath Eq 6.16)")
}

#' HONEST score (Kamath Eq 6.17)
#'
#' Hurtful completions / (prompts x k).
#'
#' @param Yhat List of per-prompt completion vectors. @param k Completions.
#' @param hurtlex Predicate or a vector of hurtful strings.
#' @return List with `estimate`, `n_hurtful`, `per_prompt`.
#' @export
morie_kamath_ch6_honest_score <- function(Yhat, k, hurtlex = NULL) {
  if (is.null(hurtlex)) stop("hurtlex is required.", call. = FALSE)
  hurt <- if (is.function(hurtlex)) hurtlex else function(y) y %in% hurtlex
  groups <- lapply(Yhat, as.list)
  k <- as.integer(k)
  if (k < 1L) stop("k must be at least 1.", call. = FALSE)
  if (length(groups) == 0L) stop("Yhat is empty.", call. = FALSE)
  if (any(lengths(groups) != k)) {
    stop("every prompt needs exactly k completions.", call. = FALSE)
  }
  per <- vapply(groups, function(g)
    sum(vapply(g, function(y) as.numeric(isTRUE(hurt(y))), numeric(1))),
    numeric(1))
  total <- sum(per); denom <- length(groups) * k
  list(estimate = total / denom, n_hurtful = as.integer(total),
       n_completions = denom, per_prompt = as.integer(per), k = k,
       n = length(groups), method = "HONEST score (Kamath Eq 6.17)")
}

.morie_km2_pair_vectors <- function(A, E, name) {
  emb <- if (is.function(E)) E else function(a) {
    if (!(a %in% names(E))) stop("a word has no embedding.", call. = FALSE)
    E[[a]]
  }
  pairs <- as.list(A)
  if (length(pairs) == 0L) stop("A is empty.", call. = FALSE)
  out <- lapply(pairs, function(p) {
    if (length(p) != 2L) stop("every element of A must be a pair.",
                              call. = FALSE)
    vi <- as.numeric(emb(p[[1]])); vj <- as.numeric(emb(p[[2]]))
    if (length(vi) != length(vj)) stop("a pair has mismatched embeddings.",
                                       call. = FALSE)
    list(vi, vj)
  })
  d <- length(out[[1]][[1]])
  if (any(vapply(out, function(p) length(p[[1]]) != d || length(p[[2]]) != d,
                 logical(1)))) {
    stop("all embeddings must share one dimension.", call. = FALSE)
  }
  out
}

#' Counterfactual-pair debiasing and the gender direction (Eq 6.18-6.19)
#'
#' @details One pair reader serves both Python modules exactly as km095
#' imports km094's `_pair_vectors`: km094 ->
#' `morie_kamath_ch6_debias_regularizer` (lam * sum of squared within-pair
#' distances), km095 -> `morie_kamath_ch6_gender_direction`
#' (mean of E(second) - E(first) over the pairs).
#'
#' @param A List of two-element word pairs. @param E Embedding map/function.
#' @param lam Non-negative weight.
#' @return List with `estimate`, `per_pair`.
#' @export
morie_kamath_ch6_debias_regularizer <- function(A, E, lam) {
  lam <- as.numeric(lam)
  if (lam < 0 || !is.finite(lam)) stop("lam must be finite, non-negative.",
                                       call. = FALSE)
  pairs <- .morie_km2_pair_vectors(A, E, "E")
  per <- vapply(pairs, function(p) sum((p[[1]] - p[[2]])^2), numeric(1))
  list(estimate = lam * sum(per), per_pair = per, unweighted = sum(per),
       lam = lam, n = length(per),
       method = "counterfactual-pair debiasing regulariser (Kamath Eq 6.18)")
}

#' @rdname morie_kamath_ch6_debias_regularizer
#' @export
morie_kamath_ch6_gender_direction <- function(A, E) {
  pairs <- .morie_km2_pair_vectors(A, E, "E")
  diffs <- t(vapply(pairs, function(p) p[[2]] - p[[1]],
                    numeric(length(pairs[[1]][[1]]))))
  g <- colMeans(diffs)
  nrm <- sqrt(sum(g^2))
  list(g = g, norm = nrm, per_pair = diffs, degenerate = nrm == 0,
       estimate = nrm, n = length(pairs),
       method = "gender direction (Kamath Eq 6.19)")
}

#' Gender-projection regulariser (Kamath Eq 6.20)
#'
#' Sum of stereotype embeddings' projections onto the unit gender direction.
#'
#' @param W_stereo Stereotype embedding rows. @param g Gender direction.
#' @return List with `estimate`, `projections`, `sum_abs`, `g_norm`.
#' @export
morie_kamath_ch6_gender_projection_reg <- function(W_stereo, g) {
  gv <- as.numeric(g); W <- as.matrix(W_stereo)
  if (nrow(W) == 0L) stop("W_stereo is empty.", call. = FALSE)
  nrm <- sqrt(sum(gv^2))
  if (nrm == 0) stop("g is the zero vector.", call. = FALSE)
  if (ncol(W) != length(gv)) stop("W_stereo and g differ in width.",
                                  call. = FALSE)
  proj <- as.numeric(W %*% (gv / nrm))
  list(estimate = sum(proj), projections = proj, sum_abs = sum(abs(proj)),
       g_norm = nrm, n = nrow(W),
       method = "gender-projection regulariser (Kamath Eq 6.20)")
}

#' Entropy-based attention regularisation (Kamath Eq 6.21)
#'
#' -lam times the summed per-layer MEAN attention row entropy. Rows must sum
#' to 1; 0 log 0 is taken as 0.
#'
#' @param A List of attention matrices. @param L Optional layer count.
#' @param lam Non-negative weight.
#' @return List with `estimate`, `per_layer_entropy`, `total_entropy`.
#' @export
morie_kamath_ch6_ear_entropy_reg <- function(A, L = NULL, lam = 1) {
  layers <- if (is.matrix(A)) list(A) else as.list(A)
  if (length(layers) == 0L) stop("A is empty.", call. = FALSE)
  if (!is.null(L) && as.integer(L) != length(layers)) {
    stop("L contradicts the attention matrices supplied.", call. = FALSE)
  }
  lam <- as.numeric(lam)
  if (lam < 0 || !is.finite(lam)) stop("lam must be finite, non-negative.",
                                       call. = FALSE)
  ents <- vapply(layers, function(M) {
    Am <- as.matrix(M)
    if (length(Am) == 0L) stop("an attention matrix is empty.", call. = FALSE)
    if (any(Am < 0)) stop("a negative attention weight.", call. = FALSE)
    if (any(abs(rowSums(Am) - 1) > 1e-8)) {
      stop("every attention row must sum to 1.", call. = FALSE)
    }
    terms <- ifelse(Am > 0, -Am * log(Am), 0)
    mean(rowSums(terms))
  }, numeric(1))
  list(estimate = -lam * sum(ents), per_layer_entropy = ents,
       total_entropy = sum(ents), lam = lam, n = length(layers),
       method = "entropy-based attention regularisation (Kamath Eq 6.21)")
}

#' Equalising log-probability-ratio regulariser (Kamath Eq 6.22)
#'
#' lam * MEAN log P(a_i)/P(a_j) over the K word pairs; the unweighted SUM is
#' reported alongside.
#'
#' @param a_i,a_j Paired probabilities in (0, 1\]. @param K Optional count.
#' @param lam Finite weight.
#' @return List with `estimate`, `unweighted`, `per_pair`.
#' @export
morie_kamath_ch6_log_prob_ratio_attr <- function(a_i, a_j, K = NULL, lam = 1) {
  pi_ <- as.numeric(a_i); pj <- as.numeric(a_j)
  if (length(pi_) == 0L) stop("a_i is empty.", call. = FALSE)
  if (length(pi_) != length(pj)) stop("the words must be paired.",
                                      call. = FALSE)
  if (any(pi_ <= 0 | pj <= 0 | pi_ > 1 | pj > 1)) {
    stop("every probability must lie in (0, 1].", call. = FALSE)
  }
  if (!is.null(K) && as.integer(K) != length(pi_)) {
    stop("K contradicts the pairs supplied.", call. = FALSE)
  }
  lam <- as.numeric(lam)
  if (!is.finite(lam)) stop("lam must be finite.", call. = FALSE)
  logs <- log(pi_ / pj)
  list(estimate = lam * mean(logs), unweighted = sum(logs), per_pair = logs,
       lam = lam, K = length(pi_), n = length(pi_),
       method = "equalising log-probability-ratio regulariser (Eq 6.22)")
}

.morie_km2_tox_scores <- function(Yhat, c, name = "Yhat") {
  outs <- as.list(Yhat)
  if (length(outs) == 0L) stop(sprintf("%s is empty.", name), call. = FALSE)
  vals <- if (is.function(c)) {
    vapply(outs, function(y) as.numeric(c(y)), numeric(1))
  } else {
    v <- as.numeric(c)
    if (length(v) != length(outs)) stop("c and Yhat differ in length.",
                                        call. = FALSE)
    v
  }
  if (any(vals < 0 | vals > 1)) stop("toxicity scores lie in [0, 1].",
                                     call. = FALSE)
  list(vals, outs)
}

#' Toxicity metrics (Kamath Eq 6.23-6.25)
#'
#' @details One scorer serves all three Python modules exactly as km099 and
#' km100 import km101's `_scores`/`_flags`: km099 ->
#' `morie_kamath_ch6_emt_metric` (max toxicity), km100 ->
#' `morie_kamath_ch6_toxicity_probability` (share of draws with at least one
#' toxic generation), km101 -> `morie_kamath_ch6_toxic_fraction` (share of
#' generations at or above the threshold). `argmax_index` is 0-BASED.
#'
#' @param Yhat Generations. @param c Classifier or score vector.
#' @return List with `estimate`, `scores`.
#' @export
morie_kamath_ch6_emt_metric <- function(Yhat, c) {
  r <- .morie_km2_tox_scores(Yhat, c)
  k <- which.max(r[[1]])
  list(estimate = r[[1]][k], argmax = r[[2]][[k]], argmax_index = k - 1L,
       scores = r[[1]], n = length(r[[2]]),
       method = "Expected Maximum Toxicity (Kamath Eq 6.23)")
}

#' @rdname morie_kamath_ch6_emt_metric
#' @param threshold Toxicity cut-off; default 0.5.
#' @export
morie_kamath_ch6_toxic_fraction <- function(Yhat, c, threshold = 0.5) {
  r <- .morie_km2_tox_scores(Yhat, c)
  flags <- as.numeric(r[[1]] >= as.numeric(threshold))
  list(estimate = mean(flags), n_toxic = as.integer(sum(flags)),
       scores = r[[1]], threshold = as.numeric(threshold),
       n = length(r[[2]]), method = "Toxic Fraction (Kamath Eq 6.25)")
}

#' @rdname morie_kamath_ch6_emt_metric
#' @export
morie_kamath_ch6_toxicity_probability <- function(Yhat, c, threshold = 0.5) {
  groups <- as.list(Yhat)
  if (length(groups) == 0L) stop("Yhat is empty.", call. = FALSE)
  nested <- all(vapply(groups, function(g)
    !(is.character(g) && length(g) == 1L) && length(g) > 1L, logical(1)))
  draws <- if (nested) groups else list(groups)
  if (nested && !is.function(c)) {
    stop("with nested generations c must be a function.", call. = FALSE)
  }
  n_gen <- 0L
  per <- vapply(draws, function(g) {
    r <- .morie_km2_tox_scores(g, c)
    n_gen <<- n_gen + length(r[[2]])
    as.numeric(sum(r[[1]] >= as.numeric(threshold)) >= 1)
  }, numeric(1))
  list(estimate = mean(per), per_draw = per, n_draws = length(per),
       n_generations = n_gen, threshold = as.numeric(threshold),
       single_draw = !nested, n = n_gen,
       method = "Toxicity Probability (Kamath Eq 6.24)")
}

#' Chain-rule sequence probability (Kamath Eq 6.26)
#'
#' @param w_1_w_M Per-step conditional probabilities in (0, 1\].
#' @return List with `estimate` (the product), `log_prob`, `per_step`.
#' @export
morie_kamath_ch6_lstm_chain_rule <- function(w_1_w_M) {
  p <- as.numeric(w_1_w_M)
  if (length(p) == 0L) stop("the sequence is empty.", call. = FALSE)
  if (any(p <= 0 | p > 1)) stop("conditionals must lie in (0, 1].",
                                call. = FALSE)
  list(estimate = prod(p), log_prob = sum(log(p)), per_step = p,
       n = length(p),
       method = "chain rule sequence probability (Kamath Eq 6.26)")
}

.morie_km2_hidden <- function(f, c, name) {
  v <- if (is.null(f)) c else if (is.function(f)) f(c) else f
  v <- as.numeric(v)
  if (length(v) == 0L) stop(sprintf("%s produced an empty vector.", name),
                            call. = FALSE)
  v
}

#' LSTM vocabulary softmax and Affect-LM (Kamath Eq 6.27-6.28)
#'
#' @details One softmax core serves both Python modules exactly as km104
#' imports km103's `_softmax_logits`/`_hidden`: km103 ->
#' `morie_kamath_ch6_lstm_softmax_word` (softmax(U f(c) + b)), km104 ->
#' `morie_kamath_ch6_affect_lm` (the same, shifted by beta V g(e)).
#' `argmax` is 0-BASED.
#'
#' @param U Vocabulary projection. @param f Hidden map or vector.
#' @param c_t_1 Previous cell state. @param b Bias.
#' @return List with `p`, `logits`, `argmax`.
#' @export
morie_kamath_ch6_lstm_softmax_word <- function(U, f, c_t_1, b) {
  Um <- as.matrix(U)
  h <- .morie_km2_hidden(f, c_t_1, "f")
  bv <- as.numeric(b)
  if (ncol(Um) != length(h)) stop("U and the hidden vector differ.",
                                  call. = FALSE)
  if (length(bv) != nrow(Um)) stop("b and the vocabulary differ.",
                                   call. = FALSE)
  logits <- as.numeric(Um %*% h) + bv
  if (any(!is.finite(logits))) stop("a logit is not finite.", call. = FALSE)
  p <- .morie_km2_soft(logits)
  list(p = p, logits = logits, argmax = which.max(p) - 1L,
       estimate = max(p), n = length(p),
       method = "LSTM vocabulary softmax (Kamath Eq 6.27)")
}

#' @rdname morie_kamath_ch6_lstm_softmax_word
#' @param V Affect projection. @param g Affect hidden map. @param c Cell state.
#' @param e Affect category. @param beta Affect strength.
#' @export
morie_kamath_ch6_affect_lm <- function(U, V, f, g, c, e, beta, b) {
  Um <- as.matrix(U); Vm <- as.matrix(V)
  hc <- .morie_km2_hidden(f, c, "f"); he <- .morie_km2_hidden(g, e, "g")
  bv <- as.numeric(b); beta <- as.numeric(beta)
  if (!is.finite(beta)) stop("beta must be finite.", call. = FALSE)
  if (ncol(Um) != length(hc)) stop("U and f(c) differ.", call. = FALSE)
  if (ncol(Vm) != length(he)) stop("V and g(e) differ.", call. = FALSE)
  if (nrow(Vm) != nrow(Um)) stop("U and V cover different vocabularies.",
                                 call. = FALSE)
  if (length(bv) != nrow(Um)) stop("b and the vocabulary differ.",
                                   call. = FALSE)
  base <- as.numeric(Um %*% hc)
  affect <- beta * as.numeric(Vm %*% he)
  p <- .morie_km2_soft(base + affect + bv)
  list(p = p, affect_term = affect, argmax = which.max(p) - 1L, beta = beta,
       estimate = max(p), n = length(p),
       method = "Affect-LM vocabulary softmax (Kamath Eq 6.28)")
}

#' GeDi combined loss (Kamath Eq 6.29)
#'
#' lam L_g + (1 - lam) L_d, a convex combination, so lam is confined to \[0, 1\].
#'
#' @param L_g,L_d Component losses. @param lam Mixing weight in \[0, 1\].
#' @return List with `estimate`, `contributions`.
#' @export
morie_kamath_ch6_gedi_combined_loss <- function(L_g, L_d, lam) {
  lam <- as.numeric(lam)
  if (lam < 0 || lam > 1) stop("lam lies outside [0, 1].", call. = FALSE)
  lg <- as.numeric(L_g); ld <- as.numeric(L_d)
  if (!is.finite(lg) || !is.finite(ld)) stop("losses must be finite.",
                                             call. = FALSE)
  contrib <- c(lam * lg, (1 - lam) * ld)
  list(estimate = sum(contrib), contributions = contrib, L_g = lg, L_d = ld,
       lam = lam, n = 2L, method = "GeDi combined loss (Kamath Eq 6.29)")
}

#' Self-diagnosis probability (Kamath Eq 6.30)
#'
#' p(Yes) renormalised against p(No) on the self-diagnosis prompt.
#'
#' @param x Text. @param y Attribute. @param M Function prompt -> named
#'   Yes/No probabilities. @param sdg Optional prompt builder.
#' @return List with `estimate`, `p_yes`, `p_no`, `prompt`.
#' @export
morie_kamath_ch6_self_diagnosis_prob <- function(x, y, M, sdg = NULL) {
  if (!is.function(M)) stop("M must be a function.", call. = FALSE)
  template <- if (is.null(sdg)) function(a, b)
    sprintf("%s\nDoes the above text contain %s?", a, b) else sdg
  if (!is.function(template)) stop("sdg must be a function.", call. = FALSE)
  prompt <- template(x, y)
  dist <- M(prompt)
  if (!all(c("Yes", "No") %in% names(dist))) {
    stop("M's output must carry Yes and No probabilities.", call. = FALSE)
  }
  py <- as.numeric(dist[["Yes"]]); pn <- as.numeric(dist[["No"]])
  if (py < 0 || pn < 0 || py > 1 || pn > 1) {
    stop("the Yes/No probabilities must lie in [0, 1].", call. = FALSE)
  }
  tot <- py + pn
  if (tot <= 0) stop("no mass on either Yes or No.", call. = FALSE)
  list(estimate = py / tot, p_yes = py, p_no = pn, mass_on_yes_no = tot,
       prompt = prompt, attribute = y, n = 2L,
       method = "self-diagnosis probability (Kamath Eq 6.30)")
}

#' ProPILE PII likelihood (Kamath Eq 6.31)
#'
#' Product of the target PII's per-token probabilities.
#'
#' @param a_m Per-token probabilities. @param A Other PII. @param x Query
#'   tokens. @param L_q,L_r Query and answer lengths.
#' @return List with `estimate`, `log_likelihood`, `context_lengths`.
#' @export
morie_kamath_ch6_pii_likelihood <- function(a_m, A, x, L_q, L_r) {
  p <- as.numeric(a_m); q <- as.list(x)
  Lq <- as.integer(L_q); Lr <- as.integer(L_r)
  if (Lr < 1L) stop("L_r must be at least 1.", call. = FALSE)
  if (length(p) != Lr) stop("a_m and L_r disagree.", call. = FALSE)
  if (length(q) != Lq) stop("x and L_q disagree.", call. = FALSE)
  if (Lq < 1L) stop("L_q must be at least 1.", call. = FALSE)
  if (any(p <= 0 | p > 1)) stop("token probabilities lie in (0, 1].",
                                call. = FALSE)
  list(estimate = prod(p), log_likelihood = sum(log(p)), per_token = p,
       context_lengths = Lq + seq_len(Lr) - 1L,
       n_other_pii = length(as.list(A)), n = Lr,
       method = "ProPILE PII likelihood (Kamath Eq 6.31)")
}

#' Differential privacy check (Kamath Eq 6.32)
#'
#' Checks P\[M(A) in S\] <= e^eps P\[M(B) in S\] and reports the epsilon that
#' would be required.
#'
#' @param M Function dataset -> named outcome distribution.
#' @param A,B Neighbouring datasets. @param S Outcome set. @param epsilon Budget.
#' @return List with `estimate`, `satisfied`, `p_A`, `p_B`, `bound`.
#' @export
morie_kamath_ch6_differential_privacy <- function(M, A, B, S, epsilon) {
  if (!is.function(M)) stop("M must be a function.", call. = FALSE)
  outs <- as.character(S)
  if (length(outs) == 0L) stop("S is empty.", call. = FALSE)
  eps <- as.numeric(epsilon)
  if (eps < 0) stop("epsilon must be non-negative.", call. = FALSE)
  mass <- function(D, nm) {
    dist <- M(D)
    .morie_km2_dist(dist, sprintf("M(%s)", nm))
    if (!all(outs %in% names(dist))) stop("an outcome is absent.",
                                          call. = FALSE)
    sum(as.numeric(dist[outs]))
  }
  pA <- mass(A, "A"); pB <- mass(B, "B")
  if (pB <= 0) stop("P[M(B) in S] is 0.", call. = FALSE)
  required <- if (pA > 0) log(pA / pB) else -Inf
  list(estimate = required, epsilon_required = required,
       satisfied = pA <= exp(eps) * pB + 1e-12, p_A = pA, p_B = pB,
       ratio = pA / pB, epsilon = eps, bound = exp(eps) * pB,
       n = length(outs),
       method = "differential privacy check (Kamath Eq 6.32)")
}

#' Worst-case perplexity leakage (Kamath Eq 6.33)
#'
#' max log(PP_public / PP_lm) over the unique sequences.
#'
#' @param S_uniq Sequences. @param PP_public,PP_lm Named vectors or functions.
#' @return List with `estimate`, `argmax`, `per_sequence`, `n_leaking`.
#' @export
morie_kamath_ch6_perplexity_leakage <- function(S_uniq, PP_public, PP_lm) {
  seqs <- as.character(S_uniq)
  if (length(seqs) == 0L) stop("S_uniq is empty.", call. = FALSE)
  pp <- function(src, w, nm) {
    v <- if (is.function(src)) as.numeric(src(w)) else {
      if (!(w %in% names(src))) stop(sprintf("%s has no perplexity.", nm),
                                     call. = FALSE)
      as.numeric(src[[w]])
    }
    if (!is.finite(v) || v <= 0) stop("a perplexity must be finite, positive.",
                                      call. = FALSE)
    v
  }
  ratios <- vapply(seqs, function(w)
    log(pp(PP_public, w, "PP_public") / pp(PP_lm, w, "PP_lm")),
    numeric(1), USE.NAMES = FALSE)
  k <- which.max(ratios)
  list(estimate = ratios[k], argmax = seqs[k], per_sequence = ratios,
       n_leaking = as.integer(sum(ratios > 0)), n = length(seqs),
       method = "worst-case perplexity leakage (Kamath Eq 6.33)")
}

# ------------------------------------------------------------- Ch 7: RAG

#' RAG faithfulness (Kamath Eq 7.2)
#'
#' Supported facts / total facts. `facts` must be 0/1 or logical.
#'
#' @param facts Support indicators.
#' @return List with `estimate`, `n_supported`, `n_facts`.
#' @export
morie_kamath_ch7_faithfulness_metric <- function(facts) {
  v <- if (is.logical(facts)) as.numeric(facts) else as.numeric(facts)
  if (length(v) == 0L) stop("the answer contains no facts.", call. = FALSE)
  if (!all(v == 0 | v == 1)) stop("facts must be 0/1 or logical.",
                                  call. = FALSE)
  list(estimate = sum(v) / length(v), n_supported = as.integer(sum(v)),
       n_facts = length(v), n = length(v),
       method = "RAG faithfulness = supported / total facts (Kamath Eq 7.2)")
}

#' RAGAS answer relevance (Kamath Eq 7.3)
#'
#' Mean cosine between the reverse-generated question embeddings and the
#' original query embedding.
#'
#' @param E_g Question embedding rows. @param E_o Query embedding.
#' @param N Optional question count.
#' @return List with `estimate`, `similarities`.
#' @export
morie_kamath_ch7_answer_relevance <- function(E_g, E_o, N = NULL) {
  G <- as.matrix(E_g); o <- as.numeric(E_o)
  if (length(G) == 0L) stop("no reverse-generated questions.", call. = FALSE)
  if (ncol(G) != length(o)) stop("embedding widths differ.", call. = FALSE)
  if (!is.null(N) && as.integer(N) != nrow(G)) {
    stop("N contradicts the embeddings supplied.", call. = FALSE)
  }
  na <- sqrt(sum(o^2)); nb <- sqrt(rowSums(G^2))
  if (na == 0 || any(nb == 0)) stop("a zero-length embedding.", call. = FALSE)
  sims <- as.numeric(G %*% o) / (nb * na)
  list(estimate = mean(sims), similarities = sims, n = nrow(G),
       method = "RAGAS answer relevance (Kamath Eq 7.3)")
}

# --------------------------------------------------------- Ch 8: metrics

#' Perplexity from token probabilities (Kamath Eq 8.1)
#'
#' exp(mean negative log-likelihood per token).
#'
#' @param X Tokens. @param N Optional token count.
#' @param p_theta Probability vector or function (token, prefix).
#' @return List with `estimate`, `mean_nll`, `log_probs`.
#' @export
morie_kamath_ch8_perplexity <- function(X, N = NULL, p_theta = NULL) {
  toks <- as.list(X)
  if (length(toks) == 0L) stop("an empty sequence has no perplexity.",
                               call. = FALSE)
  if (is.null(p_theta)) stop("p_theta is required.", call. = FALSE)
  probs <- if (is.function(p_theta)) {
    vapply(seq_along(toks), function(i)
      as.numeric(p_theta(toks[[i]],
                         if (i == 1L) list() else toks[seq_len(i - 1L)])),
      numeric(1))
  } else {
    v <- as.numeric(p_theta)
    if (length(v) != length(toks)) stop("probabilities and tokens differ.",
                                        call. = FALSE)
    v
  }
  if (any(probs < 0 | probs > 1)) stop("token probabilities lie in [0, 1].",
                                       call. = FALSE)
  if (!is.null(N) && as.integer(N) != length(probs)) {
    stop("N contradicts the tokens scored.", call. = FALSE)
  }
  logp <- log(probs)
  nll <- -mean(logp)
  list(estimate = exp(nll), mean_nll = nll, log_probs = logp,
       n = length(probs), method = "perplexity (Kamath Eq 8.1)")
}

#' BLEU clipped n-gram precision (Kamath Eq 8.2)
#'
#' @param n_grams Matrix or vector of \[clipped_matches, total_generated\].
#' @return List with `estimate`, `p_n`.
#' @export
morie_kamath_ch8_bleu_precision <- function(n_grams) {
  A <- if (is.matrix(n_grams)) n_grams else
    matrix(as.numeric(unlist(n_grams)), ncol = 2L, byrow = TRUE)
  if (ncol(A) != 2L) stop("give [clipped_matches, total_generated] rows.",
                          call. = FALSE)
  if (any(A < 0)) stop("n-gram counts cannot be negative.", call. = FALSE)
  if (any(A[, 2] == 0)) stop("an n-gram order generated nothing.",
                             call. = FALSE)
  if (any(A[, 1] > A[, 2])) stop("clipped matches exceed the generated count.",
                                 call. = FALSE)
  p <- A[, 1] / A[, 2]
  list(estimate = if (length(p) == 1L) p[1] else p, p_n = p, n = length(p),
       method = "BLEU clipped n-gram precision (Kamath Eq 8.2)")
}

#' BLEU geometric mean and final score (Kamath Eq 8.3, 8.5)
#'
#' @details One geometric mean serves both Python modules exactly as km117
#' calls km115: km115 -> `morie_kamath_ch8_bleu_n_geom_mean`, km117 ->
#' `morie_kamath_ch8_bleu_final` (BP times that mean). Any zero precision
#' collapses the mean to 0 and the log mean to -Inf.
#'
#' @param p_n Precisions in \[0, 1\]. @param N Optional order count.
#' @return List with `estimate`, `log_mean`, `p_n`.
#' @export
morie_kamath_ch8_bleu_n_geom_mean <- function(p_n, N = NULL) {
  p <- as.numeric(p_n)
  if (length(p) == 0L) stop("no precisions given.", call. = FALSE)
  if (any(p < 0 | p > 1)) stop("each p_n must lie in [0, 1].", call. = FALSE)
  if (!is.null(N) && as.integer(N) != length(p)) {
    stop("N contradicts the precisions given.", call. = FALSE)
  }
  if (any(p == 0)) {
    gm <- 0; logmean <- -Inf
  } else {
    logmean <- mean(log(p)); gm <- exp(logmean)
  }
  list(estimate = gm, log_mean = logmean, p_n = p, n = length(p),
       method = "BLEU-N geometric mean of precisions (Kamath Eq 8.3)")
}

#' @rdname morie_kamath_ch8_bleu_n_geom_mean
#' @param BP Brevity penalty in \[0, 1\].
#' @export
morie_kamath_ch8_bleu_final <- function(BP, p_n, N = NULL) {
  bp <- as.numeric(BP)
  if (bp < 0 || bp > 1) stop("the brevity penalty must lie in [0, 1].",
                             call. = FALSE)
  gm <- morie_kamath_ch8_bleu_n_geom_mean(p_n, N)
  list(estimate = bp * gm$estimate, brevity_penalty = bp,
       geometric_mean = gm$estimate, p_n = gm$p_n, n = gm$n,
       method = "BLEU (Kamath Eq 8.5; geometric mean from km115)")
}

#' BLEU brevity penalty (Kamath Eq 8.4)
#'
#' 1 when the candidate is longer than the reference, exp(1 - r/c) otherwise.
#'
#' @param c Candidate length. @param r Reference length.
#' @return List with `estimate`, `penalized`.
#' @export
morie_kamath_ch8_brevity_penalty <- function(c, r) {
  c <- as.numeric(c); r <- as.numeric(r)
  if (c <= 0) stop("the candidate length must be positive.", call. = FALSE)
  if (r < 0) stop("the reference length cannot be negative.", call. = FALSE)
  bp <- if (c > r) 1 else exp(1 - r / c)
  list(estimate = bp, c = c, r = r, penalized = c <= r, n = 1L,
       method = "BLEU brevity penalty (Kamath Eq 8.4)")
}

#' ROUGE-N recall over multiple references (Kamath Eq 8.6)
#'
#' Clipped reference n-gram overlap divided by the reference n-gram total.
#'
#' @param S List of reference token SEQUENCES. @param gram_n Order.
#' @param candidate Generated token sequence.
#' @return List with `estimate`, `matched`, `total_reference_ngrams`.
#' @export
morie_kamath_ch8_rouge_n <- function(S, gram_n, candidate = NULL) {
  n <- as.integer(gram_n)
  if (n < 1L) stop("the n-gram order must be >= 1.", call. = FALSE)
  if (is.null(candidate)) stop("candidate= is required.", call. = FALSE)
  refs <- as.list(S)
  if (length(refs) == 0L) stop("no reference summaries.", call. = FALSE)
  if (is.character(refs[[1]]) && length(refs[[1]]) == 1L) {
    stop("S must be a list of token SEQUENCES, not strings.", call. = FALSE)
  }
  cand <- .morie_km2_counts(.morie_km2_ngrams(as.character(candidate), n))
  matched <- 0; total <- 0
  for (ref in refs) {
    rc <- .morie_km2_counts(.morie_km2_ngrams(as.character(ref), n))
    total <- total + sum(rc)
    for (g in names(rc)) {
      have <- if (g %in% names(cand)) cand[[g]] else 0L
      matched <- matched + min(rc[[g]], have)
    }
  }
  if (total == 0) stop("the references contain no n-grams.", call. = FALSE)
  list(estimate = matched / total, matched = as.integer(matched),
       total_reference_ngrams = as.integer(total), gram_n = n,
       n = length(refs), method = sprintf("ROUGE-%d recall (Kamath Eq 8.6)", n))
}

.morie_km2_sim_matrix <- function(x, xhat, normalize) {
  X <- as.matrix(x); Y <- as.matrix(xhat)
  if (length(X) == 0L || length(Y) == 0L) stop("empty embeddings.",
                                               call. = FALSE)
  if (ncol(X) != ncol(Y)) stop("embedding widths differ.", call. = FALSE)
  if (normalize) {
    nx <- sqrt(rowSums(X^2)); ny <- sqrt(rowSums(Y^2))
    if (any(nx == 0) || any(ny == 0)) stop("a zero token embedding.",
                                           call. = FALSE)
    X <- X / nx; Y <- Y / ny
  }
  list(X, Y, X %*% t(Y))
}

#' BERTScore recall, precision and F1 (Kamath Eq 8.7-8.9)
#'
#' @details One greedy matcher serves the recall and precision Python
#' modules exactly as km120 calls km119 with the arguments swapped:
#' km119 -> `morie_kamath_ch8_bertscore_recall`, km120 ->
#' `morie_kamath_ch8_bertscore_precision`, km121 ->
#' `morie_kamath_ch8_bertscore_f1`. `greedy_match` is 0-BASED.
#'
#' @param x Reference token embeddings. @param xhat Candidate embeddings.
#' @param normalize Cosine instead of raw dot products.
#' @return List with `estimate`, `per_token`, `greedy_match`.
#' @export
morie_kamath_ch8_bertscore_recall <- function(x, xhat, normalize = FALSE) {
  r <- .morie_km2_sim_matrix(x, xhat, normalize)
  S <- r[[3]]
  best <- apply(S, 1, max)
  list(estimate = mean(best), per_token = best,
       greedy_match = apply(S, 1, which.max) - 1L, n = nrow(r[[1]]),
       method = "BERTScore recall (Kamath Eq 8.7)")
}

#' @rdname morie_kamath_ch8_bertscore_recall
#' @export
morie_kamath_ch8_bertscore_precision <- function(x, xhat, normalize = FALSE) {
  r <- morie_kamath_ch8_bertscore_recall(xhat, x, normalize = normalize)
  list(estimate = r$estimate, per_token = r$per_token,
       greedy_match = r$greedy_match, n = r$n,
       method = "BERTScore precision (Kamath Eq 8.8; km119 with x swapped)")
}

#' @rdname morie_kamath_ch8_bertscore_recall
#' @param P_BERT,R_BERT Precision and recall.
#' @export
morie_kamath_ch8_bertscore_f1 <- function(P_BERT, R_BERT) {
  p <- as.numeric(P_BERT); r <- as.numeric(R_BERT)
  if (p + r == 0) stop("precision and recall are both 0.", call. = FALSE)
  if (p + r < 0) stop("a negative sum has no harmonic mean.", call. = FALSE)
  list(estimate = 2 * p * r / (p + r), precision = p, recall = r, n = 2L,
       method = "BERTScore F1 (Kamath Eq 8.9)")
}

#' Word Mover's Distance as an exact transport LP (Kamath Eq 8.10)
#'
#' Minimises <C, F> subject to the marginals. When `F` is supplied its cost
#' is reported and `optimal` is FALSE; otherwise the exact optimum is solved
#' and CERTIFIED against its dual before anything is returned.
#'
#' @param x_n,y_n Marginal weights of equal total mass. @param C Cost matrix.
#' @param F Optional transport plan.
#' @return List with `estimate`, `flow`, `dual_objective`, `optimal`.
#' @export
morie_kamath_ch8_wmd <- function(x_n, y_n, C, F = NULL) {
  a <- as.numeric(x_n); b <- as.numeric(y_n); Cm <- as.matrix(C)
  if (length(a) == 0L || length(b) == 0L) stop("empty weight vector.",
                                               call. = FALSE)
  if (any(a < 0) || any(b < 0)) stop("weights cannot be negative.",
                                     call. = FALSE)
  if (!all(dim(Cm) == c(length(a), length(b)))) stop("C has the wrong shape.",
                                                     call. = FALSE)
  if (abs(sum(a) - sum(b)) > 1e-9) stop("the marginals differ in total mass.",
                                        call. = FALSE)
  if (sum(a) <= 0) stop("both weight vectors are all zero.", call. = FALSE)
  if (!is.null(F)) {
    Fm <- as.matrix(F)
    if (!all(dim(Fm) == dim(Cm))) stop("F has the wrong shape.", call. = FALSE)
    if (any(Fm < -1e-12)) stop("a plan cannot carry negative flow.",
                               call. = FALSE)
    if (max(abs(rowSums(Fm) - a)) > 1e-8 || max(abs(colSums(Fm) - b)) > 1e-8) {
      stop("F violates the marginal constraints.", call. = FALSE)
    }
    return(list(estimate = sum(Cm * Fm), flow = Fm, optimal = FALSE,
                n = length(a),
                method = "cost of a supplied transport plan (Kamath Eq 8.10)"))
  }
  sol <- .morie_km2_transport(a, b, Cm)
  cost <- sum(Cm * sol$flow)
  dual <- sum(a * sol$u) + sum(b * sol$v)
  if (abs(cost - dual) > 1e-6 * max(1, abs(cost))) {
    stop("the transport solution failed its duality check.", call. = FALSE)
  }
  list(estimate = cost, flow = sol$flow, dual_objective = dual,
       potentials_u = sol$u, potentials_v = sol$v, optimal = TRUE,
       n = length(a),
       method = "Word Mover's Distance, exact transport LP (Kamath Eq 8.10)")
}

#' MoverScore n-gram distance and Sentence Mover's Distance (Eq 8.11, 8.14)
#'
#' @details One L2 distance serves both Python modules exactly as km126
#' calls km123: km123 -> `morie_kamath_ch8_moverscore_distance`, km126 ->
#' `morie_kamath_ch8_smd`.
#'
#' @param x_i,y_j Embeddings, or objects for `E`. @param E Optional embedder.
#' @return List with `estimate`, `difference`.
#' @export
morie_kamath_ch8_moverscore_distance <- function(x_i, y_j, E = NULL) {
  if (!is.null(E)) {
    if (!is.function(E)) stop("E must be a function or NULL.", call. = FALSE)
    ex <- as.numeric(E(x_i)); ey <- as.numeric(E(y_j))
  } else {
    ex <- as.numeric(x_i); ey <- as.numeric(y_j)
  }
  if (length(ex) == 0L || length(ey) == 0L) stop("an empty embedding.",
                                                 call. = FALSE)
  if (length(ex) != length(ey)) stop("embedding widths differ.",
                                     call. = FALSE)
  if (any(!is.finite(ex)) || any(!is.finite(ey))) {
    stop("the embeddings contain non-finite values.", call. = FALSE)
  }
  diff <- ex - ey
  list(estimate = sqrt(sum(diff^2)), difference = diff, n = length(ex),
       method = "MoverScore Euclidean n-gram distance (Kamath Eq 8.11)")
}

#' @rdname morie_kamath_ch8_moverscore_distance
#' @param x,y Sentence embeddings.
#' @export
morie_kamath_ch8_smd <- function(x, y, E = NULL) {
  d <- morie_kamath_ch8_moverscore_distance(x, y, E = E)
  list(estimate = d$estimate, difference = d$difference, n = d$n,
       method = "Sentence Mover's Distance (Kamath Eq 8.14)")
}

#' MoverScore n-gram embedding (Kamath Eq 8.12)
#'
#' Sum of idf over the n tokens of one window. `i` is a 0-BASED start.
#'
#' @param x 1-D idf sequence or 2-D embedding rows. @param i 0-based start.
#' @param n Window length.
#' @return List with `estimate`, `embedding`, `window`.
#' @export
morie_kamath_ch8_ngram_embedding <- function(x, i, n) {
  A <- if (is.matrix(x)) x else as.numeric(x)
  i <- as.integer(i); n <- as.integer(n)
  if (n < 1L) stop("the n-gram length must be >= 1.", call. = FALSE)
  len <- if (is.matrix(A)) nrow(A) else length(A)
  if (i < 0L || i + n > len) stop("the window runs off the sequence.",
                                  call. = FALSE)
  win <- if (is.matrix(A)) A[(i + 1L):(i + n), , drop = FALSE] else
    A[(i + 1L):(i + n)]
  emb <- if (is.matrix(A)) colSums(win) else sum(win)
  list(estimate = if (length(emb) == 1L) as.numeric(emb) else as.numeric(emb),
       embedding = as.numeric(emb), window = as.numeric(win), i = i, n = n,
       method = "MoverScore n-gram embedding (Kamath Eq 8.12)")
}

#' MoverScore n-gram weight (Kamath Eq 8.13)
#'
#' Window idf sum divided by the normaliser Z.
#'
#' @param x idf rows. @param Z Optional normaliser; default the grand sum.
#' @return List with `estimate`, `weights`, `Z`.
#' @export
morie_kamath_ch8_ngram_weight <- function(x, Z = NULL) {
  A <- if (is.matrix(x)) x else matrix(as.numeric(x), nrow = 1L)
  if (length(A) == 0L) stop("no idf values given.", call. = FALSE)
  if (any(A < 0)) stop("idf values cannot be negative.", call. = FALSE)
  sums <- rowSums(A)
  if (is.null(Z)) {
    Z <- sum(sums)
    if (Z <= 0) stop("every idf value is 0.", call. = FALSE)
  } else {
    Z <- as.numeric(Z)
    if (Z <= 0) stop("Z must be positive.", call. = FALSE)
  }
  w <- sums / Z
  list(estimate = if (length(w) == 1L) w[1] else w, weights = w, Z = Z,
       n = length(w), method = "MoverScore n-gram weight (Kamath Eq 8.13)")
}

#' G-Eval probability-weighted score (Kamath Eq 8.15)
#'
#' Expected rating under the evaluator's score posterior.
#'
#' @param s_i Rubric score points. @param p Their probabilities.
#' @return List with `estimate`, `scores`, `probabilities`.
#' @export
morie_kamath_ch8_geval_score <- function(s_i, p) {
  s <- as.numeric(s_i); q <- as.numeric(p)
  if (length(s) == 0L) stop("no candidate scores.", call. = FALSE)
  if (length(s) != length(q)) stop("scores and probabilities differ.",
                                   call. = FALSE)
  if (any(q < 0)) stop("score probabilities cannot be negative.",
                       call. = FALSE)
  if (abs(sum(q) - 1) > 1e-6) stop("the probabilities do not sum to 1.",
                                   call. = FALSE)
  list(estimate = sum(q * s), scores = s, probabilities = q, n = length(s),
       method = "G-Eval probability-weighted score (Kamath Eq 8.15)")
}

#' Unbiased pass@k (Kamath Eq 8.16)
#'
#' 1 - C(n-c, k)/C(n, k), computed as an overflow-free running product.
#'
#' @details The Python shelf carries two modules with this arithmetic:
#' km128 -> `morie_kamath_ch8_pass_at_k` (reports `fail_probability`),
#' kmpask -> `morie_kamath_pass_at_k` (reports `empirical_rate`). Both are
#' exported so their payloads stay distinguishable.
#'
#' @param n Samples drawn. @param c Correct samples. @param k Budget.
#' @return List with `estimate`, `fail_probability`.
#' @export
morie_kamath_ch8_pass_at_k <- function(n, c, k) {
  n_i <- as.integer(n); c_i <- as.integer(c); k_i <- as.integer(k)
  if (n_i != n || c_i != c || k_i != k) stop("n, c and k must be integers.",
                                             call. = FALSE)
  if (n_i < 1L) stop("at least one sample must be generated.", call. = FALSE)
  if (c_i < 0L || c_i > n_i) stop("c must lie in [0, n].", call. = FALSE)
  if (k_i < 1L || k_i > n_i) stop("k must lie in [1, n].", call. = FALSE)
  ratio <- if (n_i - c_i < k_i) 0 else {
    r <- 1
    for (i in seq_len(k_i) - 1L) r <- r * (n_i - c_i - i) / (n_i - i)
    r
  }
  list(estimate = 1 - ratio, fail_probability = ratio, n_samples = n_i,
       n_correct = c_i, k = k_i, n = n_i,
       method = "unbiased pass@k (Kamath Eq 8.16)")
}

#' @rdname morie_kamath_ch8_pass_at_k
#' @export
morie_kamath_pass_at_k <- function(n, c, k) {
  n <- as.integer(n); c <- as.integer(c); k <- as.integer(k)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  if (c < 0L || c > n) stop("c must lie in [0, n].", call. = FALSE)
  if (k < 1L || k > n) stop("k must lie in [1, n].", call. = FALSE)
  value <- if (n - c < k) 1 else {
    fail <- 1
    for (i in seq_len(k) - 1L) fail <- fail * (n - c - i) / (n - i)
    1 - fail
  }
  list(estimate = value, pass_at_k = value, n_samples = n, n_correct = c,
       k = k, empirical_rate = c / n, n = n,
       method = "pass@k = 1 - C(n-c,k)/C(n,k) (unbiased estimator)")
}

# ------------------------------------------------------ Ch 9: multimodal

#' Modality encoder (Kamath Eq 9.1)
#'
#' F_X = ME_X(I_X): runs the caller's encoder and checks its output.
#'
#' @param I_X Raw modality input. @param ME_X Encoder function.
#' @return List with `estimate` (Frobenius norm), `features`, `shape`.
#' @export
morie_kamath_ch9_modality_encoder <- function(I_X, ME_X) {
  if (!is.function(ME_X)) stop("ME_X must be a function.", call. = FALSE)
  F <- ME_X(I_X)
  Fv <- as.numeric(F)
  if (length(Fv) == 0L) stop("the encoder returned no features.",
                             call. = FALSE)
  if (any(!is.finite(Fv))) stop("the encoder returned non-finite features.",
                                call. = FALSE)
  list(estimate = sqrt(sum(Fv^2)), features = Fv,
       shape = if (is.matrix(F)) dim(F) else length(Fv), n = length(Fv),
       method = "modality encoder F_X = ME_X(I_X) (Kamath Eq 9.1)")
}

#' Input-alignment text-generation objective (Kamath Eq 9.2)
#'
#' L_txt-gen(LLM(P_X, F_T), t), minimised over the prompt candidates.
#' `argmin` is 0-BASED.
#'
#' @param P_X Prompt features: a matrix, or a list of candidate matrices.
#' @param F_T Text features. @param t Target. @param llm Model function.
#' @param loss_fn Loss function.
#' @return List with `estimate`, `argmin`, `losses`.
#' @export
morie_kamath_ch9_input_alignment_loss <- function(P_X, F_T, t, llm = NULL,
                                                  loss_fn = NULL) {
  if (!is.function(llm)) stop("llm= must be a function.", call. = FALSE)
  if (!is.function(loss_fn)) stop("loss_fn= must be a function.",
                                  call. = FALSE)
  cands <- if (is.list(P_X)) P_X else list(as.matrix(P_X))
  losses <- vapply(cands, function(cc) {
    L <- as.numeric(loss_fn(llm(cc, F_T), t))
    if (!is.finite(L)) stop("loss_fn returned a non-finite value.",
                            call. = FALSE)
    L
  }, numeric(1))
  k <- which.min(losses)
  list(estimate = losses[k], argmin = k - 1L, losses = losses,
       n_candidates = length(losses), n = length(losses),
       method = "input-alignment text-generation objective (Kamath Eq 9.2)")
}

#' Input and output projectors (Kamath Eq 9.3, 9.19)
#'
#' @details The two Python modules are the same linear map in opposite
#' directions: km131 -> `morie_kamath_ch9_input_projector` (P_X =
#' IN_ALIGN(F_X)), km147 -> `morie_kamath_ch9_output_alignment` (H_X =
#' OUT_ALIGN(S_X)). Both accept a matrix (right-multiplied) or a function.
#'
#' @param F_X Modality features. @param in_align Matrix or function.
#' @return List with `estimate`, `prompts`, `shape`.
#' @export
morie_kamath_ch9_input_projector <- function(F_X, in_align = NULL) {
  if (is.null(in_align)) stop("in_align= is required.", call. = FALSE)
  Fm <- as.matrix(F_X)
  P <- if (is.function(in_align)) as.matrix(in_align(Fm)) else {
    W <- as.matrix(in_align)
    if (ncol(Fm) != nrow(W)) stop("F_X and W do not conform.", call. = FALSE)
    Fm %*% W
  }
  if (length(P) == 0L || any(!is.finite(P))) {
    stop("the projector returned empty or non-finite features.",
         call. = FALSE)
  }
  list(estimate = sqrt(sum(P^2)), prompts = P, shape = dim(P),
       n = nrow(P),
       method = "input projector P_X = IN_ALIGN(F_X) (Kamath Eq 9.3)")
}

#' @rdname morie_kamath_ch9_input_projector
#' @param S_X Signal tokens. @param out_align Matrix or function.
#' @export
morie_kamath_ch9_output_alignment <- function(S_X, out_align = NULL) {
  if (is.null(out_align)) stop("out_align= is required.", call. = FALSE)
  S <- as.matrix(S_X)
  H <- if (is.function(out_align)) as.matrix(out_align(S)) else {
    W <- as.matrix(out_align)
    if (ncol(S) != nrow(W)) stop("S_X and W do not conform.", call. = FALSE)
    S %*% W
  }
  if (length(H) == 0L || any(!is.finite(H))) {
    stop("the output projector returned empty or non-finite features.",
         call. = FALSE)
  }
  list(estimate = sqrt(sum(H^2)), features = H, shape = dim(H), n = nrow(H),
       method = "output projector H_X = OUT_ALIGN(S_X) (Kamath Eq 9.19)")
}

#' LLM text and signal tokens (Kamath Eq 9.4)
#'
#' (t, S_X) = LLM(P_X, F_T); the 2-element contract is enforced.
#'
#' @param P_X Prompt features. @param F_T Text features. @param llm Model.
#' @return List with `estimate` (signal-token count), `text`, `signal_tokens`.
#' @export
morie_kamath_ch9_llm_signal_tokens <- function(P_X, F_T, llm = NULL) {
  if (!is.function(llm)) stop("llm= must be a function.", call. = FALSE)
  got <- llm(P_X, F_T)
  if (!is.list(got) || length(got) != 2L) {
    stop("the LLM must return the 2-tuple (t, S_X) of Eq 9.4.", call. = FALSE)
  }
  signals <- as.list(got[[2]])
  list(estimate = length(signals), text = got[[1]], signal_tokens = signals,
       generates_modality = length(signals) > 0L, n = length(signals),
       method = "LLM text and signal tokens (Kamath Eq 9.4)")
}

#' CLIP contrastive losses (Kamath Eq 9.5-9.7)
#'
#' @details One row-wise InfoNCE serves both directional Python modules
#' exactly as km134 calls km133 with the towers swapped: km133 ->
#' `morie_kamath_ch9_clip_image_to_text`, km134 ->
#' `morie_kamath_ch9_clip_text_to_image`, km135 ->
#' `morie_kamath_ch9_clip_contrastive_total` (their sum).
#'
#' @param V Image embeddings. @param L Text embeddings. @param sigma Temperature.
#' @param N Optional batch size.
#' @return List with `estimate`, `per_pair`, `logits`.
#' @export
morie_kamath_ch9_clip_image_to_text <- function(V, L, sigma, N = NULL) {
  Vm <- as.matrix(V); Lm <- as.matrix(L)
  s <- as.numeric(sigma)
  if (s <= 0) stop("the temperature must be positive.", call. = FALSE)
  if (nrow(Vm) != nrow(Lm)) stop("the batch sizes differ.", call. = FALSE)
  if (ncol(Vm) != ncol(Lm)) stop("embedding widths differ.", call. = FALSE)
  if (nrow(Vm) == 0L) stop("the batch is empty.", call. = FALSE)
  if (!is.null(N) && as.integer(N) != nrow(Vm)) {
    stop("N contradicts the batch size.", call. = FALSE)
  }
  logits <- Vm %*% t(Lm) / s
  per <- .morie_km2_rowce(logits)
  list(estimate = mean(per), per_pair = per, logits = logits,
       temperature = s, n = nrow(Vm),
       method = "CLIP image-to-text contrastive loss (Kamath Eq 9.5)")
}

#' @rdname morie_kamath_ch9_clip_image_to_text
#' @export
morie_kamath_ch9_clip_text_to_image <- function(L, V, sigma, N = NULL) {
  r <- morie_kamath_ch9_clip_image_to_text(L, V, sigma, N)
  list(estimate = r$estimate, per_pair = r$per_pair, logits = r$logits,
       temperature = r$temperature, n = r$n,
       method = "CLIP text-to-image contrastive loss (Kamath Eq 9.6)")
}

#' @rdname morie_kamath_ch9_clip_image_to_text
#' @param L_i2t,L_t2i The two directional losses.
#' @export
morie_kamath_ch9_clip_contrastive_total <- function(L_i2t, L_t2i) {
  a <- as.numeric(L_i2t); b <- as.numeric(L_t2i)
  if (!is.finite(a) || !is.finite(b)) stop("both losses must be finite.",
                                           call. = FALSE)
  if (a < 0 || b < 0) stop("a contrastive cross-entropy cannot be negative.",
                           call. = FALSE)
  list(estimate = a + b, L_i2t = a, L_t2i = b, n = 2L,
       method = "total CLIP contrastive loss (Kamath Eq 9.7)")
}

#' Visual-linguistic matching losses (Kamath Eq 9.8-9.9)
#'
#' @details One core serves both Python modules exactly as km137 calls
#' km136: km136 -> `morie_kamath_ch9_mml_vlm_loss`, km137 ->
#' `morie_kamath_ch9_itm_hard_negative` (the same sum with a hard-negative
#' set).
#'
#' @param Pos,Neg Probabilities of the correct label in \[0, 1\].
#' @return List with `estimate`, `positive_loss`, `negative_loss`.
#' @export
morie_kamath_ch9_mml_vlm_loss <- function(Pos, Neg) {
  chk <- function(p, nm) {
    q <- as.numeric(p)
    if (length(q) == 0L) stop(sprintf("%s is empty.", nm), call. = FALSE)
    if (any(q < 0 | q > 1)) stop(sprintf("%s must lie in [0, 1].", nm),
                                 call. = FALSE)
    q
  }
  pos <- chk(Pos, "Pos"); neg <- chk(Neg, "Neg")
  lp <- -log(pos); ln <- -log(neg)
  list(estimate = sum(lp) + sum(ln), positive_loss = sum(lp),
       negative_loss = sum(ln), n_positive = length(pos),
       n_negative = length(neg), n = length(pos) + length(neg),
       method = "visual-linguistic matching loss (Kamath Eq 9.8)")
}

#' @rdname morie_kamath_ch9_mml_vlm_loss
#' @param HardNeg Hard-negative probabilities.
#' @export
morie_kamath_ch9_itm_hard_negative <- function(Pos, HardNeg) {
  r <- morie_kamath_ch9_mml_vlm_loss(Pos, HardNeg)
  list(estimate = r$estimate, positive_loss = r$positive_loss,
       negative_loss = r$negative_loss, n_positive = r$n_positive,
       n_hard_negative = r$n_negative, n = r$n,
       method = "ITM loss with hard negatives (Kamath Eq 9.9)")
}

#' SimVLM masked-token loss with visual conditioning (Kamath Eq 9.10)
#'
#' Delegates the masked-token arithmetic to `morie_kamath_mlm_loss` (Ch 2,
#' km022) exactly as the Python module imports it. `x_m` is 0-BASED.
#'
#' @param theta Optional model theta(x, v). @param x Token probabilities.
#' @param v Image-region features. @param x_m 0-based masked positions.
#' @return List with `estimate`, `per_position`, `n_image_regions`.
#' @export
morie_kamath_ch9_simvlm_mlm <- function(theta, x, v, x_m) {
  if (is.null(v)) stop("v (the image regions) is required.", call. = FALSE)
  V <- as.matrix(v)
  if (length(V) == 0L) stop("the image-region features are empty.",
                            call. = FALSE)
  probs <- if (is.null(theta)) x else {
    if (!is.function(theta)) stop("theta must be a function or NULL.",
                                  call. = FALSE)
    theta(x, v)
  }
  base <- morie_kamath_mlm_loss(probs, x_m)
  list(estimate = base$estimate, per_position = base$per_position,
       positions_scored = base$positions_scored, n_image_regions = nrow(V),
       n = base$n,
       method = "SimVLM MLM loss with visual conditioning (Kamath Eq 9.10)")
}

#' PrefixLM suffix negative log-likelihood (Kamath Eq 9.11)
#'
#' Summed -log p over the suffix, averaged over sequences. `T_p` is a
#' 0-BASED prefix length, so the suffix starts at column T_p.
#'
#' @param theta Optional model. @param x Probability rows.
#' @param T_p Prefix length.
#' @return List with `estimate`, `per_sequence`, `n_suffix_tokens`.
#' @export
morie_kamath_ch9_simvlm_prefixlm <- function(theta, x, T_p) {
  if (!is.null(theta)) {
    if (!is.function(theta)) stop("theta must be a function or NULL.",
                                  call. = FALSE)
    x <- theta(x)
  }
  P <- if (is.matrix(x)) x else matrix(as.numeric(x), nrow = 1L)
  if (length(P) == 0L) stop("no token probabilities.", call. = FALSE)
  if (any(P < 0 | P > 1)) stop("token probabilities lie in [0, 1].",
                               call. = FALSE)
  tp <- as.integer(T_p)
  if (tp < 0L || tp >= ncol(P)) stop("the prefix leaves no suffix.",
                                     call. = FALSE)
  per <- rowSums(-log(P[, (tp + 1L):ncol(P), drop = FALSE]))
  list(estimate = mean(per), per_sequence = per, prefix_length = tp,
       n_suffix_tokens = ncol(P) - tp, n = nrow(P),
       method = "PrefixLM suffix negative log-likelihood (Kamath Eq 9.11)")
}

#' Masked object classification loss (Kamath Eq 9.12)
#'
#' Summed cross-entropy over the masked image regions. 1-D `labels` are
#' 0-BASED class indices; 2-D labels must be one-hot rows. `as_printed`
#' carries the book's sign, which is the negative of a loss.
#'
#' @param theta Optional model. @param w Fallback labels. @param v Regions.
#' @param g_theta Class distributions or a function of v. @param labels Truth.
#' @return List with `estimate`, `per_region`, `true_class_probability`.
#' @export
morie_kamath_ch9_moc_loss <- function(theta, w, v, g_theta, labels = NULL) {
  if (!is.null(theta) && !is.function(theta)) {
    stop("theta must be a function or NULL.", call. = FALSE)
  }
  G <- if (is.function(g_theta)) g_theta(v) else g_theta
  G <- as.matrix(G)
  if (length(G) == 0L) stop("no class distributions.", call. = FALSE)
  if (any(G < 0 | G > 1)) stop("g_theta must be a distribution.",
                               call. = FALSE)
  if (max(abs(rowSums(G) - 1)) > 1e-6) stop("each row must sum to 1.",
                                            call. = FALSE)
  y <- if (!is.null(labels)) labels else w
  if (is.null(y)) stop("labels= are required.", call. = FALSE)
  if (is.matrix(y)) {
    onehot <- y
    if (!all(dim(onehot) == dim(G))) stop("one-hot labels do not match.",
                                          call. = FALSE)
    if (!all(onehot == 0 | onehot == 1) || any(rowSums(onehot) != 1)) {
      stop("2-D labels must be one-hot rows.", call. = FALSE)
    }
    probs <- rowSums(onehot * G)
  } else {
    idx <- as.integer(y)
    if (any(idx < 0L | idx >= ncol(G))) stop("a class index is out of range.",
                                             call. = FALSE)
    if (length(idx) != nrow(G)) stop("labels and regions differ.",
                                     call. = FALSE)
    probs <- G[cbind(seq_len(nrow(G)), idx + 1L)]
  }
  ce <- -log(probs)
  list(estimate = sum(ce), as_printed = -sum(ce), per_region = ce,
       true_class_probability = probs, n_masked_regions = nrow(G),
       n = nrow(G),
       method = "masked object classification loss (Kamath Eq 9.12)")
}

#' Image-text matching binary loss (Kamath Eq 9.13)
#'
#' Binary cross-entropy on the match scores.
#'
#' @param theta Score function or scores. @param v,t Images and texts.
#' @param y 0/1 match labels.
#' @return List with `estimate`, `per_pair`, `scores`.
#' @export
morie_kamath_ch9_itm_loss <- function(theta, v, t, y) {
  s <- if (is.function(theta)) theta(v, t) else theta
  s <- as.numeric(s); lab <- as.numeric(y)
  if (length(s) == 0L) stop("no image-text pairs were scored.", call. = FALSE)
  if (length(s) != length(lab)) stop("labels and scores differ.",
                                     call. = FALSE)
  if (any(s < 0 | s > 1)) stop("the match score is a probability.",
                               call. = FALSE)
  if (!all(lab == 0 | lab == 1)) stop("ITM labels must be 0 or 1.",
                                      call. = FALSE)
  correct <- ifelse(lab == 1, s, 1 - s)
  per <- -log(correct)
  list(estimate = mean(per), per_pair = per, scores = s, n = length(s),
       method = "image-text matching binary loss (Kamath Eq 9.13)")
}

#' Autoregressive response losses (Kamath Eq 9.14, 9.17)
#'
#' @details One per-sequence NLL serves both Python modules exactly as
#' km142 calls km145: km145 -> `morie_kamath_ch9_mmllm_autoregressive`
#' (-sum log p over one response), km142 -> `morie_kamath_ch9_itg_loss`
#' (that sum over the (X, Y) corpus).
#'
#' @param R Response token probabilities. @param I Visual context.
#' @param theta Optional model.
#' @return List with `estimate`, `mean_nll`, `per_token`.
#' @export
morie_kamath_ch9_mmllm_autoregressive <- function(R, I, theta = NULL) {
  if (!is.null(theta)) {
    if (!is.function(theta)) stop("theta must be a function or NULL.",
                                  call. = FALSE)
    R <- theta(R, I)
  }
  p <- as.numeric(R)
  if (length(p) == 0L) stop("the response is empty.", call. = FALSE)
  if (any(p < 0 | p > 1)) stop("response probabilities lie in [0, 1].",
                               call. = FALSE)
  nll <- -log(p)
  list(estimate = sum(nll), mean_nll = sum(nll) / length(p), per_token = nll,
       sequence_probability = prod(p), n = length(p),
       method = "MMLLM autoregressive response loss (Kamath Eq 9.17)")
}

#' @rdname morie_kamath_ch9_mmllm_autoregressive
#' @param x Visual contexts, or NULL. @param y List of response probabilities.
#' @export
morie_kamath_ch9_itg_loss <- function(x, y) {
  seqs <- as.list(y)
  if (length(seqs) == 0L) stop("no (x, y) pairs.", call. = FALSE)
  if (!is.null(x) && length(as.list(x)) != length(seqs)) {
    stop("the pairs do not line up.", call. = FALSE)
  }
  per <- vapply(seqs, function(s)
    morie_kamath_ch9_mmllm_autoregressive(s, NULL)$estimate, numeric(1))
  list(estimate = sum(per), per_pair = per, n = length(per),
       method = "image-conditioned text generation loss (Kamath Eq 9.14)")
}

#' Frame order modelling loss (Kamath Eq 9.15)
#'
#' -sum log P\[frame, true timestamp\]. `r_i` and `t_i` are 0-BASED indices
#' into P.
#'
#' @param r_i Frame indices. @param t_i Timestamp indices. @param R Count.
#' @param P Frame-by-timestamp probability matrix.
#' @return List with `estimate`, `per_frame`, `n_reordered`.
#' @export
morie_kamath_ch9_fom_loss <- function(r_i, t_i, R = NULL, P = NULL) {
  if (is.null(P)) stop("P= is required.", call. = FALSE)
  Pm <- as.matrix(P)
  rows <- as.integer(r_i); cols <- as.integer(t_i)
  if (length(rows) == 0L) stop("no frames were reordered.", call. = FALSE)
  if (length(rows) != length(cols)) stop("frames and timestamps differ.",
                                         call. = FALSE)
  if (any(rows < 0L | rows >= nrow(Pm))) stop("a frame index is out of range.",
                                              call. = FALSE)
  if (any(cols < 0L | cols >= ncol(Pm))) stop("a timestamp is out of range.",
                                              call. = FALSE)
  if (any(Pm < 0 | Pm > 1)) stop("P holds probabilities.", call. = FALSE)
  if (!is.null(R) && as.integer(R) != length(rows)) {
    stop("R contradicts the reordered frames.", call. = FALSE)
  }
  per <- -log(Pm[cbind(rows + 1L, cols + 1L)])
  list(estimate = sum(per), per_frame = per, n_reordered = length(rows),
       n = length(rows),
       method = "frame order modelling loss (Kamath Eq 9.15)")
}

#' Multimodal instruction-tuned prediction (Kamath Eq 9.16)
#'
#' A = f(I, M; theta), with the model contract enforced.
#'
#' @param I Instruction. @param M Multimodal input. @param theta Model.
#' @param f Optional override f(I, M, theta).
#' @return List with `estimate`, `answer`.
#' @export
morie_kamath_ch9_mm_instr_predict <- function(I, M, theta, f = NULL) {
  A <- if (!is.null(f)) {
    if (!is.function(f)) stop("f must be a function.", call. = FALSE)
    f(I, M, theta)
  } else if (is.function(theta)) theta(I, M) else {
    stop("give a function theta(I, M) or f=.", call. = FALSE)
  }
  if (is.null(A)) stop("the model returned no answer.", call. = FALSE)
  list(estimate = A, answer = A, instruction = I, multimodal_input = M,
       n = 1L,
       method = "multimodal instruction-tuned prediction (Kamath Eq 9.16)")
}

#' Output-projector MSE objective (Kamath Eq 9.18)
#'
#' Mean squared error between projected features and tau_X(t), minimised
#' over candidates. `argmin` is 0-BASED.
#'
#' @param H_X A feature matrix, or a list of candidates.
#' @param tau_X Target features or a function of t. @param t Target text.
#' @return List with `estimate`, `argmin`, `losses`.
#' @export
morie_kamath_ch9_output_projector_mse <- function(H_X, tau_X, t) {
  target <- if (is.function(tau_X)) tau_X(t) else tau_X
  target <- as.numeric(target)
  if (length(target) == 0L) stop("the target features are empty.",
                                 call. = FALSE)
  cands <- if (is.list(H_X)) H_X else list(H_X)
  losses <- vapply(cands, function(cc) {
    v <- as.numeric(cc)
    if (length(v) != length(target)) stop("H_X and tau_X(t) differ in size.",
                                          call. = FALSE)
    mean((v - target)^2)
  }, numeric(1))
  k <- which.min(losses)
  list(estimate = losses[k], argmin = k - 1L, losses = losses,
       n_candidates = length(losses), n = length(target),
       method = "output-projector MSE objective (Kamath Eq 9.18)")
}

#' Conditional LDM noise-prediction loss (Kamath Eq 9.20)
#'
#' Mean over samples of the squared L2 error between true and predicted noise.
#'
#' @param epsilon True noise (rows are samples). @param z_t Latent.
#' @param H_X Conditioning. @param eps_net Prediction or function.
#' @param t Timestep.
#' @return List with `estimate`, `per_sample`.
#' @export
morie_kamath_ch9_ldm_loss <- function(epsilon, z_t, H_X, eps_net = NULL,
                                      t = NULL) {
  if (is.null(eps_net)) stop("eps_net= is required.", call. = FALSE)
  pred <- if (is.function(eps_net)) eps_net(z_t, t, H_X) else eps_net
  eps <- epsilon
  if (length(as.numeric(eps)) != length(as.numeric(pred))) {
    stop("the noise and the prediction differ in shape.", call. = FALSE)
  }
  if (length(as.numeric(eps)) == 0L) stop("the noise array is empty.",
                                          call. = FALSE)
  if (any(!is.finite(as.numeric(pred)))) {
    stop("the denoising network returned non-finite values.", call. = FALSE)
  }
  D <- if (is.matrix(eps)) as.matrix(eps) - as.matrix(pred) else
    matrix(as.numeric(eps) - as.numeric(pred), nrow = 1L)
  per <- rowSums(D^2)
  list(estimate = mean(per), per_sample = per, n = length(per),
       method = "conditional LDM noise-prediction loss (Kamath Eq 9.20)")
}

#' Flamingo factorized likelihood and dataset mix (Kamath Eq 9.21-9.22)
#'
#' @details One per-sequence NLL serves both Python modules exactly as
#' km150 calls km149: km149 -> `morie_kamath_ch9_flamingo_factorized`
#' (product of per-token conditionals, with its log), km150 ->
#' `morie_kamath_ch9_flamingo_dataset_mix` (sum_m lambda_m times the mean
#' per-sequence NLL of dataset m).
#'
#' @param y Per-token conditionals. @param x Context. @param L Optional length.
#' @param model Optional model (y, x).
#' @return List with `estimate` (the product), `log_prob`, `nll`.
#' @export
morie_kamath_ch9_flamingo_factorized <- function(y, x = NULL, L = NULL,
                                                 model = NULL) {
  if (!is.null(model)) {
    if (!is.function(model)) stop("model must be a function or NULL.",
                                  call. = FALSE)
    y <- model(y, x)
  }
  p <- as.numeric(y)
  if (length(p) == 0L) stop("the sequence is empty.", call. = FALSE)
  if (any(p < 0 | p > 1)) stop("conditionals must lie in [0, 1].",
                               call. = FALSE)
  if (!is.null(L) && as.integer(L) != length(p)) {
    stop("L contradicts the tokens given.", call. = FALSE)
  }
  logp <- sum(log(p))
  list(estimate = prod(p), log_prob = logp, nll = -logp, per_token = p,
       n = length(p),
       method = "Flamingo factorized text likelihood (Kamath Eq 9.21)")
}

#' @rdname morie_kamath_ch9_flamingo_factorized
#' @param D_m List of datasets (each a list of probability vectors).
#' @param lambda_m Dataset weights.
#' @export
morie_kamath_ch9_flamingo_dataset_mix <- function(D_m, lambda_m, x = NULL,
                                                  y = NULL) {
  datasets <- as.list(D_m)
  lam <- as.numeric(lambda_m)
  if (length(datasets) == 0L) stop("no datasets.", call. = FALSE)
  if (length(lam) != length(datasets)) stop("weights and datasets differ.",
                                            call. = FALSE)
  if (any(lam < 0)) stop("dataset weights cannot be negative.", call. = FALSE)
  per <- vapply(datasets, function(D) {
    seqs <- as.list(D)
    if (length(seqs) == 0L) stop("a dataset is empty.", call. = FALSE)
    mean(vapply(seqs, function(s)
      morie_kamath_ch9_flamingo_factorized(s)$nll, numeric(1)))
  }, numeric(1))
  list(estimate = sum(lam * per), per_dataset_nll = per, weights = lam,
       n = length(datasets),
       method = "Flamingo weighted multi-dataset loss (Kamath Eq 9.22)")
}

# --------------------------------------------- named modules: 3H .. YaRN

#' 3H alignment score (Kamath Ch 5)
#'
#' Weighted helpful / harmless / honest rubric score; the default weights
#' are 1/3 each.
#'
#' @param helpful_score,harmless_score,honest_score Aligned score vectors.
#' @param weights Optional three weights.
#' @return List with `estimate`, `score`, `weights`.
#' @export
morie_kamath_3h_alignment <- function(helpful_score, harmless_score,
                                      honest_score, weights = NULL) {
  h <- as.numeric(helpful_score); a <- as.numeric(harmless_score)
  o <- as.numeric(honest_score)
  if (length(h) != length(a) || length(a) != length(o)) {
    stop("the three rubric scores must line up.", call. = FALSE)
  }
  if (length(h) == 0L) stop("no responses were scored.", call. = FALSE)
  w <- if (is.null(weights)) rep(1 / 3, 3) else as.numeric(weights)
  if (length(w) != 3L) stop("3H needs exactly 3 weights.", call. = FALSE)
  if (any(w < 0)) stop("3H weights cannot be negative.", call. = FALSE)
  if (sum(w) <= 0) stop("the 3H weights are all zero.", call. = FALSE)
  per <- w[1] * h + w[2] * a + w[3] * o
  list(estimate = if (length(per) == 1L) per[1] else per, score = per,
       weights = w, weight_sum = sum(w), n = length(per),
       method = "3H alignment score (Kamath Ch 5)")
}

#' AdaLoRA SVD update with importance-based pruning (Kamath Ch 4)
#'
#' P diag(s) Q^T after zeroing all but the top-importance singular values.
#' `kept` is 0-BASED and sorted.
#'
#' @param P Left factor (rows x r). @param s Singular values.
#' @param Q Right factor (rows x r). @param importance Optional scores.
#' @param target_rank How many triplets to keep.
#' @return List with `estimate`, `Delta_W`, `s_pruned`, `kept`.
#' @export
morie_kamath_adalora_rank_allocation <- function(P, s, Q, importance = NULL,
                                                 target_rank = NULL) {
  Pm <- as.matrix(P); Qm <- as.matrix(Q); sv <- as.numeric(s)
  r <- length(sv)
  if (r == 0L) stop("the rank is 0.", call. = FALSE)
  if (ncol(Pm) != r || ncol(Qm) != r) stop("P and Q must have r columns.",
                                           call. = FALSE)
  imp <- if (is.null(importance)) abs(sv) else as.numeric(importance)
  if (length(imp) != r) stop("importance scores and triplets differ.",
                             call. = FALSE)
  if (any(imp < 0)) stop("importance scores cannot be negative.",
                         call. = FALSE)
  tr <- if (is.null(target_rank)) r else as.integer(target_rank)
  if (tr < 0L || tr > r) stop("target_rank must lie in [0, r].", call. = FALSE)
  keep <- if (tr == 0L) integer(0) else
    sort(.morie_km2_stable_desc(imp)[seq_len(tr)])
  s_pruned <- rep(0, r)
  s_pruned[keep] <- sv[keep]
  dW <- (t(t(Pm) * s_pruned)) %*% t(Qm)
  list(estimate = sqrt(sum(dW^2)), Delta_W = dW, s_pruned = s_pruned,
       kept = keep - 1L, target_rank = tr,
       effective_rank = as.integer(sum(s_pruned != 0)), n = r,
       method = "AdaLoRA SVD update with importance-based pruning")
}

#' Houlsby bottleneck adapter (Kamath Ch 4)
#'
#' h + W_up GELU(W_down h) with m << d enforced. `approximate = "none"` is
#' the exact erf GELU (R: `z * pnorm(z)`), "tanh" the tanh approximation.
#'
#' @param h Hidden rows. @param W_down (m, d). @param W_up (d, m).
#' @param approximate "none" or "tanh".
#' @return List with `estimate`, `h_adapted`, `bottleneck`.
#' @export
morie_kamath_houlsby_adapter <- function(h, W_down, W_up,
                                         approximate = "none") {
  H <- as.matrix(h); Wd <- as.matrix(W_down); Wu <- as.matrix(W_up)
  m <- nrow(Wd); d <- ncol(Wd)
  if (ncol(H) != d) stop("W_down expects a different hidden width.",
                         call. = FALSE)
  if (!all(dim(Wu) == c(d, m))) stop("W_up must be d x m.", call. = FALSE)
  if (m >= d) stop("a Houlsby adapter needs m << d.", call. = FALSE)
  gelu <- function(z) {
    if (approximate == "none") z * stats::pnorm(z)
    else if (approximate == "tanh")
      0.5 * z * (1 + tanh(sqrt(2 / pi) * (z + 0.044715 * z^3)))
    else stop("approximate must be 'none' or 'tanh'.", call. = FALSE)
  }
  inner <- gelu(H %*% t(Wd))
  out <- H + inner %*% t(Wu)
  list(estimate = sqrt(sum((out - H)^2)), h_adapted = out,
       bottleneck = inner, m = m, d = d, n = nrow(H),
       method = "Houlsby bottleneck adapter (Kamath Ch 4)")
}

#' ALiBi biased attention (Kamath Ch 2)
#'
#' Adds the -m (i - j) distance bias to the scores and hands the softmax to
#' `morie_alammar_sdp_attention` -- one attention core, reused.
#'
#' @param Q,K,V Attention matrices. @param slopes Non-negative ALiBi slopes.
#' @param causal Mask the future when TRUE.
#' @return List with `output`, `attention`, `bias`, `slopes`.
#' @export
morie_kamath_alibi_bias <- function(Q, K, V, slopes, causal = FALSE) {
  Qm <- as.matrix(Q); Km <- as.matrix(K)
  m <- as.numeric(slopes)
  if (length(m) == 0L) stop("no ALiBi slopes were given.", call. = FALSE)
  if (any(m < 0)) stop("ALiBi slopes are non-negative.", call. = FALSE)
  i <- matrix(seq_len(nrow(Qm)) - 1L, nrow(Qm), nrow(Km))
  j <- matrix(seq_len(nrow(Km)) - 1L, nrow(Qm), nrow(Km), byrow = TRUE)
  D <- -(i - j)
  outs <- list(); attns <- list()
  for (k in seq_along(m)) {
    bias <- m[k] * D
    if (causal) bias[j > i] <- -Inf
    r <- morie_alammar_sdp_attention(Qm, Km, V, mask = bias)
    outs[[k]] <- r$output; attns[[k]] <- r$attention
  }
  list(estimate = outs[[1]][1, 1], output = outs, attention = attns,
       bias = D, slopes = m, n = nrow(Qm),
       method = "ALiBi biased attention (Kamath Ch 2; softmax core reused)")
}

#' AutoPrompt trigger search (Kamath Ch 3)
#'
#' Coordinate-wise search over the vocabulary for every NULL slot. Without
#' `grad_fn` the loss is MINIMISED by brute force; with one, the gradient
#' score is MAXIMISED, exactly as in Python.
#'
#' @param template List of tokens with NULL trigger slots.
#' @param dataset Passed through to `model`. @param model Loss function.
#' @param vocab Candidate tokens. @param grad_fn Optional gradient scorer.
#' @param passes Sweeps over the slots.
#' @return List with `estimate`, `trigger_tokens`, `prompt`, `positions`.
#' @export
morie_kamath_autoprompt_gradient_search <- function(template, dataset, model,
                                                    vocab = NULL,
                                                    grad_fn = NULL,
                                                    passes = 1) {
  if (!is.function(model)) stop("model must be a function.", call. = FALSE)
  if (is.null(vocab) || length(vocab) == 0L) {
    stop("vocab= is required and must be non-empty.", call. = FALSE)
  }
  V <- as.list(vocab)
  filled <- as.list(template)
  slots <- which(vapply(filled, is.null, logical(1)))
  if (length(slots) == 0L) stop("the template has no trigger slots.",
                                call. = FALSE)
  for (i in slots) filled[[i]] <- V[[1]]
  if (!is.null(grad_fn) && !is.function(grad_fn)) {
    stop("grad_fn must be a function or NULL.", call. = FALSE)
  }
  n_pass <- as.integer(passes)
  if (n_pass < 1L) stop("passes must be at least 1.", call. = FALSE)
  history <- list()
  for (p in seq_len(n_pass)) {
    for (i in slots) {
      pick <- if (is.null(grad_fn)) {
        scores <- vapply(V, function(v) {
          trial <- filled; trial[[i]] <- v
          as.numeric(model(trial, dataset))
        }, numeric(1))
        which.min(scores)
      } else {
        g <- as.numeric(grad_fn(filled, dataset, i - 1L))
        if (length(g) != length(V)) stop("grad_fn returned the wrong length.",
                                         call. = FALSE)
        which.max(g)
      }
      filled[[i]] <- V[[pick]]
      history[[length(history) + 1L]] <- list(i - 1L, V[[pick]])
    }
  }
  final <- as.numeric(model(filled, dataset))
  list(estimate = final, loss = final,
       trigger_tokens = unlist(filled[slots]), prompt = filled,
       positions = slots - 1L, history = history, n = length(slots),
       method = "AutoPrompt trigger search (Kamath Ch 3)")
}

#' RAGAS answer relevance from a reverse-question model (Kamath Ch 7)
#'
#' Generates reverse questions, embeds them, then delegates the cosine mean
#' to `morie_kamath_ch7_answer_relevance` (km112).
#'
#' @param answer Model answer. @param original_question Query (or embedding).
#' @param model Function answer -> reverse questions. @param embed Optional.
#' @return List with `estimate`, `score`, `similarities`.
#' @export
morie_kamath_ragas_answer_relevance <- function(answer, original_question,
                                                model, embed = NULL) {
  if (!is.function(model)) stop("model must be a function.", call. = FALSE)
  qs <- model(answer)
  if (is.null(qs) || length(qs) == 0L) stop("no reverse questions.",
                                            call. = FALSE)
  qs <- as.list(qs)
  if (!is.null(embed)) {
    if (!is.function(embed)) stop("embed must be a function or NULL.",
                                  call. = FALSE)
    E_g <- t(vapply(qs, function(q) as.numeric(embed(q)),
                    numeric(length(as.numeric(embed(qs[[1]]))))))
    E_o <- as.numeric(embed(original_question))
  } else {
    E_g <- if (is.matrix(qs[[1]]) || length(qs) > 1L)
      t(vapply(qs, as.numeric, numeric(length(as.numeric(qs[[1]]))))) else
        matrix(as.numeric(qs[[1]]), nrow = 1L)
    E_o <- as.numeric(original_question)
  }
  base <- morie_kamath_ch7_answer_relevance(E_g, E_o)
  list(estimate = base$estimate, score = base$estimate,
       similarities = base$similarities, n = base$n,
       method = "RAGAS answer relevance (Kamath Ch 7; km112 core)")
}

#' RAGAS context relevance (Kamath Ch 7)
#'
#' Relevant context sentences / retrieved sentences, via km111's ratio core.
#'
#' @param context_sentences Retrieved sentences. @param relevance_labels 0/1.
#' @return List with `estimate`, `score`, `n_relevant`.
#' @export
morie_kamath_ragas_context_relevance <- function(context_sentences,
                                                 relevance_labels) {
  sents <- as.list(context_sentences); labs <- as.numeric(relevance_labels)
  if (length(sents) == 0L) stop("the retrieved context has no sentences.",
                                call. = FALSE)
  if (length(sents) != length(labs)) stop("labels and sentences differ.",
                                          call. = FALSE)
  base <- morie_kamath_ch7_faithfulness_metric(labs)
  list(estimate = base$estimate, score = base$estimate,
       n_relevant = base$n_supported, n_sentences = length(sents),
       n = length(sents),
       method = "RAGAS context relevance (Kamath Ch 7; km111 core)")
}

#' Corpus-level BLEU (Kamath Ch 8)
#'
#' Counts clipped n-grams against the best-matching reference count, then
#' delegates to km114 (precision), km116 (brevity penalty) and km117 (final).
#'
#' @param hypothesis Token vector. @param references List of token vectors.
#' @param max_n Highest order.
#' @return List with `estimate`, `p_n`, `brevity_penalty`.
#' @export
morie_kamath_bleu_score <- function(hypothesis, references, max_n = 4) {
  hyp <- as.character(hypothesis)
  refs <- lapply(references, as.character)
  if (length(refs) == 0L) stop("BLEU needs at least one reference.",
                               call. = FALSE)
  if (length(hyp) == 0L) stop("the hypothesis is empty.", call. = FALSE)
  N <- as.integer(max_n)
  if (N < 1L) stop("max_n must be at least 1.", call. = FALSE)
  if (length(hyp) < N) stop("the hypothesis is shorter than max_n.",
                            call. = FALSE)
  pairs <- matrix(0, nrow = N, ncol = 2L)
  for (n in seq_len(N)) {
    hc <- .morie_km2_counts(.morie_km2_ngrams(hyp, n))
    best <- list()
    for (r in refs) {
      rc <- .morie_km2_counts(.morie_km2_ngrams(r, n))
      for (g in names(rc)) {
        cur <- if (!is.null(best[[g]])) best[[g]] else 0L
        if (rc[[g]] > cur) best[[g]] <- rc[[g]]
      }
    }
    clipped <- 0
    for (g in names(hc)) {
      cap <- if (!is.null(best[[g]])) best[[g]] else 0L
      clipped <- clipped + min(hc[[g]], cap)
    }
    pairs[n, ] <- c(clipped, sum(hc))
  }
  prec <- morie_kamath_ch8_bleu_precision(pairs)
  cc <- length(hyp)
  lens <- vapply(refs, length, integer(1))
  r_eff <- lens[order(abs(lens - cc), lens)[1]]
  bp <- morie_kamath_ch8_brevity_penalty(cc, r_eff)
  final <- morie_kamath_ch8_bleu_final(bp$estimate, prec$p_n)
  list(estimate = final$estimate, bleu = final$estimate, p_n = prec$p_n,
       clipped_counts = pairs, brevity_penalty = bp$estimate,
       candidate_length = cc, reference_length = r_eff, max_n = N,
       n = length(hyp),
       method = "BLEU (Kamath Ch 8; km114/km116/km117 cores)")
}

#' BM25 relevance score (Kamath Ch 7)
#'
#' IDF-weighted saturated term frequency with length normalisation.
#'
#' @param q_terms Query terms. @param doc_terms Document terms.
#' @param idf Named IDF vector or one value per query term.
#' @param avgdl Average document length. @param k1,b BM25 parameters.
#' @return List with `estimate`, `per_term`, `term_frequencies`.
#' @export
morie_kamath_bm25_score <- function(q_terms, doc_terms, idf, avgdl,
                                    k1 = 1.5, b = 0.75) {
  q <- as.character(q_terms); d <- as.character(doc_terms)
  if (length(q) == 0L) stop("the query has no terms.", call. = FALSE)
  if (length(d) == 0L) stop("the document has no terms.", call. = FALSE)
  avg <- as.numeric(avgdl)
  if (avg <= 0) stop("avgdl must be positive.", call. = FALSE)
  k1 <- as.numeric(k1); b <- as.numeric(b)
  if (k1 < 0) stop("k1 cannot be negative.", call. = FALSE)
  if (b < 0 || b > 1) stop("b must lie in [0, 1].", call. = FALSE)
  idfs <- if (!is.null(names(idf))) {
    if (!all(q %in% names(idf))) stop("no IDF supplied for a query term.",
                                      call. = FALSE)
    as.numeric(idf[q])
  } else {
    v <- as.numeric(idf)
    if (length(v) != length(q)) stop("IDF values and query terms differ.",
                                     call. = FALSE)
    v
  }
  tf <- vapply(q, function(t) sum(d == t), numeric(1))
  norm <- k1 * (1 - b + b * length(d) / avg)
  parts <- ifelse(tf > 0, idfs * tf * (k1 + 1) / (tf + norm), 0)
  list(estimate = sum(parts), score = sum(parts), per_term = parts,
       term_frequencies = as.integer(tf), doc_length = length(d),
       avgdl = avg, k1 = k1, b = b, n = length(q),
       method = "BM25 relevance score (Kamath Ch 7)")
}

#' Best-of-N sampling (Kamath Ch 5)
#'
#' Pick the highest reward-model score among N samples. `best_index` is
#' 0-BASED.
#'
#' @param samples Candidate responses. @param rewards Optional scores.
#' @param reward_fn Optional function (x, y). @param x Prompt.
#' @return List with `estimate`, `best`, `best_index`, `reward_spread`.
#' @export
morie_kamath_best_of_n_sampling <- function(samples, rewards = NULL,
                                            reward_fn = NULL, x = NULL) {
  ys <- as.list(samples)
  if (length(ys) == 0L) stop("no samples were generated.", call. = FALSE)
  r <- if (is.null(rewards)) {
    if (!is.function(reward_fn)) stop("give rewards= or reward_fn.",
                                      call. = FALSE)
    vapply(ys, function(y) as.numeric(reward_fn(x, y)), numeric(1))
  } else {
    v <- as.numeric(rewards)
    if (length(v) != length(ys)) stop("rewards and samples differ.",
                                      call. = FALSE)
    v
  }
  if (any(!is.finite(r))) stop("the reward model returned non-finite scores.",
                               call. = FALSE)
  k <- which.max(r)
  list(estimate = r[k], best = ys[[k]], best_index = k - 1L, rewards = r,
       reward_spread = max(r) - min(r), n = length(ys),
       method = "best-of-N sampling (Kamath Ch 5)")
}

#' Bradley-Terry preference probability, vectorised (Kamath Ch 5)
#'
#' Stable sigmoid of the winner-minus-loser reward difference.
#'
#' @param r_w,r_l Winner and loser scores.
#' @return List with `estimate`, `p_pref`, `reward_difference`.
#' @export
morie_kamath_bradley_terry_preference <- function(r_w, r_l) {
  w <- as.numeric(r_w); l <- as.numeric(r_l)
  if (length(w) != length(l)) stop("winner and loser scores differ.",
                                   call. = FALSE)
  if (length(w) == 0L) stop("no preference pairs.", call. = FALSE)
  if (any(!is.finite(w)) || any(!is.finite(l))) {
    stop("reward scores must be finite.", call. = FALSE)
  }
  d <- w - l
  p <- .morie_km2_sig(d)
  list(estimate = if (length(p) == 1L) p[1] else p, p_pref = p,
       reward_difference = d, n = length(p),
       method = "Bradley-Terry preference probability (Kamath Ch 5)")
}

#' BERTScore end to end (Kamath Ch 8)
#'
#' Embeds both token lists, then delegates to km119/km120/km121.
#'
#' @param hypothesis_tokens,reference_tokens Token vectors.
#' @param embed_fn Function or named list token -> vector.
#' @return List with `estimate`, `f1`, `precision`, `recall`.
#' @export
morie_kamath_bertscore <- function(hypothesis_tokens, reference_tokens,
                                   embed_fn) {
  hyp <- as.character(hypothesis_tokens)
  ref <- as.character(reference_tokens)
  if (length(hyp) == 0L || length(ref) == 0L) {
    stop("both token sequences must be non-empty.", call. = FALSE)
  }
  embed <- function(tokens) {
    if (!is.function(embed_fn)) {
      if (!all(tokens %in% names(embed_fn))) stop("no embedding for a token.",
                                                  call. = FALSE)
      t(vapply(tokens, function(t) as.numeric(embed_fn[[t]]),
               numeric(length(as.numeric(embed_fn[[tokens[1]]])))))
    } else {
      t(vapply(tokens, function(t) as.numeric(embed_fn(t)),
               numeric(length(as.numeric(embed_fn(tokens[1]))))))
    }
  }
  H <- embed(hyp); R <- embed(ref)
  rec <- morie_kamath_ch8_bertscore_recall(R, H, normalize = TRUE)
  pre <- morie_kamath_ch8_bertscore_precision(R, H, normalize = TRUE)
  f1 <- morie_kamath_ch8_bertscore_f1(pre$estimate, rec$estimate)
  list(estimate = f1$estimate, f1 = f1$estimate, precision = pre$estimate,
       recall = rec$estimate, candidate_match = pre$greedy_match,
       reference_match = rec$greedy_match, n = length(hyp),
       method = "BERTScore (Kamath Ch 8; km119/km120/km121 cores)")
}

#' Constitutional AI critique-revise loop (Kamath Ch 5)
#'
#' Critique then revise once per principle, in order.
#'
#' @param initial_response Starting response. @param constitution Principles.
#' @param model Function (stage, principle, response, critique).
#' @return List with `estimate`, `revised_response`, `history`.
#' @export
morie_kamath_constitutional_ai_loop <- function(initial_response,
                                                constitution, model) {
  if (!is.function(model)) stop("model must be a function.", call. = FALSE)
  principles <- as.list(constitution)
  if (length(principles) == 0L) stop("the constitution is empty.",
                                     call. = FALSE)
  y <- initial_response
  history <- list()
  for (p in principles) {
    crit <- model("critique", p, y, NULL)
    if (is.null(crit)) stop("the model returned no critique.", call. = FALSE)
    rev <- model("revise", p, y, crit)
    if (is.null(rev)) stop("the model returned no revision.", call. = FALSE)
    history[[length(history) + 1L]] <- list(principle = p, critique = crit,
                                            response_before = y,
                                            response_after = rev)
    y <- rev
  }
  list(estimate = y, revised_response = y,
       initial_response = initial_response, history = history,
       n_revisions = length(principles), n = length(principles),
       method = "Constitutional AI critique-revise loop (Kamath Ch 5)")
}

#' MoE per-expert capacity (Kamath Ch 2)
#'
#' C * tokens / experts, with the drop threshold reported.
#'
#' @param tokens_per_batch Token count. @param num_experts Expert count.
#' @param C Capacity factor.
#' @return List with `estimate`, `slots`, `total_slots`, `min_dropped`.
#' @export
morie_kamath_expert_capacity_factor <- function(tokens_per_batch, num_experts,
                                                C) {
  t <- as.numeric(tokens_per_batch); e <- as.integer(num_experts)
  c <- as.numeric(C)
  if (t <= 0) stop("a batch with no tokens has no capacity.", call. = FALSE)
  if (e < 1L) stop("there must be at least one expert.", call. = FALSE)
  if (c <= 0) stop("the capacity factor must be positive.", call. = FALSE)
  cap <- c * (t / e)
  slots <- as.integer(ceiling(cap))
  list(estimate = cap, capacity = cap, slots = slots,
       total_slots = slots * e,
       min_dropped = max(0L, as.integer(ceiling(t)) - slots * e),
       headroom_per_expert = cap - t / e, num_experts = e,
       n = as.integer(ceiling(t)),
       method = "MoE per-expert capacity (Kamath Ch 2)")
}

#' Preference-based reward learning (Kamath Ch 5)
#'
#' Scores trajectory segment pairs with r_phi and hands the Bradley-Terry
#' loss to `morie_alammar_reward_model_bt`.
#'
#' @param trajectory_pairs List of (sigma_w, sigma_l) pairs.
#' @param r_phi Return function.
#' @return List with `estimate` (summed loss), `mean_loss`, `pair_accuracy`.
#' @export
morie_kamath_christiano_deep_rl_feedback <- function(trajectory_pairs, r_phi) {
  if (!is.function(r_phi)) stop("r_phi must be a function.", call. = FALSE)
  pairs <- as.list(trajectory_pairs)
  if (length(pairs) == 0L) stop("no preference comparisons.", call. = FALSE)
  rw <- numeric(0); rl <- numeric(0)
  for (p in pairs) {
    if (length(p) != 2L) stop("a comparison is not a pair.", call. = FALSE)
    rw <- c(rw, as.numeric(r_phi(p[[1]])))
    rl <- c(rl, as.numeric(r_phi(p[[2]])))
  }
  if (any(!is.finite(c(rw, rl)))) stop("r_phi returned a non-finite return.",
                                       call. = FALSE)
  bt <- morie_alammar_reward_model_bt(rw, rl)
  list(estimate = sum(bt$losses), mean_loss = bt$estimate,
       losses = bt$losses, pair_accuracy = bt$pair_accuracy,
       returns_preferred = rw, returns_rejected = rl, n = length(pairs),
       method = "preference-based reward learning (Kamath Ch 5)")
}

#' Chinchilla compute-optimal split (Kamath Ch 1)
#'
#' N = sqrt(C / (6 * 20)), D = 20 N, checked against C = 6 N D.
#'
#' @param compute_budget Total FLOPs. @param alpha,beta Exponents summing to 1.
#' @param tokens_per_param Tokens per parameter. @param flops_per_token_param
#'   FLOPs constant.
#' @return List with `estimate`, `N_opt`, `D_opt`, `compute_check`.
#' @export
morie_kamath_chinchilla_compute_optimal <- function(
    compute_budget, alpha = 0.5, beta = 0.5, tokens_per_param = 20,
    flops_per_token_param = 6) {
  C <- as.numeric(compute_budget); ratio <- as.numeric(tokens_per_param)
  kflop <- as.numeric(flops_per_token_param)
  if (C <= 0) stop("the compute budget must be positive.", call. = FALSE)
  if (ratio <= 0) stop("the tokens-per-parameter ratio must be positive.",
                       call. = FALSE)
  if (kflop <= 0) stop("the FLOPs constant must be positive.", call. = FALSE)
  if (abs(as.numeric(alpha) + as.numeric(beta) - 1) > 1e-9) {
    stop("C = 6ND forces the exponents to sum to 1.", call. = FALSE)
  }
  N <- sqrt(C / (kflop * ratio))
  D <- ratio * N
  list(estimate = N, N_opt = N, D_opt = D, tokens_per_param = ratio,
       compute_budget = C, compute_check = kflop * N * D,
       alpha = as.numeric(alpha), beta = as.numeric(beta), n = 1L,
       method = "Chinchilla compute-optimal split (Kamath Ch 1)")
}

#' Zero-shot chain-of-thought prompting (Kamath Ch 4)
#'
#' Appends the trigger, then splits reasoning from the answer.
#'
#' @param prompt Question. @param model Function prompt -> text.
#' @param trigger Reasoning trigger. @param answer_marker Split marker.
#' @param parser Optional function text -> list(reasoning, answer).
#' @return List with `estimate`, `answer`, `reasoning`, `generation`.
#' @export
morie_kamath_chain_of_thought <- function(
    prompt, model, trigger = "Let's think step by step.",
    answer_marker = "Answer:", parser = NULL) {
  if (!is.function(model)) stop("model must be a function.", call. = FALSE)
  full <- if (!is.null(trigger) && nzchar(trigger))
    paste(prompt, trigger) else as.character(prompt)
  text <- model(full)
  if (is.null(text)) stop("the model returned no text.", call. = FALSE)
  text <- as.character(text)
  if (!is.null(parser)) {
    if (!is.function(parser)) stop("parser must be a function or NULL.",
                                   call. = FALSE)
    got <- parser(text)
    if (!is.list(got) || length(got) != 2L) {
      stop("parser must return (reasoning, answer).", call. = FALSE)
    }
    reasoning <- got[[1]]; answer <- got[[2]]
  } else {
    if (!grepl(answer_marker, text, fixed = TRUE)) {
      stop("the generation contains no answer marker.", call. = FALSE)
    }
    pos <- regexpr(answer_marker, text, fixed = TRUE)
    reasoning <- trimws(substr(text, 1L, pos - 1L))
    answer <- trimws(substr(text, pos + nchar(answer_marker), nchar(text)))
  }
  list(estimate = answer, answer = answer, reasoning = reasoning,
       prompt = full, generation = text, n = 1L,
       method = "zero-shot chain-of-thought prompting (Kamath Ch 4)")
}

#' Corrective RAG retrieval router (Kamath Ch 7)
#'
#' Grades the retrieved documents, then keeps them, falls back to the web,
#' or mixes.
#'
#' @param query Query. @param docs Documents. @param clf Confidence function.
#' @param tau_hi,tau_lo Upper and lower thresholds.
#' @return List with `estimate`, `action`, `ctx`, `scores`.
#' @export
morie_kamath_corrective_rag <- function(query, docs, clf, tau_hi, tau_lo) {
  if (!is.function(clf)) stop("clf must be a function.", call. = FALSE)
  D <- as.list(docs)
  if (length(D) == 0L) stop("no documents were retrieved.", call. = FALSE)
  hi <- as.numeric(tau_hi); lo <- as.numeric(tau_lo)
  if (lo > hi) stop("tau_lo exceeds tau_hi.", call. = FALSE)
  s <- vapply(D, function(d) as.numeric(clf(query, d)), numeric(1))
  if (any(!is.finite(s))) stop("clf returned a non-finite confidence.",
                               call. = FALSE)
  best <- max(s)
  action <- if (best >= hi) "use_docs" else if (best <= lo) "fallback_web"
  else "mixed"
  ctx <- if (action == "use_docs") D[s >= hi] else
    if (action == "fallback_web") list() else D[s > lo]
  list(estimate = best, action = action, ctx = ctx, scores = s,
       best_score = best, tau_hi = hi, tau_lo = lo, n = length(D),
       method = "Corrective RAG retrieval router (Kamath Ch 7)")
}

#' Cross-encoder re-ranking (Kamath Ch 7)
#'
#' Joint (q, d) scoring with a STABLE descending re-order; `ranking` is
#' 0-BASED.
#'
#' @param q Query. @param docs Documents. @param model Scorer.
#' @param top_k Optional cut-off.
#' @return List with `estimate`, `scores`, `ranking`, `reranked`.
#' @export
morie_kamath_cross_encoder_rerank <- function(q, docs, model, top_k = NULL) {
  if (!is.function(model)) stop("model must be a function.", call. = FALSE)
  D <- as.list(docs)
  if (length(D) == 0L) stop("there are no documents to re-rank.",
                            call. = FALSE)
  s <- vapply(D, function(d) as.numeric(model(q, d)), numeric(1))
  if (any(!is.finite(s))) stop("a non-finite score.", call. = FALSE)
  ord <- .morie_km2_stable_desc(s)
  if (!is.null(top_k)) {
    k <- as.integer(top_k)
    if (k < 1L || k > length(D)) stop("top_k is out of range.", call. = FALSE)
    ord <- ord[seq_len(k)]
  }
  list(estimate = s[ord[1]], scores = s, ranking = ord - 1L,
       reranked = D[ord], n = length(D),
       method = "cross-encoder re-ranking (Kamath Ch 7)")
}

#' CrowS-Pairs stereotype preference rate (Kamath Ch 6)
#'
#' Share of minimal pairs whose stereotyping sentence scores higher.
#'
#' @param stereo_pll,anti_pll Pseudo-log-likelihoods (must be <= 0).
#' @return List with `estimate`, `score`, `bias_gap`, `n_ties`.
#' @export
morie_kamath_crowspairs_bias <- function(stereo_pll, anti_pll) {
  s <- as.numeric(stereo_pll); a <- as.numeric(anti_pll)
  if (length(s) != length(a)) stop("CrowS-Pairs is over MINIMAL PAIRS.",
                                   call. = FALSE)
  if (length(s) == 0L) stop("no sentence pairs.", call. = FALSE)
  if (any(!is.finite(s)) || any(!is.finite(a))) {
    stop("pseudo-log-likelihoods must be finite.", call. = FALSE)
  }
  if (any(s > 0) || any(a > 0)) stop("a log-probability cannot exceed 0.",
                                     call. = FALSE)
  score <- mean(s > a)
  list(estimate = score, score = score,
       n_stereotype_preferred = as.integer(sum(s > a)),
       n_ties = as.integer(sum(s == a)), bias_gap = score - 0.5,
       n = length(s),
       method = "CrowS-Pairs stereotype preference rate (Kamath Ch 6)")
}

#' Double quantization of the scale constants (Kamath Ch 4)
#'
#' int8 codes for the block scales plus one shared fp32 constant. Rounding
#' is banker's rounding in both languages.
#'
#' @param scales_fp32 Block scales. @param bits Code width in \[2, 16\].
#' @return List with `estimate` (max error), `scales_int8`, `shared_const`.
#' @export
morie_kamath_double_quantization <- function(scales_fp32, bits = 8) {
  s <- as.numeric(scales_fp32)
  if (length(s) == 0L) stop("no quantization constants.", call. = FALSE)
  if (any(!is.finite(s))) stop("the scales must be finite.", call. = FALSE)
  b <- as.integer(bits)
  if (b < 2L || b > 16L) stop("bits must lie in [2, 16].", call. = FALSE)
  qmax <- 2^(b - 1) - 1
  peak <- max(abs(s))
  if (peak == 0) stop("every scale is 0.", call. = FALSE)
  cconst <- peak / qmax
  codes <- as.integer(pmin(pmax(round(s / cconst), -qmax - 1), qmax))
  deq <- codes * cconst
  err <- abs(deq - s)
  list(estimate = max(err), scales_int8 = codes, shared_const = cconst,
       dequantized = deq, max_abs_error = max(err),
       bits_saved_per_block = 32L - b, n = length(s),
       method = "double quantization of the scale constants (Kamath Ch 4)")
}

#' (eps, delta)-DP definition check (Kamath Ch 6)
#'
#' Checks P_D <= e^eps P_D' + delta BOTH ways and reports the worst slack.
#'
#' @param eps Privacy loss. @param delta Failure probability.
#' @param p_D,p_Dp Event probabilities on the neighbouring datasets.
#' @return List with `estimate`, `guarantee`, `slack`, `worst_event`.
#' @export
morie_kamath_differential_privacy <- function(eps, delta, p_D = NULL,
                                              p_Dp = NULL) {
  e <- as.numeric(eps); d <- as.numeric(delta)
  if (e < 0) stop("epsilon cannot be negative.", call. = FALSE)
  if (d < 0 || d > 1) stop("delta must lie in [0, 1].", call. = FALSE)
  if (is.null(p_D) != is.null(p_Dp)) {
    stop("give both p_D and p_Dp, or neither.", call. = FALSE)
  }
  if (is.null(p_D)) {
    return(list(estimate = exp(e), guarantee = NULL,
                multiplicative_bound = exp(e), epsilon = e, delta = d, n = 0L,
                method = "(eps, delta)-DP bound, no mechanism supplied"))
  }
  a <- as.numeric(p_D); b <- as.numeric(p_Dp)
  if (length(a) != length(b)) stop("the event lists differ.", call. = FALSE)
  if (length(a) == 0L) stop("no events S were given.", call. = FALSE)
  if (any(a < 0 | a > 1) || any(b < 0 | b > 1)) {
    stop("mechanism outputs must be probabilities.", call. = FALSE)
  }
  slack <- pmin(exp(e) * b + d - a, exp(e) * a + d - b)
  worst <- min(slack)
  list(estimate = worst, guarantee = worst >= -1e-12, slack = slack,
       worst_event = which.min(slack) - 1L, multiplicative_bound = exp(e),
       epsilon = e, delta = d, n = length(a),
       method = "(eps, delta)-DP definition check (Kamath Ch 6)")
}

#' DPO loss from log-probabilities (Kamath Ch 5)
#'
#' Bradley-Terry loss on the beta-scaled policy/reference log-ratios; the
#' BT core is `morie_alammar_reward_model_bt`.
#'
#' @param logp_w,logp_l,logp_ref_w,logp_ref_l Log-probabilities (<= 0).
#' @param beta Positive scale.
#' @return List with `estimate`, `per_pair`, implicit rewards.
#' @export
morie_kamath_dpo_loss <- function(logp_w, logp_l, logp_ref_w, logp_ref_l,
                                  beta) {
  arrays <- lapply(list(logp_w, logp_l, logp_ref_w, logp_ref_l), as.numeric)
  if (length(unique(lengths(arrays))) != 1L) {
    stop("the four log-probability arrays must line up.", call. = FALSE)
  }
  if (length(arrays[[1]]) == 0L) stop("no preference pairs.", call. = FALSE)
  if (any(vapply(arrays, function(a) any(a > 0), logical(1)))) {
    stop("these are LOG probabilities; a positive entry is impossible.",
         call. = FALSE)
  }
  b <- as.numeric(beta)
  if (b <= 0) stop("beta must be positive.", call. = FALSE)
  rew_w <- b * (arrays[[1]] - arrays[[3]])
  rew_l <- b * (arrays[[2]] - arrays[[4]])
  bt <- morie_alammar_reward_model_bt(rew_w, rew_l)
  list(estimate = bt$estimate, loss = bt$estimate, per_pair = bt$losses,
       pair_accuracy = bt$pair_accuracy, implicit_reward_w = rew_w,
       implicit_reward_l = rew_l, beta = b, n = length(rew_w),
       method = "DPO loss (Kamath Ch 5; the Bradley-Terry core reused)")
}

#' Dense passage retrieval top-k (Kamath Ch 7)
#'
#' Bi-encoder dot products with a STABLE top-k; indices are 0-BASED.
#'
#' @param q_embed Query embedding. @param p_embeds Passage rows. @param k Depth.
#' @return List with `estimate`, `top_k_indices`, `top_k_scores`, `scores`.
#' @export
morie_kamath_dense_passage_retrieval <- function(q_embed, p_embeds, k) {
  q <- as.numeric(q_embed); P <- as.matrix(p_embeds)
  if (length(P) == 0L) stop("the passage index is empty.", call. = FALSE)
  if (ncol(P) != length(q)) stop("query and passages differ in width.",
                                 call. = FALSE)
  kk <- as.integer(k)
  if (kk < 1L || kk > nrow(P)) stop("k is out of range for this index.",
                                    call. = FALSE)
  s <- as.numeric(P %*% q)
  ord <- .morie_km2_stable_desc(s)[seq_len(kk)]
  list(estimate = s[ord[1]], top_k_indices = ord - 1L,
       top_k_scores = s[ord], scores = s, k = kk, n = nrow(P),
       method = "dense passage retrieval top-k (Kamath Ch 7)")
}

#' Emergent-ability step metric (Kamath Ch 1)
#'
#' Heaviside-gated scores plus the above/below jump. A threshold that puts
#' every model on one side is refused.
#'
#' @param scales Model scales. @param scores Benchmark scores.
#' @param threshold Scale threshold.
#' @return List with `estimate` (the jump), `emergent_score`, `n_above`.
#' @export
morie_kamath_emergent_abilities <- function(scales, scores, threshold) {
  N <- as.numeric(scales); f <- as.numeric(scores)
  if (length(N) != length(f)) stop("scales and scores differ.", call. = FALSE)
  if (length(N) == 0L) stop("no model scales.", call. = FALSE)
  thr <- as.numeric(threshold)
  above <- N >= thr
  if (!any(above) || all(above)) stop("the threshold puts every model on one side.",
                                      call. = FALSE)
  gated <- ifelse(above, f, 0)
  jump <- mean(f[above]) - mean(f[!above])
  list(estimate = jump, jump = jump, emergent_score = gated,
       mean_above = mean(f[above]), mean_below = mean(f[!above]),
       n_above = as.integer(sum(above)), threshold = thr, n = length(N),
       method = "emergent-ability step metric (Kamath Ch 1)")
}

#' Canary exposure (Kamath Ch 6)
#'
#' log2(candidate space) - log2(the canary's rank).
#'
#' @param canary_ll Canary log-likelihood. @param candidate_lls Competitors.
#' @return List with `estimate`, `rank`, `max_exposure`.
#' @export
morie_kamath_memorization_exposure <- function(canary_ll, candidate_lls) {
  cc <- as.numeric(canary_ll); others <- as.numeric(candidate_lls)
  if (length(others) == 0L) stop("no competing candidates.", call. = FALSE)
  if (!is.finite(cc) || any(!is.finite(others))) {
    stop("log-likelihoods must be finite.", call. = FALSE)
  }
  total <- length(others) + 1L
  rank <- 1L + as.integer(sum(others >= cc))
  exposure <- log2(total) - log2(rank)
  list(estimate = exposure, exposure = exposure, rank = rank,
       n_candidates = total, max_exposure = log2(total), n = total,
       method = "canary exposure (Kamath Ch 6)")
}

#' FactScore over atomic claims (Kamath Ch 6)
#'
#' Share of atomic claims the knowledge base supports; the ratio core is
#' km111.
#'
#' @param atomic_claims Claims. @param knowledge_base Predicate or vector.
#' @return List with `estimate`, `supported`, `unsupported`.
#' @export
morie_kamath_factscore <- function(atomic_claims, knowledge_base) {
  claims <- as.list(atomic_claims)
  if (length(claims) == 0L) stop("no atomic claims.", call. = FALSE)
  flags <- if (is.function(knowledge_base)) {
    vapply(claims, function(c) as.numeric(isTRUE(knowledge_base(c))),
           numeric(1))
  } else {
    vapply(claims, function(c) as.numeric(c %in% knowledge_base), numeric(1))
  }
  base <- morie_kamath_ch7_faithfulness_metric(flags)
  list(estimate = base$estimate, score = base$estimate,
       supported = claims[flags == 1], unsupported = claims[flags == 0],
       n_supported = base$n_supported, n = length(claims),
       method = "FactScore (Kamath Ch 6; the ratio core in km111)")
}

#' Top-K few-shot exemplar selection (Kamath Ch 3)
#'
#' Ranks the exemplar pool by cosine or dot similarity to the query;
#' `selected` is 0-BASED and the order is STABLE.
#'
#' @param D Exemplar embedding rows. @param query_embed Query.
#' @param K How many to keep. @param metric "cosine" or "dot".
#' @return List with `selected`, `similarities`, `estimate`.
#' @export
morie_kamath_few_shot_exemplar_selection <- function(D, query_embed, K,
                                                     metric = "cosine") {
  Dm <- as.matrix(D); q <- as.numeric(query_embed)
  K <- as.integer(K); n <- nrow(Dm)
  if (ncol(Dm) != length(q)) stop("exemplars and query differ in width.",
                                  call. = FALSE)
  if (K < 1L || K > n) stop("K is out of range.", call. = FALSE)
  if (!(metric %in% c("cosine", "dot"))) {
    stop("metric must be 'cosine' or 'dot'.", call. = FALSE)
  }
  sims <- if (metric == "cosine") {
    nq <- sqrt(sum(q^2)); nd <- sqrt(rowSums(Dm^2))
    if (nq == 0 || any(nd == 0)) stop("a zero embedding has no direction.",
                                      call. = FALSE)
    as.numeric(Dm %*% q) / (nd * nq)
  } else as.numeric(Dm %*% q)
  ord <- .morie_km2_stable_desc(sims)[seq_len(K)]
  list(selected = ord - 1L, similarities = sims[ord],
       all_similarities = sims, estimate = sims[ord[1]], K = K,
       metric = metric, n = n,
       method = "Top-K few-shot exemplar selection by similarity")
}

#' FastText subword sum (Kamath Ch 2)
#'
#' v_w = sum of the character n-gram vectors of <word>. The boundary-marked
#' whole word is always a gram.
#'
#' @param word Word. @param ngram_embeddings Named list gram -> vector.
#' @param n_min,n_max Gram length range.
#' @return List with `vector`, `ngrams`, `n_known`, `missing`.
#' @export
morie_kamath_fasttext_subword <- function(word, ngram_embeddings, n_min,
                                          n_max) {
  grams <- morie_kamath_word_ngrams(word, n_min, n_max)
  if (!is.list(ngram_embeddings) || length(ngram_embeddings) == 0L) {
    stop("ngram_embeddings must be a non-empty named list.", call. = FALSE)
  }
  total <- NULL; dim <- NULL; known <- 0L; missing <- character(0)
  for (g in grams) {
    z <- ngram_embeddings[[g]]
    if (is.null(z)) { missing <- c(missing, g); next }
    z <- as.numeric(z)
    if (is.null(dim)) { dim <- length(z); total <- z }
    else {
      if (length(z) != dim) stop("an n-gram has the wrong width.",
                                 call. = FALSE)
      total <- total + z
    }
    known <- known + 1L
  }
  if (is.null(total)) stop("none of the n-grams is in the table.",
                           call. = FALSE)
  list(vector = total, estimate = total[1], ngrams = grams, n_known = known,
       n_missing = length(missing), missing = missing, n = length(grams),
       method = "FastText subword sum v_w = sum z_g")
}

#' @rdname morie_kamath_fasttext_subword
#' @param boundary Two-character boundary marker.
#' @export
morie_kamath_word_ngrams <- function(word, n_min, n_max, boundary = "<>") {
  n_min <- as.integer(n_min); n_max <- as.integer(n_max)
  if (n_min < 1L || n_max < n_min) stop("need 1 <= n_min <= n_max.",
                                        call. = FALSE)
  if (!nzchar(word)) stop("the empty word has no n-grams.", call. = FALSE)
  marked <- paste0(substr(boundary, 1L, 1L), word, substr(boundary, 2L, 2L))
  L <- nchar(marked)
  grams <- character(0)
  for (n in n_min:n_max) {
    if (L - n + 1L < 1L) next
    grams <- c(grams, vapply(seq_len(L - n + 1L),
                             function(i) substr(marked, i, i + n - 1L),
                             character(1)))
  }
  if (!(marked %in% grams)) grams <- c(grams, marked)
  grams
}

#' G-Eval rubric score from judge logits (Kamath Ch 8)
#'
#' sum_s s * softmax(judge logits)_s over the rubric.
#'
#' @param x,y Source and candidate. @param rubric Score points or a list
#'   carrying `scores`. @param model Judge returning one logit per point.
#' @return List with `estimate`, `probabilities`, `score_points`.
#' @export
morie_kamath_g_eval <- function(x, y, rubric, model) {
  scores <- if (is.list(rubric) && !is.null(rubric$scores)) rubric$scores
  else if (is.list(rubric)) stop("a list rubric must carry a 'scores' entry.",
                                 call. = FALSE) else rubric
  s <- as.numeric(scores)
  if (length(s) < 2L) stop("a rubric needs at least two score points.",
                           call. = FALSE)
  if (anyDuplicated(s)) stop("the rubric repeats a score point.",
                             call. = FALSE)
  if (!is.function(model)) stop("model must be a function.", call. = FALSE)
  logits <- as.numeric(model(x, y, rubric))
  if (length(logits) != length(s)) stop("the judge returned the wrong count.",
                                        call. = FALSE)
  if (any(!is.finite(logits))) stop("a non-finite logit.", call. = FALSE)
  p <- .morie_km2_soft(logits)
  list(estimate = sum(s * p), probabilities = p, score_points = s,
       logits = logits, n = length(s),
       method = "G-Eval probability-weighted rubric score")
}

#' GloVe weighted least-squares cost (Kamath Ch 2)
#'
#' sum f(X_ij) (w_i . w~_j + b_i + b~_j - log X_ij)^2 over the NON-ZERO
#' counts only.
#'
#' @param X Co-occurrence counts. @param W,W_tilde Word and context vectors.
#' @param b,b_tilde Biases. @param x_max,alpha Weighting parameters.
#' @return List with `estimate`, `weights`, `residuals`, `n_nonzero`.
#' @export
morie_kamath_glove_cost <- function(X, W, W_tilde, b, b_tilde, x_max = 100,
                                    alpha = 0.75) {
  X <- as.matrix(X); Wm <- as.matrix(W); Wt <- as.matrix(W_tilde)
  b <- as.numeric(b); bt <- as.numeric(b_tilde)
  x_max <- as.numeric(x_max); alpha <- as.numeric(alpha)
  if (x_max <= 0) stop("x_max must be positive.", call. = FALSE)
  if (alpha <= 0) stop("alpha must be positive.", call. = FALSE)
  if (any(X < 0)) stop("co-occurrence counts must be non-negative.",
                       call. = FALSE)
  V <- nrow(X); C <- ncol(X)
  if (nrow(Wm) != V || nrow(Wt) != C) stop("W / W_tilde do not match X.",
                                           call. = FALSE)
  if (ncol(Wm) != ncol(Wt)) stop("word and context vectors differ in width.",
                                 call. = FALSE)
  if (length(b) != V || length(bt) != C) stop("the biases do not match X.",
                                              call. = FALSE)
  nz <- X > 0
  if (!any(nz)) stop("every co-occurrence count is 0.", call. = FALSE)
  pred <- Wm %*% t(Wt) + outer(b, rep(1, C)) + outer(rep(1, V), bt)
  resid <- matrix(0, V, C)
  resid[nz] <- pred[nz] - log(X[nz])
  f <- matrix(0, V, C)
  f[nz] <- morie_kamath_glove_weight(X[nz], x_max, alpha)
  J <- sum(f * resid^2)
  list(estimate = J, cost = J, weights = f, residuals = resid,
       n_nonzero = as.integer(sum(nz)), n = V * C,
       method = "GloVe weighted least-squares cost")
}

#' @rdname morie_kamath_glove_cost
#' @param x Counts to weight.
#' @export
morie_kamath_glove_weight <- function(x, x_max, alpha) {
  x <- as.numeric(x)
  ifelse(x < x_max, (x / x_max)^alpha, 1)
}

#' Groundedness reward (Kamath Ch 7)
#'
#' |answer tokens present in the context| / |answer tokens|.
#'
#' @param y_tokens Answer tokens. @param ctx_tokens Context tokens.
#' @param lowercase Case-fold before matching.
#' @return List with `estimate`, `n_grounded`, `ungrounded`.
#' @export
morie_kamath_groundedness_reward <- function(y_tokens, ctx_tokens,
                                             lowercase = TRUE) {
  y <- as.character(y_tokens); ctx <- as.character(ctx_tokens)
  if (length(y) == 0L) stop("the answer has no tokens.", call. = FALSE)
  if (length(ctx) == 0L) stop("the context has no tokens.", call. = FALSE)
  if (lowercase) { y <- tolower(y); ctx <- tolower(ctx) }
  flags <- y %in% ctx
  list(estimate = sum(flags) / length(y), n_grounded = as.integer(sum(flags)),
       n_tokens = length(y), ungrounded = sort(unique(y[!flags])),
       n = length(y),
       method = "Groundedness reward (answer tokens found in context)")
}

#' Hybrid dense/sparse score fusion (Kamath Ch 7)
#'
#' lam * dense + (1 - lam) * sparse, ranked; `ranking` is 0-BASED.
#'
#' @param s_dense,s_sparse Arm scores. @param lam Mixing weight in \[0, 1\].
#' @param normalize Min-max the arms first.
#' @return List with `scores`, `ranking`, `estimate`.
#' @export
morie_kamath_hybrid_retrieval_fusion <- function(s_dense, s_sparse, lam,
                                                 normalize = FALSE) {
  d <- as.numeric(s_dense); s <- as.numeric(s_sparse); lam <- as.numeric(lam)
  if (length(d) != length(s)) stop("the two arms score different counts.",
                                   call. = FALSE)
  if (length(d) == 0L) stop("no documents to fuse.", call. = FALSE)
  if (lam < 0 || lam > 1) stop("lam must lie in [0, 1].", call. = FALSE)
  if (any(!is.finite(d)) || any(!is.finite(s))) stop("scores must be finite.",
                                                     call. = FALSE)
  mm <- function(v) {
    if (max(v) == min(v)) stop("an arm gives every document the same score.",
                               call. = FALSE)
    (v - min(v)) / (max(v) - min(v))
  }
  dd <- if (normalize) mm(d) else d
  ss <- if (normalize) mm(s) else s
  fused <- lam * dd + (1 - lam) * ss
  ord <- .morie_km2_stable_desc(fused)
  list(scores = fused, ranking = ord - 1L, estimate = fused[ord[1]],
       lam = lam, normalized = normalize, n = length(fused),
       method = "Hybrid dense/sparse score fusion")
}

#' HyDE retrieval via a hypothetical document (Kamath Ch 7)
#'
#' Embeds the model's fake answer and retrieves the top-k by cosine.
#'
#' @param query Query. @param model Function query -> hypothetical text.
#' @param embeddings Named list or matrix of document embeddings.
#' @param embed Optional text embedder. @param k Depth.
#' @return List with `retrieved`, `similarities`, `hypothetical`, `estimate`.
#' @export
morie_kamath_hyde_hypothetical_doc <- function(query, model, embeddings,
                                               embed = NULL, k = 3) {
  if (!is.function(model)) stop("model must be a function.", call. = FALSE)
  if (!is.null(embed) && !is.function(embed)) stop("embed must be a function.",
                                                   call. = FALSE)
  if (is.list(embeddings) && !is.matrix(embeddings)) {
    if (length(embeddings) == 0L) stop("the embedding table is empty.",
                                       call. = FALSE)
    ids <- names(embeddings)
    D <- t(vapply(embeddings, as.numeric,
                  numeric(length(as.numeric(embeddings[[1]])))))
  } else {
    D <- as.matrix(embeddings)
    if (nrow(D) == 0L) stop("the embedding matrix is empty.", call. = FALSE)
    ids <- seq_len(nrow(D)) - 1L
  }
  hypo <- model(query)
  if (is.null(hypo)) stop("the model returned no hypothetical document.",
                          call. = FALSE)
  q <- as.numeric(if (!is.null(embed)) embed(hypo) else hypo)
  if (length(q) != ncol(D)) stop("the hypothetical embedding has the wrong width.",
                                 call. = FALSE)
  k <- as.integer(k)
  if (k < 1L || k > nrow(D)) stop("k is out of range.", call. = FALSE)
  nq <- sqrt(sum(q^2)); nd <- sqrt(rowSums(D^2))
  if (nq == 0 || any(nd == 0)) stop("a zero embedding has no direction.",
                                    call. = FALSE)
  sims <- unname(as.numeric(D %*% q) / (nd * nq))
  ord <- .morie_km2_stable_desc(sims)[seq_len(k)]
  list(retrieved = ids[ord], similarities = sims[ord], hypothetical = hypo,
       estimate = sims[ord[1]], k = k, n = nrow(D),
       method = "HyDE retrieval via a hypothetical document")
}

#' In-context learning conditional probability (Kamath Ch 3)
#'
#' P(y | \[ex_1..ex_K, x\]) from a caller-supplied LM.
#'
#' @param demonstrations Exemplar strings. @param query Test input.
#' @param model Function (prompt, answer) -> probability. @param answer Target.
#' @param sep Separator.
#' @return List with `estimate`, `log_prob`, `prompt`, `K`.
#' @export
morie_kamath_in_context_learning_prob <- function(demonstrations, query,
                                                  model, answer = NULL,
                                                  sep = "\n") {
  demos <- as.character(demonstrations)
  if (!is.function(model)) stop("model must be a function.", call. = FALSE)
  parts <- c(demos, as.character(query))
  prompt <- paste(parts, collapse = sep)
  p <- as.numeric(model(prompt, answer))
  if (p < 0 || p > 1) stop("the model did not return a probability.",
                           call. = FALSE)
  list(estimate = p, probability = p,
       log_prob = if (p == 0) -Inf else log(p), prompt = prompt,
       K = length(demos), n = length(parts),
       method = "In-context learning conditional probability")
}

#' Instruction-tuning cross entropy (Kamath Ch 4)
#'
#' Causal-LM cross entropy with the instruction masked out (-100).
#'
#' @param logits (T, V) logits. @param response_mask Logical or 0/1 mask.
#' @param targets Token ids (0-BASED).
#' @return List with `estimate`, `perplexity`, `n_response_tokens`.
#' @export
morie_kamath_instruction_tuning_loss <- function(logits, response_mask,
                                                 targets) {
  logits <- as.matrix(logits)
  m <- as.numeric(response_mask); t <- as.integer(targets)
  if (length(m) != nrow(logits)) stop("the mask has the wrong length.",
                                      call. = FALSE)
  if (length(t) != nrow(logits)) stop("targets has the wrong length.",
                                      call. = FALSE)
  if (!all(m %in% c(0, 1))) stop("response_mask must be logical or 0/1.",
                                 call. = FALSE)
  keep <- m == 1
  if (!any(keep)) stop("the response mask selects no token.", call. = FALSE)
  masked <- ifelse(keep, t, -100L)
  base <- .morie_km2_causal_lm_loss(logits, masked, ignore_index = -100L)
  list(estimate = base$loss, loss = base$loss, perplexity = base$perplexity,
       token_losses = base$token_losses,
       n_response_tokens = base$n_tokens, vocab_size = base$vocab_size,
       n = nrow(logits),
       method = "Instruction-tuning CE over response tokens")
}

#' Image-text contrastive loss (Kamath Ch 9)
#'
#' Symmetric InfoNCE over cosine(I, T) / tau; the batch must hold at least
#' two pairs.
#'
#' @param I_emb,T_emb Tower outputs. @param tau Temperature.
#' @return List with `estimate`, `loss_i2t`, `loss_t2i`, `similarity`.
#' @export
morie_kamath_image_text_contrastive <- function(I_emb, T_emb, tau) {
  I <- as.matrix(I_emb); Tm <- as.matrix(T_emb)
  tau <- as.numeric(tau)
  if (!all(dim(I) == dim(Tm))) stop("the two towers must match.",
                                    call. = FALSE)
  B <- nrow(I)
  if (B < 2L) stop("InfoNCE needs at least two pairs.", call. = FALSE)
  if (tau <= 0) stop("tau must be positive.", call. = FALSE)
  ni <- sqrt(rowSums(I^2)); nt <- sqrt(rowSums(Tm^2))
  if (any(ni == 0) || any(nt == 0)) stop("a zero embedding.", call. = FALSE)
  S <- (I / ni) %*% t(Tm / nt)
  logits <- S / tau
  i2t <- .morie_km2_rowce(logits)
  t2i <- .morie_km2_rowce(t(logits))
  loss <- 0.5 * (mean(i2t) + mean(t2i))
  list(estimate = loss, loss = loss, loss_i2t = mean(i2t),
       loss_t2i = mean(t2i), similarity = S, tau = tau, n = B,
       method = "Image-text contrastive (symmetric InfoNCE)")
}

#' Image-text matching head (Kamath Ch 9)
#'
#' p = sigmoid(w . \[I; T\] + b), or a caller-supplied fusion.
#'
#' @param image_emb,text_emb Embeddings. @param W Head weights. @param b Bias.
#' @param fuse Optional fusion function.
#' @return List with `estimate`, `logit`, `match`, `fused`.
#' @export
morie_kamath_image_text_matching <- function(image_emb, text_emb, W, b,
                                             fuse = NULL) {
  I <- as.numeric(image_emb); Tv <- as.numeric(text_emb)
  if (length(I) == 0L || length(Tv) == 0L) stop("empty embedding.",
                                                call. = FALSE)
  fused <- if (is.null(fuse)) c(I, Tv) else {
    if (!is.function(fuse)) stop("fuse must be a function.", call. = FALSE)
    as.numeric(fuse(I, Tv))
  }
  how <- if (is.null(fuse)) "concatenation [I; T]" else "caller-supplied fusion"
  w <- as.numeric(W)
  if (length(w) != length(fused)) stop("W does not match the fused vector.",
                                       call. = FALSE)
  z <- sum(w * fused) + as.numeric(b)
  p <- .morie_km2_sig(z)
  list(estimate = p, probability = p, logit = z, match = p >= 0.5,
       fused = fused, fusion = how, n = length(fused),
       method = "Image-text matching head p = sigmoid(w.fused + b)")
}

#' KL-shaped RLHF reward (Kamath Ch 5)
#'
#' r_phi - beta * KL; a negative KL or a negative beta is refused.
#'
#' @param r_phi Rewards. @param kl_divergence KL values (scalar broadcasts).
#' @param beta Non-negative weight.
#' @return List with `shaped`, `estimate`, `mean_kl`, `penalty`.
#' @export
morie_kamath_kl_reward_shaping <- function(r_phi, kl_divergence, beta) {
  r <- as.numeric(r_phi); kl <- as.numeric(kl_divergence)
  beta <- as.numeric(beta)
  if (length(r) == 0L) stop("no rewards supplied.", call. = FALSE)
  if (length(kl) == 1L && length(r) > 1L) kl <- rep(kl, length(r))
  if (length(r) != length(kl)) stop("rewards and KL values differ.",
                                    call. = FALSE)
  if (beta < 0) stop("beta must be non-negative.", call. = FALSE)
  if (any(kl < 0)) stop("a KL divergence is non-negative by definition.",
                        call. = FALSE)
  shaped <- r - beta * kl
  list(shaped = shaped, estimate = mean(shaped), mean_reward = mean(r),
       mean_kl = mean(kl), penalty = beta * mean(kl), beta = beta,
       n = length(r), method = "KL-shaped RLHF reward r - beta * KL")
}

#' MoE auxiliary load-balancing loss (Kamath Ch 2)
#'
#' alpha * N * sum f_i P_i; equals alpha under perfect balance.
#'
#' @param fractions Dispatch fractions. @param gate_means Mean router probs.
#' @param N Expert count. @param alpha Weight. @param tol Sum tolerance.
#' @return List with `estimate`, `per_expert`, `imbalance_ratio`.
#' @export
morie_kamath_moe_load_balance_loss <- function(fractions, gate_means, N,
                                               alpha, tol = 1e-6) {
  f <- as.numeric(fractions); P <- as.numeric(gate_means)
  N <- as.integer(N); alpha <- as.numeric(alpha)
  if (N < 1L) stop("N must be at least 1.", call. = FALSE)
  if (length(f) != N || length(P) != N) stop("expert counts disagree.",
                                             call. = FALSE)
  if (any(f < 0) || any(P < 0)) stop("fractions and gate means must be non-negative.",
                                     call. = FALSE)
  if (abs(sum(f) - 1) > tol) stop("the dispatch fractions do not sum to 1.",
                                  call. = FALSE)
  if (abs(sum(P) - 1) > tol) stop("the mean router probabilities do not sum to 1.",
                                  call. = FALSE)
  if (alpha < 0) stop("alpha must be non-negative.", call. = FALSE)
  val <- alpha * N * sum(f * P)
  list(estimate = val, loss = val, per_expert = f * P,
       balanced_floor = alpha,
       imbalance_ratio = if (alpha > 0) val / alpha else NaN,
       alpha = alpha, n = N,
       method = "MoE auxiliary load-balancing loss alpha*N*sum f_i P_i")
}

#' LoRA forward with alpha/r scaling (Kamath Ch 4)
#'
#' h = W0 x + (alpha/r) B A x. km058's Eq 4.5 has no scaling; this named
#' module carries the implementation's alpha/r, so the two stay separate.
#'
#' @param W0 Frozen weight. @param A (r, k). @param B (d, r).
#' @param alpha Scale numerator. @param r Rank. @param x Input.
#' @return List with `h`, `base`, `delta`, `scaling`, `n_trainable`.
#' @export
morie_kamath_lora_weight_update <- function(W0, A, B, alpha, r, x) {
  W0 <- as.matrix(W0); A <- as.matrix(A); B <- as.matrix(B)
  x <- as.numeric(x); r <- as.integer(r); alpha <- as.numeric(alpha)
  if (r < 1L) stop("the rank r must be at least 1.", call. = FALSE)
  d <- nrow(W0); k <- ncol(W0)
  if (!all(dim(A) == c(r, k))) stop("A must be (r, k).", call. = FALSE)
  if (!all(dim(B) == c(d, r))) stop("B must be (d, r).", call. = FALSE)
  if (length(x) != k) stop("x has the wrong length.", call. = FALSE)
  scale <- alpha / r
  base <- as.numeric(W0 %*% x)
  delta <- scale * as.numeric(B %*% (A %*% x))
  list(h = base + delta, base = base, delta = delta,
       estimate = (base + delta)[1], scaling = scale, rank = r,
       alpha = alpha, n_trainable = length(A) + length(B),
       n_frozen = length(W0), n = length(base),
       method = "LoRA forward h = W0 x + (alpha/r) B A x")
}

#' LLaVA visual instruction assembly (Kamath Ch 9)
#'
#' \[W ViT(image); text\] soft tokens; with `lm_head` and `targets` the causal
#' cross entropy is added. `targets` are 0-BASED with -100 ignored.
#'
#' @param image Image. @param W Projection (d, d_v).
#' @param visual_encoder Function image -> (n_patches, d_v).
#' @param text_tokens Text embedding rows. @param lm_head Optional head.
#' @param targets Optional target ids. @param ignore_index Ignored id.
#' @return List with `visual_tokens`, `inputs`, optionally `loss`.
#' @export
morie_kamath_llava_visual_instruction <- function(image, W, visual_encoder,
                                                  text_tokens, lm_head = NULL,
                                                  targets = NULL,
                                                  ignore_index = -100L) {
  if (!is.function(visual_encoder)) stop("visual_encoder must be a function.",
                                         call. = FALSE)
  feats <- as.matrix(visual_encoder(image))
  if (length(feats) == 0L) stop("the visual encoder returned nothing.",
                                call. = FALSE)
  W <- as.matrix(W)
  d <- nrow(W); d_v <- ncol(W)
  if (ncol(feats) != d_v) stop("W does not match the encoder output.",
                               call. = FALSE)
  txt <- as.matrix(text_tokens)
  if (ncol(txt) != d) stop("the visual and text tokens differ in width.",
                           call. = FALSE)
  z_v <- feats %*% t(W)
  inputs <- rbind(z_v, txt)
  out <- list(visual_tokens = z_v, inputs = inputs, n_visual = nrow(z_v),
              n_text = nrow(txt), d_model = d, estimate = z_v[1, 1],
              n = nrow(inputs),
              method = "LLaVA visual instruction assembly")
  if (is.null(lm_head) != is.null(targets)) {
    stop("lm_head and targets must be supplied together.", call. = FALSE)
  }
  if (!is.null(lm_head)) {
    if (!is.function(lm_head)) stop("lm_head must be a function.",
                                    call. = FALSE)
    logits <- as.matrix(lm_head(inputs))
    tgt <- as.integer(targets)
    if (nrow(logits) != nrow(inputs)) stop("lm_head returned the wrong shape.",
                                           call. = FALSE)
    if (length(tgt) != nrow(inputs)) stop("targets must cover all positions.",
                                          call. = FALSE)
    loss <- .morie_km2_causal_lm_loss(logits, tgt, ignore_index)
    out$loss <- loss$loss; out$perplexity <- loss$perplexity
    out$n_response_tokens <- loss$n_tokens; out$estimate <- loss$loss
  }
  out
}

#' Multimodal MAE squared reconstruction loss (Kamath Ch 9)
#'
#' sum_m ||x_masked - Decoder_m(visible)||^2 across the modalities.
#'
#' @param x_visible,x_masked_true,masks,decoders Named lists keyed by modality
#'   (a bare object is treated as the single modality "default").
#' @return List with `estimate`, `per_modality`, `n_masked`.
#' @export
morie_kamath_multimodal_mae <- function(x_visible, x_masked_true, masks,
                                        decoders = NULL) {
  if (is.null(decoders)) stop("decoders is required.", call. = FALSE)
  asdict <- function(o, nm) {
    if (is.list(o) && !is.null(names(o)) && all(nzchar(names(o)))) {
      if (length(o) == 0L) stop(sprintf("%s is empty.", nm), call. = FALSE)
      o
    } else list(default = o)
  }
  vis <- asdict(x_visible, "x_visible"); true <- asdict(x_masked_true, "x")
  msk <- asdict(masks, "masks"); dec <- asdict(decoders, "decoders")
  keys <- names(vis)
  for (other in list(true, msk, dec)) {
    if (!setequal(names(other), keys)) stop("the modality sets disagree.",
                                            call. = FALSE)
  }
  per <- list(); total <- 0; n_masked <- 0L
  for (m in keys) {
    f <- dec[[m]]
    if (!is.function(f)) stop("a decoder is not a function.", call. = FALSE)
    mask <- msk[[m]]
    if (!is.logical(mask) && !all(mask %in% c(0, 1))) {
      stop("a mask must be logical or 0/1.", call. = FALSE)
    }
    mask <- as.logical(mask)
    if (!any(mask)) stop("a modality has nothing masked.", call. = FALSE)
    gold <- as.numeric(true[[m]])
    if (length(gold) != sum(mask)) stop("the truth does not match the mask.",
                                        call. = FALSE)
    rec <- as.numeric(f(vis[[m]], mask))
    if (length(rec) != length(gold)) stop("a decoder returned the wrong count.",
                                          call. = FALSE)
    sse <- sum((gold - rec)^2)
    per[[m]] <- sse; total <- total + sse; n_masked <- n_masked + length(gold)
  }
  list(estimate = total, loss = total, per_modality = per, modalities = keys,
       n_masked = n_masked, n = length(keys),
       method = "Multimodal MAE squared reconstruction loss")
}

#' Mamba selective SSM scan (Kamath Ch 2)
#'
#' h_t = exp(dt A) h_{t-1} + dt B x_t; y_t = C . h_t, with a DIAGONAL A.
#'
#' @param x Input sequence. @param A State diagonal. @param B,C Per-step
#'   projections. @param delta Step sizes.
#' @return List with `y`, `states`, `A_bar`, `estimate`.
#' @export
morie_kamath_mamba_ssm <- function(x, A, B, C, delta) {
  x <- as.numeric(x)
  if (is.matrix(A)) stop("A must be the diagonal of the state matrix.",
                         call. = FALSE)
  A <- as.numeric(A)
  T <- length(x); N <- length(A)
  if (T == 0L) stop("the input sequence is empty.", call. = FALSE)
  if (N == 0L) stop("the state dimension is 0.", call. = FALSE)
  per_step <- function(v, nm) {
    if (is.matrix(v)) {
      if (!all(dim(v) == c(T, N))) stop(sprintf("%s has the wrong shape.", nm),
                                        call. = FALSE)
      v
    } else {
      vv <- as.numeric(v)
      if (length(vv) != N) stop(sprintf("%s has the wrong length.", nm),
                                call. = FALSE)
      matrix(vv, nrow = T, ncol = N, byrow = TRUE)
    }
  }
  Bm <- per_step(B, "B"); Cm <- per_step(C, "C")
  d <- as.numeric(delta)
  if (length(d) == 1L) d <- rep(d, T)
  if (length(d) != T) stop("delta must be scalar or length T.", call. = FALSE)
  if (any(d <= 0)) stop("delta must be positive.", call. = FALSE)
  Abar <- exp(outer(d, A))
  Bbar <- Bm * d
  h <- rep(0, N); ys <- numeric(T); hs <- matrix(0, T, N)
  for (t in seq_len(T)) {
    h <- Abar[t, ] * h + Bbar[t, ] * x[t]
    hs[t, ] <- h
    ys[t] <- sum(Cm[t, ] * h)
  }
  list(y = ys, states = hs, A_bar = Abar, estimate = ys[T], state_dim = N,
       n = T, method = "Mamba selective SSM scan (ZOH, diagonal A)")
}

#' Membership inference by loss threshold (Kamath Ch 6)
#'
#' Predict member iff loss < tau; with labels, TPR / FPR / advantage follow.
#'
#' @param losses Per-example losses. @param threshold Cut-off.
#' @param labels Optional 0/1 membership truth.
#' @return List with `predictions`, `member_rate`, optionally `accuracy`.
#' @export
morie_kamath_membership_inference <- function(losses, threshold,
                                              labels = NULL) {
  L <- as.numeric(losses); tau <- as.numeric(threshold)
  if (length(L) == 0L) stop("no losses supplied.", call. = FALSE)
  if (any(!is.finite(L))) stop("a non-finite loss.", call. = FALSE)
  pred <- as.integer(L < tau)
  out <- list(predictions = pred, n_predicted_members = as.integer(sum(pred)),
              member_rate = mean(pred), threshold = tau, n = length(L),
              estimate = mean(pred),
              method = "Membership inference by loss threshold")
  if (!is.null(labels)) {
    y <- as.integer(labels)
    if (length(y) != length(L)) stop("labels and losses differ.",
                                     call. = FALSE)
    if (!all(y %in% c(0L, 1L))) stop("labels must be 0 or 1.", call. = FALSE)
    pos <- sum(y == 1L); neg <- sum(y == 0L)
    if (pos == 0L || neg == 0L) stop("the labels contain only one class.",
                                     call. = FALSE)
    out$accuracy <- mean(pred == y)
    out$tpr <- sum(pred == 1L & y == 1L) / pos
    out$fpr <- sum(pred == 1L & y == 0L) / neg
    out$advantage <- out$tpr - out$fpr
    out$estimate <- out$accuracy
  }
  out
}

#' Medusa multi-head speculative prediction (Kamath Ch 8)
#'
#' k heads produce k future tokens (0-BASED ids); with `verify` the run of
#' accepted tokens is counted and reported as the estimate.
#'
#' @param hidden_state Hidden vector. @param medusa_heads Matrices or functions.
#' @param k Heads used. @param verify Optional (position, token) -> logical.
#' @return List with `tokens`, `probabilities`, `accepted`.
#' @export
morie_kamath_medusa_heads <- function(hidden_state, medusa_heads, k,
                                      verify = NULL) {
  h <- as.numeric(hidden_state); k <- as.integer(k)
  heads <- as.list(medusa_heads)
  if (k < 1L) stop("k must be at least 1.", call. = FALSE)
  if (k > length(heads)) stop("more speculative tokens than heads.",
                              call. = FALSE)
  tokens <- integer(0); probs <- numeric(0); all_probs <- list()
  for (i in seq_len(k)) {
    head <- heads[[i]]
    logits <- if (is.function(head)) as.numeric(head(h)) else {
      Wm <- as.matrix(head)
      if (nrow(Wm) != length(h)) stop("a head does not match the state.",
                                      call. = FALSE)
      as.numeric(h %*% Wm)
    }
    if (length(logits) < 2L) stop("a vocabulary of one predicts nothing.",
                                  call. = FALSE)
    if (any(!is.finite(logits))) stop("a non-finite logit.", call. = FALSE)
    p <- .morie_km2_soft(logits)
    t <- which.max(p) - 1L
    tokens <- c(tokens, t); probs <- c(probs, p[t + 1L])
    all_probs[[i]] <- p
  }
  accepted <- NULL
  if (!is.null(verify)) {
    if (!is.function(verify)) stop("verify must be a function.",
                                   call. = FALSE)
    accepted <- 0L
    for (i in seq_along(tokens)) {
      if (!isTRUE(verify(i - 1L, tokens[i]))) break
      accepted <- accepted + 1L
    }
  }
  list(tokens = tokens, probabilities = probs, distributions = all_probs,
       n_heads_available = length(heads), n_heads_used = k,
       accepted = accepted,
       estimate = if (!is.null(accepted)) accepted else probs[1], n = k,
       method = "Medusa multi-head speculative prediction")
}

#' Softmax top-k MoE router (Kamath Ch 2)
#'
#' Delegates to `morie_kamath_moe_topk_gating` (km040) and
#' `morie_kamath_moe_output` (km039); unselected experts are never run.
#'
#' @param x Token. @param Wr Router projection. @param experts Expert list.
#' @param k Experts kept.
#' @return List with `output`, `gate_weights`, `selected_experts` (0-based).
#' @export
morie_kamath_moe_router_softmax <- function(x, Wr, experts, k) {
  x <- as.numeric(x); W <- as.matrix(Wr); experts <- as.list(experts)
  if (ncol(W) != length(experts)) stop("the router and experts disagree.",
                                       call. = FALSE)
  gate <- morie_kamath_moe_topk_gating(x, W, k = as.integer(k))
  combined <- morie_kamath_moe_output(x, gate$weights, experts)
  list(output = combined$output, gate_weights = gate$weights,
       selected_experts = gate$selected_experts, n_active = gate$n_active,
       experts_evaluated = combined$experts_evaluated,
       estimate = combined$estimate, k = as.integer(k), n = length(experts),
       method = "Softmax top-k MoE router (delegates to km040 + km039)")
}

#' MoverScore from exact Word Mover's Distance (Kamath Ch 8)
#'
#' 1 - exact WMD / max pair cost. The transport LP is the same core the Eq
#' 8.10 module uses, so the two cannot drift.
#'
#' @param hypothesis_embeddings,reference_embeddings Token embedding rows.
#' @param weights_h,weights_r Optional weights (normalised to sum to 1).
#' @param metric "euclidean" or "cosine". @param normalizer Optional divisor.
#' @return List with `estimate`, `wmd`, `normalizer`.
#' @export
morie_kamath_moverscore <- function(hypothesis_embeddings,
                                    reference_embeddings, weights_h = NULL,
                                    weights_r = NULL, metric = "euclidean",
                                    normalizer = NULL) {
  H <- as.matrix(hypothesis_embeddings); R <- as.matrix(reference_embeddings)
  if (length(H) == 0L || length(R) == 0L) stop("both token sets must be non-empty.",
                                               call. = FALSE)
  if (ncol(H) != ncol(R)) stop("the embeddings differ in width.",
                               call. = FALSE)
  if (!(metric %in% c("euclidean", "cosine"))) {
    stop("metric must be 'euclidean' or 'cosine'.", call. = FALSE)
  }
  wf <- function(w, k, nm) {
    if (is.null(w)) return(rep(1 / k, k))
    v <- as.numeric(w)
    if (length(v) != k) stop(sprintf("%s has the wrong length.", nm),
                             call. = FALSE)
    if (any(v < 0)) stop(sprintf("%s must be non-negative.", nm),
                         call. = FALSE)
    if (sum(v) == 0) stop(sprintf("%s sums to 0.", nm), call. = FALSE)
    v / sum(v)
  }
  p <- wf(weights_h, nrow(H), "weights_h")
  q <- wf(weights_r, nrow(R), "weights_r")
  C <- if (metric == "euclidean") {
    outer(seq_len(nrow(H)), seq_len(nrow(R)),
          Vectorize(function(i, j) sqrt(sum((H[i, ] - R[j, ])^2))))
  } else {
    nh <- sqrt(rowSums(H^2)); nr <- sqrt(rowSums(R^2))
    if (any(nh == 0) || any(nr == 0)) stop("a zero embedding.", call. = FALSE)
    1 - (H / nh) %*% t(R / nr)
  }
  wmd <- sum(C * .morie_km2_transport(p, q, C)$flow)
  norm <- if (is.null(normalizer)) max(C) else as.numeric(normalizer)
  if (norm <= 0) stop("the normaliser is 0.", call. = FALSE)
  score <- 1 - wmd / norm
  list(estimate = score, score = score, wmd = wmd, normalizer = norm,
       metric = metric, n_hypothesis = nrow(H), n_reference = nrow(R),
       n = nrow(H), method = "MoverScore = 1 - exact WMD / normalizer")
}

#' @rdname morie_kamath_moverscore
#' @param cost Cost matrix. @param p,q Marginals. @param max_iter Guard.
#' @export
morie_kamath_word_movers_distance <- function(cost, p, q, max_iter = 10000) {
  C <- as.matrix(cost)
  sum(C * .morie_km2_transport(as.numeric(p), as.numeric(q), C)$flow)
}

#' NF4 equal-mass normal quantile grid (Kamath Ch 4)
#'
#' q_i = Phi^-1((i + 0.5)/n) plus the max-normalised levels and bin widths.
#'
#' @param n_bins Levels; 16 gives NF4.
#' @return List with `levels`, `normalized`, `bin_widths`, `n_bits`.
#' @export
morie_kamath_nf4_datatype <- function(n_bins = 16) {
  n <- as.integer(n_bins)
  if (n < 2L) stop("a data type needs at least 2 levels.", call. = FALSE)
  levels <- morie_kamath_normal_quantile((seq_len(n) - 0.5) / n)
  m <- max(abs(levels))
  if (m == 0) stop("the quantile grid collapsed to zero.", call. = FALSE)
  list(levels = levels, normalized = levels / m, bin_widths = diff(levels),
       n_bits = if (bitwAnd(n, n - 1L) == 0L) log2(n) else NULL,
       estimate = levels[n], n = n,
       method = "NF4 equal-mass normal quantile grid")
}

#' @rdname morie_kamath_nf4_datatype
#' @param u Probabilities strictly inside (0, 1).
#' @export
morie_kamath_normal_quantile <- function(u) {
  u <- as.numeric(u)
  if (any(u <= 0 | u >= 1)) stop("the argument must lie inside (0, 1).",
                                 call. = FALSE)
  stats::qnorm(u)
}

#' N-gram MLE conditional probability (Kamath Ch 2)
#'
#' count(ngram) / count(prefix); 0/0 is refused. Delegates to
#' `morie_burkov_ngram_mle`.
#'
#' @param counts_ngram,counts_prefix Counts.
#' @return List with `estimate`, `count_ngram`, `count_prefix`.
#' @export
morie_kamath_ngram_language_model <- function(counts_ngram, counts_prefix) {
  base <- morie_burkov_ngram_mle(counts_ngram, counts_prefix)
  list(estimate = base$estimate, probability = base$estimate,
       count_ngram = base$count_ngram, count_prefix = base$count_prefix,
       n = base$n,
       method = "N-gram MLE conditional probability (delegates to bkngr)")
}

#' Nucleus (top-p) truncation (Kamath Ch 8)
#'
#' Temperature-scaled softmax truncated to the smallest set with cumulative
#' mass >= p, then renormalised. `kept` is 0-BASED.
#'
#' @param logits Logits. @param p Nucleus mass in (0, 1\]. @param T Temperature.
#' @return List with `probabilities`, `kept`, `n_kept`, `kept_mass`.
#' @export
morie_kamath_nucleus_sampling <- function(logits, p, T = 1) {
  z <- as.numeric(logits)
  if (length(z) == 0L) stop("no logits supplied.", call. = FALSE)
  if (any(!is.finite(z))) stop("logits must be finite.", call. = FALSE)
  if (!(as.numeric(p) > 0 && as.numeric(p) <= 1)) {
    stop("p must lie in (0, 1].", call. = FALSE)
  }
  if (as.numeric(T) <= 0) stop("the temperature must be positive.",
                               call. = FALSE)
  base <- .morie_km2_top_p(z, as.numeric(p), as.numeric(T))
  raw <- .morie_km2_soft(z / as.numeric(T))
  list(probabilities = base$tensor, kept = which(base$keep_mask) - 1L,
       n_kept = base$n_kept, kept_mass = sum(raw[base$keep_mask]),
       estimate = max(base$tensor), p = as.numeric(p),
       temperature = as.numeric(T), n = length(z),
       method = "Nucleus (top-p) truncation")
}

#' NExT-GPT any-to-any pipeline (Kamath Ch 9)
#'
#' encoders -> llm -> decoders, with every contract checked. No input may be
#' silently dropped.
#'
#' @param inputs_by_modality Named list of inputs. @param encoders Named list.
#' @param llm Function features -> state. @param decoders Named list.
#' @param output_modalities Optional subset.
#' @return List with `outputs`, `features`, `llm_state`.
#' @export
morie_kamath_nextgpt_any2any <- function(inputs_by_modality, encoders, llm,
                                         decoders, output_modalities = NULL) {
  if (!is.list(inputs_by_modality) || length(inputs_by_modality) == 0L) {
    stop("inputs_by_modality must be a non-empty named list.", call. = FALSE)
  }
  if (!is.list(encoders)) stop("encoders must be a named list.",
                               call. = FALSE)
  if (!is.list(decoders) || length(decoders) == 0L) {
    stop("decoders must be a non-empty named list.", call. = FALSE)
  }
  if (!is.function(llm)) stop("llm must be a function.", call. = FALSE)
  in_mods <- sort(names(inputs_by_modality))
  if (!all(in_mods %in% names(encoders))) {
    stop("no encoder for an input modality.", call. = FALSE)
  }
  feats <- list()
  for (m in in_mods) {
    f <- encoders[[m]]
    if (!is.function(f)) stop("an encoder is not a function.", call. = FALSE)
    e <- f(inputs_by_modality[[m]])
    if (is.null(e)) stop("an encoder returned nothing.", call. = FALSE)
    feats[[m]] <- e
  }
  state <- llm(feats)
  if (is.null(state)) stop("the LLM returned no state.", call. = FALSE)
  out_mods <- if (is.null(output_modalities)) sort(names(decoders)) else
    as.character(output_modalities)
  if (!all(out_mods %in% names(decoders))) {
    stop("no decoder for a requested modality.", call. = FALSE)
  }
  outputs <- list()
  for (m in out_mods) {
    g <- decoders[[m]]
    if (!is.function(g)) stop("a decoder is not a function.", call. = FALSE)
    outputs[[m]] <- g(state)
  }
  list(outputs = outputs, features = feats, llm_state = state,
       input_modalities = in_mods, output_modalities = out_mods,
       estimate = length(outputs), n = length(in_mods) + length(out_mods),
       method = "NExT-GPT any-to-any encode / LLM / decode pipeline")
}

#' P-tuning v2 deep prefix concatenation (Kamath Ch 4)
#'
#' \[P_K_l; K_l\] and \[P_V_l; V_l\] at every layer.
#'
#' @param prefixes_by_layer List of list(P_K, P_V) per layer.
#' @param inputs_by_layer List of list(K, V) per layer.
#' @return List with `K`, `V`, `prefix_len`, `n_trainable`.
#' @export
morie_kamath_p_tuning_v2 <- function(prefixes_by_layer, inputs_by_layer) {
  pre <- as.list(prefixes_by_layer); inp <- as.list(inputs_by_layer)
  if (length(pre) == 0L) stop("no prefixes supplied.", call. = FALSE)
  if (length(pre) != length(inp)) stop("one prefix per layer is required.",
                                       call. = FALSE)
  Ks <- list(); Vs <- list(); plens <- integer(0); trainable <- 0L
  for (l in seq_along(pre)) {
    if (length(pre[[l]]) != 2L || length(inp[[l]]) != 2L) {
      stop("expected (K, V) pairs on both sides.", call. = FALSE)
    }
    PK <- as.matrix(pre[[l]][[1]]); PV <- as.matrix(pre[[l]][[2]])
    K <- as.matrix(inp[[l]][[1]]); V <- as.matrix(inp[[l]][[2]])
    if (nrow(PK) != nrow(PV)) stop("the key and value prefixes differ.",
                                   call. = FALSE)
    if (nrow(K) != nrow(V)) stop("the input keys and values differ.",
                                 call. = FALSE)
    if (ncol(PK) != ncol(K)) stop("key widths differ.", call. = FALSE)
    if (ncol(PV) != ncol(V)) stop("value widths differ.", call. = FALSE)
    Ks[[l]] <- rbind(PK, K); Vs[[l]] <- rbind(PV, V)
    plens <- c(plens, nrow(PK))
    trainable <- trainable + length(PK) + length(PV)
  }
  list(K = Ks, V = Vs, prefix_len = plens, n_layers = length(pre),
       n_trainable = trainable, estimate = trainable, n = length(pre),
       method = "P-tuning v2 deep prefix concatenation")
}

#' Perplexity from log-probabilities (Kamath Ch 8)
#'
#' exp(-mean log p); positive log-probabilities are refused because they are
#' probabilities or logits, not natural logs.
#'
#' @param log_probs Log-probabilities (<= 0). @param base "e" or "2".
#' @return List with `estimate`, `mean_nll`, `bits_per_token`.
#' @export
morie_kamath_perplexity <- function(log_probs, base = "e") {
  lp <- as.numeric(log_probs)
  if (length(lp) == 0L) stop("no tokens scored.", call. = FALSE)
  if (any(is.nan(lp))) stop("a log-probability is NaN.", call. = FALSE)
  if (any(lp > 0)) stop("log-probabilities must be <= 0.", call. = FALSE)
  if (!(base %in% c("e", "2"))) stop("base must be 'e' or '2'.",
                                     call. = FALSE)
  mean_nll <- -mean(lp)
  ppl <- if (base == "e") exp(mean_nll) else 2^mean_nll
  list(estimate = ppl, perplexity = ppl, mean_nll = mean_nll,
       total_nll = -sum(lp), bits_per_token = mean_nll / log(2),
       base = base, n = length(lp),
       method = "Perplexity exp(mean negative log-likelihood)")
}

#' PET loss (Kamath Ch 3)
#'
#' Verbalizer cross entropy + alpha * the km022 MLM loss.
#'
#' @param verbalizer_logits Class logits. @param y_true 0-BASED true class.
#' @param mlm_logits (T, V) logits. @param mlm_targets Ids, -100 ignored.
#' @param alpha Non-negative weight. @param ignore_index Ignored id.
#' @return List with `estimate`, `loss_ce`, `loss_mlm`, `n_masked`.
#' @export
morie_kamath_pet_loss <- function(verbalizer_logits, y_true, mlm_logits,
                                  mlm_targets, alpha, ignore_index = -100L) {
  vz <- as.numeric(verbalizer_logits); alpha <- as.numeric(alpha)
  if (length(vz) < 2L) stop("the verbalizer must score two classes.",
                            call. = FALSE)
  if (any(!is.finite(vz))) stop("a verbalizer logit is non-finite.",
                                call. = FALSE)
  y <- as.integer(y_true)
  if (y < 0L || y >= length(vz)) stop("y_true is out of range.",
                                      call. = FALSE)
  if (alpha < 0) stop("alpha must be non-negative.", call. = FALSE)
  ce <- -log(.morie_km2_soft(vz)[y + 1L])
  ml <- as.matrix(mlm_logits); mt <- as.integer(mlm_targets)
  if (length(mt) != nrow(ml)) stop("mlm_targets has the wrong length.",
                                   call. = FALSE)
  masked <- which(mt != ignore_index)
  if (length(masked) == 0L) stop("no position is masked.", call. = FALSE)
  if (any(mt[masked] < 0L | mt[masked] >= ncol(ml))) {
    stop("a masked target id is out of range.", call. = FALSE)
  }
  p_true <- rep(1, nrow(ml))
  for (t in masked) p_true[t] <- .morie_km2_soft(ml[t, ])[mt[t] + 1L]
  mlm <- morie_kamath_mlm_loss(p_true, masked - 1L)
  total <- ce + alpha * mlm$estimate
  list(estimate = total, loss = total, loss_ce = ce,
       loss_mlm = mlm$estimate, alpha = alpha, n_masked = length(masked),
       n = nrow(ml),
       method = "PET loss = verbalizer CE + alpha * MLM (via km022)")
}

#' Post-LN and pre-LN transformer blocks (Kamath Ch 2)
#'
#' @details One LayerNorm serves both Python modules exactly as kmprln
#' imports kmpoln's `layer_norm`: kmpoln ->
#' `morie_kamath_post_ln_transformer` (LN after the add), kmprln ->
#' `morie_kamath_pre_ln_transformer` (LN before the sublayer). The variance
#' is the POPULATION variance, matching numpy's `var`.
#'
#' @param x Input rows. @param attn_fn,ffn_fn Shape-preserving sublayers.
#' @param eps LayerNorm epsilon.
#' @return List with `output`, `after_attention`, `placement`.
#' @export
morie_kamath_post_ln_transformer <- function(x, attn_fn, ffn_fn, eps = 1e-5) {
  x <- as.matrix(x)
  if (!is.function(attn_fn) || !is.function(ffn_fn)) {
    stop("attn_fn and ffn_fn must be functions.", call. = FALSE)
  }
  if (eps < 0) stop("eps must be non-negative.", call. = FALSE)
  sub <- function(f, v) {
    out <- as.matrix(f(v))
    if (!all(dim(out) == dim(v))) stop("a sublayer changed the shape.",
                                       call. = FALSE)
    out
  }
  y <- .morie_km2_layer_norm(x + sub(attn_fn, x), eps)
  z <- .morie_km2_layer_norm(y + sub(ffn_fn, y), eps)
  list(output = z, after_attention = y, estimate = z[1, 1],
       placement = "post-LN", eps = as.numeric(eps), n = nrow(z),
       method = "Post-LayerNorm transformer block")
}

#' @rdname morie_kamath_post_ln_transformer
#' @export
morie_kamath_pre_ln_transformer <- function(x, attn_fn, ffn_fn, eps = 1e-5) {
  x <- as.matrix(x)
  if (!is.function(attn_fn) || !is.function(ffn_fn)) {
    stop("attn_fn and ffn_fn must be functions.", call. = FALSE)
  }
  if (eps < 0) stop("eps must be non-negative.", call. = FALSE)
  sub <- function(f, v) {
    out <- as.matrix(f(v))
    if (!all(dim(out) == dim(v))) stop("a sublayer changed the shape.",
                                       call. = FALSE)
    out
  }
  y <- x + sub(attn_fn, .morie_km2_layer_norm(x, eps))
  z <- y + sub(ffn_fn, .morie_km2_layer_norm(y, eps))
  list(output = z, after_attention = y, estimate = z[1, 1],
       placement = "pre-LN", eps = as.numeric(eps), n = nrow(z),
       method = "Pre-LayerNorm transformer block")
}

#' @rdname morie_kamath_post_ln_transformer
#' @export
morie_kamath_layer_norm_rows <- function(x, eps = 1e-5) {
  .morie_km2_layer_norm(x, eps)
}

#' PPO-RLHF objective (Kamath Ch 5)
#'
#' mean(r - beta (log pi_theta - log pi_ref)).
#'
#' @param rewards Rewards. @param logp_theta,logp_ref Log-probabilities (<= 0).
#' @param beta Non-negative weight.
#' @return List with `estimate`, `mean_reward`, `kl_estimate`, `per_sample`.
#' @export
morie_kamath_ppo_rlhf_objective <- function(rewards, logp_theta, logp_ref,
                                            beta) {
  r <- as.numeric(rewards); lt <- as.numeric(logp_theta)
  lr <- as.numeric(logp_ref); beta <- as.numeric(beta)
  if (!(length(r) == length(lt) && length(lt) == length(lr))) {
    stop("the batch sizes disagree.", call. = FALSE)
  }
  if (length(r) == 0L) stop("the batch is empty.", call. = FALSE)
  if (beta < 0) stop("beta must be non-negative.", call. = FALSE)
  if (any(lt > 0) || any(lr > 0)) stop("log-probabilities must be <= 0.",
                                       call. = FALSE)
  ratio <- lt - lr
  per <- r - beta * ratio
  list(estimate = mean(per), objective = mean(per), mean_reward = mean(r),
       kl_estimate = mean(ratio), penalty = beta * mean(ratio),
       per_sample = per, beta = beta, n = length(r),
       method = "PPO-RLHF objective E[r] - beta * E[log pi/pi_ref]")
}

#' Prefix tuning key/value concatenation (Kamath Ch 4)
#'
#' \[P_K; K\] and \[P_V; V\]; with `Q` the attention is run through
#' `morie_alammar_sdp_attention`.
#'
#' @param prefix_K,prefix_V Virtual key/value tokens.
#' @param K_input,V_input Real keys and values. @param Q Optional queries.
#' @return List with `K`, `V`, `prefix_len`, `n_trainable`.
#' @export
morie_kamath_prefix_tuning <- function(prefix_K, prefix_V, K_input, V_input,
                                       Q = NULL) {
  PK <- as.matrix(prefix_K); PV <- as.matrix(prefix_V)
  K <- as.matrix(K_input); V <- as.matrix(V_input)
  if (nrow(PK) != nrow(PV)) stop("the key and value prefixes differ.",
                                 call. = FALSE)
  if (nrow(PK) == 0L) stop("an empty prefix tunes nothing.", call. = FALSE)
  if (nrow(K) != nrow(V)) stop("the input keys and values differ.",
                               call. = FALSE)
  if (ncol(PK) != ncol(K)) stop("key widths differ.", call. = FALSE)
  if (ncol(PV) != ncol(V)) stop("value widths differ.", call. = FALSE)
  Kf <- rbind(PK, K); Vf <- rbind(PV, V)
  out <- list(K = Kf, V = Vf, prefix_len = nrow(PK), seq_len = nrow(Kf),
              n_trainable = length(PK) + length(PV), estimate = nrow(PK),
              n = nrow(Kf),
              method = "Prefix tuning key/value concatenation")
  if (!is.null(Q)) {
    att <- morie_alammar_sdp_attention(Q, Kf, Vf)
    out$attention_output <- att$output
    out$attention_weights <- att$attention
    out$prefix_attention_mass <- rowSums(
      att$attention[, seq_len(nrow(PK)), drop = FALSE])
    out$estimate <- att$estimate
    out$method <- paste(out$method, "+ reused attention core")
  }
  out
}

#' Prompt tuning soft-prompt prepend (Kamath Ch 4)
#'
#' X_aug = \[P; X\]; only the L_p x d prompt is trained.
#'
#' @param P Soft prompt rows. @param X Input embedding rows.
#' @return List with `X_aug`, `prompt_len`, `n_trainable`.
#' @export
morie_kamath_prompt_tuning <- function(P, X) {
  P <- as.matrix(P); X <- as.matrix(X)
  if (nrow(P) == 0L) stop("a zero-length soft prompt tunes nothing.",
                          call. = FALSE)
  if (nrow(X) == 0L) stop("the input sequence is empty.", call. = FALSE)
  if (ncol(P) != ncol(X)) stop("the prompt must live in the embedding space.",
                               call. = FALSE)
  aug <- rbind(P, X)
  list(X_aug = aug, prompt_len = nrow(P), seq_len = nrow(aug),
       d_model = ncol(P), n_trainable = length(P), estimate = length(P),
       n = nrow(aug), method = "Prompt tuning soft-prompt prepend")
}

#' Q-Former cross-attention (Kamath Ch 9)
#'
#' Learned queries attend to the visual patches through
#' `morie_alammar_sdp_attention`, with an optional output projection.
#'
#' @param queries Query rows. @param visual_features Patch rows.
#' @param W_out Optional projection.
#' @return List with `Z`, `attention`, `compression`.
#' @export
morie_kamath_q_former <- function(queries, visual_features, W_out = NULL) {
  Q <- as.matrix(queries); F <- as.matrix(visual_features)
  if (nrow(Q) == 0L) stop("a Q-Former with no queries outputs nothing.",
                          call. = FALSE)
  if (nrow(F) == 0L) stop("no visual features to attend to.", call. = FALSE)
  if (ncol(Q) != ncol(F)) stop("cross-attention needs a shared width.",
                               call. = FALSE)
  att <- morie_alammar_sdp_attention(Q, F, F)
  Z <- as.matrix(att$output)
  if (!is.null(W_out)) {
    W <- as.matrix(W_out)
    if (nrow(W) != ncol(Z)) stop("W_out does not fit Z.", call. = FALSE)
    Z <- Z %*% W
  }
  list(Z = Z, attention = att$attention, n_queries = nrow(Q),
       n_patches = nrow(F), compression = nrow(F) / nrow(Q),
       estimate = Z[1, 1], n = nrow(Q),
       method = "Q-Former cross-attention (reused attention core)")
}

#' QLoRA 4-bit forward (Kamath Ch 4)
#'
#' The NF4 grid dequantises W0, then `morie_kamath_lora_weight_update` adds
#' (alpha/r) B A x. Codes are 0-BASED indices into the grid.
#'
#' @param W0_nf4 List with `codes` and `absmax`. @param A,B LoRA factors.
#' @param alpha,r Scaling. @param x Input. @param n_bins Grid size.
#' @return List with `h`, `W0_dequantized`, `n_frozen_4bit`.
#' @export
morie_kamath_qlora_4bit <- function(W0_nf4, A, B, alpha, r, x, n_bins = 16) {
  if (!is.list(W0_nf4) || is.null(W0_nf4$codes) || is.null(W0_nf4$absmax)) {
    stop("W0_nf4 must carry 'codes' and 'absmax'.", call. = FALSE)
  }
  W0 <- morie_kamath_dequantize_nf4(W0_nf4$codes, W0_nf4$absmax,
                                    n_bins = n_bins)
  base <- morie_kamath_lora_weight_update(W0, A, B, alpha, r, x)
  list(h = base$h, base = base$base, delta = base$delta,
       W0_dequantized = W0, scaling = base$scaling, rank = base$rank,
       n_trainable = base$n_trainable,
       n_frozen_4bit = length(as.matrix(W0_nf4$codes)),
       estimate = base$estimate, n = base$n,
       method = "QLoRA: NF4 dequantised base + LoRA adapter")
}

#' @rdname morie_kamath_qlora_4bit
#' @param codes 0-based NF4 codes. @param absmax Scalar or per-row scale.
#' @export
morie_kamath_dequantize_nf4 <- function(codes, absmax, n_bins = 16) {
  codes <- as.matrix(codes)
  if (any(codes != round(codes))) stop("NF4 codes must be integers.",
                                       call. = FALSE)
  grid <- morie_kamath_nf4_datatype(n_bins)$normalized
  if (any(codes < 0 | codes >= length(grid))) {
    stop("a code lies outside the NF4 grid.", call. = FALSE)
  }
  out <- matrix(grid[codes + 1L], nrow = nrow(codes), ncol = ncol(codes))
  s <- as.numeric(absmax)
  if (length(s) == 1L) {
    if (s <= 0) stop("absmax must be positive.", call. = FALSE)
    return(out * s)
  }
  if (length(s) != nrow(codes)) stop("blockwise absmax needs one per row.",
                                     call. = FALSE)
  if (any(s <= 0)) stop("every block scale must be positive.", call. = FALSE)
  out * s
}

#' RetNet retention (Kamath Ch 2)
#'
#' ((Q K^T) .* gamma^(i-j) causal) V, with the recurrent form returned
#' alongside as the equality check the parallel form must satisfy.
#'
#' @param Q,K,V Sequence matrices. @param gamma Decay in (0, 1\].
#' @return List with `output`, `decay`, `recurrent_output`.
#' @export
morie_kamath_retnet_retention <- function(Q, K, V, gamma) {
  Q <- as.matrix(Q); K <- as.matrix(K); V <- as.matrix(V)
  gamma <- as.numeric(gamma)
  if (ncol(Q) != ncol(K)) stop("Q and K must share a width.", call. = FALSE)
  if (nrow(Q) != nrow(K) || nrow(K) != nrow(V)) {
    stop("retention is causal within one sequence.", call. = FALSE)
  }
  if (gamma <= 0 || gamma > 1) stop("gamma must lie in (0, 1].",
                                    call. = FALSE)
  T <- nrow(Q)
  i <- matrix(seq_len(T) - 1L, T, T)
  j <- matrix(seq_len(T) - 1L, T, T, byrow = TRUE)
  D <- ifelse(i >= j, gamma^(i - j), 0)
  out <- (Q %*% t(K) * D) %*% V
  S <- matrix(0, ncol(Q), ncol(V))
  rec <- matrix(0, T, ncol(V))
  for (t in seq_len(T)) {
    S <- gamma * S + outer(K[t, ], V[t, ])
    rec[t, ] <- as.numeric(Q[t, ] %*% S)
  }
  list(output = out, decay = D, recurrent_output = rec,
       estimate = out[T, 1], gamma = gamma, n = T,
       method = "RetNet retention ((QK^T) .* D) V")
}

#' RLHF pipeline SFT -> RM -> PPO (Kamath Ch 5)
#'
#' The KL reference is the SFT policy, and that identity is reported.
#'
#' @param demos Demonstrations. @param preferences Preference pairs.
#' @param pi0 Base policy. @param sft,train_rm,ppo Stage functions.
#' @return List with `policy`, `policy_sft`, `reward_model`, `stages`.
#' @export
morie_kamath_rlhf_pipeline <- function(demos, preferences, pi0, sft = NULL,
                                       train_rm = NULL, ppo = NULL) {
  for (f in list(sft, train_rm, ppo)) {
    if (is.null(f) || !is.function(f)) {
      stop("sft, train_rm and ppo must all be supplied as functions.",
           call. = FALSE)
    }
  }
  demos <- as.list(demos); prefs <- as.list(preferences)
  if (length(demos) == 0L) stop("no demonstrations.", call. = FALSE)
  if (length(prefs) == 0L) stop("no preference pairs.", call. = FALSE)
  pi_sft <- sft(pi0, demos)
  if (is.null(pi_sft)) stop("sft returned no policy.", call. = FALSE)
  r_phi <- train_rm(prefs)
  if (!is.function(r_phi)) stop("train_rm must return a reward function.",
                                call. = FALSE)
  ref <- pi_sft
  pi_rlhf <- ppo(pi_sft, r_phi, ref)
  if (is.null(pi_rlhf)) stop("ppo returned no policy.", call. = FALSE)
  list(policy = pi_rlhf, policy_sft = pi_sft, reward_model = r_phi,
       kl_reference = ref, kl_reference_is_sft = identical(ref, pi_sft),
       stages = c("sft", "reward_model", "ppo"), n_demos = length(demos),
       n_preferences = length(prefs), estimate = 3L, n = 3L,
       method = "RLHF pipeline SFT -> RM -> PPO (KL anchored to SFT)")
}

#' RLAIF Bradley-Terry reward fit (Kamath Ch 5)
#'
#' MM iteration for the BT strengths over the AI preference graph; a graph
#' that is not strongly connected has no finite MLE and is refused. The
#' pairwise loss comes from `morie_kamath_reward_model_training_loss`.
#'
#' @param ai_preferences List of (winner, loser) pairs.
#' @param max_iter,tol Iteration controls.
#' @return List with `items`, `strengths`, `scores`, `loss`, `accuracy`.
#' @export
morie_kamath_rlaif_objective <- function(ai_preferences, max_iter = 1000,
                                         tol = 1e-12) {
  prefs <- lapply(ai_preferences, function(p) {
    if (!is.null(names(p)) && all(c("winner", "loser") %in% names(p))) {
      c(p[["winner"]], p[["loser"]])
    } else {
      pp <- unlist(p)
      if (length(pp) != 2L) stop("each preference must be (winner, loser).",
                                 call. = FALSE)
      pp
    }
  })
  if (length(prefs) == 0L) stop("no AI preferences supplied.", call. = FALSE)
  items <- sort(unique(unlist(prefs)))
  n <- length(items)
  if (n < 2L) stop("preferences over a single item carry no information.",
                   call. = FALSE)
  edges <- t(vapply(prefs, function(p) match(p, items), integer(2)))
  if (any(edges[, 1] == edges[, 2])) {
    stop("an item cannot be preferred to itself.", call. = FALSE)
  }
  reach <- function(from, to) {
    seen <- 1L; stack <- 1L
    while (length(stack)) {
      u <- stack[length(stack)]; stack <- stack[-length(stack)]
      vs <- to[from == u]
      new <- setdiff(vs, seen)
      seen <- c(seen, new); stack <- c(stack, new)
    }
    length(seen) == n
  }
  if (!reach(edges[, 1], edges[, 2]) || !reach(edges[, 2], edges[, 1])) {
    stop("the preference graph is not strongly connected.", call. = FALSE)
  }
  wins <- rep(0, n); counts <- matrix(0, n, n)
  for (e in seq_len(nrow(edges))) {
    w <- edges[e, 1]; l <- edges[e, 2]
    wins[w] <- wins[w] + 1
    counts[w, l] <- counts[w, l] + 1
    counts[l, w] <- counts[l, w] + 1
  }
  p <- rep(1 / n, n)
  for (it in seq_len(as.integer(max_iter))) {
    denom <- vapply(seq_len(n), function(i)
      sum(counts[i, ] / (p[i] + p)) - counts[i, i] / (2 * p[i]), numeric(1))
    if (any(denom <= 0)) stop("the BT iteration hit a zero denominator.",
                              call. = FALSE)
    new <- wins / denom
    new <- new / sum(new)
    done <- max(abs(new - p)) < tol
    p <- new
    if (done) break
  }
  scores <- log(p)
  loss <- morie_kamath_reward_model_training_loss(scores[edges[, 1]],
                                                  scores[edges[, 2]])
  list(items = items, strengths = p, scores = scores, loss = loss$estimate,
       accuracy = loss$accuracy, n_preferences = nrow(edges),
       estimate = loss$estimate, n = n,
       method = "RLAIF Bradley-Terry reward fit (loss via kmrmloss)")
}

#' Bradley-Terry reward-model NLL over preference pairs (Kamath Ch 5)
#'
#' mean log(1 + exp(-(r_w - r_l))), with the pair accuracy reported.
#'
#' @param scores_w,scores_l Chosen and rejected scores.
#' @return List with `estimate`, `per_pair`, `margins`, `accuracy`.
#' @export
morie_kamath_reward_model_training_loss <- function(scores_w, scores_l) {
  w <- as.numeric(scores_w); l <- as.numeric(scores_l)
  if (length(w) != length(l)) stop("the loss is over PAIRS.", call. = FALSE)
  if (length(w) == 0L) stop("no preference pairs supplied.", call. = FALSE)
  if (any(!is.finite(w)) || any(!is.finite(l))) {
    stop("reward scores must be finite.", call. = FALSE)
  }
  d <- w - l
  per <- log1p(exp(-abs(d))) + pmax(-d, 0)
  list(estimate = mean(per), loss = mean(per), per_pair = per, margins = d,
       mean_margin = mean(d), accuracy = mean(d > 0), n = length(w),
       method = "Bradley-Terry reward-model NLL over preference pairs")
}

#' RMSNorm (Kamath Ch 2)
#'
#' x / sqrt(mean(x^2) + eps) * g; no mean subtraction, no bias.
#'
#' @param x Input rows. @param g Optional gain. @param eps Epsilon.
#' @return List with `y`, `tensor`, `rms`.
#' @export
morie_kamath_rms_norm <- function(x, g = NULL, eps = 1e-6) {
  X <- x
  if (length(as.numeric(X)) == 0L) stop("nothing to normalise.",
                                        call. = FALSE)
  if (any(!is.finite(as.numeric(X)))) stop("x must be finite.", call. = FALSE)
  eps <- as.numeric(eps)
  if (eps < 0) stop("eps must be non-negative.", call. = FALSE)
  width <- if (is.matrix(X)) ncol(X) else length(as.numeric(X))
  if (!is.null(g) && length(as.numeric(g)) != width) {
    stop("the gain does not match the feature axis.", call. = FALSE)
  }
  if (eps == 0 && all(as.numeric(X) == 0)) {
    stop("RMS(x) is 0 with eps = 0.", call. = FALSE)
  }
  base <- .morie_km2_rms(X, gamma = g, eps = eps)
  y <- as.numeric(base$tensor)
  list(y = y, tensor = base$tensor, rms = base$rms, estimate = y[1],
       eps = eps, n = width,
       method = "RMSNorm x / sqrt(mean(x^2) + eps) * g")
}

#' Rotary positional embedding (Kamath Ch 2)
#'
#' Rotates feature PAIRS by m * base^(-2i/d) at explicit positions.
#'
#' @param q (d,) or (seq_len, d) input. @param positions Optional positions.
#' @param base RoPE base.
#' @return List with `y`, `angles`, `theta`, `positions`.
#' @export
morie_kamath_rotary_positional_embedding <- function(q, positions = NULL,
                                                     base = 10000) {
  x <- if (is.matrix(q)) q else matrix(as.numeric(q), nrow = 1L)
  T <- nrow(x); d <- ncol(x)
  if (T == 0L || d == 0L) stop("q is empty.", call. = FALSE)
  if (d %% 2L != 0L) stop("RoPE rotates feature PAIRS, so d must be even.",
                          call. = FALSE)
  base <- as.numeric(base)
  if (base <= 1) stop("base must exceed 1.", call. = FALSE)
  m <- if (is.null(positions)) seq_len(T) - 1 else as.numeric(positions)
  if (length(m) != T) stop("positions and rows differ.", call. = FALSE)
  if (any(m < 0)) stop("positions must be non-negative.", call. = FALSE)
  half <- d %/% 2L
  i <- seq_len(half) - 1
  theta <- base^(-2 * i / d)
  angles <- outer(m, theta)
  even <- x[, seq(1L, d, by = 2L), drop = FALSE]
  odd <- x[, seq(2L, d, by = 2L), drop = FALSE]
  y <- matrix(0, T, d)
  y[, seq(1L, d, by = 2L)] <- even * cos(angles) - odd * sin(angles)
  y[, seq(2L, d, by = 2L)] <- even * sin(angles) + odd * cos(angles)
  list(y = y, angles = angles, theta = theta, positions = m,
       estimate = y[1, 1], base = base, d = d, n = T,
       method = "Rotary positional embedding at explicit positions")
}

#' ROUGE-N against a single reference (Kamath Ch 8)
#'
#' Clipped n-gram matches / reference n-grams, with precision and F1.
#'
#' @param hypothesis,reference Token vectors or whitespace strings.
#' @param n Order.
#' @return List with `estimate` (recall), `precision`, `f1`, `n_matched`.
#' @export
morie_kamath_rouge_n <- function(hypothesis, reference, n = 1) {
  n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  tok <- function(x) if (is.character(x) && length(x) == 1L)
    strsplit(trimws(x), "\\s+")[[1]] else as.character(x)
  h <- tok(hypothesis); r <- tok(reference)
  if (length(r) == 0L) stop("the reference is empty.", call. = FALSE)
  if (length(r) < n) stop("the reference is too short for n-grams.",
                          call. = FALSE)
  if (length(h) == 0L) stop("the hypothesis is empty.", call. = FALSE)
  hg <- .morie_km2_counts(.morie_km2_ngrams(h, n))
  rg <- .morie_km2_counts(.morie_km2_ngrams(r, n))
  total_r <- sum(rg); total_h <- sum(hg)
  match <- 0
  for (g in names(rg)) {
    have <- if (g %in% names(hg)) hg[[g]] else 0L
    match <- match + min(rg[[g]], have)
  }
  recall <- match / total_r
  precision <- if (total_h) match / total_h else 0
  f1 <- if (recall + precision == 0) 0 else
    2 * recall * precision / (recall + precision)
  list(estimate = recall, recall = recall, precision = precision, f1 = f1,
       n_matched = as.integer(match), n_reference_ngrams = as.integer(total_r),
       n_hypothesis_ngrams = as.integer(total_h), order = n,
       n = as.integer(total_r),
       method = sprintf("ROUGE-%d recall with clipped n-gram counts", n))
}

#' Reciprocal rank fusion (Kamath Ch 7)
#'
#' sum_r 1/(k + rank_r(d)); documents missing from a list simply score
#' nothing there. Ties break on the document label.
#'
#' @param rankings List of ranked id vectors. @param k Damping constant.
#' @return List with `ranking`, `scores`, `appearances`.
#' @export
morie_kamath_reciprocal_rank_fusion <- function(rankings, k = 60) {
  lists <- lapply(rankings, function(r) as.character(unlist(r)))
  k <- as.numeric(k)
  if (length(lists) == 0L) stop("no rankings supplied.", call. = FALSE)
  if (any(lengths(lists) == 0L)) stop("a ranker returned an empty list.",
                                      call. = FALSE)
  if (k <= 0) stop("k must be positive.", call. = FALSE)
  scores <- list(); seen <- list()
  for (r in lists) {
    if (anyDuplicated(r)) stop("a ranking repeats a document.",
                               call. = FALSE)
    for (pos in seq_along(r)) {
      d <- r[pos]
      scores[[d]] <- (if (is.null(scores[[d]])) 0 else scores[[d]]) +
        1 / (k + pos)
      seen[[d]] <- (if (is.null(seen[[d]])) 0L else seen[[d]]) + 1L
    }
  }
  docs <- names(scores)
  sc <- unlist(scores)[docs]
  ord <- order(-sc, docs)
  list(ranking = docs[ord], scores = sc, appearances = unlist(seen),
       n_rankers = length(lists), n_documents = length(docs),
       estimate = sc[ord[1]], k = k, n = length(docs),
       method = "Reciprocal rank fusion sum 1/(k + rank)")
}

#' Rejection-sampling fine-tuning (Kamath Ch 5)
#'
#' Per-prompt top-k by reward (STABLE ties), retained in original order.
#'
#' @param prompts Prompts. @param samples Per-prompt sample lists.
#' @param rewards Per-prompt reward lists. @param k Kept per prompt.
#' @param sft Optional trainer over the retained pairs.
#' @return List with `retained`, `retained_rewards`, `n_dropped`.
#' @export
morie_kamath_rejection_sampling_finetune <- function(prompts, samples,
                                                     rewards, k, sft = NULL) {
  prompts <- as.list(prompts)
  samples <- lapply(samples, as.list); rewards <- lapply(rewards, as.numeric)
  k <- as.integer(k)
  if (length(prompts) == 0L) stop("no prompts supplied.", call. = FALSE)
  if (length(samples) != length(prompts) ||
      length(rewards) != length(prompts)) {
    stop("one sample list and one reward list per prompt.", call. = FALSE)
  }
  if (k < 1L) stop("k must be at least 1.", call. = FALSE)
  retained <- list(); kept_rewards <- numeric(0); dropped <- 0L
  for (i in seq_along(prompts)) {
    ys <- samples[[i]]; r <- rewards[[i]]
    if (length(ys) != length(r)) stop("samples and rewards differ.",
                                      call. = FALSE)
    if (length(ys) == 0L) stop("a prompt has no samples.", call. = FALSE)
    if (any(!is.finite(r))) stop("a reward is non-finite.", call. = FALSE)
    take <- min(k, length(ys))
    ord <- sort(.morie_km2_stable_desc(r)[seq_len(take)])
    for (j in ord) {
      retained[[length(retained) + 1L]] <- list(prompts[[i]], ys[[j]])
      kept_rewards <- c(kept_rewards, r[j])
    }
    dropped <- dropped + length(ys) - take
  }
  out <- list(retained = retained, retained_rewards = kept_rewards,
              n_retained = length(retained), n_dropped = dropped,
              mean_retained_reward = mean(kept_rewards), k = k,
              estimate = length(retained), n = length(prompts),
              method = "Rejection-sampling fine-tuning (per-prompt top-k)")
  if (!is.null(sft)) {
    if (!is.function(sft)) stop("sft must be a function.", call. = FALSE)
    out$policy <- sft(retained)
  }
  out
}

#' RWKV time-mixing (Kamath Ch 2)
#'
#' Decayed normalised value average, computed in log space so long decays
#' never overflow.
#'
#' @param k,v Key and value sequences. @param w Decay (non-negative).
#' @param u Current-token bonus.
#' @return List with `wkv`, `estimate`.
#' @export
morie_kamath_rwkv_time_mix <- function(k, v, w, u = 0) {
  kv <- as.numeric(k); vv <- as.numeric(v)
  w <- as.numeric(w); u <- as.numeric(u)
  if (length(kv) != length(vv)) stop("keys and values must be paired.",
                                     call. = FALSE)
  if (length(kv) == 0L) stop("the sequence is empty.", call. = FALSE)
  if (any(!is.finite(kv)) || any(!is.finite(vv))) {
    stop("k and v must be finite.", call. = FALSE)
  }
  if (w < 0) stop("the decay w must be non-negative.", call. = FALSE)
  T <- length(kv)
  out <- numeric(T)
  a <- 0; b <- 0; p <- -Inf
  for (t in seq_len(T)) {
    q <- max(p, u + kv[t])
    e1 <- if (is.finite(p)) exp(p - q) else 0
    e2 <- exp(u + kv[t] - q)
    out[t] <- (a * e1 + e2 * vv[t]) / (b * e1 + e2)
    q2 <- max(p, kv[t])
    f1 <- if (is.finite(p)) exp(p - q2) else 0
    f2 <- exp(kv[t] - q2)
    a <- a * f1 + f2 * vv[t]
    b <- b * f1 + f2
    p <- q2 - w
  }
  list(wkv = out, estimate = out[T], w = w, u = u, n = T,
       method = "RWKV time-mixing (log-space stable scan)")
}

#' Self-consistency majority vote (Kamath Ch 4)
#'
#' Mode of the parsed answers; ties are reported and broken by first
#' appearance, exactly as in Python.
#'
#' @param samples Sampled traces. @param parse Optional answer extractor.
#' @return List with `answer`, `votes`, `agreement`, `tie`.
#' @export
morie_kamath_self_consistency <- function(samples, parse = NULL) {
  traces <- as.list(samples)
  if (length(traces) == 0L) stop("no samples.", call. = FALSE)
  if (!is.null(parse) && !is.function(parse)) stop("parse must be a function.",
                                                   call. = FALSE)
  answers <- character(0); unparsed <- 0L
  for (s in traces) {
    a <- if (!is.null(parse)) parse(s) else s
    if (is.null(a)) { unparsed <- unparsed + 1L; next }
    answers <- c(answers, as.character(a))
  }
  if (length(answers) == 0L) stop("all samples failed to parse.",
                                  call. = FALSE)
  counts <- table(answers)
  top <- max(counts)
  tied <- names(counts)[counts == top]
  winner <- answers[answers %in% tied][1]
  list(answer = winner, votes = as.integer(top),
       agreement = top / length(answers),
       counts = stats::setNames(as.integer(counts), names(counts)),
       tie = length(tied) > 1L, tied_answers = sort(tied),
       n_unparsed = unparsed, n_voted = length(answers),
       estimate = top / length(answers), n = length(traces),
       method = "Self-consistency majority vote over parsed answers")
}

#' Power-law scaling (Kamath Ch 1)
#'
#' L(N) = (N_c/N)^alpha + L_inf, with the reducible part reported.
#'
#' @param N Model scale(s). @param N_c Critical scale. @param alpha_N Exponent.
#' @param L_inf Irreducible loss.
#' @return List with `estimate`, `reducible`, `irreducible`.
#' @export
morie_kamath_scaling_laws <- function(N, N_c, alpha_N, L_inf = 0) {
  n <- as.numeric(N); N_c <- as.numeric(N_c)
  alpha <- as.numeric(alpha_N); L_inf <- as.numeric(L_inf)
  if (length(n) == 0L) stop("no scale supplied.", call. = FALSE)
  if (any(n <= 0)) stop("N must be positive.", call. = FALSE)
  if (N_c <= 0) stop("N_c must be positive.", call. = FALSE)
  if (alpha <= 0) stop("alpha_N must be positive.", call. = FALSE)
  if (L_inf < 0) stop("the irreducible loss must be non-negative.",
                      call. = FALSE)
  reducible <- (N_c / n)^alpha
  loss <- reducible + L_inf
  scalar <- length(n) == 1L
  list(estimate = if (scalar) loss[1] else loss,
       loss = if (scalar) loss[1] else loss,
       reducible = if (scalar) reducible[1] else reducible,
       irreducible = L_inf, N_c = N_c, alpha_N = alpha, n = length(n),
       method = "Power-law scaling L(N) = (N_c/N)^alpha + L_inf")
}

# ---- unigram / SentencePiece tokenizers -------------------------------

.morie_km2_pieces_by_end <- function(text, vocab, maxlen) {
  L <- nchar(text)
  ends <- vector("list", L + 1L)
  for (j in seq_len(L)) {
    got <- character(0)
    for (nn in seq_len(min(maxlen, j))) {
      w <- substr(text, j - nn + 1L, j)
      if (!is.null(vocab[[w]])) got <- c(got, w)
    }
    ends[[j + 1L]] <- got
  }
  ends
}

.morie_km2_forward <- function(text, probs, maxlen) {
  L <- nchar(text)
  ends <- .morie_km2_pieces_by_end(text, probs, maxlen)
  alpha <- rep(0, L + 1L)
  alpha[1] <- 1
  for (j in seq_len(L)) {
    tot <- 0
    for (w in ends[[j + 1L]]) tot <- tot + alpha[j - nchar(w) + 1L] * probs[[w]]
    alpha[j + 1L] <- tot
  }
  list(alpha = alpha, ends = ends)
}

.morie_km2_backward <- function(text, probs, maxlen) {
  L <- nchar(text)
  beta <- rep(0, L + 1L)
  beta[L + 1L] <- 1
  for (j in seq(L - 1L, 0L)) {
    tot <- 0
    for (nn in seq_len(min(maxlen, L - j))) {
      w <- substr(text, j + 1L, j + nn)
      if (!is.null(probs[[w]])) tot <- tot + probs[[w]] * beta[j + nn + 1L]
    }
    beta[j + 1L] <- tot
  }
  beta
}

#' Unigram LM tokenizer by EM (Kamath Ch 2)
#'
#' Forward-backward expected counts over every segmentation, then Viterbi
#' segmentations at the fitted piece probabilities.
#'
#' @param corpus Character vector. @param vocab Candidate pieces.
#' @param max_iter,tol EM controls.
#' @return List with `probs`, `log_likelihood`, `segmentations`.
#' @export
morie_kamath_unigram_lm_tokenizer <- function(corpus, vocab, max_iter = 100,
                                              tol = 1e-12) {
  corpus <- corpus[nzchar(corpus)]
  if (length(corpus) == 0L) stop("the corpus is empty.", call. = FALSE)
  vocab <- unique(as.character(vocab))
  if (length(vocab) == 0L) stop("the vocabulary is empty.", call. = FALSE)
  if (any(nchar(vocab) == 0L)) stop("the empty string is not a piece.",
                                    call. = FALSE)
  maxlen <- max(nchar(vocab))
  probs <- stats::setNames(as.list(rep(1 / length(vocab), length(vocab))),
                           vocab)
  morie_kamath_unigram_loglik(corpus, probs)
  history <- numeric(0)
  for (it in seq_len(as.integer(max_iter))) {
    counts <- stats::setNames(as.list(rep(0, length(vocab))), vocab)
    ll <- 0
    for (s in corpus) {
      fw <- .morie_km2_forward(s, probs, maxlen)
      bw <- .morie_km2_backward(s, probs, maxlen)
      Z <- fw$alpha[nchar(s) + 1L]
      ll <- ll + log(Z)
      for (j in seq_len(nchar(s))) {
        for (w in fw$ends[[j + 1L]]) {
          counts[[w]] <- counts[[w]] +
            fw$alpha[j - nchar(w) + 1L] * probs[[w]] * bw[j + 1L] / Z
        }
      }
    }
    history <- c(history, ll)
    total <- sum(unlist(counts))
    if (total <= 0) stop("the E-step produced no expected counts.",
                         call. = FALSE)
    new <- lapply(counts, function(cc) cc / total)
    delta <- max(abs(unlist(new)[vocab] - unlist(probs)[vocab]))
    probs <- new
    if (delta < tol) break
  }
  final_ll <- morie_kamath_unigram_loglik(corpus, probs)
  history <- c(history, final_ll)
  segs <- lapply(corpus, function(s) morie_kamath_viterbi_segment(s, probs)[[1]])
  list(probs = probs, log_likelihood = final_ll,
       log_likelihood_history = history, n_iterations = length(history) - 1L,
       segmentations = segs, vocab_size = length(vocab), estimate = final_ll,
       n = length(corpus),
       method = "Unigram LM EM (forward-backward expected counts)")
}

#' @rdname morie_kamath_unigram_lm_tokenizer
#' @param probs Named list of piece probabilities.
#' @export
morie_kamath_unigram_loglik <- function(corpus, probs) {
  if (length(probs) == 0L) stop("the piece table is empty.", call. = FALSE)
  maxlen <- max(nchar(names(probs)))
  total <- 0
  for (s in corpus) {
    if (!nzchar(s)) next
    a <- .morie_km2_forward(s, probs, maxlen)$alpha
    if (a[length(a)] <= 0) stop("a string has no segmentation.",
                                call. = FALSE)
    total <- total + log(a[length(a)])
  }
  total
}

#' @rdname morie_kamath_unigram_lm_tokenizer
#' @param text One string to segment.
#' @export
morie_kamath_viterbi_segment <- function(text, probs) {
  maxlen <- max(nchar(names(probs)))
  L <- nchar(text)
  best <- rep(-Inf, L + 1L); back <- vector("list", L + 1L)
  best[1] <- 0
  for (j in seq_len(L)) {
    for (nn in seq_len(min(maxlen, j))) {
      w <- substr(text, j - nn + 1L, j)
      p <- probs[[w]]
      if (is.null(p) || p <= 0 || best[j - nn + 1L] == -Inf) next
      cand <- best[j - nn + 1L] + log(p)
      if (cand > best[j + 1L]) { best[j + 1L] <- cand; back[[j + 1L]] <- w }
    }
  }
  if (best[L + 1L] == -Inf) stop("the text cannot be segmented.",
                                 call. = FALSE)
  out <- character(0); j <- L
  while (j > 0L) {
    w <- back[[j + 1L]]
    out <- c(w, out)
    j <- j - nchar(w)
  }
  list(out, best[L + 1L])
}

#' SentencePiece unigram trainer (Kamath Ch 2)
#'
#' Seed vocabulary, EM through `morie_kamath_unigram_lm_tokenizer`, then
#' prune by the EXACT likelihood loss of dropping each multi-character piece.
#'
#' @param corpus Character vector. @param vocab_size Target size.
#' @param max_piece_len Longest seed piece. @param seed_multiplier Seed factor.
#' @param shrink Fraction retained per prune round. @param max_iter EM cap.
#' @return List with `vocab`, `probs`, `log_likelihood`, `n_prune_rounds`.
#' @export
morie_kamath_sentencepiece_tokenizer <- function(corpus, vocab_size,
                                                 max_piece_len = 8,
                                                 seed_multiplier = 4,
                                                 shrink = 0.75,
                                                 max_iter = 50) {
  corpus <- corpus[nzchar(corpus)]
  if (length(corpus) == 0L) stop("the corpus is empty.", call. = FALSE)
  vocab_size <- as.integer(vocab_size)
  chars <- sort(unique(unlist(strsplit(corpus, ""))))
  if (vocab_size < length(chars)) {
    stop("vocab_size is below the distinct character count.", call. = FALSE)
  }
  if (max_piece_len < 2) stop("max_piece_len must be at least 2.",
                              call. = FALSE)
  if (shrink <= 0 || shrink >= 1) stop("shrink must lie in (0, 1).",
                                       call. = FALSE)
  seed_size <- max(vocab_size, as.integer(seed_multiplier * vocab_size))
  freq <- list()
  for (s in corpus) {
    L <- nchar(s)
    for (i in seq_len(L)) {
      up <- min(max_piece_len, L - i + 1L)
      if (up < 2L) next
      for (nn in 2:up) {
        w <- substr(s, i, i + nn - 1L)
        freq[[w]] <- (if (is.null(freq[[w]])) 0L else freq[[w]]) + 1L
      }
    }
  }
  ws <- names(freq)
  ranked <- if (length(ws)) {
    key <- -vapply(ws, function(w) freq[[w]] * nchar(w), numeric(1))
    ws[order(key, ws)]
  } else character(0)
  room <- max(0L, seed_size - length(chars))
  vocab <- c(chars, utils::head(ranked, room))
  fit <- morie_kamath_unigram_lm_tokenizer(corpus, vocab, max_iter = max_iter)
  probs <- fit$probs
  rounds <- 0L
  while (length(probs) > vocab_size) {
    rounds <- rounds + 1L
    base <- morie_kamath_unigram_loglik(corpus, probs)
    losses <- list()
    for (w in names(probs)) {
      if (nchar(w) == 1L) next
      trimmed <- probs[setdiff(names(probs), w)]
      s <- sum(unlist(trimmed))
      trimmed <- lapply(trimmed, function(v) v / s)
      losses[[w]] <- tryCatch(base - morie_kamath_unigram_loglik(corpus, trimmed),
                              error = function(e) Inf)
    }
    if (length(losses) == 0L) stop("only single-character pieces remain.",
                                   call. = FALSE)
    keep_n <- max(vocab_size, as.integer(length(probs) * shrink))
    n_drop <- min(length(losses), length(probs) - keep_n)
    if (n_drop <= 0L) n_drop <- 1L
    lw <- names(losses); lv <- unlist(losses)[lw]
    drop <- lw[order(lv, lw)][seq_len(n_drop)]
    vocab <- setdiff(names(probs), drop)
    fit <- morie_kamath_unigram_lm_tokenizer(corpus, vocab, max_iter = max_iter)
    probs <- fit$probs
  }
  pv <- unlist(probs)
  segs <- lapply(corpus, function(s) morie_kamath_viterbi_segment(s, probs)[[1]])
  list(vocab = names(pv)[order(-pv, names(pv))], probs = probs,
       vocab_size = length(probs),
       log_likelihood = morie_kamath_unigram_loglik(corpus, probs),
       segmentations = segs, n_prune_rounds = rounds,
       estimate = length(probs), n = length(corpus),
       method = "SentencePiece unigram: seed, EM, exact-loss prune")
}

#' Speculative decoding accept/reject (Kamath Ch 8)
#'
#' min(1, p_t/p_d) acceptance with residual resampling; `proposed` is a
#' 0-BASED token id.
#'
#' @param draft_probs,target_probs Distributions over one vocabulary.
#' @param proposed Optional 0-based proposal. @param u Optional uniform draw.
#' @return List with `estimate`, `accept_prob`, `residual`, `rejection_rate`.
#' @export
morie_kamath_speculative_decoding <- function(draft_probs, target_probs,
                                              proposed = NULL, u = NULL) {
  chk <- function(p, nm) {
    v <- as.numeric(p)
    if (length(v) == 0L) stop(sprintf("%s is empty.", nm), call. = FALSE)
    if (any(v < 0)) stop(sprintf("%s has a negative probability.", nm),
                         call. = FALSE)
    if (!is.finite(sum(v)) || abs(sum(v) - 1) > 1e-6) {
      stop(sprintf("%s is not a distribution.", nm), call. = FALSE)
    }
    v
  }
  pd <- chk(draft_probs, "draft_probs"); pt <- chk(target_probs, "target_probs")
  if (length(pd) != length(pt)) stop("the two models must share a vocabulary.",
                                     call. = FALSE)
  t <- if (is.null(proposed)) which.max(pd) - 1L else as.integer(proposed)
  if (t < 0L || t >= length(pd)) stop("the proposed token is out of range.",
                                      call. = FALSE)
  if (pd[t + 1L] == 0) stop("the draft cannot have proposed that token.",
                            call. = FALSE)
  ratio <- pt[t + 1L] / pd[t + 1L]
  accept_p <- min(1, ratio)
  resid <- pmax(0, pt - pd)
  mass <- sum(resid)
  resid_norm <- if (mass > 0) resid / mass else rep(0, length(resid))
  out <- list(estimate = accept_p, accept_prob = accept_p, ratio = ratio,
              proposed = t, residual = resid_norm, residual_mass = mass,
              rejection_rate = 1 - sum(pmin(pd, pt)), n = length(pd),
              method = "Speculative decoding accept/reject with residual resampling")
  if (!is.null(u)) {
    u <- as.numeric(u)
    if (u < 0 || u > 1) stop("u must lie in [0, 1].", call. = FALSE)
    acc <- u < accept_p
    out$accepted <- acc
    out$resample_from_residual <- !acc
    if (!acc && mass == 0) stop("rejected with an empty residual.",
                                call. = FALSE)
  }
  out
}

#' T5 span corruption (Kamath Ch 2)
#'
#' Spans replaced by one sentinel each. The span layout is drawn from the
#' SAME linear congruential generator Python uses (`.morie_al_lcg`), so a
#' given seed reproduces the Python output draw for draw.
#'
#' @param tokens Token vector. @param mean_span_len Mean span length.
#' @param corruption_rate Fraction masked. @param seed LCG seed.
#' @param sentinel Sentinel template containing `%d`-style `{}`.
#' @return List with `input`, `target`, `spans`, `n_masked`, `n_spans`.
#' @export
morie_kamath_t5_span_corruption <- function(tokens, mean_span_len = 3,
                                            corruption_rate = 0.15, seed = 0,
                                            sentinel = "<extra_id_{}>") {
  toks <- as.character(tokens)
  L <- length(toks)
  if (L < 2L) stop("need at least 2 tokens to corrupt a span.", call. = FALSE)
  if (corruption_rate <= 0 || corruption_rate >= 1) {
    stop("corruption_rate must lie in (0, 1).", call. = FALSE)
  }
  if (mean_span_len < 1) stop("mean_span_len must be at least 1.",
                              call. = FALSE)
  n_mask <- as.integer(round(L * as.numeric(corruption_rate)))
  n_mask <- max(1L, min(n_mask, L - 1L))
  n_spans <- as.integer(round(n_mask / as.numeric(mean_span_len)))
  n_spans <- max(1L, min(n_spans, n_mask))
  n_keep <- L - n_mask
  if (n_keep < n_spans) stop("too few unmasked tokens to separate the spans.",
                             call. = FALSE)
  state <- as.numeric(seed) %% 2^32
  rnd <- function() {
    state <<- .morie_al_lcg(state)
    (state + 0.5) / 2^32
  }
  segment <- function(total, parts) {
    if (parts > total) stop("cannot split into non-empty spans.",
                            call. = FALSE)
    cuts <- numeric(0)
    while (length(cuts) < parts - 1L) {
      cc <- 1 + floor(rnd() * (total - 1))
      cc <- min(cc, total - 1)
      if (!(cc %in% cuts)) cuts <- c(cuts, cc)
    }
    bounds <- c(0, sort(cuts), total)
    diff(bounds)
  }
  noise <- segment(n_mask, n_spans)
  keep <- segment(n_keep, n_spans)
  sent <- function(i) sub("{}", i, sentinel, fixed = TRUE)
  inp <- character(0); tgt <- character(0); spans <- list()
  pos <- 0L
  for (i in seq_len(n_spans)) {
    if (keep[i] > 0) inp <- c(inp, toks[(pos + 1L):(pos + keep[i])])
    pos <- pos + keep[i]
    span <- if (noise[i] > 0) toks[(pos + 1L):(pos + noise[i])] else character(0)
    pos <- pos + noise[i]
    s <- sent(i - 1L)
    inp <- c(inp, s)
    tgt <- c(tgt, s, span)
    spans[[i]] <- span
  }
  if (pos < L) inp <- c(inp, toks[(pos + 1L):L])
  tgt <- c(tgt, sent(n_spans))
  list(input = inp, target = tgt, spans = spans, n_masked = n_mask,
       n_spans = n_spans, span_lengths = noise,
       compression = length(inp) / L, estimate = n_mask, n = L,
       method = "T5 span corruption with sentinel tokens")
}

#' Self-RAG reflection-token decisions (Kamath Ch 7)
#'
#' A CLOSED reflection vocabulary; an unknown or contradictory token is
#' refused rather than ignored.
#'
#' @param context Retrieved context. @param reflection_model Token emitter.
#' @param question Optional question.
#' @return List with `tokens`, `by_group`, `retrieve`, `utility`.
#' @export
morie_kamath_self_rag <- function(context, reflection_model, question = NULL) {
  if (!is.function(reflection_model)) stop("reflection_model must be a function.",
                                           call. = FALSE)
  REF <- list(retrieve = c("[Retrieve]", "[No Retrieve]"),
              relevance = c("[Relevant]", "[Irrelevant]"),
              support = c("[Supported]", "[Partially Supported]",
                          "[No Support]"),
              utility = c("[Utility:1]", "[Utility:2]", "[Utility:3]",
                          "[Utility:4]", "[Utility:5]"))
  lookup <- unlist(lapply(names(REF), function(g)
    stats::setNames(rep(g, length(REF[[g]])), REF[[g]])))
  toks <- reflection_model(context, question)
  if (is.character(toks) && length(toks) == 1L) {
    m <- gregexpr("\\[[^]]*\\]", toks)[[1]]
    toks <- if (m[1] == -1L) character(0) else regmatches(toks,
                                                          gregexpr("\\[[^]]*\\]", toks))[[1]]
  }
  toks <- as.character(toks)
  if (length(toks) == 0L) stop("the reflection model emitted no tokens.",
                               call. = FALSE)
  groups <- list()
  for (t in toks) {
    g <- lookup[[t]]
    if (is.null(g) || is.na(g)) stop("not a Self-RAG reflection token.",
                                     call. = FALSE)
    if (!is.null(groups[[g]]) && groups[[g]] != t) {
      stop("contradictory reflection tokens.", call. = FALSE)
    }
    groups[[g]] <- t
  }
  flag <- function(g, positive) if (is.null(groups[[g]])) NULL else
    groups[[g]] == positive
  utility <- if (!is.null(groups$utility))
    as.integer(sub("\\]", "", strsplit(groups$utility, ":")[[1]][2])) else NULL
  list(tokens = toks, by_group = groups,
       retrieve = flag("retrieve", "[Retrieve]"),
       relevant = flag("relevance", "[Relevant]"),
       supported = if (is.null(groups$support)) NULL else
         groups$support == "[Supported]",
       support_level = groups$support, utility = utility,
       estimate = length(groups), n = length(toks),
       method = "Self-RAG reflection-token decisions")
}

#' Step-back prompting (Kamath Ch 4)
#'
#' The LLM abstracts the query; contexts from both queries are unioned with
#' the abstract query's documents FIRST and duplicates dropped.
#'
#' @param query Question. @param model Abstraction function.
#' @param retrieve Optional retriever. @param answer Optional answerer.
#' @return List with `step_back_query`, `context`, `stepped_back`.
#' @export
morie_kamath_step_back_prompting <- function(query, model, retrieve = NULL,
                                             answer = NULL) {
  if (!is.function(model)) stop("model must be a function.", call. = FALSE)
  q_high <- model(query)
  if (is.null(q_high)) stop("the model returned no step-back query.",
                            call. = FALSE)
  stepped <- !identical(q_high, query)
  ctx <- list(); seen <- character(0); per_query <- list()
  if (!is.null(retrieve)) {
    if (!is.function(retrieve)) stop("retrieve must be a function.",
                                     call. = FALSE)
    for (q in list(q_high, query)) {
      docs <- as.list(retrieve(q))
      per_query[[as.character(q)]] <- docs
      for (d in docs) {
        key <- paste(deparse(d), collapse = "")
        if (!(key %in% seen)) {
          seen <- c(seen, key)
          ctx[[length(ctx) + 1L]] <- d
        }
      }
    }
  }
  out <- list(step_back_query = q_high, query = query,
              stepped_back = stepped, context = ctx,
              retrieved_by_query = per_query, n_context = length(ctx),
              estimate = length(ctx), n = length(ctx),
              method = "Step-back prompting (abstract query, then specific)")
  if (!stepped) out$warning <- "the model returned the original question"
  if (!is.null(answer)) {
    if (!is.function(answer)) stop("answer must be a function.",
                                   call. = FALSE)
    out$answer <- answer(query, ctx)
  }
  out
}

#' Summarisation from human feedback (Kamath Ch 5)
#'
#' Combines `morie_kamath_reward_model_training_loss` on the summary pairs
#' with `morie_kamath_ppo_rlhf_objective` on the policy batch.
#'
#' @param preferences List of (score_chosen, score_rejected).
#' @param rewards,pi_logprobs,ref_logprobs PPO batch. @param beta KL weight.
#' @return List with `loss_rm`, `rm_accuracy`, `objective`, `kl_estimate`.
#' @export
morie_kamath_summarize_from_feedback <- function(preferences, rewards,
                                                 pi_logprobs, ref_logprobs,
                                                 beta) {
  pairs <- lapply(preferences, function(p) as.numeric(unlist(p)))
  if (length(pairs) == 0L) stop("no summary preference pairs.",
                                call. = FALSE)
  if (any(lengths(pairs) != 2L)) {
    stop("each preference must be (score_chosen, score_rejected).",
         call. = FALSE)
  }
  w <- vapply(pairs, function(p) p[1], numeric(1))
  l <- vapply(pairs, function(p) p[2], numeric(1))
  rm <- morie_kamath_reward_model_training_loss(w, l)
  rl <- morie_kamath_ppo_rlhf_objective(rewards, pi_logprobs, ref_logprobs,
                                        beta)
  list(loss_rm = rm$estimate, rm_accuracy = rm$accuracy,
       rm_mean_margin = rm$mean_margin, objective = rl$estimate,
       mean_reward = rl$mean_reward, kl_estimate = rl$kl_estimate,
       beta = as.numeric(beta), n_preferences = length(pairs),
       estimate = rl$estimate, n = rl$n,
       method = "Summarisation from human feedback (kmrmloss + kmppok)")
}

#' StereoSet stereotype-preference fraction (Kamath Ch 6)
#'
#' Fraction of pairs preferring the stereotype; 0.5 is unbiased.
#'
#' @param stereo_probs,anti_probs Paired probabilities.
#' @return List with `estimate`, `ss_score`, `bias_magnitude`, `n_ties`.
#' @export
morie_kamath_stereoset_bias <- function(stereo_probs, anti_probs) {
  s <- as.numeric(stereo_probs); a <- as.numeric(anti_probs)
  if (length(s) != length(a)) stop("StereoSet compares PAIRS.", call. = FALSE)
  if (length(s) == 0L) stop("no pairs supplied.", call. = FALSE)
  if (any(s < 0) || any(a < 0)) stop("probabilities must be non-negative.",
                                     call. = FALSE)
  if (any(!is.finite(s)) || any(!is.finite(a))) {
    stop("probabilities must be finite.", call. = FALSE)
  }
  wins <- sum(s > a)
  score <- wins / length(s)
  list(estimate = score, ss_score = score,
       n_stereotype_preferred = as.integer(wins),
       n_anti_preferred = as.integer(sum(s < a)),
       n_ties = as.integer(sum(s == a)),
       bias_magnitude = abs(score - 0.5), unbiased_point = 0.5,
       n = length(s),
       method = "StereoSet stereotype-preference fraction")
}

#' SwiGLU activation (Kamath Ch 2)
#'
#' Swish(xW + b) gates (xV + c) elementwise.
#'
#' @param x Input. @param W,V Equal-shaped projections. @param b,c Biases.
#' @return List with `output`, `gate`, `linear`.
#' @export
morie_kamath_swiglu_activation <- function(x, W, V, b = NULL, c = NULL) {
  x <- as.numeric(x); W <- as.matrix(W); V <- as.matrix(V)
  if (!all(dim(W) == dim(V))) stop("W and V must match.", call. = FALSE)
  if (nrow(W) != length(x)) stop("x and W disagree.", call. = FALSE)
  gate_pre <- as.numeric(x %*% W)
  up_pre <- as.numeric(x %*% V)
  if (!is.null(b)) {
    bb <- as.numeric(b)
    if (length(bb) != length(gate_pre)) stop("b has the wrong length.",
                                             call. = FALSE)
    gate_pre <- gate_pre + bb
  }
  if (!is.null(c)) {
    cc <- as.numeric(c)
    if (length(cc) != length(up_pre)) stop("c has the wrong length.",
                                           call. = FALSE)
    up_pre <- up_pre + cc
  }
  g <- morie_kamath_swish(gate_pre)
  out <- g * up_pre
  list(output = out, gate = g, linear = up_pre, estimate = out[1],
       hidden_dim = length(out), n = length(out),
       method = "SwiGLU: Swish(xW + b) * (xV + c)")
}

#' @rdname morie_kamath_swiglu_activation
#' @param z Pre-activation. @param beta Swish sharpness.
#' @export
morie_kamath_swish <- function(z, beta = 1) {
  z <- as.numeric(z)
  ifelse(beta * z >= 0, z / (1 + exp(-beta * z)),
         z * exp(beta * z) / (1 + exp(beta * z)))
}

#' Temperature-scaled softmax (Kamath Ch 8)
#'
#' softmax(z / T), max-shifted; T <= 0 is greedy decoding and is refused.
#'
#' @param logits Logits. @param T Temperature.
#' @return List with `probabilities`, `entropy`, `max_entropy`, `argmax`.
#' @export
morie_kamath_temperature_sampling <- function(logits, T) {
  z <- as.numeric(logits); T <- as.numeric(T)
  if (length(z) == 0L) stop("no logits supplied.", call. = FALSE)
  if (any(!is.finite(z))) stop("logits must be finite.", call. = FALSE)
  if (T <= 0) stop("the temperature must be positive.", call. = FALSE)
  p <- .morie_km2_soft(z / T)
  nz <- p[p > 0]
  list(probabilities = p, estimate = max(p), entropy = -sum(nz * log(nz)),
       max_entropy = log(length(z)), argmax = which.max(p) - 1L,
       temperature = T, n = length(z),
       method = "Temperature-scaled softmax")
}

#' Top-k gate renormalisation (Kamath Ch 2)
#'
#' Keeps the top-k gates and renormalises them -- NOT a masked softmax.
#' `selected_experts` is 0-BASED and sorted.
#'
#' @param gates Non-negative gate scores. @param k Experts kept.
#' @return List with `weights`, `selected_experts`, `kept_mass`.
#' @export
morie_kamath_moe_top_k_gating <- function(gates, k) {
  g <- as.numeric(gates); k <- as.integer(k); n <- length(g)
  if (n == 0L) stop("no gate scores supplied.", call. = FALSE)
  if (k < 1L || k > n) stop("k is out of range.", call. = FALSE)
  if (any(!is.finite(g))) stop("gate scores must be finite.", call. = FALSE)
  if (any(g < 0)) stop("gate scores must be non-negative.", call. = FALSE)
  ord <- .morie_km2_stable_desc(g)[seq_len(k)]
  kept <- rep(0, n)
  kept[ord] <- g[ord]
  total <- sum(kept)
  if (total == 0) stop("every selected gate is 0.", call. = FALSE)
  w <- kept / total
  list(weights = w, selected_experts = sort(ord) - 1L,
       kept_mass = if (sum(g) > 0) total / sum(g) else 0,
       n_active = as.integer(sum(w > 0)), estimate = max(w), k = k, n = n,
       method = "Top-k gate renormalisation (not a masked softmax)")
}

#' Tree-of-thoughts beam search (Kamath Ch 4)
#'
#' Beam search over model-scored thoughts; path scores accumulate.
#'
#' @param problem Root state. @param branch_factor Children per node.
#' @param max_depth Depth. @param model Function (state, b) -> list of
#'   list(thought, score). @param beam Beam width.
#' @return List with `best_state`, `best_path`, `best_score`, `frontier`.
#' @export
morie_kamath_tree_of_thoughts <- function(problem, branch_factor, max_depth,
                                          model, beam = 1) {
  b <- as.integer(branch_factor); depth <- as.integer(max_depth)
  beam <- as.integer(beam)
  if (b < 1L) stop("branch_factor must be at least 1.", call. = FALSE)
  if (depth < 1L) stop("max_depth must be at least 1.", call. = FALSE)
  if (beam < 1L) stop("beam must be at least 1.", call. = FALSE)
  if (!is.function(model)) stop("model must be a function.", call. = FALSE)
  frontier <- list(list(state = problem, score = 0, path = list()))
  expanded <- 0L; dead_ends <- 0L
  for (d in seq_len(depth)) {
    children <- list()
    for (node in frontier) {
      kids <- model(node$state, b)
      expanded <- expanded + 1L
      kids <- as.list(kids)
      if (length(kids) > b) stop("the model returned too many children.",
                                 call. = FALSE)
      if (length(kids) == 0L) { dead_ends <- dead_ends + 1L; next }
      for (cc in kids) {
        if (length(cc) != 2L) stop("each child must be a (thought, score) pair.",
                                   call. = FALSE)
        children[[length(children) + 1L]] <- list(
          state = cc[[1]], score = node$score + as.numeric(cc[[2]]),
          path = c(node$path, list(cc[[1]])))
      }
    }
    if (length(children) == 0L) break
    ord <- order(-vapply(children, function(x) x$score, numeric(1)))
    frontier <- children[ord[seq_len(min(beam, length(children)))]]
  }
  if (length(frontier) == 0L || length(frontier[[1]]$path) == 0L) {
    stop("the search produced no complete thought path.", call. = FALSE)
  }
  best <- frontier[[1]]
  list(best_state = best$state, best_path = best$path,
       best_score = best$score,
       frontier = lapply(frontier, function(x) list(x$state, x$score)),
       n_expanded = expanded, n_dead_ends = dead_ends,
       depth = length(best$path), beam = beam, branch_factor = b,
       estimate = best$score, n = length(frontier),
       method = "Tree-of-thoughts beam search over scored thoughts")
}

#' ToxiGen classifier toxicity probability (Kamath Ch 6)
#'
#' Accepts a bare probability, a named list carrying `toxic`, or a
#' (p_benign, p_toxic) pair validated to sum to 1.
#'
#' @param text Text. @param classifier Classifier. @param threshold Cut-off.
#' @return List with `estimate`, `toxic`, `threshold`.
#' @export
morie_kamath_toxigen_score <- function(text, classifier, threshold = 0.5) {
  if (!is.function(classifier)) stop("classifier must be a function.",
                                     call. = FALSE)
  if (as.numeric(threshold) < 0 || as.numeric(threshold) > 1) {
    stop("threshold must lie in [0, 1].", call. = FALSE)
  }
  raw <- classifier(text)
  p <- if (!is.null(names(raw)) && "toxic" %in% names(raw)) raw[["toxic"]]
  else if (length(raw) == 2L) {
    v <- as.numeric(unlist(raw))
    if (abs(sum(v) - 1) > 1e-6) stop("the class probabilities do not sum to 1.",
                                     call. = FALSE)
    v[2]
  } else if (length(raw) == 1L) raw else
    stop("a sequence result must be (p_benign, p_toxic).", call. = FALSE)
  p <- as.numeric(p)
  if (p < 0 || p > 1) stop("the classifier did not return a probability.",
                           call. = FALSE)
  list(estimate = p, probability = p, toxic = p >= as.numeric(threshold),
       threshold = as.numeric(threshold), text = text, n = 1L,
       method = "ToxiGen classifier toxicity probability")
}

#' VeRA adapter (Kamath Ch 4)
#'
#' h = W0 x + Lambda_b B Lambda_d A x; only the two diagonals train, A and B
#' stay frozen and shared.
#'
#' @param W0 Frozen weight. @param A_frozen,B_frozen Shared factors.
#' @param lam_b,lam_d Trainable diagonals. @param x Input.
#' @return List with `h`, `delta`, `rank`, `n_trainable`.
#' @export
morie_kamath_vera_adapter <- function(W0, A_frozen, B_frozen, lam_b, lam_d,
                                      x) {
  W0 <- as.matrix(W0); A <- as.matrix(A_frozen); B <- as.matrix(B_frozen)
  lb <- as.numeric(lam_b); ld <- as.numeric(lam_d); x <- as.numeric(x)
  d <- nrow(W0); k <- ncol(W0); r <- nrow(A)
  if (ncol(A) != k) stop("A must be (r, k).", call. = FALSE)
  if (!all(dim(B) == c(d, r))) stop("B must be (d, r).", call. = FALSE)
  if (length(ld) != r) stop("Lambda_d must be r-dimensional.", call. = FALSE)
  if (length(lb) != d) stop("Lambda_b must be d-dimensional.", call. = FALSE)
  if (length(x) != k) stop("x has the wrong length.", call. = FALSE)
  base <- as.numeric(W0 %*% x)
  delta <- lb * as.numeric(B %*% (ld * as.numeric(A %*% x)))
  list(h = base + delta, base = base, delta = delta,
       estimate = (base + delta)[1], rank = r,
       n_trainable = length(lb) + length(ld),
       n_trainable_lora_equivalent = length(A) + length(B),
       n_frozen = length(W0) + length(A) + length(B), n = length(base),
       method = "VeRA h = W0 x + Lambda_b B Lambda_d A x")
}

#' Verbalizer class probabilities (Kamath Ch 3)
#'
#' Sums the softmax mass of each class's answer tokens; the leftover mass is
#' reported rather than hidden, and a token may verbalise only one class.
#'
#' @param logits Vocabulary logits. @param vocab Token vector.
#' @param verbalizer_map Named list class -> answer tokens.
#' @return List with `probabilities`, `normalized`, `prediction`.
#' @export
morie_kamath_verbalizer_mapping <- function(logits, vocab, verbalizer_map) {
  z <- as.numeric(logits); vocab <- as.character(vocab)
  if (length(z) != length(vocab)) stop("logits and vocabulary differ.",
                                       call. = FALSE)
  if (any(!is.finite(z))) stop("logits must be finite.", call. = FALSE)
  if (!is.list(verbalizer_map) || length(verbalizer_map) == 0L) {
    stop("verbalizer_map must be a non-empty named list.", call. = FALSE)
  }
  if (anyDuplicated(vocab)) stop("the vocabulary repeats a token.",
                                 call. = FALSE)
  p <- .morie_km2_soft(z)
  seen <- list(); probs <- list(); used <- list()
  for (cls in names(verbalizer_map)) {
    toks <- as.character(verbalizer_map[[cls]])
    if (length(toks) == 0L) stop("a class has no answer tokens.",
                                 call. = FALSE)
    tot <- 0
    for (t in toks) {
      idx <- match(t, vocab)
      if (is.na(idx)) stop("an answer token is not in the vocabulary.",
                           call. = FALSE)
      if (!is.null(seen[[t]])) stop("a token verbalises two classes.",
                                    call. = FALSE)
      seen[[t]] <- cls
      tot <- tot + p[idx]
    }
    probs[[cls]] <- tot; used[[cls]] <- toks
  }
  pv <- unlist(probs)
  total <- sum(pv)
  if (total <= 0) stop("every answer token has probability 0.",
                       call. = FALSE)
  pred <- max(names(pv)[pv == max(pv)])
  list(probabilities = pv, normalized = pv / total, prediction = pred,
       mass_on_labels = total, mass_outside = 1 - total,
       answer_tokens = used, estimate = pv[[pred]], n = length(pv),
       method = "Verbalizer class probability = summed answer-token mass")
}

#' Skip-gram log-likelihood with the full softmax (Kamath Ch 2)
#'
#' sum log softmax(u_o . v_c) over the (center, context) pairs. Indices are
#' 0-BASED.
#'
#' @param center_indices,context_indices 0-based vocabulary indices.
#' @param V,U Input and output embedding matrices.
#' @return List with `log_likelihood`, `per_pair`, `probabilities`.
#' @export
morie_kamath_word2vec_skipgram <- function(center_indices, context_indices,
                                           V, U) {
  cc <- as.integer(center_indices); oo <- as.integer(context_indices)
  V <- as.matrix(V); U <- as.matrix(U)
  if (length(cc) != length(oo)) stop("the objective sums over PAIRS.",
                                     call. = FALSE)
  if (length(cc) == 0L) stop("no (center, context) pairs.", call. = FALSE)
  if (!all(dim(V) == dim(U))) stop("V and U must match.", call. = FALSE)
  n_vocab <- nrow(V)
  if (any(cc < 0L | cc >= n_vocab) || any(oo < 0L | oo >= n_vocab)) {
    stop("an index lies outside the vocabulary.", call. = FALSE)
  }
  if (any(cc == oo)) stop("a word is listed as its own context.",
                          call. = FALSE)
  scores <- V[cc + 1L, , drop = FALSE] %*% t(U)
  logZ <- .morie_km2_lse_rows(scores)
  per <- scores[cbind(seq_along(cc), oo + 1L)] - logZ
  list(log_likelihood = sum(per), mean_log_likelihood = mean(per),
       per_pair = per, probabilities = exp(per), estimate = mean(per),
       vocab_size = n_vocab, n = length(cc),
       method = "Skip-gram log-likelihood with the full softmax")
}

#' YaRN NTK-aware RoPE frequency rescaling (Kamath Ch 2)
#'
#' theta_i / s^(2i/d); the top frequencies move least. With `ramp` the old
#' and new frequencies are blended linearly across the given band.
#'
#' @param theta RoPE base (scalar) or the d/2 frequencies.
#' @param scale Context multiplier. @param d Embedding width (even).
#' @param ramp Optional (lo, hi) blend band.
#' @return List with `theta`, `theta_new`, `scale_factors`.
#' @export
morie_kamath_yarn_context_extrapolation <- function(theta, scale, d,
                                                    ramp = NULL) {
  d <- as.integer(d); s <- as.numeric(scale)
  if (d < 2L || d %% 2L != 0L) stop("d must be a positive even width.",
                                    call. = FALSE)
  if (s <= 0) stop("the scale factor must be positive.", call. = FALSE)
  half <- d %/% 2L
  i <- seq_len(half) - 1
  t <- as.numeric(theta)
  freqs <- if (length(t) == 1L) {
    if (t <= 1) stop("a scalar theta must exceed 1.", call. = FALSE)
    t^(-2 * i / d)
  } else {
    if (length(t) != half) stop("an array theta must hold d/2 frequencies.",
                                call. = FALSE)
    if (any(t <= 0)) stop("RoPE frequencies must be positive.", call. = FALSE)
    t
  }
  factor <- s^(-2 * i / d)
  new <- freqs * factor
  if (!is.null(ramp)) {
    lo <- as.numeric(ramp[1]); hi <- as.numeric(ramp[2])
    if (!(lo >= 0 && lo < hi && hi <= half)) {
      stop("ramp must be (lo, hi) with 0 <= lo < hi <= d/2.", call. = FALSE)
    }
    w <- pmin(pmax((i - lo) / (hi - lo), 0), 1)
    new <- (1 - w) * freqs + w * new
  }
  list(theta = freqs, theta_new = new, scale_factors = factor,
       effective_context_multiplier = s,
       ramp = if (is.null(ramp)) NULL else c(as.numeric(ramp[1]),
                                             as.numeric(ramp[2])),
       estimate = new[half], d = d, n = half,
       method = "YaRN NTK-aware RoPE frequency rescaling")
}

#' Reciprocal rank fusion score (Kamath Eq 7.1)
#'
#' Score_RRF = 1/(r + k) per 1-based rank, SUMMED across rankings,
#' with Cormack et al.'s damping k = 60 as the book's default.
#' Mirrors morie.fn.km110 (ported by the lead; the agent shard missed
#' this module and kmfait).
#'
#' @param r 1-based rank(s). @param k Damping constant.
#' @export
morie_kamath_rrf_score <- function(r, k = 60) {
  r <- as.numeric(r)
  if (length(r) == 0L) {
    stop("no ranks given; RRF over an empty ranking list is undefined.",
         call. = FALSE)
  }
  if (any(r < 1)) {
    stop("ranks are 1-based; a rank below 1 is not a search position.",
         call. = FALSE)
  }
  if (any(abs(r + k) < 1e-12)) {
    stop(sprintf("r + k = 0 for some rank with k = %s; the RRF score is a pole there.",
                 k), call. = FALSE)
  }
  scores <- 1 / (r + as.numeric(k))
  list(estimate = sum(scores), scores = scores, ranks = r,
       k = as.numeric(k), n = length(r),
       method = "reciprocal rank fusion score (Kamath Eq 7.1)")
}

#' RAGAS faithfulness (Kamath Ch 7)
#'
#' faithfulness = supported claims / total claims. `answer` is a
#' character vector of claims or one string split on sentence
#' punctuation; `entails` an optional judge function(claim, context)
#' -> logical, defaulting to the same strict lexical containment the
#' Python uses (every content token of the claim present in the
#' context) -- under-reports rather than hallucinating support, and
#' the judge used is named in the payload. Mirrors morie.fn.kmfait.
#'
#' @param answer Claims. @param context Retrieved passage(s).
#' @param entails Optional judge.
#' @export
morie_kamath_ragas_faithfulness <- function(answer, context,
                                            entails = NULL) {
  toks <- function(x) {
    x <- tolower(paste(x, collapse = " "))
    regmatches(x, gregexpr("[a-z0-9]+", x))[[1]]
  }
  claims <- if (length(answer) == 1L && is.character(answer)) {
    parts <- trimws(strsplit(answer, "[.!?\n]+")[[1]])
    parts[nzchar(parts)]
  } else {
    a <- trimws(as.character(answer))
    a[nzchar(a)]
  }
  if (length(claims) == 0L) {
    stop("the answer contains no claims; faithfulness is 0/0 and calling that 1.0 would be a decision, not a measurement.",
         call. = FALSE)
  }
  ctx_tokens <- toks(context)
  judge_name <- if (is.null(entails)) "lexical containment" else "caller"
  supported_flags <- vapply(claims, function(cl) {
    if (!is.null(entails)) return(isTRUE(entails(cl, context)))
    ct <- toks(cl)
    if (length(ct) == 0L) {
      stop("a claim contains no tokens at all.", call. = FALSE)
    }
    all(ct %in% ctx_tokens)
  }, logical(1))
  list(estimate = mean(supported_flags), supported = sum(supported_flags),
       n_claims = length(claims), claims = claims,
       claim_supported = unname(supported_flags), judge = judge_name,
       n = length(claims),
       method = "RAGAS faithfulness = supported/total claims (Kamath Ch 7)")
}
