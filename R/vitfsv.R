# SPDX-License-Identifier: AGPL-3.0-or-later
#' ViT fine-tuning: the downstream head attached to z_L^0
#'
#' SOURCE.  Dosovitskiy et al. (2021), "An Image is Worth 16x16 Words:
#' Transformers for Image Recognition at Scale", ICLR 2021; arXiv:2010.11929v2.
#' Read from the PDF rendered as page images.
#'
#' Section 3.2 "Fine-tuning and higher resolution", p. 4: "Typically, we
#' pre-train ViT on large datasets, and fine-tune to (smaller) downstream tasks.
#' For this, we remove the pre-trained prediction head and attach a
#' zero-initialized D x K feedforward layer, where K is the number of downstream
#' classes."  Section 3.1, p. 3, adds that the head is "a MLP with one hidden
#' layer at pre-training time and by a single linear layer at fine-tuning time",
#' attached to z_L^0.
#'
#' Section 4.1 "Setup", Metrics, p. 5: "Few-shot accuracies are obtained by
#' solving a regularized least-squares regression problem that maps the (frozen)
#' representation of a subset of training images to \{-1, 1\}^K target vectors.
#' This formulation allows us to recover the exact solution in closed form."
#'
#' Two modes are therefore implemented, and both are the paper's own.
#' mode = "init" is the head exactly as Section 3.2 attaches it, all zeros:
#' every logit is 0, so with the first-maximum tie rule every image is assigned
#' class 1.  That is the state of the model before any fine-tuning step and is
#' included because it is the one point where the paper pins the head's
#' numerical value.  mode = "fewshot" is the closed-form regularized least
#' squares head of Section 4.1, W = (X'X + lambda I)^\{-1\} X' T with T the
#' \{-1, 1\}^K target matrix and X the frozen representations.  mode = "full"
#' would unfreeze the backbone, which requires backpropagation through the
#' encoder; this package does not implement that, and the function raises rather
#' than pretending.
#'
#' The paper does not state a tie-breaking rule for argmax; first maximum is
#' used, matching which.max, and is stated as this implementation's choice.
#'
#' @param model n-by-D matrix of frozen representations; row i is the y of
#'   Equation (4) for image i.
#' @param data length-n integer class labels in 1 ... K.
#' @param mode "init", "fewshot" or "full".
#' @param ridge lambda of the regularized least squares, "fewshot" only.
#' @param n_classes K; defaults to the largest label seen.
#' @return list: estimate, accuracy, head, logits, pred, confusion, n_correct,
#'   n_classes, embed_dim, mode, n, method.
#' @keywords internal
#' @examples
#' Vitfsv(matrix(c(1, 0, 0, 1), 2, 2), c(1, 2), "fewshot")$confusion
#' @export
Vitfsv <- function(model, data, mode = "init", ridge = 1e-8, n_classes = NULL) {
  X <- .s03mat(model)
  n <- nrow(X)
  if (n < 1L) stop("vit_finetune: model must have at least one row")
  d <- ncol(X)
  y <- as.integer(.s03vec(data))
  if (length(y) != n) {
    stop("vit_finetune: data must have one label per row of model")
  }
  kk <- if (is.null(n_classes)) max(y) else as.integer(n_classes)
  if (is.na(kk) || kk < 1L) stop("vit_finetune: n_classes must be at least 1")
  if (any(y < 1L) || any(y > kk)) {
    stop("vit_finetune: labels must lie in 1 ... K")
  }
  if (identical(mode, "full")) {
    stop(paste0("vit_finetune: mode 'full' unfreezes the backbone, which needs ",
                "backpropagation through the encoder; not implemented"))
  }
  if (!(identical(mode, "init") || identical(mode, "fewshot"))) {
    stop("vit_finetune: mode must be 'init', 'fewshot' or 'full'")
  }
  W <- matrix(0, d, kk)
  if (identical(mode, "fewshot")) {
    for (c in seq_len(kk)) {
      tc <- ifelse(y == c, 1, -1)
      W[, c] <- .s03lstsq(X, tc, as.numeric(ridge))
    }
  }
  logits <- .s03matmul(X, W)
  pred <- integer(n)
  for (i in seq_len(n)) pred[i] <- .vitargmax(logits[i, ])
  conf <- matrix(0L, kk, kk)
  hit <- 0L
  for (i in seq_len(n)) {
    conf[y[i], pred[i]] <- conf[y[i], pred[i]] + 1L
    if (pred[i] == y[i]) hit <- hit + 1L
  }
  list(estimate = hit / n, accuracy = hit / n, head = W, logits = logits,
       pred = pred, confusion = conf, n_correct = hit, n_classes = kk,
       embed_dim = d, mode = mode, n = n,
       method = paste0("zero-initialised D x K head (Dosovitskiy et al. 2021, ",
                       "Sec. 3.2 p. 4); closed-form regularized least squares ",
                       "onto {-1,1}^K (Sec. 4.1 Metrics p. 5)"))
}
