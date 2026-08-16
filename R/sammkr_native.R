# Three masks, one gradient: SAM's answer to ambiguity.
# Sources: Kirillov, A., Mintun, E., Ravi, N., Mao, H., Rolland, C.,
# Gustafson, L., Xiao, T., Whitehead, S., Berg, A. C., Lo, W.-Y.,
# Dollar, P. & Girshick, R. (2023) "Segment Anything", *ICCV 2023*,
# 4015-4026, arXiv:2304.02643. Sec. 3, "Resolving ambiguity": that
# with one output the model will AVERAGE multiple valid masks given
# an ambiguous prompt; the modification to predict multiple output
# masks for a single prompt; that 3 mask outputs is sufficient for
# most common cases since nested masks are often at most three deep
# (whole, part and subpart); that during training only the MINIMUM
# loss over masks is backpropagated; and that the model predicts a
# confidence score (estimated IoU) for each mask so they can be
# ranked.

.SAMMKR_EPS <- 1e-12
.SAMMKR_NESTING <- c("whole", "part", "subpart")

#' .sammkr_flat
#'
#' A step of the sammkr_native implementation. Called by \code{iou}, \code{whole_part_subpart}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.sammkr_flat <- function(m) {
  M <- m
  if (is.list(M) && !is.matrix(M)) {
    if (length(M) > 0 && is.list(M[[1]])) {
      nr <- length(M); nc <- length(M[[1]])
      M <- matrix(0, nrow = nr, ncol = nc)
      for (i in 1:nr) for (j in 1:nc) M[i, j] <- M[[i]][[j]]
    } else {
      M <- unlist(M)
    }
  }
  as.numeric(M)
}

#' average_of_valid_masks
#'
#' A step of the sammkr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param masks A vector; its length is taken.
#' @return A list with \code{mask}, \code{ambiguous_fraction}, \code{n_averaged}, \code{note}.
#' @export
average_of_valid_masks <- function(masks) {
  if (length(masks) == 0)
    stop("sammkr: no masks given")
  F <- lapply(masks, .sammkr_flat)
  n <- length(F[[1]])
  if (any(lengths(F) != n))
    stop("sammkr: the masks differ in size")
  acc <- numeric(n)
  for (f in F) acc <- acc + f
  avg <- acc / length(F)
  frac <- sum(avg > 0.05 & avg < 0.95) / n
  list(mask = avg, ambiguous_fraction = frac, n_averaged = length(F),
       note = "pixels strictly between 0 and 1 belong to no single valid interpretation")
}

#' iou
#'
#' A step of the sammkr_native implementation. Called by \code{rank_masks}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Passed to \code{.sammkr_flat}.
#' @param b Passed to \code{.sammkr_flat}.
#' @param threshold Passed to \code{>}. Defaults to \code{0.5}.
#' @return One of two values, depending on the branch taken.
#' @export
iou <- function(a, b, threshold = 0.5) {
  x <- as.numeric(.sammkr_flat(a) > threshold)
  y <- as.numeric(.sammkr_flat(b) > threshold)
  if (length(x) != length(y))
    stop("sammkr: the masks differ in size")
  inter <- sum(x * y)
  union <- sum(pmax(x, y))
  if (union == 0) 1.0 else inter / union
}

#' min_loss_over_masks
#'
#' A step of the sammkr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param predictions A vector; its length is taken.
#' @param target Passed to \code{loss_fn}.
#' @param loss_fn Accepted by the signature and not used anywhere in the body.
#' @return A list with \code{loss}, \code{index}, \code{losses}, \code{mean_loss}, \code{gap}, \code{note}.
#' @export
min_loss_over_masks <- function(predictions, target, loss_fn) {
  if (length(predictions) == 0)
    stop("sammkr: no predictions given")
  losses <- sapply(predictions, function(p) as.numeric(loss_fn(p, target)))
  j <- which.min(losses)
  meanl <- mean(losses)
  list(loss = losses[j], index = as.integer(j) - 1L, losses = losses,
       mean_loss = meanl, gap = meanl - losses[j],
       note = sprintf("only output %d receives gradient; the others are free to specialise elsewhere",
                      as.integer(j) - 1L))
}

#' whole_part_subpart
#'
#' A step of the sammkr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param masks A vector; its length is taken and its elements indexed.
#' @param target_hierarchy Accepted by the signature and not used anywhere in the body.
#' @return A list with \code{assignment}, \code{sizes}, \code{nested}, \code{note}.
#' @export
whole_part_subpart <- function(masks, target_hierarchy = NULL) {
  if (length(masks) != 3)
    stop(sprintf("sammkr: the paper's argument is about THREE outputs (whole, part, subpart), got %d",
                 length(masks)))
  sizes <- sapply(masks, function(m) sum(.sammkr_flat(m) > 0.5))
  order_idx <- order(-sizes)
  named <- list()
  for (rank in 0:2)
    named[[.SAMMKR_NESTING[rank + 1L]]] <- order_idx[rank + 1L] - 1L
  flat_order <- lapply(order_idx, function(i) .sammkr_flat(masks[[i + 1L]]))
  nested <- TRUE
  for (r in 1:2) {
    inner <- which(flat_order[[r + 1]] > 0.5)
    outer <- which(flat_order[[r]] > 0.5)
    if (!all(inner %in% outer)) { nested <- FALSE; break }
  }
  list(assignment = named, sizes = sizes, nested = nested,
       note = "nested masks are often at most three deep")
}

#' rank_masks
#'
#' A step of the sammkr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param masks A vector; its length is taken.
#' @param predicted_iou Passed to \code{unlist}.
#' @param target Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return The value of \code{out}, as built in the body.
#' @export
rank_masks <- function(masks, predicted_iou, target = NULL) {
  p <- as.numeric(unlist(predicted_iou))
  if (length(p) != length(masks))
    stop(sprintf("sammkr: %d masks but %d predicted IoUs",
                 length(masks), length(p)))
  order_idx <- order(-p) - 1L
  out <- list(order = order_idx, best = order_idx[1], predicted_iou = p)
  if (!is.null(target)) {
    true <- sapply(masks, function(m) iou(m, target))
    best_true <- which.max(true) - 1L
    out$true_iou <- true
    out$best_true <- best_true
    out$correct <- order_idx[1] == best_true
    out$calibration_error <- sum(abs(p - true)) / length(p)
    out$regret <- true[best_true + 1L] - true[order_idx[1] + 1L]
  }
  out$estimate <- order_idx[1]
  out$method <- "multi-mask output with IoU ranking; Kirillov et al. (2023)"
  out$note <- paste("the score is a LEARNED estimate, so its error is ",
                    "reported rather than assumed away", sep = "")
  out
}

#' .sammkr_cheatsheet
#'
#' A step of the sammkr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.sammkr_cheatsheet <- function() {
  paste("sammkr: one output forces the model to AVERAGE the valid ",
        "masks of an ambiguous prompt -- a blur that answers nobody. ",
        "So predict THREE, because segmentation nesting is usually at ",
        "most three deep: whole, part, subpart. During training ",
        "backprop only the MINIMUM loss, which is what makes the three ",
        "specialise instead of collapsing into one (the mean would ",
        "collapse them). At inference there is no ground truth, so the ",
        "model predicts its own IoU per mask to rank them -- a learned ",
        "estimate, so report its calibration error.", sep = "")
}

sammultimask <- rank_masks
sam_multi_mask_rank <- rank_masks

morie_sammkr <- rank_masks
