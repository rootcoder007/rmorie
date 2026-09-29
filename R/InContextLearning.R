#' In-context learning by prompt-conditioned scoring
#'
#' R arm of \code{morie.fn.hmicl}: assembles the k-shot prompt from the
#' demonstrations (\code{template} with \code{x} and \code{y} fields, joined
#' by \code{separator}, the query line last), scores every candidate label
#' with the supplied \code{model(prompt, candidate)} log-probability and
#' returns the arg-max with the softmax posterior. Candidates default to the
#' labels seen in the demonstrations.
#'
#' @param model Function \code{(prompt, candidate) -> log-probability}.
#' @param examples List of \code{list(x, y)} demonstrations.
#' @param query The input to classify.
#' @param candidates Optional label set.
#' @param template Format with the literal fields \code{x} and \code{y} in
#'   braces.
#' @param separator Joined between lines.
#' @return List with \code{prediction}, \code{prompt}, \code{log_probs},
#'   \code{posterior}, \code{n_shot}, \code{candidates}, \code{estimate}.
#' @references Brown, T. B. et al. (2020). Language models are few-shot
#'   learners. NeurIPS 33, 1877-1901.
#' @examples
#' sc <- function(p, cand) lengths(regmatches(p, gregexpr(cand, p, fixed = TRUE)))
#' Hmicl(sc, list(list("a", "pos"), list("b", "pos"), list("c", "neg")), "d")$prediction
#' @export
Hmicl <- function(model, examples, query, candidates = NULL, template = "{x} -> {y}", separator = "\n") {
  if (!is.function(model)) stop("model must be a function")
  if (!grepl("{x}", template, fixed = TRUE)) stop("template must contain an {x} field")
  fill <- function(x, y) gsub("{y}", y, gsub("{x}", x, template, fixed = TRUE), fixed = TRUE)
  lines <- vapply(examples, function(e) fill(as.character(e[[1]]), as.character(e[[2]])), "")
  if (is.null(candidates)) candidates <- unique(vapply(examples, function(e) as.character(e[[2]]), ""))
  if (!length(candidates)) stop("no candidate labels -- pass candidates explicitly when examples is empty")
  prompt <- paste(c(lines, sub("\\s+$", "", fill(as.character(query), ""))), collapse = separator)
  lp <- vapply(candidates, function(cand) {
    s <- as.numeric(model(prompt, cand))
    if (!is.finite(s)) stop("model returned a non-finite log-probability for candidate ", cand)
    s
  }, 0)
  e <- exp(lp - max(lp))
  post <- e / sum(e)
  best <- which.max(lp)
  list(prediction = candidates[best], prompt = prompt, log_probs = unname(lp), posterior = unname(post),
       n_shot = length(examples), candidates = candidates, estimate = post[[best]], n = length(examples))
}
