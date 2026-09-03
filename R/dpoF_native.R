# Direct Preference Optimization: loss, implicit rewards, gradient weights.
#
# Rafailov, R., Sharma, A., Mitchell, E., Ermon, S., Manning, C. D., &
# Finn, C. (2023) "Direct Preference Optimization: Your Language Model is
# Secretly a Reward Model", arXiv:2305.18290.
#
# The KL-constrained RLHF objective has the closed-form optimum (eq. 4)
#   pi_r(y|x) = (1/Z(x)) * pi_ref(y|x) * exp(r(x,y)/beta),
# which rearranges (eq. 5) to express the reward in terms of its own
# optimal policy. Because Bradley-Terry depends only on reward
# differences, the intractable beta*log Z(x) cancels and the preference
# likelihood becomes a function of the policy alone. Maximum likelihood
# on that gives the DPO loss (eq. 7):
#   L_DPO = -E[log sigma(beta*log pi(y_w|x)/pi_ref(y_w|x)
#                        - beta*log pi(y_l|x)/pi_ref(y_l|x))].
# The implicit reward is rhat = beta*log(pi_theta/pi_ref), and the
# per-pair gradient weight is sigma(rhat_l - rhat_w).
# Both bradley-terry (eq. 7) and plackett-luce (eq. 20) are implemented.

#' .dpoF_logsigmoid
#'
#' A step of the dpoF_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Numeric; passed to \code{exp}.
#' @return A numeric value.
#' @export
.dpoF_logsigmoid <- function(z) {
  if (z >= 0.0) {
    return(-log1p(exp(-z)))
  }
  return(z - log1p(exp(z)))
}

#' .dpoF_sigmoid
#'
#' A step of the dpoF_native implementation. Called by \code{morie_dpoF}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Numeric; passed to \code{exp}.
#' @return A numeric value.
#' @export
.dpoF_sigmoid <- function(z) {
  if (z >= 0.0) {
    return(1.0 / (1.0 + exp(-z)))
  }
  e <- exp(z)
  return(e / (1.0 + e))
}

#' .dpoF_logsumexp
#'
#' A step of the dpoF_native implementation. Called by \code{.dpoF_plackett_luce},
#' \code{optimal_policy}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param vals A vector; its length is taken.
#' @return A numeric value.
#' @export
.dpoF_logsumexp <- function(vals) {
  if (length(vals) == 0L) {
    return(-Inf)
  }
  m <- max(vals)
  if (m == -Inf) {
    return(m)
  }
  return(m + log(sum(exp(vals - m))))
}

#' .dpoF_vec
#'
#' A step of the dpoF_native implementation. Called by \code{morie_dpoF}, \code{optimal_policy}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param name Passed to \code{sprintf}.
#' @return The value of \code{v}, as built in the body.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' txt <- c('alpha', 'beta', 'gamma', 'delta')
#' res <- .dpoF_vec(x = x, name = txt)
#' res
.dpoF_vec <- function(x, name) {
  v <- as.numeric(x)
  if (length(v) == 0L) {
    stop(sprintf("dpoF: %s must be non-empty", name))
  }
  return(v)
}

#' morie_dpoF
#'
#' A step of the dpoF_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param logp_w Passed to \code{.dpoF_vec}.
#' @param logp_l Passed to \code{.dpoF_vec}.
#' @param logp_ref_w Passed to \code{.dpoF_vec}.
#' @param logp_ref_l Passed to \code{.dpoF_vec}.
#' @param beta Numeric; combined arithmetically in the body. Defaults to \code{0.1}.
#' @param model Compared against \code{"plackett-luce"}. Defaults to \code{"bradley-terry"}.
#' @param logp Passed to \code{.dpoF_plackett_luce}.
#' @param logp_ref Passed to \code{.dpoF_plackett_luce}.
#' @param label_smoothing Coerced to numeric by the body, with \code{as.numeric}.
#' Defaults to \code{0}.
#' @return A list with \code{estimate}, \code{loss}, \code{losses}, \code{reward_w},
#' \code{reward_l}, \code{margin}, \code{grad_weight}, \code{accuracy}, \code{beta},
#' \code{n}, \code{model}, \code{method}.
#' @export
morie_dpoF <- function(logp_w = NULL, logp_l = NULL,
                       logp_ref_w = NULL, logp_ref_l = NULL,
                       beta = 0.1, model = "bradley-terry",
                       logp = NULL, logp_ref = NULL,
                       label_smoothing = 0.0) {
  MODELS <- c("bradley-terry", "plackett-luce")

  if (!(model %in% MODELS)) {
    stop(sprintf("dpoF: model must be one of %s, got %s",
                 paste(shQuote(MODELS), collapse = ", "), model))
  }
  beta <- as.numeric(beta)
  if (!(beta > 0.0)) {
    stop(sprintf("dpoF: beta must be > 0, got %s", beta))
  }

  if (model == "plackett-luce") {
    return(.dpoF_plackett_luce(logp, logp_ref, beta))
  }

  eps <- as.numeric(label_smoothing)
  if (!(eps >= 0.0 && eps < 0.5)) {
    stop(sprintf("dpoF: label_smoothing must lie in [0, 0.5), got %s", eps))
  }

  pw <- .dpoF_vec(logp_w, "logp_w")
  pl <- .dpoF_vec(logp_l, "logp_l")
  rw <- .dpoF_vec(logp_ref_w, "logp_ref_w")
  rl <- .dpoF_vec(logp_ref_l, "logp_ref_l")
  n <- length(pw)

  if (!(length(pl) == n && length(rw) == n && length(rl) == n)) {
    stop("dpoF: logp_w, logp_l, logp_ref_w and logp_ref_l must have the same length")
  }

  reward_w <- beta * (pw - rw)
  reward_l <- beta * (pl - rl)
  margin <- reward_w - reward_l

  if (eps == 0.0) {
    losses <- -sapply(margin, .dpoF_logsigmoid)
  } else {
    losses <- -(1.0 - eps) * sapply(margin, .dpoF_logsigmoid) -
              eps * sapply(-margin, .dpoF_logsigmoid)
  }

  grad_w <- sapply(margin, function(m) .dpoF_sigmoid(-m))
  acc <- sum(margin > 0.0) / n

  loss <- sum(losses) / n

  return(list(
    estimate = as.numeric(loss),
    loss = as.numeric(loss),
    losses = losses,
    reward_w = reward_w,
    reward_l = reward_l,
    margin = margin,
    grad_weight = grad_w,
    accuracy = as.numeric(acc),
    beta = beta,
    n = n,
    model = "bradley-terry",
    method = "DPO (Rafailov et al. 2023 eq. 7)"
  ))
}

#' .dpoF_plackett_luce
#'
#' A step of the dpoF_native implementation. Called by \code{morie_dpoF}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param logp Optional; may be \code{NULL}. A matrix; passed to \code{dim}.
#' @param logp_ref Optional; may be \code{NULL}. A matrix; passed to \code{dim}.
#' @param beta Numeric; combined arithmetically in the body.
#' @return A list with \code{estimate}, \code{loss}, \code{losses}, \code{rewards},
#' \code{beta}, \code{n}, \code{model}, \code{method}.
#' @export
.dpoF_plackett_luce <- function(logp, logp_ref, beta) {
  if (is.null(logp) || is.null(logp_ref)) {
    stop("dpoF: model='plackett-luce' needs logp and logp_ref, shape (n_rankings, K)")
  }

  if (is.null(dim(logp))) {
    P <- matrix(as.numeric(logp), nrow = 1L)
  } else {
    P <- as.matrix(logp)
    storage.mode(P) <- "double"
  }
  if (is.null(dim(logp_ref))) {
    R <- matrix(as.numeric(logp_ref), nrow = 1L)
  } else {
    R <- as.matrix(logp_ref)
    storage.mode(R) <- "double"
  }

  if (nrow(P) != nrow(R)) {
    stop("dpoF: logp and logp_ref must have the same number of rankings")
  }

  n_rankings <- nrow(P)
  K <- ncol(P)

  if (K < 2L) {
    stop("dpoF: each ranking needs K >= 2 completions")
  }
  if (ncol(R) != K) {
    stop(sprintf("dpoF: ranking has %d policy entries but %d reference entries",
                 K, ncol(R)))
  }

  losses <- numeric(n_rankings)
  rewards <- vector("list", n_rankings)

  for (i in seq_len(n_rankings)) {
    rhat <- beta * (P[i, ] - R[i, ])
    rewards[[i]] <- rhat
    ll <- 0.0
    for (k in seq_len(K)) {
      ll <- ll + rhat[k] - .dpoF_logsumexp(rhat[k:K])
    }
    losses[i] <- -ll
  }

  loss <- sum(losses) / length(losses)

  return(list(
    estimate = as.numeric(loss),
    loss = as.numeric(loss),
    losses = losses,
    rewards = rewards,
    beta = beta,
    n = length(losses),
    model = "plackett-luce",
    method = "DPO (Rafailov et al. 2023 eq. 20)"
  ))
}

#' optimal_policy
#'
#' A step of the dpoF_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param logp_ref Passed to \code{.dpoF_vec}.
#' @param reward Passed to \code{.dpoF_vec}.
#' @param beta Numeric; combined arithmetically in the body.
#' @return The value of \code{list}.
#' @export
optimal_policy <- function(logp_ref, reward, beta) {
  lr <- .dpoF_vec(logp_ref, "logp_ref")
  rr <- .dpoF_vec(reward, "reward")
  if (length(lr) != length(rr)) {
    stop("optimal_policy: logp_ref and reward must have the same length")
  }
  beta <- as.numeric(beta)
  if (!(beta > 0.0)) {
    stop("optimal_policy: beta must be > 0")
  }
  unnorm <- lr + rr / beta
  logZ <- .dpoF_logsumexp(unnorm)
  return(list(unnorm - logZ, as.numeric(logZ)))
}

#' .dpoF_cheatsheet
#'
#' A step of the dpoF_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .dpoF_cheatsheet()
#' res
.dpoF_cheatsheet <- function() {
  return("dpoF: DPO loss -log sigma(beta log pi_w/ref_w - beta log pi_l/ref_l) (Rafailov 2023 eq. 7); implicit reward rhat = beta log pi/pi_ref; grad weight sigma(rhat_l - rhat_w); model='plackett-luce' is eq. 20 and reduces to eq. 7 at K=2. optimal_policy() is eq. 4.")
}

# compact aliases per ledger/NAMING.md
dpoF <- morie_dpoF
dpo_loss <- morie_dpoF
dpoloss <- morie_dpoF
