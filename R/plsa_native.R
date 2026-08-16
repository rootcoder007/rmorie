# morie.fn -- function file (rootcoder007/morie)
# Probabilistic Latent Semantic Analysis by EM.
#
# LSA maps documents and terms into a low-dimensional latent space by
# truncated SVD of the term-document matrix. It works, and its
# theoretical foundation is, in Hofmann's words, unsatisfactory and
# incomplete: the objective is an L2 matrix approximation, which is not
# a statement about counts, and the resulting coordinates can be
# negative.
#
# The aspect model gives it a generative story. Associate an
# unobserved class z in {z_1,...,z_K} with each observation. The joint
# over documents and words is then, in the asymmetric parameterisation
# (eq. 1),
#     P(d, w) = P(d) P(w|d),  P(w|d) = sum_z P(w|z) P(z|d),
# and, equivalently, in the symmetric one (eq. 2),
#     P(d, w) = sum_z P(z) P(d|z) P(w|z).
# The two are the same model written from different sides -- the second
# is symmetric in documents and words, which makes the conditional
# independence explicit: d and w are independent GIVEN z. Because |z|
# is smaller than the number of documents or words, z is a bottleneck,
# and that bottleneck is what forces the model to find structure.
#
# EM, in closed form. The E step (eq. 3) is Bayes' rule over the
# latent class,
#     P(z|d,w) = P(z) P(d|z) P(w|z) / sum_z' P(z') P(d|z') P(w|z'),
# and the M step (eqs. 4-6) is expected-count normalisation,
#     P(w|z)  propto sum_d  n(d,w) P(z|d,w),
#     P(d|z)  propto sum_w  n(d,w) P(z|d,w),
#     P(z)    propto sum_d sum_w n(d,w) P(z|d,w).
#
# What it fixes and what it does not. Against LSA it gains a proper
# likelihood -- maximising it minimises the KL divergence between the
# empirical and modelled distributions -- and non-negative,
# interpretable parameters. What it does not gain is a generative
# story for UNSEEN documents: P(z|d) is a parameter fitted per
# training document, so the number of parameters grows with the
# corpus and a new document requires re-running EM. Putting a
# Dirichlet prior on P(z|d) is exactly what lda does, and that is
# the gap it closes.
#
# References
# ----------
# Hofmann, T. (1999) "Probabilistic Latent Semantic Analysis",
# Proceedings of the Fifteenth Conference on Uncertainty in
# Artificial Intelligence (UAI 1999), 289-296, arXiv:1301.6705.
# Sec. 2 (LSA by SVD and the assessment of its theoretical
# foundation), Sec. 3 (the aspect model, eq. (1) asymmetric and
# eq. (2) symmetric parameterisations, the conditional independence
# of d and w given z, and z as a bottleneck), and Sec. 3.2 (EM:
# eq. (3) for the E step and eqs. (4)-(6) for the M step; maximum
# likelihood as minimisation of the cross entropy or KL divergence
# against the empirical distribution).
#
# NOTE: the text layer of the local PDF is garbled across the equation
# region; the equations above were recovered by OCR (ocrpg.sh, 300 dpi
# + tesseract) rather than by pdftotext.
#
# Deerwester, S., Dumais, S. T., Furnas, G. W., Landauer, T. K. &
# Harshman, R. (1990) "Indexing by latent semantic analysis", Journal
# of the American Society for Information Science 41(6), 391-407. The
# LSA method this gives a probabilistic footing to.
#
# Blei, D. M., Ng, A. Y. & Jordan, M. I. (2003) "Latent Dirichlet
# Allocation", Journal of Machine Learning Research 3, 993-1022. The
# Dirichlet prior that removes the per-document parameter growth;
# implemented in lda.

.plsa_EPS <- 1e-300

#' .plsa_check
#'
#' A step of the plsa_native implementation. Called by \code{.plsa_e_step}, \code{.plsa_log_likelihood}, \code{.plsa_m_step} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_dw A matrix; passed to \code{as.matrix}.
#' @return A list with \code{N}, \code{D}, \code{V}.
#' @export
.plsa_check <- function(n_dw) {
  if (is.list(n_dw) && !is.data.frame(n_dw) && !is.matrix(n_dw)) {
    N <- do.call(rbind, lapply(n_dw, as.numeric))
  } else {
    N <- as.matrix(n_dw)
    storage.mode(N) <- "double"
  }
  if (nrow(N) == 0L || ncol(N) == 0L) stop("plsa: the count matrix is empty")
  if (any(N < 0)) stop("plsa: counts must be non-negative")
  if (sum(N) <= 0) stop("plsa: the corpus is empty")
  list(N = N, D = nrow(N), V = ncol(N))
}

#' .plsa_e_step
#'
#' A step of the plsa_native implementation. Called by \code{e_step}, \code{morie_plsa}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_dw Passed to \code{.plsa_check}.
#' @param Pz A vector; its length is taken.
#' @param Pd_z A matrix; indexed by row and column.
#' @param Pw_z A matrix; indexed by row and column.
#' @return The value of \code{post}, as built in the body.
#' @export
.plsa_e_step <- function(n_dw, Pz, Pd_z, Pw_z) {
  chk <- .plsa_check(n_dw)
  N <- chk$N
  D <- chk$D
  V <- chk$V
  K <- length(Pz)
  post <- array(0, dim = c(D, V, K))
  for (d in seq_len(D)) {
    for (w in seq_len(V)) {
      if (N[d, w] <= 0) next
      num <- Pz * Pd_z[, d] * Pw_z[, w]
      s <- sum(num)
      if (s <= .plsa_EPS) {
        post[d, w, ] <- rep(1.0 / K, K)
      } else {
        post[d, w, ] <- num / s
      }
    }
  }
  post
}

#' .plsa_m_step
#'
#' A step of the plsa_native implementation. Called by \code{m_step}, \code{morie_plsa}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_dw Passed to \code{.plsa_check}.
#' @param post A matrix; indexed by row and column.
#' @param K A count; the body uses it as \code{seq_len(...)}.
#' @return A list with \code{Pz}, \code{Pd_z}, \code{Pw_z}.
#' @export
.plsa_m_step <- function(n_dw, post, K) {
  chk <- .plsa_check(n_dw)
  N <- chk$N
  D <- chk$D
  V <- chk$V
  K <- as.integer(K)
  Pw_z <- matrix(.plsa_EPS, nrow = K, ncol = V)
  Pd_z <- matrix(.plsa_EPS, nrow = K, ncol = D)
  Pz <- rep(.plsa_EPS, K)
  for (d in seq_len(D)) {
    for (w in seq_len(V)) {
      c <- N[d, w]
      if (c <= 0) next
      r <- c * post[d, w, ]
      Pw_z[, w] <- Pw_z[, w] + r
      Pd_z[, d] <- Pd_z[, d] + r
      Pz <- Pz + r
    }
  }
  for (z in seq_len(K)) {
    Pw_z[z, ] <- Pw_z[z, ] / sum(Pw_z[z, ])
    Pd_z[z, ] <- Pd_z[z, ] / sum(Pd_z[z, ])
  }
  Pz <- Pz / sum(Pz)
  list(Pz = Pz, Pd_z = Pd_z, Pw_z = Pw_z)
}

#' .plsa_joint_probability
#'
#' A step of the plsa_native implementation. Called by \code{.plsa_log_likelihood}, \code{joint_probability}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Pz A vector; its length is taken.
#' @param Pd_z A matrix; indexed by row and column.
#' @param Pw_z A matrix; indexed by row and column.
#' @return The value of \code{P}, as built in the body.
#' @export
.plsa_joint_probability <- function(Pz, Pd_z, Pw_z) {
  K <- length(Pz)
  D <- ncol(Pd_z)
  V <- ncol(Pw_z)
  P <- matrix(0, nrow = D, ncol = V)
  for (d in seq_len(D)) {
    for (w in seq_len(V)) {
      P[d, w] <- sum(Pz * Pd_z[, d] * Pw_z[, w])
    }
  }
  P
}

#' .plsa_log_likelihood
#'
#' A step of the plsa_native implementation. Called by \code{.plsa_perplexity}, \code{morie_plsa}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_dw Passed to \code{.plsa_check}.
#' @param Pz Passed to \code{.plsa_joint_probability}.
#' @param Pd_z Passed to \code{.plsa_joint_probability}.
#' @param Pw_z Passed to \code{.plsa_joint_probability}.
#' @return The value of \code{.plsa_log_likelihood}.
#' @export
.plsa_log_likelihood <- function(n_dw, Pz, Pd_z, Pw_z) {
  chk <- .plsa_check(n_dw)
  N <- chk$N
  P <- .plsa_joint_probability(Pz, Pd_z, Pw_z)
  P <- pmax(P, .plsa_EPS)
  sum(N * log(P))
}

#' morie_plsa
#'
#' A step of the plsa_native implementation. Called by \code{probabilisticlsa}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_dw Passed to \code{.plsa_check}.
#' @param K A count; the body uses it as \code{seq_len(...)}.
#' @param iters Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-08}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @return A list with \code{estimate}, \code{P_z}, \code{P_d_given_z}, \code{P_w_given_z}, \code{loglik_history}, \code{final_loglik}, \code{iterations}, \code{K}, \code{n_docs}, \code{vocab}, \code{n_parameters}, \code{method}, \code{caveat}.
#' @export
morie_plsa <- function(n_dw, K, iters = 100, tol = 1e-8, seed = 0) {
  chk <- .plsa_check(n_dw)
  N <- chk$N
  D <- chk$D
  V <- chk$V
  if (as.integer(K) < 1L) stop("plsa: K must be at least 1")
  K <- as.integer(K)

  state <- .ghc_rng(seed)

  res <- .ghc_unif(state, K)
  state <- res$st
  tmp <- 0.5 + res$x
  Pz <- tmp / sum(tmp)

  Pd_z <- matrix(0, nrow = K, ncol = D)
  for (z in seq_len(K)) {
    res <- .ghc_unif(state, D)
    state <- res$st
    tmp <- 0.5 + res$x
    Pd_z[z, ] <- tmp / sum(tmp)
  }

  Pw_z <- matrix(0, nrow = K, ncol = V)
  for (z in seq_len(K)) {
    res <- .ghc_unif(state, V)
    state <- res$st
    tmp <- 0.5 + res$x
    Pw_z[z, ] <- tmp / sum(tmp)
  }

  hist <- numeric(0)
  prev <- NULL
  it <- 0L
  iters_i <- as.integer(iters)
  for (it in seq_len(iters_i)) {
    post <- .plsa_e_step(N, Pz, Pd_z, Pw_z)
    ms <- .plsa_m_step(N, post, K)
    Pz <- ms$Pz
    Pd_z <- ms$Pd_z
    Pw_z <- ms$Pw_z
    ll <- .plsa_log_likelihood(N, Pz, Pd_z, Pw_z)
    hist <- c(hist, ll)
    if (!is.null(prev) && abs(ll - prev) < tol) break
    prev <- ll
  }

  list(
    estimate = Pw_z,
    P_z = Pz,
    P_d_given_z = Pd_z,
    P_w_given_z = Pw_z,
    loglik_history = hist,
    final_loglik = hist[length(hist)],
    iterations = as.integer(it),
    K = K,
    n_docs = D,
    vocab = V,
    n_parameters = K * (D + V) + K,
    method = "EM for the aspect model; Hofmann (1999) eqs. (3)-(6)",
    caveat = paste("P(z|d) is a per-document PARAMETER, so the count",
                   "grows with the corpus and an unseen document needs",
                   "EM re-run -- the gap LDA's Dirichlet prior closes")
  )
}

#' .plsa_perplexity
#'
#' A step of the plsa_native implementation. Called by \code{perplexity}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_dw Passed to \code{.plsa_check}.
#' @param Pz Passed to \code{.plsa_log_likelihood}.
#' @param Pd_z Passed to \code{.plsa_log_likelihood}.
#' @param Pw_z Passed to \code{.plsa_log_likelihood}.
#' @return A numeric value.
#' @export
.plsa_perplexity <- function(n_dw, Pz, Pd_z, Pw_z) {
  chk <- .plsa_check(n_dw)
  N <- chk$N
  tot <- sum(N)
  exp(-.plsa_log_likelihood(N, Pz, Pd_z, Pw_z) / tot)
}

#' .plsa_cheatsheet
#'
#' A step of the plsa_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.plsa_cheatsheet <- function() {
  paste("plsa: the ASPECT model. P(d,w) = sum_z P(z)P(d|z)P(w|z)",
        "-- d and w independent GIVEN z, with |z| small so z is a",
        "bottleneck. EM: E step is Bayes over z, M step is",
        "expected-count normalisation. Fixes LSA's missing",
        "likelihood and gives non-negative parameters. Does NOT",
        "fix generalisation: P(z|d) is a per-document parameter,",
        "so parameters grow with the corpus -- that is what LDA's",
        "Dirichlet prior removes.")
}

# Compact aliases per ledger/NAMING.md
#' Compact aliases per ledger/NAMING.md
#'
#' A step of the plsa_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_dw Passed to \code{morie_plsa}.
#' @param K Passed to \code{morie_plsa}.
#' @param iters Passed to \code{morie_plsa}. Defaults to \code{100}.
#' @param tol Passed to \code{morie_plsa}. Defaults to \code{1e-08}.
#' @param seed Passed to \code{morie_plsa}. Defaults to \code{0}.
#' @return The value of \code{morie_plsa}.
#' @export
probabilisticlsa <- function(n_dw, K, iters = 100, tol = 1e-8, seed = 0) {
  morie_plsa(n_dw, K, iters, tol, seed)
}
plsa <- probabilisticlsa

# Public helpers
#' Public helpers
#'
#' A step of the plsa_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_dw Passed to \code{.plsa_e_step}.
#' @param Pz Passed to \code{.plsa_e_step}.
#' @param Pd_z Passed to \code{.plsa_e_step}.
#' @param Pw_z Passed to \code{.plsa_e_step}.
#' @return The value of \code{.plsa_e_step}.
#' @export
e_step <- function(n_dw, Pz, Pd_z, Pw_z) .plsa_e_step(n_dw, Pz, Pd_z, Pw_z)
#' m_step
#'
#' A step of the plsa_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_dw Passed to \code{.plsa_m_step}.
#' @param post Passed to \code{.plsa_m_step}.
#' @param K Passed to \code{.plsa_m_step}.
#' @return The value of \code{.plsa_m_step}.
#' @export
m_step <- function(n_dw, post, K) .plsa_m_step(n_dw, post, K)
#' .plsa_log_likelihood
#'
#' Internal helper in plsa_native.R; see the file header for
#' the source the module follows.
#'
#' @param n_dw Passed to \code{.plsa_check}.
#' @param Pz Passed to \code{.plsa_joint_probability}.
#' @param Pd_z Passed to \code{.plsa_joint_probability}.
#' @param Pw_z Passed to \code{.plsa_joint_probability}.
#' @return The value of \code{sum}.
#' @export
.plsa_log_likelihood <- function(n_dw, Pz, Pd_z, Pw_z) .plsa_log_likelihood(n_dw, Pz, Pd_z, Pw_z)
#' joint_probability
#'
#' A step of the plsa_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Pz Passed to \code{.plsa_joint_probability}.
#' @param Pd_z Passed to \code{.plsa_joint_probability}.
#' @param Pw_z Passed to \code{.plsa_joint_probability}.
#' @return The value of \code{.plsa_joint_probability}.
#' @export
joint_probability <- function(Pz, Pd_z, Pw_z) .plsa_joint_probability(Pz, Pd_z, Pw_z)
#' perplexity
#'
#' A step of the plsa_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_dw Passed to \code{.plsa_perplexity}.
#' @param Pz Passed to \code{.plsa_perplexity}.
#' @param Pd_z Passed to \code{.plsa_perplexity}.
#' @param Pw_z Passed to \code{.plsa_perplexity}.
#' @return The value of \code{.plsa_perplexity}.
#' @export
perplexity <- function(n_dw, Pz, Pd_z, Pw_z) .plsa_perplexity(n_dw, Pz, Pd_z, Pw_z)
.plsa_cheatsheet <- .plsa_cheatsheet
