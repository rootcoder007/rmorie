# morie.fn -- function file (rootcoder007/morie)
# Latent Dirichlet Allocation by variational EM.
#
# A document is a mixture over topics; a topic is a distribution over
# words. The generative story is short:
#
# 1. choose theta ~ Dir(alpha);
# 2. for each of the N words, choose a topic z_n ~ Multinomial(theta),
#    then a word w_n ~ p(w_n | z_n, beta).
#
# with beta a k x V matrix, beta_{ij} = p(w^j = 1 | z^i = 1). The
# document length N is ancillary -- it is drawn from a Poisson in the
# paper's statement but nothing downstream depends on that, so its
# randomness is ignored.
#
# Why the posterior is intractable. Marginalising theta and z couples
# them through beta, so p(theta, z | w, alpha, beta) has no closed form.
#
# The variational fix is a deliberate act of vandalism. Delete the
# edges between theta, z and w, drop the w nodes, and give the wreckage
# its own free parameters. The resulting family factorises, and the
# best member of it is the one closest in KL divergence to the true
# posterior (eq. 5). Setting the derivatives of that divergence to
# zero gives a fixed point:
#
#   phi_{ni}  prop beta_{i w_n} exp{E_q[log theta_i | gamma]},
#   gamma_i   = alpha_i + sum_{n=1}^{N} phi_{ni},
#
# (eqs. 6-7) with the expectation available in closed form,
#
#   E_q[log theta_i | gamma] = Psi(gamma_i) - Psi(sum_j gamma_j),
#
# (eq. 8) where Psi is the digamma function.
#
# Both updates are recognisable. The gamma update is a posterior
# Dirichlet given the expected topic counts E[z_n | phi_n] -- prior
# plus observations. The phi update is Bayes' theorem, p(z_n | w_n)
# prop p(w_n | z_n) p(z_n), with the prior replaced by the
# exponential of the expected log topic weight. Not the expected
# weight -- the exponential of the expected log, which is smaller by
# Jensen, and that gap is exactly what makes variational inference
# under-confident about rare topics.
#
# The variational parameters are per-document. The optimisation is run
# for fixed w, so (gamma*, phi*) are functions of the document.
# gamma*(w) is what represents that document in the topic simplex.
#
# Why the bound only ever goes up. Each update maximises the same
# lower bound over one block with the other fixed, so the bound is
# monotone across iterations. That is a property of the algorithm and
# the anchor checks it directly rather than checking that the fit
# looks plausible.
#
# References
# ----------
# Blei, D. M., Ng, A. Y. & Jordan, M. I. (2003) "Latent Dirichlet
# Allocation", Journal of Machine Learning Research 3, 993-1022.
# Sec. 3 (the generative process, the k x V matrix beta, and the
# ancillary role of N), Sec. 5.1 (the variational family obtained by
# dropping edges, eq. (5) as a KL minimisation, the fixed-point updates
# of eqs. (6)-(7), the digamma expression of eq. (8), and the
# interpretation of both updates), and the variational inference
# algorithm initialising phi at 1/k and gamma at alpha + N/k.
#
# Hofmann, T. (1999) "Probabilistic Latent Semantic Analysis",
# Proceedings of the Fifteenth Conference on Uncertainty in Artificial
# Intelligence (UAI 1999), 289-296, arXiv:1301.6705. The aspect model
# LDA places a Dirichlet prior over; implemented in plsa.

.morie_lda_eps <- 1e-12

.morie_lda_e_log_theta <- function(gamma) {
  g <- as.numeric(gamma)
  if (any(g <= 0.0)) {
    stop(sprintf("lda: gamma must be strictly positive, got %g", min(g)))
  }
  s <- .s03digamma(sum(g))
  return(.s03digamma(g) - s)
}

.morie_lda_variational_inference <- function(doc, alpha, beta, iters=100, tol=1e-8) {
  w <- as.integer(doc)
  B <- as.matrix(beta)
  storage.mode(B) <- "double"
  K <- nrow(B)
  if (K < 1) stop("lda: beta must have at least one topic")
  V <- ncol(B)
  if (any(w < 0 | w >= V)) {
    stop(sprintf("lda: a word index is outside the vocabulary of %d", V))
  }
  N <- length(w)
  if (N < 1) stop("lda: the document is empty")
  if (length(alpha) == 1) {
    a <- rep(as.numeric(alpha), K)
  } else {
    a <- as.numeric(alpha)
  }
  if (length(a) != K) {
    stop(sprintf("lda: alpha has %d entries for %d topics", length(a), K))
  }
  if (any(a <= 0.0)) stop("lda: alpha must be strictly positive")
  phi <- matrix(1.0 / K, nrow=N, ncol=K)
  gam <- a + N / as.numeric(K)
  it <- 0
  conv <- FALSE
  for (i in seq_len(iters)) {
    it <- i
    elog <- .morie_lda_e_log_theta(gam)
    new_phi <- matrix(0, nrow=N, ncol=K)
    for (n in seq_len(N)) {
      row <- B[, w[n]] * exp(elog)
      z <- sum(row)
      if (z <= .morie_lda_eps) {
        stop(sprintf("lda: word %d has zero probability under every topic", w[n]))
      }
      new_phi[n, ] <- row / z
    }
    ng <- a + colSums(new_phi)
    delta <- max(abs(ng - gam))
    phi <- new_phi
    gam <- ng
    if (delta < as.numeric(tol)) {
      conv <- TRUE
      break
    }
  }
  list(phi=phi, gamma=gam, iterations=it, converged=conv, K=K, N=N,
       topic_proportions=gam/sum(gam))
}

.morie_lda_elbo <- function(doc, alpha, beta, phi, gamma) {
  w <- as.integer(doc)
  B <- as.matrix(beta)
  storage.mode(B) <- "double"
  K <- nrow(B)
  N <- length(w)
  if (length(alpha) == 1) {
    a <- rep(as.numeric(alpha), K)
  } else {
    a <- as.numeric(alpha)
  }
  g <- as.numeric(gamma)
  elog <- .morie_lda_e_log_theta(g)
  val <- k.lgamma(sum(a)) - sum(k.lgamma(a))
  val <- val + sum((a - 1.0) * elog)
  for (n in seq_len(N)) {
    for (i in seq_len(K)) {
      p <- phi[n, i]
      if (p <= .morie_lda_eps) next
      val <- val + p * elog[i]
      val <- val + p * log(max(B[i, w[n]], .morie_lda_eps))
      val <- val - p * log(p)
    }
  }
  val <- val - (k.lgamma(sum(g)) - sum(k.lgamma(g)))
  val <- val - sum((g - 1.0) * elog)
  return(val)
}

.morie_lda_variational_em <- function(docs, K, V, alpha=0.1, iters=30, inner=50,
                                      seed=0, tol=1e-6) {
  D <- lapply(docs, as.integer)
  if (length(D) == 0) stop("lda: no documents given")
  if (as.integer(K) < 1 || as.integer(V) < 1) stop("lda: K and V must be at least 1")
  rng <- .ghc_rng(seed)
  K <- as.integer(K)
  V <- as.integer(V)
  B <- matrix(0, nrow=K, ncol=V)
  for (kk in seq_len(K)) {
    u <- .ghc_unif(rng, V)
    row <- 0.1 + u
    z <- sum(row)
    B[kk, ] <- row / z
  }
  hist <- c()
  prev <- NULL
  for (it in seq_len(iters)) {
    counts <- matrix(.morie_lda_eps, nrow=K, ncol=V)
    total <- 0.0
    for (d in D) {
      if (length(d) == 0) next
      r <- .morie_lda_variational_inference(d, alpha, B, iters=inner)
      total <- total + .morie_lda_elbo(d, alpha, B, r$phi, r$gamma)
      for (n in seq_along(d)) {
        wn <- d[n]
        for (i in seq_len(K)) {
          counts[i, wn] <- counts[i, wn] + r$phi[n, i]
        }
      }
    }
    for (i in seq_len(K)) {
      z <- sum(counts[i, ])
      B[i, ] <- counts[i, ] / z
    }
    hist <- c(hist, total)
    if (!is.null(prev) && abs(total - prev) < as.numeric(tol)) break
    prev <- total
  }
  list(
    estimate=B, beta=B, elbo_history=hist,
    final_elbo=if (length(hist) > 0) hist[length(hist)] else NaN,
    K=K, V=V, n_docs=length(D),
    iterations=length(hist),
    method="variational EM; Blei, Ng & Jordan (2003) Sec. 5.1, eqs. (6)-(8)"
  )
}

.morie_lda_topic_words <- function(beta, n_top=5, vocab=NULL) {
  B <- as.matrix(beta)
  storage.mode(B) <- "double"
  out <- list()
  for (i in seq_len(nrow(B))) {
    idx <- order(B[i, ], decreasing=TRUE)[seq_len(min(as.integer(n_top), ncol(B)))]
    out[[i]] <- lapply(idx, function(j) {
      if (is.null(vocab)) list(j, B[i, j])
      else list(vocab[j], B[i, j])
    })
  }
  return(out)
}

.morie_lda_cheatsheet <- function() {
  paste(c(
    "lda: theta ~ Dir(alpha), z_n ~ Mult(theta), w_n ~ ",
    "p(.|z_n, beta). The posterior is intractable because ",
    "theta and z couple through beta, so DELETE those edges ",
    "and fit the wreckage by minimising KL (eq. 5). Fixed ",
    "point: phi_ni prop beta_{i,w_n} exp(E_q[log theta_i]), ",
    "gamma_i = alpha_i + sum_n phi_ni, with E_q[log theta_i] ",
    "= Psi(gamma_i) - Psi(sum gamma). Note it is exp(E[log]) ",
    "not E[.] -- smaller by Jensen, which is why variational ",
    "inference under-weights rare topics. The bound only ",
    "rises."), collapse="")
}

# public names resolved by fn/_lazy_map.json
morie_lda_e_log_theta <- .morie_lda_e_log_theta
morie_lda_variational_inference <- .morie_lda_variational_inference
morie_lda_elbo <- .morie_lda_elbo
morie_lda_variational_em <- .morie_lda_variational_em
morie_lda_topic_words <- .morie_lda_topic_words
morie_lda_cheatsheet <- .morie_lda_cheatsheet

# compact alias per ledger/NAMING.md
morie_lda_latentdirichlet <- .morie_lda_variational_em
morie_lda_lda_topic <- .morie_lda_variational_em
morie_lda_ldatopic <- .morie_lda_variational_em

# main entry point
morie_lda <- .morie_lda_variational_em
