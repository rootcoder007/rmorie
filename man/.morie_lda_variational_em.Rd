% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_lda_variational_em}
\alias{.morie_lda_variational_em}
\title{.morie_lda_variational_em}
\usage{
.morie_lda_variational_em(docs, K, V, alpha = 0.1, iters = 30, inner = 50, seed = 0, tol = 1e-06)
}
\arguments{
\item{docs}{See Usage.}
\item{K}{See Usage.}
\item{V}{See Usage.}
\item{alpha}{Defaults to \code{0.1}.}
\item{iters}{Defaults to \code{30}.}
\item{inner}{Defaults to \code{50}.}
\item{seed}{Defaults to \code{0}.}
\item{tol}{Defaults to \code{1e-06}.}
}
\description{
Part of the lda_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{beta}, \code{elbo_history}, \code{final_elbo}, \code{K}, \code{V}, \code{n_docs}, \code{iterations}, \code{method}.
}
\references{
----------
Blei, D. M., Ng, A. Y. & Jordan, M. I. (2003) "Latent Dirichlet
Allocation", Journal of Machine Learning Research 3, 993-1022.
Sec. 3 (the generative process, the k x V matrix beta, and the
ancillary role of N), Sec. 5.1 (the variational family obtained by
dropping edges, eq. (5) as a KL minimisation, the fixed-point updates
of eqs. (6)-(7), the digamma expression of eq. (8), and the
interpretation of both updates), and the variational inference
algorithm initialising phi at 1/k and gamma at alpha + N/k.

Hofmann, T. (1999) "Probabilistic Latent Semantic Analysis",
Proceedings of the Fifteenth Conference on Uncertainty in Artificial
Intelligence (UAI 1999), 289-296, arXiv:1301.6705. The aspect model
LDA places a Dirichlet prior over; implemented in plsa.
}
\keyword{internal}
