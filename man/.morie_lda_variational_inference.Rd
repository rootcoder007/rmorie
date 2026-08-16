% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_lda_variational_inference}
\alias{.morie_lda_variational_inference}
\title{.morie_lda_variational_inference}
\usage{
.morie_lda_variational_inference(doc, alpha, beta, iters = 100, tol = 1e-08)
}
\arguments{
\item{doc}{Coerced to integer by the body, with \code{as.integer}.}
\item{alpha}{A vector; its length is taken.}
\item{beta}{A matrix; passed to \code{as.matrix}.}
\item{iters}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{100}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-08}.}
}
\description{
A step of the lda_native implementation. Called by \code{.morie_lda_variational_em}. See the file header for the source the module follows.
}
\value{
A list with \code{phi}, \code{gamma}, \code{iterations}, \code{converged}, \code{K}, \code{N}, \code{topic_proportions}.
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
