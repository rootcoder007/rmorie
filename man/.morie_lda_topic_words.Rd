% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_lda_topic_words}
\alias{.morie_lda_topic_words}
\title{.morie_lda_topic_words}
\usage{
.morie_lda_topic_words(beta, n_top = 5, vocab = NULL)
}
\arguments{
\item{beta}{A matrix; passed to \code{as.matrix}.}
\item{n_top}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5}.}
\item{vocab}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
}
\description{
A step of the lda_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_lda_topic_words(V)
}
