% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_locdp}
\alias{morie_locdp}
\title{Returns:}
\usage{
morie_locdp(x, epsilon = 1, seed = NULL) 
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{epsilon}{A vector; its length is taken. Defaults to \code{1}.}
\item{seed}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
Named list (RichResult payload) with components: y : numeric vector, randomized responses (same length as x) epsilon : numeric, the privacy parameter used p : numeric, the flip probability 1 / (1 + exp(epsilon)) n : integer, length of x rng : the RNG state returned by .ghc_rng / .ghc_unif
}
\value{
A list with \code{y}, \code{epsilon}, \code{p}, \code{n}, \code{rng}.
}
\references{
----------
Kasiviswanathan, S. P., Lee, H. K., Nissim, K., Raskhodnikova, S., &
    Smith, A. (2011). What can we learn privately? *SIAM Journal on
    Computing*, 40(3), 793-826. (Local model, section 1; randomized
    response as the basic local protocol.)
Warner, S. L. (1965). Randomized response: a survey technique for
    eliminating evasive answer bias. *JASA*, 60(309), 63-69.
Dwork, C., & Roth, A. (2014). *FnT-TCS*, 9(3-4), section 3.2.
    Local source: /run/media/rootcoder/WD_BLACK/library/pdf/fetched-wave3/dwork-roth-2014-algorithmic-foundations-differential-privacy.pdf
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_locdp(V)
}
