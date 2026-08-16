% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tl1step_as_numvec}
\alias{.tl1step_as_numvec}
\title{.tl1step_as_numvec}
\usage{
.tl1step_as_numvec(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
A step of the tl1step_native implementation. Called by \code{.tl1step_build_ulfm}, \code{.tl1step_is_universal}, \code{.tl1step_iterative_tmle} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\references{
----------
van der Laan, M. J. & Rose, S. (2018) *Targeted Learning in Data
Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 5
(one-dimensional universal least favorable parametric submodels for
univariate, multivariate and infinite-dimensional target parameters;
the definition by which the score at every epsilon equals the
canonical gradient; the resulting one-step TMLE solving the efficient
influence curve equation without iteration; the reading of the
universal submodel as a shortest path achieving the desired bias
reduction; the argument that the iterative TMLE can be unstable when
the data provide sparse information about the target; and the worked
treatment-specific survival example).

van der Laan, M. J. & Gruber, S. (2016) "One-step targeted minimum
loss-based estimation based on universal least favorable
one-dimensional submodels", *International Journal of Biostatistics*
12(1), 351-378, doi:10.1515/ijb-2015-0054. The construction this
chapter relies on.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.tl1step_as_numvec(V)
}
