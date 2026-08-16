% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tl1step_cheatsheet}
\alias{.tl1step_cheatsheet}
\title{.tl1step_cheatsheet}
\usage{
.tl1step_cheatsheet()
}
\description{
A step of the tl1step_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A character value.
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
.tl1step_cheatsheet()
}
