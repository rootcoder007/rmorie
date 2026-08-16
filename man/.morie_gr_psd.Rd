% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_psd}
\alias{.morie_gr_psd}
\title{.morie_gr_psd}
\usage{
.morie_gr_psd(x)
}
\arguments{
\item{x}{Passed to \code{.morie_gr_pvar}.}
}
\description{
A step of the geron_ml_native implementation. Called by \code{morie_geron_credit_assignment}, \code{morie_geron_dcgan_generator}, \code{morie_geron_randomized_search_cv}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_gr_psd(V)
}
