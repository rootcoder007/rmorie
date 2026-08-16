% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_need}
\alias{.morie_gr_need}
\title{.morie_gr_need}
\usage{
.morie_gr_need(cond, msg)
}
\arguments{
\item{cond}{A flag; the body branches on it.}
\item{msg}{Passed to \code{stop}.}
}
\description{
A step of the geron_ml_native implementation. Called by \code{.morie_gr_attend}, \code{.morie_gr_check_mdp}, \code{.morie_gr_conv2d_valid} and 396 others in the module. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
.morie_gr_need(cond = 5L, msg = c(1, 2, 3, 4, 5, 6, 7, 8))
}
