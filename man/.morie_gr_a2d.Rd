% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_a2d}
\alias{.morie_gr_a2d}
\title{Np.atleast_2d: a vector becomes a ONE-ROW matrix}
\usage{
.morie_gr_a2d(x)
}
\arguments{
\item{x}{A matrix; passed to \code{dim}.}
}
\description{
A step of the geron_train_native implementation. Called by \code{.morie_gr_attend}, \code{.morie_gr_check_mdp}, \code{.morie_gr_mse_core} and 71 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{m}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_gr_a2d(V)
}
