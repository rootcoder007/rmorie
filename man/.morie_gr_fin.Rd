% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_fin}
\alias{.morie_gr_fin}
\title{.morie_gr_fin}
\usage{
.morie_gr_fin(x, name)
}
\arguments{
\item{x}{Passed to \code{is.finite}.}
\item{name}{Passed to \code{paste0}.}
}
\description{
A step of the geron_train_native implementation. Called by \code{.morie_gr_attend}, \code{.morie_gr_check_mdp}, \code{.morie_gr_mse_core} and 105 others in the module. See the file header for the source the module follows.
}
\value{
Invisibly,a logical value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_gr_fin(V, V)
}
