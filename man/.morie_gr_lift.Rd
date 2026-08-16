% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_lift}
\alias{.morie_gr_lift}
\title{.morie_gr_lift}
\usage{
.morie_gr_lift(o)
}
\arguments{
\item{o}{Passed to \code{morie_gr_dual}.}
}
\description{
A step of the geron_train_native implementation. Called by \code{Ops.morie_gr_dual}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_gr_lift(V)
}
