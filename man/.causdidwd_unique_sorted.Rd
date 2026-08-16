% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causdidwd_unique_sorted}
\alias{.causdidwd_unique_sorted}
\title{.causdidwd_unique_sorted}
\usage{
.causdidwd_unique_sorted(v)
}
\arguments{
\item{v}{See Usage.}
}
\description{
A step of the causdidwd_native implementation. Called by \code{morie_etwfe}, \code{morie_imputation}, \code{morie_two_way_fixed_effects}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.causdidwd_unique_sorted(V)
}
