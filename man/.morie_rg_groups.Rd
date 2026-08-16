% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_groups}
\alias{.morie_rg_groups}
\title{Split rows by label, preserving first-seen order}
\usage{
.morie_rg_groups(X, y)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y}{Passed to \code{unique}.}
}
\description{
A step of the rangayyan_class implementation. Called by \code{FishLda}, \code{Qda}, \code{SepIndex}. See the file header for the source the module follows.
}
\value{
A list with \code{order}, \code{groups}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.morie_rg_groups(D, V)
}
