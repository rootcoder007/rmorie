% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_rank_first}
\alias{.s4_rank_first}
\title{.s4_rank_first}
\usage{
.s4_rank_first(x)
}
\arguments{
\item{x}{A vector; its length is taken.}
}
\description{
A step of the s04core implementation. Called by \code{Matrim}. See the file header for the source the module follows.
}
\value{
The value of \code{r}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s4_rank_first(V)
}
