% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t4_kendalltaub}
\alias{.t4_kendalltaub}
\title{Kendall tau-b and the normal-approximation z, matching}
\usage{
.t4_kendalltaub(x, y)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{y}{Passed to \code{.t4_kendallS}.}
}
\description{
stats::cor.test(method = "kendall", exact = FALSE).
}
\value{
A list with \code{tau}, \code{z}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.t4_kendalltaub(V, V)
}
