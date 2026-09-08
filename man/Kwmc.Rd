% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Kwmc}
\alias{Kwmc}
\title{Kruskal-Wallis multiple comparisons -- eq. (10.4.8), p. 357}
\usage{
Kwmc(rank_means, ns, alpha = 0.2) 
}
\description{
Kruskal-Wallis multiple comparisons -- eq. (10.4.8), p. 357
}
\arguments{
\item{rank_means}{Argument \code{rank_means}; see Usage.}
\item{ns}{Argument \code{ns}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
}
\value{
A list with \code{bound}, \code{bounds}, \code{diffs}, \code{significant}, \code{zstar}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Kwmc(V, V)
}
