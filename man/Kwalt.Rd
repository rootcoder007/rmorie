% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Kwalt}
\alias{Kwalt}
\title{Kruskal-Wallis defining form -- eqs. (10.4.2)/(10.4.7), pp. 354, 357}
\usage{
Kwalt(rank_sums, ns) 
}
\description{
Kruskal-Wallis defining form -- eqs. (10.4.2)/(10.4.7), pp. 354, 357
}
\arguments{
\item{rank_sums}{Argument \code{rank_sums}; see Usage.}
\item{ns}{Argument \code{ns}; see Usage.}
}
\value{
A list with \code{statistic}, \code{h_computing}, \code{resid}, \code{df}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Kwalt(V, V)
}
