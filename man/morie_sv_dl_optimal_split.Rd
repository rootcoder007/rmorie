% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sv_dl_optimal_split}
\alias{morie_sv_dl_optimal_split}
\title{Argmax_\{i<j\} f_i + r_j -- the split with a microinsertion gap}
\usage{
morie_sv_dl_optimal_split(f, r) 
}
\arguments{
\item{f}{A vector; its length is taken and its elements indexed.}
\item{r}{A vector; its length is taken and its elements indexed.}
}
\description{
Indices are 1-based over the consensus, as in the paper.
}
\value{
A list with \code{i}, \code{j}, \code{score}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_sv_dl_optimal_split(V, V)
}
