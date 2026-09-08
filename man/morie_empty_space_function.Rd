% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_empty_space_function}
\alias{morie_empty_space_function}
\title{morie_empty_space_function}
\usage{
morie_empty_space_function(coords, r_grid = NULL, window = NULL, n_grid = 40) 
}
\description{
morie_empty_space_function
}
\arguments{
\item{coords}{Argument \code{coords}; see Usage.}
\item{r_grid}{Argument \code{r_grid}; see Usage.}
\item{window}{Argument \code{window}; see Usage.}
\item{n_grid}{Argument \code{n_grid}; see Usage.}
}
\value{
A list with \code{r}, \code{f}, \code{f_csr}, \code{empty_space_distances}, \code{lambda_est}, \code{n_sample}, \code{method}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
rmorie:::morie_empty_space_function(M)
}
