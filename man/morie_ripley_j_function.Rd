% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ripley_j_function}
\alias{morie_ripley_j_function}
\title{morie_ripley_j_function}
\usage{
morie_ripley_j_function(points, window = NULL, r = NULL, n_grid = 40) 
}
\description{
morie_ripley_j_function
}
\arguments{
\item{points}{Argument \code{points}; see Usage.}
\item{window}{Argument \code{window}; see Usage.}
\item{r}{Argument \code{r}; see Usage.}
\item{n_grid}{Argument \code{n_grid}; see Usage.}
}
\value{
A list with \code{r}, \code{j}, \code{g}, \code{f}, \code{j_csr}, \code{lambda_est}, \code{n_defined}, \code{method}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
rmorie:::morie_ripley_j_function(M)
}
