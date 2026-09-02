% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ripley_l}
\alias{morie_ripley_l}
\title{morie_ripley_l}
\usage{
morie_ripley_l(coords, r_grid = NULL, correction = "border") 
}
\description{
morie_ripley_l
}
\arguments{
\item{coords}{Argument \code{coords}; see Usage.}
\item{r_grid}{Argument \code{r_grid}; see Usage.}
\item{correction}{Argument \code{correction}; see Usage.}
}
\value{
A list with \code{r}, \code{l}, \code{l_uncentred}, \code{k}, \code{lambda_est}, \code{method}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
morie_ripley_l(M)
}
