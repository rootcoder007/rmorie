% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ripley_l_function}
\alias{morie_ripley_l_function}
\title{morie_ripley_l_function}
\usage{
morie_ripley_l_function(points, window = NULL, r = NULL, correction = "border") 
}
\description{
morie_ripley_l_function
}
\arguments{
\item{points}{Argument \code{points}; see Usage.}
\item{window}{Argument \code{window}; see Usage.}
\item{r}{Argument \code{r}; see Usage.}
\item{correction}{Argument \code{correction}; see Usage.}
}
\value{
A list with \code{r}, \code{l}, \code{l_uncentred}, \code{k}, \code{lambda_est}, \code{method}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
rmorie:::morie_ripley_l_function(M)
}
