% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{KrigFDA}
\alias{KrigFDA}
\title{KrigFDA}
\usage{
KrigFDA(coords, values, new_coords, model = "exponential", nugget = 0, 
    sill = 1, range_ = 1, trend_order = 1) 
}
\description{
KrigFDA
}
\arguments{
\item{coords}{Argument \code{coords}; see Usage.}
\item{values}{Argument \code{values}; see Usage.}
\item{new_coords}{Argument \code{new_coords}; see Usage.}
\item{model}{Argument \code{model}; see Usage.}
\item{nugget}{Argument \code{nugget}; see Usage.}
\item{sill}{Argument \code{sill}; see Usage.}
\item{range_}{Argument \code{range_}; see Usage.}
\item{trend_order}{Argument \code{trend_order}; see Usage.}
}
\value{
A list with \code{estimate}, \code{se}, \code{beta}, \code{residuals}, \code{weights}, \code{n}, \code{p}, \code{method}.
}
\keyword{internal}
\examples{
rmorie:::KrigFDA(coords = c(1, 2, 3, 4, 5, 6, 7, 8), values = c(1, 2, 3, 4, 5, 6, 7, 8), new_coords = c(1, 2, 3, 4, 5, 6, 7, 8))
}
