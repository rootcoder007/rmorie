% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MAF}
\alias{MAF}
\title{Build a Masked Autoregressive Flow}
\usage{
MAF(dim_x, dim_t, n_layers = 5L, hidden = 20L, seed = 0L) 
}
\description{
Build a Masked Autoregressive Flow
}
\arguments{
\item{dim_x}{Argument \code{dim_x}; see Usage.}
\item{dim_t}{Argument \code{dim_t}; see Usage.}
\item{n_layers}{Argument \code{n_layers}; see Usage.}
\item{hidden}{Argument \code{hidden}; see Usage.}
\item{seed}{Argument \code{seed}; see Usage.}
}
\value{
A list with \code{layers}, \code{dim_x}, \code{dim_t}.
}
\keyword{internal}
\examples{
MAF(dim_x = 5L, dim_t = 5L)
}
