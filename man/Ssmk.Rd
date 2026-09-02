% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ssmk}
\alias{Ssmk}
\title{Structured state-space convolution kernel (Gu, Goel & Re 2022, S4)}
\usage{
Ssmk(A, B, C, L) 
}
\description{
Structured state-space convolution kernel (Gu, Goel & Re 2022, S4)
}
\arguments{
\item{A}{Argument \code{A}; see Usage.}
\item{B}{Argument \code{B}; see Usage.}
\item{C}{Argument \code{C}; see Usage.}
\item{L}{Argument \code{L}; see Usage.}
}
\value{
A list with \code{K}, \code{L}, \code{state_dim}.
}
\keyword{internal}
\examples{
Ssmk(A = c(1, 2, 3, 4, 5, 6, 7, 8), B = c(1, 2, 3, 4, 5, 6, 7, 8), C = c(1, 2, 3, 4, 5, 6, 7, 8), L = 5L)
}
