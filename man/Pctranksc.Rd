% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Pctranksc}
\alias{Pctranksc}
\title{Percentile modified rank test for scale -- Sec. 9.6, p. 323}
\usage{
Pctranksc(x, y, s = 0.5, r = NULL) 
}
\description{
Percentile modified rank test for scale -- Sec. 9.6, p. 323
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
\item{s}{Argument \code{s}; see Usage.}
\item{r}{Argument \code{r}; see Usage.}
}
\value{
A list with \code{statistic}, \code{tupper}, \code{blower}, \code{mean}, \code{var}, \code{mean_book}, \code{var_book}, \code{z}, \code{p_value}, \code{S}, \code{R}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Pctranksc(V, V)
}
