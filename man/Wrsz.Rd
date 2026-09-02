% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Wrsz}
\alias{Wrsz}
\title{Rank-sum normal approximation -- Sec. 8.2, p. 291}
\usage{
Wrsz(w, m, n, alternative = "two-sided", correct = FALSE, ties = NULL) 
}
\description{
Rank-sum normal approximation -- Sec. 8.2, p. 291
}
\arguments{
\item{w}{Argument \code{w}; see Usage.}
\item{m}{Argument \code{m}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
\item{alternative}{Argument \code{alternative}; see Usage.}
\item{correct}{Argument \code{correct}; see Usage.}
\item{ties}{Argument \code{ties}; see Usage.}
}
\value{
A list with \code{z}, \code{p_value}, \code{mean}, \code{var}, \code{var_uncorrected}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
Wrsz(w = c(1, 2, 3, 4, 5, 6, 7, 8), m = 5L, n = 5L)
}
