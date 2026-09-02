% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Jtstat}
\alias{Jtstat}
\title{Jonckheere-Terpstra B -- Sec. 10.6, eqs. (10.6.2)-(10.6.3), p. 365}
\usage{
Jtstat(samples, alternative = "greater") 
}
\description{
Jonckheere-Terpstra B -- Sec. 10.6, eqs. (10.6.2)-(10.6.3), p. 365
}
\arguments{
\item{samples}{Argument \code{samples}; see Usage.}
\item{alternative}{Argument \code{alternative}; see Usage.}
}
\value{
A list with \code{statistic}, \code{mean}, \code{var}, \code{z}, \code{p_value}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Jtstat(V)
}
