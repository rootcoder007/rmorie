% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Kctrlasymp}
\alias{Kctrlasymp}
\title{Control-vector covariance -- Theorem 10.7.1, p. 375}
\usage{
Kctrlasymp(lam, dens, pval) 
}
\description{
Control-vector covariance -- Theorem 10.7.1, p. 375
}
\arguments{
\item{lam}{Argument \code{lam}; see Usage.}
\item{dens}{Argument \code{dens}; see Usage.}
\item{pval}{Argument \code{pval}; see Usage.}
}
\value{
A list with \code{sigma}, \code{q}, \code{p}, \code{k}.
}
\keyword{internal}
\examples{
rmorie:::Kctrlasymp(lam = c(0.4, 0.3, 0.3), dens = c(0.39, 0.35, 0.30),
           pval = c(0.5, 0.55, 0.6))
}
