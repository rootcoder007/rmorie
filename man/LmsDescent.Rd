% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LmsDescent}
\alias{LmsDescent}
\title{Eqs. (3.201)-(3.202): the instantaneous gradient is -2 e r, so}
\usage{
LmsDescent(w, e, rvec, mu)
}
\arguments{
\item{w}{See Usage.}
\item{e}{See Usage.}
\item{rvec}{See Usage.}
\item{mu}{See Usage.}
}
\description{
steepest descent gives w - mu grad, which is exactly Widrow-Hoff.
}
\value{
A list with \code{gradient}, \code{w_next}, \code{mu}, \code{e}, \code{order}, \code{equals_widrow_hoff}, \code{method}.
}
\keyword{internal}
