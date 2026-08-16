% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.med_effects}
\alias{.med_effects}
\title{Valeri and VanderWeele (2013), eq. (0.3), and its two mirror images}
\usage{
.med_effects(beta, theta, cbar, a, astar)
}
\arguments{
\item{beta}{A vector; its length is taken and its elements indexed.}
\item{theta}{A vector; indexed elementwise.}
\item{cbar}{Numeric; combined arithmetically in the body.}
\item{a}{Numeric; combined arithmetically in the body.}
\item{astar}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the ctde implementation. Called by \code{Ctde}, \code{Pnie}, \code{Tnie}. See the file header for the source the module follows.
}
\value{
A list with \code{pnde}, \code{tnde}, \code{tnie}, \code{pnie}, \code{te}, \code{mediated_interaction}, \code{beta}, \code{theta}.
}
\keyword{internal}
