% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Anc}
\alias{Anc}
\title{Anc}
\usage{
Anc(primary, reference, order = 8, mu = 0.01, method = "lms", lam = 0.98, delta = 1)
}
\arguments{
\item{primary}{See Usage.}
\item{reference}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{order}{Defaults to \code{8}.}
\item{mu}{Defaults to \code{0.01}.}
\item{method}{One of \code{"lms"}, \code{"rls"}. Defaults to \code{"lms"}.}
\item{lam}{Defaults to \code{0.98}.}
\item{delta}{Defaults to \code{1}.}
}
\description{
A step of the rangayyan_adapt implementation. Called by \code{FetalEcg}. See the file header for the source the module follows.
}
\value{
The value of \code{r}, as built in the body.
}
\keyword{internal}
