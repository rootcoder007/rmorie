% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.safrl_finish}
\alias{.safrl_finish}
\title{.safrl_finish}
\usage{
.safrl_finish(step, g, cols, cv, H, delta, lam, nu, feasible, recovery)
}
\arguments{
\item{step}{A vector; indexed elementwise.}
\item{g}{A vector; its length is taken and its elements indexed.}
\item{cols}{A vector; its length is taken and its elements indexed.}
\item{cv}{A vector; indexed elementwise.}
\item{H}{A vector; indexed elementwise.}
\item{delta}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{lam}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{nu}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{feasible}{Coerced to logical by the body, with \code{as.logical}.}
\item{recovery}{Coerced to logical by the body, with \code{as.logical}.}
}
\description{
A step of the safrl_native implementation. Called by \code{morie_safrl}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{step}, \code{lambda_}, \code{nu}, \code{feasible}, \code{recovery}, \code{predicted_gain}, \code{predicted_violation}, \code{kl}, \code{delta}, \code{method}.
}
\keyword{internal}
