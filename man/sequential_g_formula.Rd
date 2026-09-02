% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sequential_g_formula}
\alias{sequential_g_formula}
\title{sequential_g_formula}
\usage{
sequential_g_formula(Q_functions, L_supports, L_probs, rule) 
}
\arguments{
\item{Q_functions}{A vector; indexed elementwise.}
\item{L_supports}{A vector; its length is taken and its elements indexed.}
\item{L_probs}{A vector; its length is taken and its elements indexed.}
\item{rule}{Accepted by the signature and not used anywhere in the body.}
}
\description{
A step of the tlgcmp_native implementation. Called by \code{morie_tlgcmp}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{psi}, \code{horizon}, \code{method}, \code{note}, \code{assumptions}.
}
\keyword{internal}
