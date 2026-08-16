% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Counterfactual}
\alias{Counterfactual}
\title{Counterfactual}
\usage{
Counterfactual(evidence, equations, exogenous, do, query, u_support = NULL)
}
\arguments{
\item{evidence}{See Usage.}
\item{equations}{See Usage.}
\item{exogenous}{See Usage.}
\item{do}{See Usage.}
\item{query}{See Usage.}
\item{u_support}{Defaults to \code{NULL}.}
}
\description{
Part of the counterfactual implementation; see the file header for the source it follows.
}
\value{
A list with \code{counterfactual}, \code{factual}, \code{abducted}, \code{n_compatible_u}, \code{counterfactual_unique}, \code{residual}, \code{do}, \code{query}, \code{method}.
}
\keyword{internal}
