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
\item{evidence}{A vector; indexed elementwise.}
\item{equations}{See Usage.}
\item{exogenous}{A vector; its length is taken.}
\item{do}{A vector; indexed elementwise.}
\item{query}{Carried through into a list the body builds.}
\item{u_support}{The body requires: candidates is too large; pass a smaller u_support.}
}
\description{
A step of the counterfactual implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{counterfactual}, \code{factual}, \code{abducted}, \code{n_compatible_u}, \code{counterfactual_unique}, \code{residual}, \code{do}, \code{query}, \code{method}.
}
\keyword{internal}
\examples{
.pearl_eqs <- list(x = list(parents = "u1", fn = function(u1) u1), 
    y = list(parents = c("x", "u2"), fn = function(x, u2) if (abs(x - 
        u2) < 0.5) 1 else 0))
Counterfactual(evidence = list(x = 1, y = 1), equations = .pearl_eqs, 
    exogenous = c("u1", "u2"), do = list(x = 0), query = "y")
}
