% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_karpV_ramped}
\alias{morie_karpV_ramped}
\title{Ramped half-and-half initialisation}
\usage{
morie_karpV_ramped(e, n, functions, terminals, erc, max_depth)
}
\arguments{
\item{e}{the generator, from the module's own xorshift.}
\item{n}{population size.}
\item{functions}{list of name and arity pairs.}
\item{terminals}{character vector of variable names.}
\item{erc}{length-two range for ephemeral constants, or NULL.}
\item{max_depth}{deepest initial tree.}
}
\description{
Ramped half-and-half initialisation
}
\value{
a list of trees with a spread of depths and shapes.
}
\keyword{internal}
\examples{
morie_karpV_ramped(.karpv_rng(3), 60L, .KARPV_FUNCTIONS, "x", 
    c(-5, 5), 6L)
}
