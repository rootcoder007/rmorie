% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_karpV_raw_fitness}
\alias{morie_karpV_raw_fitness}
\title{Raw fitness: sum of absolute errors over the fitness cases}
\usage{
morie_karpV_raw_fitness(node, cases, terminals) 
}
\arguments{
\item{node}{the tree.}
\item{cases}{list of list(inputs, target).}
\item{terminals}{variable names, matched positionally to inputs.}
}
\description{
Raw fitness: sum of absolute errors over the fitness cases
}
\value{
the summed absolute error, or Inf if the tree misbehaves.
}
\keyword{internal}
\examples{
cases_for <- function(f, lo = -1, hi = 1, n = 21) lapply(0:(n - 
    1), function(i) {
    x <- lo + (hi - lo) * i/(n - 1)
    list(x, f(x))
})
QUAD <- cases_for(function(x) x * x + x + 1)
t <- .karpv_fnode("+", list(.karpv_fnode("+", list(.karpv_fnode("*", 
    list(.karpv_tnode("x"), .karpv_tnode("x"))), .karpv_tnode("x"))), 
    .karpv_tnode(1)))
morie_karpV_raw_fitness(t, QUAD, "x")
}
