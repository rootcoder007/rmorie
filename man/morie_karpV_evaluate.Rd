% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_karpV_evaluate}
\alias{morie_karpV_evaluate}
\title{Evaluate an expression tree}
\usage{
morie_karpV_evaluate(node, env) 
}
\arguments{
\item{node}{the tree.}
\item{env}{named list of variable values.}
}
\description{
Evaluate an expression tree
}
\value{
the numeric value.
}
\keyword{internal}
\examples{
z <- .karpv_fnode("\%", list(.karpv_tnode(1), .karpv_tnode(0)))
morie_karpV_evaluate(z, list())
}
