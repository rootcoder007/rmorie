% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_karpV_to_string}
\alias{morie_karpV_to_string}
\title{Print an expression tree}
\usage{
morie_karpV_to_string(node) 
}
\arguments{
\item{node}{the tree.}
}
\description{
Print an expression tree
}
\value{
a prefix string.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_karpV_to_string(D)
}
