% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.prsLR_parse}
\alias{.prsLR_parse}
\title{.prsLR_parse}
\usage{
.prsLR_parse(g, tokens, method, tables)
}
\arguments{
\item{g}{Passed to \code{.prsLR_build_tables}.}
\item{tokens}{Coerced to character by the body, with \code{as.character}.}
\item{method}{Passed to \code{.prsLR_build_tables}.}
\item{tables}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the prsLR_native implementation. Called by \code{morie_parse}, \code{morie_prsLR}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\references{
----------
Knuth, D. E. (1965) "On the translation of languages from left to
right", Information and Control 8(6), 607-639,
doi:10.1016/S0019-9958(65)90426-2.

DeRemer, F. L. (1971) "Simple LR(k) grammars", Communications of the
ACM 14(7), 453-460, doi:10.1145/362619.362625.
}
\keyword{internal}
