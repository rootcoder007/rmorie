% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prsLR}
\alias{morie_prsLR}
\title{morie_prsLR}
\usage{
morie_prsLR(grammar_, tokens, method = "lr1")
}
\arguments{
\item{grammar_}{Passed to \code{.prsLR_grammar}.}
\item{tokens}{Passed to \code{.prsLR_parse}.}
\item{method}{Passed to \code{.prsLR_build_tables}. Defaults to \code{"lr1"}.}
}
\description{
A step of the prsLR_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{tree}, \code{method}, \code{n_states}, \code{conflicts}, \code{tokens}, \code{yield}.
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
