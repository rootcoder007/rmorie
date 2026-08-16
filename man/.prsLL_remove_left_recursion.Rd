% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.prsLL_remove_left_recursion}
\alias{.prsLL_remove_left_recursion}
\title{.prsLL_remove_left_recursion}
\usage{
.prsLL_remove_left_recursion(g)
}
\arguments{
\item{g}{A list; the body reads \code{$rules}, \code{$start} from it.}
}
\description{
A step of the prsLL_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.prsLL_grammar}.
}
\references{
----------
Knuth, D. E. (1971) "Top-down syntax analysis", Acta Informatica
1(2), 79-110, doi:10.1007/BF00289517. Top-down (LL) analysis, the
role of one-symbol lookahead, and the failure of top-down methods on
left-recursive grammars.

Knuth, D. E. (1965) "On the translation of languages from left to
right", Information and Control 8(6), 607-639,
doi:10.1016/S0019-9958(65)90426-2, for the LR(k) classes against
which the LL(1) restriction is measured.
}
\keyword{internal}
