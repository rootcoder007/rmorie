% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.prsLL_first_seq}
\alias{.prsLL_first_seq}
\title{.prsLL_first_seq}
\usage{
.prsLL_first_seq(seq, first, nts)
}
\arguments{
\item{seq}{See Usage.}
\item{first}{A vector; indexed elementwise.}
\item{nts}{Passed to \code{\%in\%}.}
}
\description{
A step of the prsLL_native implementation. Called by \code{.prsLL_first_of}, \code{.prsLL_first_sets}, \code{.prsLL_follow_sets} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
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
\examples{
.prsLL_first_seq(seq = c(1, 2, 3, 4, 5, 6, 7, 8), first = c(1, 2, 3, 4, 5, 6, 7, 8), nts = c(1, 2, 3, 4, 5, 6, 7, 8))
}
