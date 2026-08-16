% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_parse}
\alias{morie_parse}
\title{morie_parse}
\usage{
morie_parse(g, tokens, method = "lr1", tables = NULL)
}
\arguments{
\item{g}{Passed to \code{.prsLR_parse}.}
\item{tokens}{Passed to \code{.prsLR_parse}.}
\item{method}{Passed to \code{.prsLR_parse}. Defaults to \code{"lr1"}.}
\item{tables}{Passed to \code{.prsLR_parse}.}
}
\description{
A step of the prsLR_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.prsLR_parse}.
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
\examples{
g <- list(rules = list(list("E", list("E", "+", "T")),
                       list("E", list("T")),
                       list("T", list("id"))),
          start = "E")
r <- morie_parse(g, c("id", "+", "id"))
str(r, max.level = 1)
}
