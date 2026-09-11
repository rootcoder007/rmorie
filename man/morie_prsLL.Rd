% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prsLL}
\alias{morie_prsLL}
\title{Entry point -----}
\usage{
morie_prsLL(grammar_, tokens, route = "table") 
}
\arguments{
\item{grammar_}{A list; the body reads \code{$rules}, \code{$start} from it.}
\item{tokens}{Passed to \code{.prsLL_parse}.}
\item{route}{Passed to \code{.prsLL_parse}. Defaults to \code{"table"}.}
}
\description{
A step of the prsLL_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{tree}, \code{route}, \code{tokens}, \code{yield}, \code{method}.
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
# The expression grammar of Aho, Lam, Sethi & Ullman (2nd ed., 4.4.2):
#   E -> T E';  E' -> + T E' | eps;  T -> F T';  T' -> * F T' | eps;
#   F -> ( E ) | id
rules <- list(
  list("E",  c("T", "Ep")),
  list("Ep", c("+", "T", "Ep")),
  list("Ep", character(0)),
  list("T",  c("F", "Tp")),
  list("Tp", c("*", "F", "Tp")),
  list("Tp", character(0)),
  list("F",  c("(", "E", ")")),
  list("F",  "id")
)
g <- .prsLL_grammar(rules, start = "E")
res <- morie_prsLL(g, c("id", "+", "id", "*", "id"))
res$yield
# the table-driven and recursive-descent routes must agree
rd <- morie_prsLL(g, c("id", "+", "id", "*", "id"),
                  route = "recursive_descent")
identical(res$tree, rd$tree)
}
