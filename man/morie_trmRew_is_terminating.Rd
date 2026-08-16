% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_trmRew_is_terminating}
\alias{morie_trmRew_is_terminating}
\title{Whether every rule strictly decreases in the LPO. Sufficient, not}
\usage{
morie_trmRew_is_terminating(rules, precedence)
}
\arguments{
\item{rules}{A vector; its length is taken and its elements indexed.}
\item{precedence}{Passed to \code{morie_trmRew_lpo_greater}.}
}
\description{
necessary. Returns 0-based indices of unoriented rules.
}
\value{
A list with \code{terminating}, \code{unoriented}, \code{method}.
}
\references{
----------
Knuth, D. E. & Bendix, P. B. (1970) "Simple word problems in
universal algebras", in J. Leech (ed.) Computational Problems in
Abstract Algebra, Pergamon Press, 263-297,
doi:10.1016/B978-0-08-012975-4.50028-X.

Newman, M. H. A. (1942) "On theories with a combinatorial definition
of \'equivalence\'", Annals of Mathematics 43(2), 223-243,
doi:10.2307/1968867.

Baader, F. & Nipkow, T. (1998) Term Rewriting and All That,
Cambridge University Press, ISBN 978-0-521-77920-3. Chs. 2, 5, 7.
}
\keyword{internal}
