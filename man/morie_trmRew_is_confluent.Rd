% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_trmRew_is_confluent}
\alias{morie_trmRew_is_confluent}
\title{Confluence via Newman\'s lemma: terminating and locally confluent}
\usage{
morie_trmRew_is_confluent(rules, precedence, max_steps = 10000)
}
\arguments{
\item{rules}{Passed to \code{morie_trmRew_is_terminating}.}
\item{precedence}{Passed to \code{morie_trmRew_is_terminating}.}
\item{max_steps}{Passed to \code{morie_trmRew_is_locally_confluent}. Defaults to \code{10000}.}
}
\description{
A step of the trmRew_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{confluent}, \code{terminating}, \code{locally_confluent}, \code{n_critical_pairs}, \code{unjoinable}, \code{method}.
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
