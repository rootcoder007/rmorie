% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_trmRew_rewrite_step}
\alias{morie_trmRew_rewrite_step}
\title{One rewrite, or NULL when the term is in normal form. Innermost}
\usage{
morie_trmRew_rewrite_step(t, rules, strategy = "innermost")
}
\arguments{
\item{t}{Passed to \code{morie_trmRew_positions}.}
\item{rules}{A vector; its length is taken and its elements indexed.}
\item{strategy}{Compared against \code{"innermost"}. Defaults to \code{"innermost"}.}
}
\description{
reduces arguments before the term above them; outermost the other way round.
}
\value{
Nothing; the function is called for its effect.
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
