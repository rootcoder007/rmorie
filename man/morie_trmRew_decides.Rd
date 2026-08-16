% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_trmRew_decides}
\alias{morie_trmRew_decides}
\title{Whether two terms are equal in the theory the rules present. Sound}
\usage{
morie_trmRew_decides(s, t, rules, max_steps = 10000)
}
\arguments{
\item{s}{Passed to \code{morie_trmRew_normal_form}.}
\item{t}{Passed to \code{morie_trmRew_normal_form}.}
\item{rules}{Passed to \code{morie_trmRew_normal_form}.}
\item{max_steps}{Passed to \code{morie_trmRew_normal_form}. Defaults to \code{10000}.}
}
\description{
only for a convergent system.
}
\value{
A list with \code{equal}, \code{left}, \code{right}.
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
\examples{
x <- morie_unifAlg_var("x")
zero <- morie_unifAlg_const("0")
r1 <- morie_trmRew_rule(morie_unifAlg_app("plus", zero, x), x)
a <- morie_unifAlg_app("plus", zero, morie_unifAlg_const("a"))
morie_trmRew_decides(a, morie_unifAlg_const("a"), list(r1))$equal
}
