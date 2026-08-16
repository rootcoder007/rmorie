% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_trmRew_normal_form}
\alias{morie_trmRew_normal_form}
\title{morie_trmRew_normal_form}
\usage{
morie_trmRew_normal_form(t, rules, strategy = "innermost", max_steps = 10000)
}
\arguments{
\item{t}{See Usage.}
\item{rules}{Passed to \code{morie_trmRew_rewrite_step}.}
\item{strategy}{Passed to \code{morie_trmRew_rewrite_step}. Defaults to \code{"innermost"}.}
\item{max_steps}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10000}.}
}
\description{
A step of the trmRew_native implementation. Called by \code{.trmRew_interreduce}, \code{morie_trmRew_complete}, \code{morie_trmRew_decides} and 2 others in the module. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
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
t <- morie_unifAlg_app("plus", zero,
                       morie_unifAlg_app("plus", zero,
                                         morie_unifAlg_const("a")))
morie_trmRew_normal_form(t, list(r1))
}
