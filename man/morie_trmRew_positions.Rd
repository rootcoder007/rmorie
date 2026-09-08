% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_trmRew_positions}
\alias{morie_trmRew_positions}
\title{Every position in a term, as a 0-based integer vector of argument}
\usage{
morie_trmRew_positions(t) 
}
\arguments{
\item{t}{A vector; indexed elementwise.}
}
\description{
indices. The empty position is integer(0).
}
\value{
The value of \code{out}, as built in the body.
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
t <- morie_unifAlg_app("plus", morie_unifAlg_const("0"), x)
morie_trmRew_positions(t)
}
