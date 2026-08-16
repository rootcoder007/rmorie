% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.trmRew_prec}
\alias{.trmRew_prec}
\title{.trmRew_prec}
\usage{
.trmRew_prec(precedence, sym)
}
\arguments{
\item{precedence}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{sym}{Passed to \code{\%in\%}.}
}
\description{
A step of the trmRew_native implementation. Called by \code{morie_trmRew_lpo_greater}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
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
.trmRew_prec(precedence = c(1, 2, 3, 4, 5, 6, 7, 8), sym = 5L)
}
