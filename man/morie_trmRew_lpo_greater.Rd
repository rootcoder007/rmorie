% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_trmRew_lpo_greater}
\alias{morie_trmRew_lpo_greater}
\title{The lexicographic path order, s >_lpo t}
\usage{
morie_trmRew_lpo_greater(s, t, precedence) 
}
\arguments{
\item{s}{A vector; indexed elementwise.}
\item{t}{A vector; indexed elementwise.}
\item{precedence}{Passed to \code{morie_trmRew_lpo_greater}.}
}
\description{
A step of the trmRew_native implementation. Called by \code{morie_trmRew_complete}, \code{morie_trmRew_is_terminating}. See the file header for the source the module follows.
}
\value{
A logical value.
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
morie_trmRew_lpo_greater(s = c(1, 2, 3, 4, 5, 6, 7, 8), t = c(1, 2, 3, 4, 5, 6, 7, 8), precedence = c(1, 2, 3, 4, 5, 6, 7, 8))
}
