% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_trmRew_subterm_at}
\alias{morie_trmRew_subterm_at}
\title{The subterm at a position}
\usage{
morie_trmRew_subterm_at(t, pos)
}
\arguments{
\item{t}{See Usage.}
\item{pos}{See Usage.}
}
\description{
A step of the trmRew_native implementation. Called by \code{.trmRew_overlap}, \code{morie_trmRew_rewrite_step}. See the file header for the source the module follows.
}
\value{
The value of \code{cur}, as built in the body.
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
