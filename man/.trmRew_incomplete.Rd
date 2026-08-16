% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.trmRew_incomplete}
\alias{.trmRew_incomplete}
\title{.trmRew_incomplete}
\usage{
.trmRew_incomplete(rules, why, pair)
}
\arguments{
\item{rules}{A vector; its length is taken.}
\item{why}{See Usage.}
\item{pair}{See Usage.}
}
\description{
A step of the trmRew_native implementation. Called by \code{morie_trmRew_complete}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{rules}, \code{complete}, \code{reason}, \code{pair}, \code{n_rules}, \code{method}.
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
