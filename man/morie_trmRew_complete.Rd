% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_trmRew_complete}
\alias{morie_trmRew_complete}
\title{morie_trmRew_complete}
\usage{
morie_trmRew_complete(equations, precedence, max_rules = 60, max_steps = 10000, max_iter = 4000)
}
\arguments{
\item{equations}{Iterated over elementwise, with \code{lapply}.}
\item{precedence}{Passed to \code{.trmRew_interreduce}.}
\item{max_rules}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{60}.}
\item{max_steps}{Passed to \code{morie_trmRew_normal_form}. Defaults to \code{10000}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4000}.}
}
\description{
A step of the trmRew_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.trmRew_incomplete}.
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
