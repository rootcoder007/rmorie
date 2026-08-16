% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vepan_coding_terms}
\alias{.vepan_coding_terms}
\title{Everything that depends on the protein: the predicate set. Returns}
\usage{
.vepan_coding_terms(t, v, cds, coding, gpos)
}
\arguments{
\item{t}{A list; the body reads \code{$strand} from it.}
\item{v}{Passed to \code{.vepan_apply}.}
\item{cds}{A vector; its length is taken.}
\item{coding}{Passed to \code{.vepan_apply}.}
\item{gpos}{Passed to \code{.vepan_apply}.}
}
\description{
list(terms, info).
}
\value{
A list with \code{terms}, \code{info}.
}
\keyword{internal}
