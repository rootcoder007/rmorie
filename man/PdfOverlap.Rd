% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PdfOverlap}
\alias{PdfOverlap}
\title{BC(p1, p2) = sum_l sqrt(p1 p2): the OVERLAP between two PDFs,}
\usage{
PdfOverlap(p1, p2)
}
\arguments{
\item{p1}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{p2}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
bounded in [0, 1].  This is what the Bhattacharyya DISTANCE is built from, D_B = -ln BC, and what makes the error bound work: the overlap of the two class-conditional densities IS the region where the optimal classifier must make mistakes.  NOT FROM THIS BOOK.
}
\value{
A list with \code{coefficient}, \code{overlap}, \code{distance}, \code{identical}, \code{disjoint}, \code{in_unit_interval}, \code{the_overlap_is_where_errors_must_happen}, \code{not_from_this_book}, \code{reference}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
PdfOverlap(V, V)
}
