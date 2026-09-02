% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{IntMag}
\alias{IntMag}
\title{Eq (3.117).  The book prints 1/w, which is right for w > 0 and is how}
\usage{
IntMag(omega) 
}
\arguments{
\item{omega}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
the response is plotted; a magnitude cannot be negative, so the absolute value is taken here.
}
\value{
A list with \code{magnitude}, \code{omega}, \code{book_prints_one_over_omega}, \code{absolute_value_needed_for_negative_omega}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
IntMag(V)
}
