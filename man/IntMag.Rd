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
\item{omega}{See Usage.}
}
\description{
the response is plotted; a magnitude cannot be negative, so the absolute value is taken here.
}
\value{
A list with \code{magnitude}, \code{omega}, \code{book_prints_one_over_omega}, \code{absolute_value_needed_for_negative_omega}, \code{method}.
}
\keyword{internal}
