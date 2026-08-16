% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CDiff3Ph}
\alias{CDiff3Ph}
\title{Eq (3.131): slope -1, so a WHOLE sample of group delay against the}
\usage{
CDiff3Ph(omega)
}
\arguments{
\item{omega}{See Usage.}
}
\description{
half sample of the plain difference.  An integer delay can be undone by shifting the output back.
}
\value{
A list with \code{phase}, \code{omega}, \code{group_delay}, \code{slope}, \code{quarter_turn_offset}, \code{integer_delay_can_be_undone_by_shifting}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CDiff3Ph(V)
}
