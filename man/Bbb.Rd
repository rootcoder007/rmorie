% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Bbb}
\alias{Bbb}
\title{Section 10.2.1.  Bundle-branch block desynchronises ventricular}
\usage{
Bbb(qrsdur, criteria = NULL)
}
\arguments{
\item{qrsdur}{A vector; its length is taken.}
\item{criteria}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
}
\description{
contraction and shows as a wider-than-normal QRS; the published logic is a conjunction of duration and amplitude measurements on named leads, so a program applies it once those measurements exist.
}
\value{
A list with \code{blocktype}, \code{qrsdur}, \code{wide}, \code{left}, \code{right}, \code{satisfied}, \code{method}.
}
\keyword{internal}
\examples{
Bbb(qrsdur = 5L)
}
