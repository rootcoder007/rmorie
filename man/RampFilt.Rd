% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RampFilt}
\alias{RampFilt}
\title{Eq (3.42): h(t) = 10 (0.25 - t), 0 <= t <= 0.25 s, at fs = 2 kHz}
\usage{
RampFilt(x = NULL, fs = 2000, duration = 0.25, slope = 10) 
}
\arguments{
\item{x}{Optional; may be \code{NULL}. Passed to \code{.morie_rg_aslist}.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{2000}.}
\item{duration}{Numeric; combined arithmetically in the body. Defaults to \code{0.25}.}
\item{slope}{Numeric; combined arithmetically in the body. Defaults to \code{10}.}
}
\description{
The text immediately after states the output was divided by the sum of all values of h(n) -- the normalization is part of the method; the raw taps sum to 626.25 for the book\'s constants.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
RampFilt()
}
