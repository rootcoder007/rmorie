% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RefPattern}
\alias{RefPattern}
\title{Eqs (4.53)-(4.54): g(n) = 3 d(n) + 2 d(n-1) + d(n-2) and its filter}
\usage{
RefPattern(amplitudes = NULL) 
}
\arguments{
\item{amplitudes}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
h(n) = d(n) + 2 d(n-1) + 3 d(n-2) -- the same three numbers reversed. The book notes the output around the peak reproduces the ACF of g, which is checked here rather than asserted.
}
\value{
A list with \code{g}, \code{h}, \code{y}, \code{acf}, \code{delay}, \code{max_difference}, \code{output_is_acf}, \code{method}.
}
\keyword{internal}
\examples{
RefPattern()
}
