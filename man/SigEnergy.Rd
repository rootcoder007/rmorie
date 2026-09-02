% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SigEnergy}
\alias{SigEnergy}
\title{Eq (4.40): E_x = integral x^2 dt = integral |X|^2 df, Parseval named}
\usage{
SigEnergy(x = NULL, t = NULL, dt = 1, X = NULL, freqs = NULL) 
}
\arguments{
\item{x}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{t}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{dt}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{X}{Optional; may be \code{NULL}. Coerced to complex by the body, with \code{as.complex}.}
\item{freqs}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
for the role it plays here -- E_x is constant for a given signal, so maximizing eq (4.41) is equivalent to maximizing eq (4.39).
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
SigEnergy(c(4, 0, 0, 0), dt = 1)
}
