% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ctft}
\alias{Ctft}
\title{Eqs (3.75)-(3.76): one transform in two frequency variables,}
\usage{
Ctft(x, t = NULL, omega = NULL, f = NULL, dt = NULL)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{t}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{omega}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{f}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{dt}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
omega = 2 pi f.  Integrated over the supplied samples, so the limits are the duration of the signal.
}
\value{
A list with \code{X}, \code{omega}, \code{f}, \code{variable}, \code{duration}, \code{method}.
}
\keyword{internal}
