% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CLogPz}
\alias{CLogPz}
\title{CLogPz: complex log of a rational X(z) over its poles and zeros;}
\usage{
CLogPz(z, A = 1, r = 0, a_k = complex(0), b_k = complex(0), c_k = complex(0), d_k = complex(0), M_I = NULL, M_O = NULL, N_I = NULL, N_O = NULL)
}
\arguments{
\item{z}{Coerced to complex by the body, with \code{as.complex}.}
\item{A}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{r}{A flag; the body branches on it. Defaults to \code{0}.}
\item{a_k}{Coerced to complex by the body, with \code{as.complex}. Defaults to \code{complex(0)}.}
\item{b_k}{Coerced to complex by the body, with \code{as.complex}. Defaults to \code{complex(0)}.}
\item{c_k}{Coerced to complex by the body, with \code{as.complex}. Defaults to \code{complex(0)}.}
\item{d_k}{Coerced to complex by the body, with \code{as.complex}. Defaults to \code{complex(0)}.}
\item{M_I}{Carried through into a list the body builds.}
\item{M_O}{Carried through into a list the body builds.}
\item{N_I}{Carried through into a list the body builds.}
\item{N_O}{Carried through into a list the body builds.}
}
\description{
Rangayyan (2024) eq. (4.68) with eq. (4.67).
}
\value{
A list with \code{z_real}, \code{z_imag}, \code{xhat_real}, \code{xhat_imag}, \code{X_real}, \code{X_imag}, \code{max_abs_error}, \code{terms}, \code{counts}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CLogPz(V)
}
