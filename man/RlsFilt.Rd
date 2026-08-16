% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RlsFilt}
\alias{RlsFilt}
\title{Section 3.10.3, eqs. (3.215), (3.221), (3.224)-(3.225).  P is}
\usage{
RlsFilt(primary, reference, order = 8, lam = 0.98, delta = 1)
}
\arguments{
\item{primary}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{reference}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.}
\item{lam}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.98}.}
\item{delta}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
symmetrized every sample: in exact arithmetic the update preserves symmetry, in floating point it does not, and the asymmetry grows until P loses positive definiteness and the filter diverges.  The size of that drift is returned rather than hidden.
}
\value{
A list with \code{e}, \code{output}, \code{y}, \code{final_weights}, \code{P}, \code{order}, \code{lam}, \code{delta}, \code{memory}, \code{p_symmetry_error}, \code{p_symmetrized}, \code{input_power}, \code{output_power}, \code{power_reduction}, \code{method}.
}
\keyword{internal}
