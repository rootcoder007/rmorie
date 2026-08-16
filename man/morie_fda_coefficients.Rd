% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_fda_coefficients}
\alias{morie_fda_coefficients}
\title{Basis coefficients of an observed curve (MVSML eq. 14.6)}
\usage{
morie_fda_coefficients(Psi, x_t)
}
\arguments{
\item{Psi}{basis matrix from [morie_fda_basis()]}
\item{x_t}{observed curve values on the same grid}
}
\description{
Least-squares projection c-hat = (Psi\'Psi)^-1 Psi\'x of a discretely observed curve onto the basis.
}
\value{
numeric vector of basis coefficients
}
\keyword{internal}
