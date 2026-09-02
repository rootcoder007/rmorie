% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_msm_design}
\alias{morie_msm_design}
\title{morie_msm_design}
\usage{
morie_msm_design(treatment_history, extra = NULL) 
}
\description{
morie_msm_design
}
\arguments{
\item{treatment_history}{Argument \code{treatment_history}; see Usage.}
\item{extra}{Argument \code{extra}; see Usage.}
}
\value{
A list with \code{X}, \code{a_bar}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
rmorie:::morie_msm_design(M)
}
