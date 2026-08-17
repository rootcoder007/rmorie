% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{scale_to_liver}
\alias{scale_to_liver}
\title{scale_to_liver}
\usage{
scale_to_liver(clint_in_vitro, fu_incubation, system = "hepatocytes", species = "human", pbsf = NULL, liver_weight = NULL)
}
\arguments{
\item{clint_in_vitro}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fu_incubation}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{system}{One of \code{"hepatocytes"}, \code{"microsomes"}. Defaults to \code{"hepatocytes"}.}
\item{species}{The body requires: clrnt: species must be 'human' or 'rat'. Defaults to \code{"human"}.}
\item{pbsf}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{liver_weight}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the clrnt_native implementation. Called by \code{clrnt}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
scale_to_liver(clint_in_vitro = 15, fu_incubation = 0.8,
               system = "hepatocytes", species = "human")
}
