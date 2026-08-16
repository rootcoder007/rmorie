% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.frwil_predict_activity}
\alias{.frwil_predict_activity}
\title{.frwil_predict_activity}
\usage{
.frwil_predict_activity(fit, compound)
}
\arguments{
\item{fit}{A list; the body reads \code{$coefficients}, \code{$groups}, \code{$n_positions} from it.}
\item{compound}{See Usage.}
}
\description{
A step of the frwil_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{total}, as built in the body.
}
\references{
----------
Free, S. M. & Wilson, J. W. (1964) "A mathematical contribution to
structure-activity studies", Journal of Medicinal Chemistry 7(4),
395-399, doi:10.1021/jm00334a001. The additive model above, the
indicator design over substituent positions, least-squares fitting,
and the singularity that forces a constraint.
}
\keyword{internal}
