% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{print.morie_mrm_reconciliation}
\alias{print.morie_mrm_reconciliation}
\title{Print method for \code{morie_mrm_reconciliation} objects}
\usage{
\method{print}{morie_mrm_reconciliation}(x, ...) 
}
\arguments{
\item{x}{A \code{morie_mrm_reconciliation} object.}
\item{...}{Ignored; accepted for S3 consistency.}
}
\description{
Print method for \code{morie_mrm_reconciliation} objects
}
\value{
The value of \code{invisible}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
print.morie_mrm_reconciliation(D)
}
