% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FormFactor}
\alias{FormFactor}
\title{Eqs (5.25)-(5.26): activity = var(x); mobility = sd(x\')/sd(x);}
\usage{
FormFactor(x)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
form factor = mobility(x\')/mobility(x) = (sd(x\'\')/sd(x\'))/(sd(x\')/sd(x)). The book states a sinusoid has complexity 1 and that more variable waveforms give larger values. Ratios are dimensionless, so no fs.
}
\value{
A list with \code{form_factor}, \code{complexity}, \code{mobility}, \code{activity}, \code{mobility_of_derivative}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
FormFactor(x = c(2.5, 1.0, 3.5, 4.0, 2.0, 5.5, 3.0, 6.5))
}
