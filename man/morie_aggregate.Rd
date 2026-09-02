% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_aggregate}
\alias{morie_aggregate}
\title{Collapse the ATT(g, t) to one number or a profile (Sec. 7)}
\usage{
morie_aggregate(result, scheme = "simple", weights = NULL) 
}
\arguments{
\item{result}{Output of \code{morie_etwfe} or   \code{morie_imputation}.}
\item{scheme}{One of \code{"simple"} (uniform weights),   \code{"event"} (profile by \code{(g, t)}), or \code{"cohort"}   (profile by calendar period \code{t}).}
\item{weights}{Optional named numeric vector of weights for the   \code{"simple"} scheme; defaults to uniform.}
}
\description{
Collapse the ATT(g, t) to one number or a profile (Sec. 7)
}
\value{
A named list: for \code{"simple"} \code{estimate},   \code{weights}, \code{scheme}; for \code{"event"} or   \code{"cohort"} \code{profile}, \code{scheme}, \code{estimate}.
}
\keyword{internal}
\examples{
set.seed(1)
nu <- 4; T <- 5
unit <- rep(1:nu, each = T)
period <- rep(1:T, nu)
first_treated <- rep(c(NA, NA, 4, 4), each = T)
treated_now <- !is.na(first_treated) & period >= first_treated
Y <- rnorm(nu * T) + treated_now * 2
res <- morie_imputation(Y, unit, period, first_treated)
morie_aggregate(res, scheme = "simple")
}
