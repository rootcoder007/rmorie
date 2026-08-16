% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gb_loss_path}
\alias{.morie_gb_loss_path}
\title{Internal helper: training loss after each boosting round}
\usage{
.morie_gb_loss_path(fit, X, y)
}
\description{
Replays the stored trees so callers can see the loss path without the fit having to carry it. Squared error for regression, mean binomial deviance for classification.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.morie_gb_loss_path(fit = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8))
}
