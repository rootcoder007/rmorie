% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.val_brier}
\alias{.val_brier}
\title{Internal helper: Val Brier}
\usage{
.val_brier(y_true, y_pred)
}
\description{
Internal helper: Val Brier
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.val_brier(V, V)
}
