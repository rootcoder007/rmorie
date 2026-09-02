% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Lrem}
\alias{Lrem}
\title{Log-ratio EM below a detection limit (Palarea-Albaladejo & Martin-Fernandez 2008)}
\usage{
Lrem(X, dl, n_iter = 20) 
}
\description{
Log-ratio EM below a detection limit (Palarea-Albaladejo & Martin-Fernandez 2008)
}
\arguments{
\item{X}{Argument \code{X}; see Usage.}
\item{dl}{Argument \code{dl}; see Usage.}
\item{n_iter}{Argument \code{n_iter}; see Usage.}
}
\value{
The value of \code{morie_unclr_lr_impute}.
}
\keyword{internal}
\examples{
Lrem(X = c(1, 2, 3, 4, 5, 6, 7, 8), dl = 5L)
}
