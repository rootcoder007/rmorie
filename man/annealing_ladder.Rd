% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{annealing_ladder}
\alias{annealing_ladder}
\title{annealing_ladder}
\usage{
annealing_ladder(n_steps, phi_max = 50, phi_min = 0.1, kind = "geometric")
}
\arguments{
\item{n_steps}{A count; the body uses it as \code{seq_len(...)}.}
\item{phi_max}{Numeric; combined arithmetically in the body. Defaults to \code{50}.}
\item{phi_min}{Numeric; combined arithmetically in the body. Defaults to \code{0.1}.}
\item{kind}{One of \code{"geometric"}, \code{"linear"}. Defaults to \code{"geometric"}.}
}
\description{
A step of the smcopt_native implementation. Called by \code{smcopt}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
annealing_ladder(n_steps = 5L)
}
