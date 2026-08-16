% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_fauzi_rratio}
\alias{.morie_fauzi_rratio}
\title{The book\'s `R(z) = sqrt(2 pi) z^(z+1/2) / (e^z Gamma(z+1))`, Eq. (1.12).}
\usage{
.morie_fauzi_rratio(z)
}
\description{
The Stirling defect of the gamma function: `R(z)` increases monotonically to 1 from below (Remark 1.2), which is exactly the fact the book uses to read off the `O(n^-1 h^-1/4)` interior and `O(n^-1 h^-3/4)` boundary rates of `Var[A_h(x)]` in (1.11). Computed through `lgamma` so the `z^(z+1/2)` factor cannot overflow at the small bandwidths this suite uses (`z ~ h^(-1/2)`).
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_fauzi_rratio(V)
}
