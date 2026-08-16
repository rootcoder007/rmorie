% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ma8Fr}
\alias{Ma8Fr}
\title{Eq (3.111).  The book\\'s factored form is EXACT: the bracket is the}
\usage{
Ma8Fr(omega)
}
\arguments{
\item{omega}{See Usage.}
}
\description{
sum over lags -3..3, and exp(-j4w) shifts that to lags 1..7, which with the leading 1 is the whole sum.  Rendering it as a product of two brackets -- as the placeholder docstring did -- is a different and wrong function.  Both forms are computed and compared.
}
\value{
A list with \code{H}, \code{factored}, \code{omega}, \code{magnitude}, \code{max_difference}, \code{factored_form_agrees}, \code{bracket_is_inside_the_product}, \code{method}.
}
\keyword{internal}
