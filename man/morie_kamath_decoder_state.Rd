% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_decoder_state}
\alias{morie_kamath_decoder_state}
\title{morie_kamath_decoder_state}
\usage{
morie_kamath_decoder_state(s_t_1, y_t_1, c, g = NULL) 
}
\arguments{
\item{s_t_1,y_t_1,c}{Decoder inputs.}
\item{g}{Optional cell.}
}
\description{
morie_kamath_decoder_state
}
\value{
A list with \code{s}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_kamath_decoder_state(s_t_1 = c(1, 2, 3, 4, 5, 6, 7, 8),
  y_t_1 = c(1, 2, 3, 4, 5, 6, 7, 8), c = c(1, 2, 3, 4, 5, 6, 7, 8))
}
