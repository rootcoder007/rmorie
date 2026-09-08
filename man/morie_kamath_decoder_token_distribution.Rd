% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_decoder_token_distribution}
\alias{morie_kamath_decoder_token_distribution}
\title{morie_kamath_decoder_token_distribution}
\usage{
morie_kamath_decoder_token_distribution(s_t_1, y_t_1, c, W = NULL) 
}
\arguments{
\item{W}{Optional vocab x 3d score projection.}

\item{s_t_1}{Argument \code{s_t_1}; see Usage.}

\item{y_t_1}{Argument \code{y_t_1}; see Usage.}

\item{c}{Argument \code{c}; see Usage.}
}
\description{
morie_kamath_decoder_token_distribution
}
\value{
A list with \code{distribution}, \code{predicted_token}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_kamath_decoder_token_distribution(s_t_1 = c(1, 2, 3, 4, 5, 6, 7, 8),
  y_t_1 = c(1, 2, 3, 4, 5, 6, 7, 8), c = c(1, 2, 3, 4, 5, 6, 7, 8))
}
