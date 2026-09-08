% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_bits_per_character}
\alias{morie_burkov_bits_per_character}
\title{Bits per character from cross-entropy (Burkov Ch 2)}
\usage{
morie_burkov_bits_per_character(ce_loss, n_tokens, n_characters) 
}
\arguments{
\item{ce_loss}{Cross-entropy in nats per token.}
\item{n_tokens,n_characters}{Counts.}
}
\description{
Bits per character from cross-entropy (Burkov Ch 2)
}
\value{
A list with \code{estimate}, \code{bits_per_token}, \code{chars_per_token}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_burkov_bits_per_character(2, 100, 400)
}
