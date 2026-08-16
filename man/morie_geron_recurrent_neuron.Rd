% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_recurrent_neuron}
\alias{morie_geron_recurrent_neuron}
\title{Recurrent neuron step: h_t = phi(Wx x_t + Wh `h_{t-1}` + b) (Geron Ch 13, hmrnn)}
\usage{
morie_geron_recurrent_neuron(x_t, h_prev, Wx, Wh, b, activation = "tanh")
}
\arguments{
\item{x_t,h_prev}{Input and previous state.}
\item{Wx,Wh,b}{Weights.}
\item{activation}{One of tanh/relu/sigmoid/identity.}
}
\description{
Recurrent neuron step: h_t = phi(Wx x_t + Wh `h_{t-1}` + b) (Geron Ch 13, hmrnn)
}
\value{
A list with \code{h}, \code{h_next}, \code{z}, \code{jacobian}, \code{jacobian_norm}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
