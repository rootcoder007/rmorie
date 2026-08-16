% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_peephole_lstm}
\alias{morie_geron_peephole_lstm}
\title{Peephole LSTM cell forward step: gates also see the cell state (Geron Ch 13, hmphp)}
\usage{
morie_geron_peephole_lstm(x_t, h_prev, c_prev, weights)
}
\arguments{
\item{x_t,h_prev,c_prev}{Input and previous state.}
\item{weights}{List with W_x, W_h, b, p_i, p_f, p_o.}
}
\description{
Peephole LSTM cell forward step: gates also see the cell state (Geron Ch 13, hmphp)
}
\value{
A list with \code{h}, \code{c}, \code{i}, \code{f}, \code{g}, \code{o}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
W <- list(W_x = matrix(c(0, 0, 1, 0), ncol = 1), W_h = matrix(0, 
    4, 1), b = rep(0, 4))
morie_geron_peephole_lstm(1, 0, 0, W)
}
