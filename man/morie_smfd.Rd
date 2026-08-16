% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_smfd}
\alias{morie_smfd}
\title{P-spline fit}
\usage{
morie_smfd(x, y, nseg = 10L, degree = 3L, lam = 1, order = 2L, weights = NULL)
}
\arguments{
\item{x}{Predictor values.}
\item{y}{Response values, same length as \code{x}.}
\item{nseg}{Number of interior segments; the total number of   knots is \code{nseg + 2 * degree + 1}.}
\item{degree}{B-spline degree (0 gives piecewise constant).}
\item{lam}{Non-negative penalty weight.}
\item{order}{Penalty order; the limit as \code{lam -> Inf} is   the polynomials of degree \code{order - 1}.}
\item{weights}{Optional non-negative observation weights.}
}
\description{
Penalised least squares on a B-spline basis with a difference penalty on the coefficients (Eilers & Marx 1996). The normal equations are \code{(B\'B + lambda D\'D) a = B\'y} with \code{B} the B-spline design matrix and \code{D} the \code{order}-th difference matrix. The penalty order fixes the limit: as \code{lam} tends to infinity the fit becomes a polynomial of degree \code{order - 1} exactly, with \code{order = 2} giving the OLS line. The effective dimension is the trace of the smoother matrix \code{H = B (B\'B + lambda D\'D)^{-1} B\'}, and the leave-one-out deletion residual is \code{e_i / (1 - h_ii)} from the hat diagonal.
}
\value{
A list with \code{estimate} (the effective dimension),   \code{coefficients}, \code{fitted}, \code{residuals},   \code{rss}, \code{hat_diagonal}, \code{effective_dimension},   \code{knots}, \code{degree}, \code{nseg}, \code{order},   \code{lam}, \code{n}, \code{p}, \code{sigma2}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_smfd(V, V)
}
