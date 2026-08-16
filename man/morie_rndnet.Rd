% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rndnet}
\alias{morie_rndnet}
\title{Public entry point: compute the RND exploration bonus along an}
\usage{
morie_rndnet(observations, n_hidden = 64, n_out = 8, lr = 0.05, clip = 5, normalize_obs = TRUE, normalize_reward = TRUE, init_steps = 0, gamma_int = 0.99, seed = 0, target = NULL, predictor = NULL, update = TRUE)
}
\arguments{
\item{observations}{See Usage.}
\item{n_hidden}{Defaults to \code{64}.}
\item{n_out}{Defaults to \code{8}.}
\item{lr}{Defaults to \code{0.05}.}
\item{clip}{Defaults to \code{5}.}
\item{normalize_obs}{Defaults to \code{TRUE}.}
\item{normalize_reward}{Defaults to \code{TRUE}.}
\item{init_steps}{Defaults to \code{0}.}
\item{gamma_int}{Defaults to \code{0.99}.}
\item{seed}{Defaults to \code{0}.}
\item{target}{Defaults to \code{NULL}.}
\item{predictor}{Defaults to \code{NULL}.}
\item{update}{Defaults to \code{TRUE}.}
}
\description{
observation stream.
}
\value{
A list with \code{estimate}, \code{intrinsic_reward}, \code{raw_error}, \code{returns}, \code{mse}, \code{mean_first}, \code{mean_last}, \code{n}, \code{target}, \code{predictor}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_rndnet(V)
}
