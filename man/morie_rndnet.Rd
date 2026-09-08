% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rndnet}
\alias{morie_rndnet}
\title{Public entry point: compute the RND exploration bonus along an}
\usage{
morie_rndnet(observations, n_hidden = 64, n_out = 8, lr = 0.05, clip = 5, 
    normalize_obs = TRUE, normalize_reward = TRUE, init_steps = 0, gamma_int = 0.99, 
    seed = 0, target = NULL, predictor = NULL, update = TRUE) 
}
\arguments{
\item{observations}{A matrix; passed to \code{as.matrix}.}
\item{n_hidden}{Passed to \code{.rndnet_random_features_new}. Defaults to \code{64}.}
\item{n_out}{Passed to \code{.rndnet_random_features_new}. Defaults to \code{8}.}
\item{lr}{Defaults to \code{0.05}.}
\item{clip}{Numeric; combined arithmetically in the body. Defaults to \code{5}.}
\item{normalize_obs}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{normalize_reward}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{init_steps}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{0}.}
\item{gamma_int}{Numeric; combined arithmetically in the body. Defaults to \code{0.99}.}
\item{seed}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{target}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{predictor}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{update}{A flag; the body branches on it. Defaults to \code{TRUE}.}
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
