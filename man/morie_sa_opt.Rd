% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sa_opt}
\alias{morie_sa_opt}
\title{morie_sa_opt}
\usage{
morie_sa_opt(fun, x0, step = 1, T0 = 1, n_iter = 1000, schedule = "geometric", 
    alpha = 0.99, lower = NULL, upper = NULL, seed = 0) 
}
\arguments{
\item{fun}{Accepted by the signature and not used anywhere in the body.}
\item{x0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{step}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{T0}{Passed to \code{.sa_opt_temperature}. Defaults to \code{1}.}
\item{n_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{1000}.}
\item{schedule}{Coerced to character by the body, with \code{as.character}. Defaults to \code{"geometric"}.}
\item{alpha}{Passed to \code{.sa_opt_temperature}. Defaults to \code{0.99}.}
\item{lower}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{upper}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
}
\description{
A step of the sa_opt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{result}, as built in the body.
}
\keyword{internal}
