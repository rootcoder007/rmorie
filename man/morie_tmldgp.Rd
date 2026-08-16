% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tmldgp}
\alias{morie_tmldgp}
\title{Penalised nuisance fits, unpenalised targeting}
\usage{
morie_tmldgp(y, D, X, penalty = 0.05, iters = 100)
}
\arguments{
\item{y}{Outcome vector in [0,1].}
\item{D}{Treatment indicator vector.}
\item{X}{Covariate matrix.}
\item{penalty}{Lasso regularisation strength.}
\item{iters}{Maximum Newton steps.}
}
\description{
Both bar Q and g are fitted by post-lasso; the one-dimensional fluctuation is then fitted by maximum likelihood, unregularised.
}
\value{
A list with \code{estimate}, \code{psi}, \code{epsilon},   \code{se}, \code{ci}, \code{mean_eic}, \code{solves_eic},   \code{g_support}, \code{Q_support}, \code{penalty},   \code{method}, \code{note}.
}
\keyword{internal}
