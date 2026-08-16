% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gb_fit}
\alias{.morie_gb_fit}
\title{Internal helper: native gradient boosting}
\usage{
.morie_gb_fit(X, y, task = c("regression", "classification"), n_estimators = 100L, learning_rate = 0.1, max_depth = 3L, min_node = 1L, lambda = 0, alpha = 0, gamma_pen = 0, subsample = 1)
}
\description{
`task = "regression"` uses squared-error loss (ESL Table 10.2: the negative gradient is the ordinary residual). `task = "classification"` uses the binomial deviance on a logit scale, for which the negative gradient is y - p and the second derivative p(1 - p); the Newton step -G/(H + lambda) is then the exact line search of Algorithm 10.3 step 2(c) for that loss.
}
\value{
A list with \code{f0}, \code{trees}, \code{learning_rate}, \code{raw}, \code{importance}, \code{task}, \code{fitted}.
}
\keyword{internal}
