% Hand-written: gen_rd_tree.R derives argument prose from the body, and
% for this function it produced statements that are false (it reported
% `fun` and `grad` as unused). roxygen2 hangs on this package, so this
% page is maintained directly and must be kept in step with the
% roxygen block in R/lbfgsm_native.R.
\name{morie_lbfgsm}
\alias{morie_lbfgsm}
\title{Limited-memory BFGS minimisation}
\usage{
morie_lbfgsm(fun, x0, grad, m = 10, max_iter = 200, tol = 1e-08,
    tol_type = c("absolute", "relative", "initial"), precond = NULL,
    c1 = 1e-04, c2 = 0.9, max_ls = 60)
}
\arguments{
\item{fun}{Objective, called as \code{fun(x)} and returning a length-1
numeric.}
\item{x0}{Starting point, coerced with \code{as.numeric}.}
\item{grad}{Gradient, called as \code{grad(x)} and returning a vector
the length of \code{x0}.}
\item{m}{Number of correction pairs held. Raising it models the inverse
Hessian better on an ill-conditioned problem, at three times the
storage for \code{m = 30}. Defaults to \code{10}.}
\item{max_iter}{Iteration cap. Defaults to \code{200}.}
\item{tol}{Stopping tolerance, applied to whichever criterion
\code{tol_type} names. Defaults to \code{1e-08}.}
\item{tol_type}{Which criterion \code{tol} applies to.
\code{"absolute"} (the default) tests the gradient norm directly;
\code{"initial"} tests it against its value at the starting point; and
\code{"relative"} against \code{1 + |f|}, the form SciPy and NLopt
default to. Read the conditioning note before choosing
\code{"relative"}.}
\item{precond}{Diagonal preconditioner for the initial inverse Hessian:
\code{NULL} for none, a positive numeric vector giving the Hessian's
diagonal, or \code{"auto"} to estimate it from the most recent
curvature pair. On the quadratics measured below \code{"auto"} did not
help; a supplied diagonal is the useful form.}
\item{c1}{Armijo constant. Defaults to \code{1e-04}.}
\item{c2}{Curvature constant. Defaults to \code{0.9}.}
\item{max_ls}{Line-search trial cap. Defaults to \code{60}.}
}
\description{
Minimises \code{fun} by the limited-memory BFGS method of Liu and
Nocedal (1989), forming the search direction with their two-loop
recursion so the inverse Hessian approximation is applied without ever
being stored as a matrix.
}
\section{Conditioning, and what the tolerance means}{
An \strong{absolute} gradient test depends on how the objective is
scaled: multiply \code{f} by a thousand and the same point stops
passing. It is also a strict thing to ask of L-BFGS on an
ill-conditioned problem. Measured over 120 random twenty-dimensional
quadratics with condition numbers from about 10 to 1.4e5, at
\code{tol = 1e-7}:

\tabular{llll}{
  \strong{setting} \tab \strong{not converged} \tab
    \strong{claimed away from the optimum} \tab \strong{median x error} \cr
  \code{"absolute"}, \code{m = 10} \tab 72 / 120 \tab 0 \tab 2.6e-07 \cr
  \code{"absolute"}, \code{m = 30} \tab 63 / 120 \tab 0 \tab 2.5e-08 \cr
  \code{"initial"},  \code{m = 10} \tab 58 / 120 \tab 0 \tab 3.9e-07 \cr
  \code{"relative"}, \code{m = 10} \tab 21 / 120 \tab \strong{11} \tab 4.7e-07
}

Read the third column before the second. \code{"relative"} converges far
more often, and reaches that number partly by declaring convergence at
points which are not stationary -- eleven of 120, at errors above 1e-5.
Failing to reach an unnaturally tight tolerance is better than reporting
success at a non-stationary point, so \code{"absolute"} remains the
default and \code{"relative"} is offered for callers who want the SciPy
convention and know what it admits. \code{"initial"} is the scale-free
criterion without that failure mode.

A problem with a condition number at or above 1e4 naturally stalls
around a relative error of 1e-5, and no choice of tolerance changes
that: the iteration stops at the \emph{first} iterate under the
threshold, so the margin is about 1.5 times at every tolerance. Raising
\code{m} from 10 to 30 is the adjustment that helps, cutting the
failures and improving the median solution by an order of magnitude.
}
\section{Line search}{
The Armijo condition compares a required decrease against a
\emph{measured} change in \code{f}, and once the trial step is small
enough the required decrease falls below the precision with which
\code{f} can be evaluated at all. The comparison therefore carries a
tolerance of \code{4 * .Machine$double.eps * max(1, |f|)}, without
which the test reads arithmetic noise and cannot be satisfied for any
number of trials.

A failed line search does not concede immediately: the curvature
history is discarded and the iteration retaken from steepest descent,
which is always a descent direction. \code{converged} means the
requested tolerance was met and nothing else -- a stall at the optimum
with the tolerance unmet is reported through \code{status}, not as
success.
}
\value{
A list with \code{estimate}, \code{x}, \code{fun}, \code{grad},
\code{grad_norm}, \code{grad_norm_relative}, \code{grad_norm_ratio},
\code{tol}, \code{tol_type}, \code{iterations}, \code{n_fun},
\code{memory}, \code{converged}, \code{status}, \code{history} and
\code{method}. All three gradient measures are reported whichever
criterion was applied, so a result can be judged against a criterion
the caller did not select.
}
\references{
Liu, D. C. and Nocedal, J. (1989). On the limited memory BFGS method
for large scale optimization. \emph{Mathematical Programming}
\strong{45}, 503-528.
}
\keyword{internal}
