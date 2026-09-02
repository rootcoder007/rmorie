% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prphet_predict}
\alias{morie_prphet_predict}
\title{morie_prphet_predict}
\usage{
morie_prphet_predict(fit, t.new, seasonalities = NULL, holidays = NULL, holiday_window = c(0, 
    0)) 
}
\arguments{
\item{fit}{A list; the body reads \code{$beta}, \code{$changepoints}, \code{$columns} from it.}
\item{t.new}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{seasonalities}{Passed to \code{morie_prphet_design}.}
\item{holidays}{Passed to \code{morie_prphet_design}.}
\item{holiday_window}{Passed to \code{morie_prphet_design}. Defaults to \code{c(0, 0)}.}
}
\description{
A step of the prphet_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
