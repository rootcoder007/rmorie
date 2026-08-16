% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{simulate_outbreak}
\alias{simulate_outbreak}
\title{simulate_outbreak}
\usage{
simulate_outbreak(R0 = 2.5, dispersion = 0.16, n_initial = 20, trace_prob = 0.8, delay_mean = 3.83, delay_sd = 2.4, si_mean = 4.7, si_sd = 2.9, subclinical = 0, max_cases = 5000, max_weeks = 12, seed = 0, allow_presymptomatic = TRUE)
}
\arguments{
\item{R0}{Passed to \code{negbinom_offspring}. Defaults to \code{2.5}.}
\item{dispersion}{Passed to \code{negbinom_offspring}. Defaults to \code{0.16}.}
\item{n_initial}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20}.}
\item{trace_prob}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.8}.}
\item{delay_mean}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{3.83}.}
\item{delay_sd}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{2.4}.}
\item{si_mean}{Passed to \code{serial_interval_draw}. Defaults to \code{4.7}.}
\item{si_sd}{Passed to \code{serial_interval_draw}. Defaults to \code{2.9}.}
\item{subclinical}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{max_cases}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5000}.}
\item{max_weeks}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{12}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{allow_presymptomatic}{Passed to \code{serial_interval_draw}. Defaults to \code{TRUE}.}
}
\description{
A step of the ttrace_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{controlled}, \code{total_cases}, \code{weekly}, \code{hit_cap}, \code{extinct}.
}
\keyword{internal}
\examples{
simulate_outbreak()
}
