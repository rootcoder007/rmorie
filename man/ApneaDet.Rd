% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ApneaDet}
\alias{ApneaDet}
\title{ApneaDet: sleep apnea detection from ECG, SpO2 and snore}
\usage{
ApneaDet(ecg, spo2, snore, fs, epoch_s = 60, desat_pct = 4)
}
\arguments{
\item{ecg}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{spo2}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{snore}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{epoch_s}{Numeric; combined arithmetically in the body. Defaults to \code{60}.}
\item{desat_pct}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{4}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{epochs}, \code{n_flagged}, \code{apnea_suspected}, \code{events_per_hour}, \code{severity}, \code{n_epochs}, \code{epoch_s}, \code{fs_hz}, \code{desat_criterion_pct}, \code{heuristic}, \code{units}, \code{method}.
}
\keyword{internal}
