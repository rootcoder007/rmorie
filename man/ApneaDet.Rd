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
\item{ecg}{See Usage.}
\item{spo2}{See Usage.}
\item{snore}{See Usage.}
\item{fs}{See Usage.}
\item{epoch_s}{Defaults to \code{60}.}
\item{desat_pct}{Defaults to \code{4}.}
}
\description{
Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{epochs}, \code{n_flagged}, \code{apnea_suspected}, \code{events_per_hour}, \code{severity}, \code{n_epochs}, \code{epoch_s}, \code{fs_hz}, \code{desat_criterion_pct}, \code{heuristic}, \code{units}, \code{method}.
}
\keyword{internal}
