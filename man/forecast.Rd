% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{forecast}
\alias{forecast}
\title{forecast}
\usage{
forecast(fitted, h = 12)
}
\arguments{
\item{fitted}{A list; the body reads \code{$ar}, \code{$ma}, \code{$order}, \code{$residuals}, \code{$s}, \code{$seasonal_order}, \code{$sigma2}, \code{$y} from it.}
\item{h}{A count; the body uses it as \code{numeric(...)}. Defaults to \code{12}.}
}
\description{
A step of the sarima_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{forecast}, \code{variance}, \code{se}, \code{psi}, \code{method}.
}
\keyword{internal}
\examples{
y <- c(1, 0.5, 0.25, 0.125, 0.0625, 0.03125)
r <- morie_geron_arima(y, p = 1, d = 0, q = 0, include_mean = FALSE)
r$ar
r$sigma2
r$forecast(2)
d1 <- morie_geron_arima(c(1, 3, 5, 7), p = 0, d = 1, q = 0)
d1$intercept
d1$forecast(2)
d1$sigma2
y2 <- c(1, 2, 1.5, 2.5, 2, 3, 2.5, 3.5, 3, 4, 3.5, 4.5)
ar <- morie_geron_arima(y2, p = 1, d = 0, q = 1)
ar$ar
ar$ma
ar$intercept
ar$sigma2
ar$aic
ar$forecast(2)
f <- morie_geron_arima_forecast(c(1, 2, 3, 4.5), phi = 0.5, theta = 0.25, d = 1)
f$forecast
f$forecast_differenced
f$residuals
f$differenced
f$sigma2
f$forecast_differenced
f$forecast
}
