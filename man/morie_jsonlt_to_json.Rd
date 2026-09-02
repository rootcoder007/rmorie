% Generated from the module source by ledger/wave3/gen_rd_jsonlt.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_jsonlt_to_json}
\alias{morie_jsonlt_to_json}
\title{Serialise an R object to JSON}
\usage{
morie_jsonlt_to_json(x, dataframe = c("rows", "columns", "values"), matrix = c("rowmajor", 
    "columnmajor"), Date = c("ISO8601", "epoch"), POSIXt = c("string", 
    "ISO8601", "epoch", "mongo"), factor = c("string", "integer"), complex = c("string", 
    "list"), raw = c("base64", "hex", "mongo", "int", "js"), null = c("list", 
    "null"), na = c("null", "string"), auto_unbox = FALSE, digits = 4, 
    pretty = FALSE, force = FALSE, ...) 
}
\arguments{
\item{x}{object to serialise.}
\item{pretty}{FALSE, TRUE, or an indent width.}
\item{...}{toJSON options: dataframe (rows, columns, values), matrix (rowmajor, columnmajor), Date (ISO8601, epoch), POSIXt (string, ISO8601, epoch, mongo), factor (string, integer), complex (string, list), raw (base64, hex, int, mongo), null (list, null), na (NULL, null, string), auto_unbox, digits, force.}

\item{dataframe}{Argument \code{dataframe}; see Usage.}

\item{matrix}{Argument \code{matrix}; see Usage.}

\item{Date}{Argument \code{Date}; see Usage.}

\item{POSIXt}{Argument \code{POSIXt}; see Usage.}

\item{factor}{Argument \code{factor}; see Usage.}

\item{complex}{Argument \code{complex}; see Usage.}

\item{raw}{Argument \code{raw}; see Usage.}

\item{null}{Argument \code{null}; see Usage.}

\item{na}{Argument \code{na}; see Usage.}

\item{auto_unbox}{Argument \code{auto_unbox}; see Usage.}

\item{digits}{Argument \code{digits}; see Usage.}

\item{force}{Argument \code{force}; see Usage.}
}
\description{
The jsonlite toJSON mapping, natively. Every documented option, same defaults.
}
\value{
A length-one character vector of JSON.
}
\references{
Ooms, J. (2014) "The jsonlite Package: A Practical and Consistent
  Mapping Between JSON Data and R Objects." arXiv:1403.2805.

Bray, T. (ed.) (2017) "The JavaScript Object Notation (JSON) Data
  Interchange Format." RFC 8259.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_jsonlt_to_json(V)
}
