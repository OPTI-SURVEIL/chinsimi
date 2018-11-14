% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_radicals.R
\name{ChStr2rad}
\alias{ChStr2rad}
\title{Extract radical decomposition of Chinese Character strings.}
\usage{
ChStr2rad(Chin.strs = "", sep = "_", parallel = FALSE,
  full = FALSE)
}
\arguments{
\item{Chin.strs}{A vector of Chinese character strings}

\item{sep}{Separating character to be placed between results for individual
Hanzi}

\item{parallel}{option to run with parallel calculations}

\item{full}{Boolean option to return either a single round of decomposition
(if F), or full character decomposition (if T)}
}
\value{
A vector of radical decomposition strings from the original input
}
\description{
Extract radical decomposition of Chinese Character strings.
}
\examples{
ChStr2rad('凨冪',full=F)
ChStr2rad('凨冪',full=T)
}