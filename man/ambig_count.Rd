% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ambig_count.R
\encoding{UTF-8}
\name{ambig_count}
\alias{ambig_count}
\title{Functions to indicate the presence of characters signalling uncertainty}
\usage{
ambig_count(n)
}
\arguments{
\item{n}{A string or vector of strings. Should be Chinese names.}
}
\value{
An integer vector of the same length as n
}
\description{
Functions to indicate the presence of characters signalling uncertainty
}
\details{
ambig_count counts for open-closed parentheses pairs,
question marks, and strings such as "You ming" which may indicate that
ambiguous or multiple name information is included in the field;
}
