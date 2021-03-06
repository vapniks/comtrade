% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/comtrade.R
\name{comtrade.codes}
\alias{comtrade.codes}
\title{comtrade.codes}
\usage{
comtrade.codes(names = NULL)
}
\arguments{
\item{names}{An optional list of country names to return the codes for. If this is left blank then all available
codes will be returned. You can include "World" in the this list.}
}
\value{
A data.frame containing country names and associated reporter codes.
}
\description{
Obtain country codes for comtrade.data reporter and partner params
}
\details{
This function is copied from http://comtrade.un.org/data/Doc/api/ex/r with a few alteration,
and works as of 06/05/2015
}
\author{
Ben Veal
}

