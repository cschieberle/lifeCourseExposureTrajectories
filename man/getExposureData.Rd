% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getExposureData.R
\name{getExposureData}
\alias{getExposureData}
\title{Reads individual-specific daily exposure data from a given path.}
\usage{
getExposureData(indiv.id, stressors)
}
\arguments{
\item{indiv.id}{The identifier of an individual.}

\item{stressors}{List of stressors to be assessed.}
}
\description{
It is assumed that there is a subfolder for each individual stressor.
Furthermore, it is assumed that one separate file per individual is given.
}
