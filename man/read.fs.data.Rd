% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FreeSurferFunctions.R
\name{read.fs.data}
\alias{read.fs.data}
\title{Reads FreeSurfer data for several individuals using parallel computing}
\usage{
read.fs.data(SUBJECTS_DIR, subjid, feature, fwhm, hemi = "lh.rh", ncores = 10)
}
\arguments{
\item{SUBJECTS_DIR}{freesurfer SUBJECTS_DIR}

\item{subjid}{individual subject ids}

\item{feature}{morphometric feature (e.g. thickness, area etc.)}

\item{fwhm}{smoothing kernes (e.g. 10)}

\item{hemi}{either "lh.rh" or "lh" or "rh"}

\item{ncores}{number of cores used to read in data}
}
\description{
Reads FreeSurfer data for several individuals using parallel computing
}
\examples{
ncores = parallel::detectCores() - 1 # identify number of cores
SUBJECTS_DIR <- c("/Users/sphache/Data/LEAP_wave1/")
brain.data <- ReadFreeSurferData(SUBJECTS_DIR, demographics$Subject, "thickness", 10, "lh.rh", 10)

}
