% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exodusangle_netcdf.R
\name{exodusangle_netcdf}
\alias{exodusangle_netcdf}
\title{Calculates the mean reflectivity of each sweep of .netcdf radar files.}
\usage{
exodusangle_netcdf(radar, corrected = F, lowerr = 10000, upperr = 40000,
  nexrad_n = "V:/Documents/nexrad_site_list_with_utm.csv",
  years = all_years, in_netcdf, out_netcdf, basegrid_n, mask = base$id,
  waterfilt = T, waterfilt_op = 0, terminal = F, parallel = T,
  legacy = F, clutter = T, width = 50, z = T)
}
\arguments{
\item{radar}{the 3 letter radar code}

\item{corrected}{is the data range corrected}

\item{lowerr}{lowest considered range in meters}

\item{years}{unique vector of years to be calculated from the available netcdf sweeps}

\item{in_netcdf}{the file path of the netcdfs}

\item{out_netcdf}{the location to save the file ref file}

\item{basegrid_n}{the location of the basegrid}

\item{mask}{the id numbers taken from the basegrid that you'd like to consider in calculation of reflectivity}

\item{waterfilt}{do you want to consider water in the mean calculation}

\item{waterfilt_op}{if waterfilt==T then whether you want to only look over water (1) or only over land (1)}

\item{terminal}{Is the radar data terminal doppler}

\item{parallel}{would you like to run the function using parallel computing}

\item{legacy}{are the files legacy (warning: not rigorously tested)}

\item{clutter}{do you wante to filter out clutter}

\item{width}{what ranges from the height of the radar do you want to consider}

\item{z}{do you want the values to be converted to units Z}

\item{higherr}{highest considered range in meters}
}
\description{
Used primarily for fitting exodus curves.
}

