% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ros_exodus.R
\name{ros_exodus}
\alias{ros_exodus}
\title{Calculates the a summary table using regression on order statistics for a radar}
\usage{
ros_exodus(radar, folder, ..., basegrid_n, out_summary, out_ros, ea = F,
  total = T, seperate = F,
  date_mask = as.POSIXlt(colnames(rosdataall)[grepl("^z",
  colnames(rosdataall))], format = "z\%y\%m\%d"))
}
\arguments{
\item{radar}{The 3 letter radar code}

\item{folder}{The project name that will be tacked onto the name of the files}

\item{basegrid_n}{The location of the basegrid}

\item{out_summary}{File path for the summary file created from summary()}

\item{out_ros}{Folder to save the output ROS summary file}

\item{ea}{This is the fabled 'eagle file'. It is a seldom used argument that creates a large file with all nights, summarized every year, and all ID fields. It gets large fast and takes a while to run.}

\item{total}{Do you want to run a summary combining all years}

\item{seperate}{Do you want to run a summary each year}

\item{date_mask}{A vector of dates that the program will subset from the summary file. Must be in 'yyyy-mm-dd' format.}
}
\description{
This program inputs the summary file created from summary(). Technically you just need a data.frame with id, range, clutter, waterfilt, and all nights with column names 'zyymmdd'. The file will automatically be named K'radar'_ros_'folder'_year. So the folder is the main vector for changing file names with successive runs. You can input a paste0() function if looping through different inputs and would like to change the out put name
}

