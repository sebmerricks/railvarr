% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate-dwell_times.R
\name{estimate_dwell_times}
\alias{estimate_dwell_times}
\title{Estimate dwell times}
\usage{
estimate_dwell_times(
  berth_events_groups,
  berth_lengths,
  station_names,
  stopping_patterns,
  a_brake,
  a_tract,
  station_berth_lengths = NULL
)
}
\arguments{
\item{berth_events_groups}{Berth-level Centrix data}

\item{berth_lengths}{Data frame with 2 columns:
\itemize{
\item{\code{berth}}{Berth ID}
\item{\code{L}}{Berth length in metres}
}}

\item{station_names}{Data frame with 2 columns:
\itemize{
\item{\code{berth}}{Berth ID}
\item{\code{station}}{Station name}
}
This provides a 1-1 mapping from berth ID to station name. The station
names should correspond to those in the timetable}

\item{stopping_patterns}{Data frame with 2 columns:
\itemize{
\item{\code{group}}{Group name}
\item{\code{station}}{Station name}
}
This provides a list of stations that each group stops at.}

\item{a_brake}{Braking capacity in m/s^2}

\item{a_tract}{Acceleration in m/s^2}

\item{station_berth_lengths}{(Optional) Data frame with 3 columns:
\itemize{
\item{\code{station}}{Station name}
\item{\code{L1}}{Distance from start of berth to station}
\item{\code{L2}}{Distance from station to end of berth}
}
If station_berth_lengths is NULL (the default), then L1 and L2 will be
equal to the berth length halved}
}
\value{
Data frame with 22 columns:
\itemize{
\item{\code{signal}}{Signal ID}
\item{\code{berth}}{Berth ID}
\item{\code{station}}{Station name}
\item{\code{train_id}}{Train ID}
\item{\code{group}}{Group name}
\item{\code{t_enters}}{Date and time that train enters berth}
\item{\code{T_travel}}{Berth travel time}
\item{\code{v_entry}}{Velocity at entry to berth in m/s}
\item{\code{a_brake}}{Braking capacity in m/s^2}
\item{\code{L1}}{Distance from start of berth to station}
\item{\code{v_exit}}{Velocity at exit of berth in m/s}
\item{\code{a_tract}}{Acceleration in m/s^2}
\item{\code{L2}}{Distance from station to end of berth}
\item{\code{T_brake}}{Time spent braking}
\item{\code{S_brake}}{Distance spent braking}
\item{\code{T_const_one}}{Time spent moving at a constant speed before
braking for station}
\item{\code{T_move_one}}{Total time moving before reaching station}
\item{\code{T_tract}}{Time spent accelerating}
\item{\code{S_tract}}{Distance spent accelerating}
\item{\code{T_const_two}}{Time spent moving at a constant speed before
vacating berth}
\item{\code{T_move_two}}{Total time moving after leaving station}
\item{\code{T_dwell}}{Dwell time at station}
}
}
\description{
This function estimates dwell times from Centrix data
}
\examples{
data(berth_events_groups, berth_lengths)

# Set up station mapping
station_names <- dplyr::tribble(
  ~berth, ~station,
  "A", "geo110",
  "D", "geo111",
  "F", "geo112"
)

# Specify stopping patterns
stopping_patterns <- dplyr::tribble(
  ~group, ~station,
  "stopping-all", "geo110",
  "stopping-all", "geo111",
  "stopping-all", "geo112",
  "stopping-geo112", "geo112"
)

# Define braking and accelerating capacities
a_brake = 0.4
a_tract = 0.35

# Calculate dwell times
dwell_times <- estimate_dwell_times(berth_events_groups,
                                    berth_lengths,
                                    station_names,
                                    stopping_patterns,
                                    a_brake,
                                    a_tract)

# View dwell times
dwell_times

# Summarise dwell times
# There are some negative values, which indicate outliers and potentially invalid observations
summary(dwell_times$T_dwell)

}
\seealso{
\code{\link[=cluster_journeys]{cluster_journeys()}} \code{\link[=estimate_berth_lengths]{estimate_berth_lengths()}} \code{\link[=estimate_delays]{estimate_delays()}}
\link{dwell_times}
}
