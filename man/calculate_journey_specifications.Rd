% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timetable-specification.R
\name{calculate_journey_specifications}
\alias{calculate_journey_specifications}
\title{Calculate journey specifications}
\usage{
calculate_journey_specifications(timetable, spec_stations)
}
\arguments{
\item{timetable}{Data frame containing timetable data. Strict input validation
is performed. Must be a data frame with the following columns:
\itemize{
\item{\code{train_header}}{\code{\link[=character]{character()}} Train identifier}
\item{\code{dt_origin}}{\code{\link[lubridate:posix_utils]{lubridate::POSIXct()}} Datetime at which the train
originated}
\item{\code{geo}}{\code{\link[=character]{character()}} Name of event location}
\item{\code{event}}{\code{\link[=character]{character()}} Type of event, must only contain the
following values: \code{Pass}, \code{Arrive}, \code{Depart}, \code{Originate},
\code{Terminate}}
\item{\code{wtt}}{\code{\link[lubridate:posix_utils]{lubridate::POSIXct()}} Scheduled datetime of event}
\item{\code{t}}{\code{\link[lubridate:posix_utils]{lubridate::POSIXct()}} Actual datetime of event}
\item{\code{delay}}{\code{\link[=numeric]{numeric()}} Difference between actual time and scheduled
time of event}
\item{\code{allow}}{\code{\link[=numeric]{numeric()}} Timetabled delay allowance}
}}

\item{spec_stations}{List of stations for which to calculate journey times.
Calculates the total journey time from the first station in the list to the
last station in the list, as well as every component of that journey.}
}
\value{
Journey times as specified by \code{spec_stations}. A data frame with 10
columns:
\itemize{
\item{\code{train_header}}{Train identifier}
\item{\code{dt_origin}}{Datetime at which the train originated}
\item{\code{wtt}}{Scheduled datetime of event}
\item{\code{group}}{Calling pattern}
\item{\code{xi}}{Start of journey component}
\item{\code{xk}}{End of journey component}
\item{\code{duration}}{Duration of journey component}
\item{\code{type}}{Type of component, either \code{moving} for sections in-between
stations, or \code{dwell} when the component represents a stop
at a station}
\item{\code{j}}{Component ID}
\item{\code{T_journey}}{Total journey time across all components}
}
}
\description{
Uses scheduled times in the timetable to calculate how long it should take to
travel along the track. Calculates journey times between stations specified
in the \code{spec_stations} parameter.
}
\examples{
data(timetable_subset)
timetable_subset
# spec_stations contains stations that completely encompass the Centrix track
spec_stations <- list("geo6", "geo110", "geo111", "geo112", "geo7")
calculate_journey_specifications(timetable_subset, spec_stations)

}
\seealso{
\code{\link[=wrangle_timetable]{wrangle_timetable()}} \code{\link[=match_ids]{match_ids()}} \code{\link[=estimate_berth_lengths]{estimate_berth_lengths()}}
\link{timetable_subset} \link{timetable_specification}
}
