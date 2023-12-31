% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/centrix-windows.R
\name{filter_aspect_events}
\alias{filter_aspect_events}
\alias{filter_track_events}
\title{Filter Centrix events by time intervals}
\usage{
filter_aspect_events(aspect_events, time_windows, asset_map)

filter_track_events(track_events, time_windows, asset_map)
}
\arguments{
\item{aspect_events}{A data frame containing pre-processed aspect events with
columns:
\itemize{
\item{\code{signal}} \code{\link[=character]{character()}} signal ID.
\item{\code{dt}} \code{\link[lubridate:posix_utils]{lubridate::POSIXct()}} datetime.
\item{\code{aspect}} \code{\link[=factor]{factor()}} signal aspect after train enters the signal
section.
\item{\code{past_aspect}} \code{\link[=factor]{factor()}} signal aspect before train enters the
signal section.
}}

\item{time_windows}{A data frame containing window IDs and time intervals in
the columns:
\itemize{
\item{\code{window}} \code{\link[=numeric]{numeric()}} window ID.
\item{\code{interval}} \code{\link[lubridate:interval]{lubridate::interval()}} time intervals.
}
Each window indicates a time interval in which the data contain valid
observations.}

\item{asset_map}{The asset map is a representation of the track section.
Currently, the track is represented by a linear data frame. However, this
does not support more complex track layouts, such as junctions. The asset
map should be a data frame containing a 1-1 mapping from signal ID to berth
name, a 1-many mapping from berth name to track ID, and an extra \code{event}
column. The \code{event} column represents the fact that there are separate
Centrix observations for trains entering and exiting a track. Therefore,
each track can have both an \code{'enters'} and a \code{'vacates'} events associated
with it. Note that in order to calculate berth travel times, it is
necessary to know when the train entered to next berth. Therefore, it may
be impossible to calculate travel times for the final berth. The asset map
data frame should adhere to the following structure:
\itemize{
\item{\code{signal}}: A \code{\link[=character]{character()}} vector containing the signal ID. It
should follow the regex pattern \code{"S[0-9]+"}.
\item{\code{berth}}: A \code{\link[=character]{character()}} vector containing the berth ID. It
should follow the regex pattern \code{"[A-Z]+"}.
\item{\code{track}}: A \code{\link[=character]{character()}} vector containing the track ID. It
should follow the regex pattern \code{"T[A-Z]+(-[0-9])?"}.
\item{\code{event}}: A \code{\link[=character]{character()}} vector containing the event type. It
must be one of either "enters" or "vacates".
}}

\item{track_events}{A data frame containing pre-processed track events with
columns:
\itemize{
\item{\code{track}} \code{\link[=character]{character()}} track ID.
\item{\code{dt}} \code{\link[lubridate:posix_utils]{lubridate::POSIXct()}} datetime.
\item{\code{occcupied}} \code{\link[=logical]{logical()}} TRUE if train enters track, else FALSE.
\item{\code{event}} \code{\link[=character]{character()}} 'enters' if train enters track, else
'vacates'.
}}
}
\value{
A data frame containing filtered events, with additional columns:
\itemize{
\item{\code{window}} \code{\link[=numeric]{numeric()}} the window ID.
\item{\code{interval}} \code{\link[lubridate:interval]{lubridate::interval()}} the time interval.
}
}
\description{
Filter Centrix events to only those that occur within the
calculated valid time intervals.

\code{filter_track_events()} filters to only the tracks specified in the asset
map.

\code{filter_aspect_events()} filters to only the signals specified in the asset
map.
}
\examples{
data(aspect_events, time_windows, asset_map)
aspect_events
valid_aspect_events <- filter_aspect_events(aspect_events,
                                            time_windows,
                                            asset_map)
valid_aspect_events

#----------------------------------------------------------------------------

data(track_events)
track_events
valid_track_events <- filter_track_events(track_events,
                                          time_windows,
                                          asset_map)
valid_track_events

}
\seealso{
\code{\link[=wrangle_centrix]{wrangle_centrix()}} \code{\link[=calculate_time_windows]{calculate_time_windows()}} \link{valid_aspect_events}
\link{valid_track_events}
}
