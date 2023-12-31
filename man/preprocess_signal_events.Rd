% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/centrix-preprocessing.R
\name{preprocess_signal_events}
\alias{preprocess_signal_events}
\alias{preprocess_track_events}
\title{Preprocess raw Centrix data}
\usage{
preprocess_signal_events(raw_centrix, asset_map, state_map)

preprocess_track_events(raw_centrix, asset_map)
}
\arguments{
\item{raw_centrix}{A data frame containing raw Centrix data. Strict input
validation is applied to ensure the data adhere to the expected structure.
The data should be in a data frame with the following columns:
\itemize{
\item{\code{asset}}: A \code{\link[=character]{character()}} vector containing the asset ID.
Tracks are expected to follow the regex pattern \code{"T[A-Z]+(-[0-9])?"}.
E.g., 'TABC' 'TABC-1' 'TABC-2'.
Signals are expected to follow the regex pattern \code{"S[0-9]+\\s[A-Z]+"}.
E.g., 'S123 HGE' 'S123 I'.
The second part of the signal ID should be a state code referring to
the signal aspect, e.g. 'RGE' for red or 'DGE' for green. For other
codes see:
\url{https://wiki.openraildata.com/index.php/Signalling_Nomenclature#Signals}.
\item{\code{dt}}: A \code{\link[lubridate:posix_utils]{lubridate::POSIXct()}} object representing the date
and time at which the observation was made.
\item{\code{transition}}: A \code{\link[=character]{character()}} vector representing the state
transition that caused the observation. A transition can be one of
"UP to DN" or "DN to UP", anything else will be ignored. For tracks,
"UP to DN" signifies a train entering the track, while "DN to UP" is a
train vacating the track. For signals, "UP to DN" represents an aspect
change. Therefore, any signals with a "DN to UP" transition will be
ignored.
}}

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

\item{state_map}{The state mapping provides a 1-1 mapping from signal state
codes to signal aspects. This is standardised, so \code{railvarr} provides a
default mapping (see \link{state_mapping}). Simply leave this parameter blank to
make use of this default mapping. See \href{https://wiki.openraildata.com/index.php/Signalling_Nomenclature#Signals}{this website}
for other codes.}
}
\value{
\code{preprocess_signal_events} returns a data frame containing aspect
events with columns:
\itemize{
\item{\code{signal}} \code{\link[=character]{character()}} signal ID.
\item{\code{dt}} \code{\link[lubridate:posix_utils]{lubridate::POSIXct()}} datetime.
\item{\code{aspect}} \code{\link[=factor]{factor()}} signal aspect caused by the event.
\item{\code{past_aspect}} \code{\link[=factor]{factor()}} signal aspect preceding the event.
}

\code{preprocess_track_events} returns a data frame containing track
events with columns:
\itemize{
\item{\code{track}} \code{\link[=character]{character()}} track ID.
\item{\code{dt}} \code{\link[lubridate:posix_utils]{lubridate::POSIXct()}} datetime.
\item{\code{occcupied}} \code{\link[=logical]{logical()}} TRUE if train enters track, else
FALSE.
\item{\code{event}} \code{\link[=character]{character()}} 'enters' if train enters track, else
'vacates'.
}
}
\description{
\code{preprocess_signal_events()} handles only signals, defined by
\code{raw_centrix$asset} starting with the character 'S'.

\code{preprocess_track_events()} handles only tracks, defined by
\code{raw_centrix$asset} starting with the character 'T'.
}
\details{
\code{preprocess_signal_events()} converts \code{raw_centrix$asset} into
signal ID and signal state, the latter of which is converted to signal
aspect using the \code{state_map}..

\code{preprocess_track_events()} converts \code{raw_centrix$transition} into track
entry and exit events.
}
\examples{
data(raw_centrix, asset_map, state_mapping)
raw_centrix
aspect_events <- preprocess_signal_events(raw_centrix,
                                          asset_map,
                                          state_mapping)
aspect_events

#----------------------------------------------------------------------------

data(raw_centrix, asset_map)
raw_centrix
track_events <- preprocess_track_events(raw_centrix,
                                        asset_map)
track_events

}
\seealso{
\code{\link[=wrangle_centrix]{wrangle_centrix()}} \code{\link[=calculate_time_windows]{calculate_time_windows()}} \link{aspect_events} \link{track_events}
}
