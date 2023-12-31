% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/centrix-data.R
\docType{data}
\name{track_events}
\alias{track_events}
\title{Example track events}
\format{
\describe{
\item{track}{Track ID}
\item{dt}{Date and time of event}
\item{occupied}{TRUE if train enters track, else FALSE}
\item{event}{'enters' if train enters track, else 'vacates'}
}
}
\usage{
track_events
}
\description{
An example of Centrix data which have been pre-processed into track events.
}
\examples{
track_events

}
\seealso{
\code{\link[=wrangle_centrix]{wrangle_centrix()}} \code{\link[=preprocess_track_events]{preprocess_track_events()}}
\code{\link[=calculate_time_windows]{calculate_time_windows()}}

A data frame containing 25,242 rows and 4 columns.
}
\keyword{datasets}
