% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate-data.R
\docType{data}
\name{delays}
\alias{delays}
\title{Example delays}
\format{
\describe{
\item{\code{signal}}{Signal ID}
\item{\code{train_id}}{Train ID (Centrix)}
\item{\code{station}}{Station name}
\item{\code{T_dwell}}{Dwell time in seconds}
\item{\code{arrival_delay}}{Arrival delay in seconds, can be negative if the
train arrives early}
\item{\code{departure_delay}}{Departure delay in seconds, can be negative if
the train departs early}
\item{\code{excess_dwell_time}}{Difference between actual dwell time and
scheduled dwell time. Can be negative if the
train spends less time in the station than
scheduled}
\item{\code{early}}{Logical vector indicating whether the train arrived at the
station earlier than scheduled}
}
}
\usage{
delays
}
\description{
An example of a data frame containing estimated delays
}
\examples{
delays

}
\seealso{
\code{\link[=estimate_delays]{estimate_delays()}}

A data frame containing 66 row and 8 columns
}
\keyword{datasets}
