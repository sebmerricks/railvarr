% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matching-data.R
\docType{data}
\name{berth_events_groups}
\alias{berth_events_groups}
\title{Example of wrangled Centrix data}
\format{
\describe{
\item{signal}{Signal ID}
\item{berth}{Berth ID}
\item{train_id}{Train ID}
\item{aspect}{Signal aspect as train enters berth}
\item{t_enters}{Date and time that train enters berth}
\item{t_red_on}{Date and time that signal aspect changes to red}
\item{t_enters_next}{Date and time that train enters next berth}
\item{t_vacates}{Date and time that train vacates berth}
\item{t_red_off}{Date and time that signal aspect changes from red}
\item{TSAR}{Amount of time signal aspect is set to red}
\item{T_onset}{Time between train entering berth and signal aspect changing
to red}
\item{T_clear}{Amount of time taken for train to fully clear the berth}
\item{T_offset}{Time between train vacating berth and signal aspect
changing from red}
\item{T_travel}{Amount of time taken for train to travel the length of the
berth}
\item{T_coach}{Amount of time taken for train to travel its own length}
\item{group}{Group name corresponding to calling pattern}
}
}
\usage{
berth_events_groups
}
\description{
Data frame containing wrangled Centrix data with group labels calculated
through K-means clustering.
}
\seealso{
\code{\link[=cluster_journeys]{cluster_journeys()}} \code{\link[=plot_clusters]{plot_clusters()}}

A data frame with 6,952 rows and 16 columns
}
\keyword{datasets}
