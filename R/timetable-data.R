#' Example timetable data
#'
#' Raw timetable data example. This is an anonymised version of a real data set.
#'
#' A data frame with 8,958 rows and 11 columns
#'
#' @format
#' \describe{
#'   \item{train_header}{Train ID. This usually contains headcode information}
#'   \item{dt_origin}{Date and time of service origin}
#'   \item{geo}{Name of TIPLOCs that trains pass through or stop at}
#'   \item{event}{Type of timetable event, e.g., 'Arrive', 'Depart', 'Pass'}
#'   \item{wtt}{Scheduled date and time of event}
#'   \item{t}{Actual date and time of event}
#'   \item{delay}{Difference between actual time and scheduled time}
#'   \item{allow}{Total delay allowance}
#'   \item{allow_perf}{Performance allowance}
#'   \item{allow_path}{Pathing allowance}
#'   \item{allow_eng}{Engineering allowance}
#' }
#'
#' @examples
#' timetable
#'
#' @seealso [wrangle_timetable()]
"timetable"

#' Example stations
#'
#' List of example stations. This is used for timetable wrangling
#'
#' A list with 6 elements.
#'
#' @examples
#' stations
#'
#' @seealso [wrangle_timetable()]
"stations"

#' Example stopping stations
#'
#' List containing stations at which trains stop. This is used for finding
#' calling patterns
#'
#' A list with 3 elements.
#'
#' @examples
#' stopping_stations
#'
#' @seealso [wrangle_timetable()] [find_calling_patterns()]
"stopping_stations"
