#' Example timetable data
#'
#' Raw timetable data example. This is an anonymised version of a real data set.
#'
#' @seealso [wrangle_timetable()]
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
"timetable"

#' Example stations
#'
#' List of example stations. This is used for timetable wrangling
#'
#' @seealso [wrangle_timetable()]
#'
#' A list with 6 elements.
#'
#' @examples
#' stations
#'
"stations"

#' Example stopping stations
#'
#' List containing stations at which trains stop. This is used for finding
#' calling patterns
#'
#' @seealso [wrangle_timetable()] [find_calling_patterns()]
#'
#' A list with 3 elements.
#'
#' @examples
#' stopping_stations
#'
"stopping_stations"

#' Example timetable specification
#'
#' Timetable specification containing information about expected journey times
#'
#' Data frame with 281 rows and 10 columns
#'
#' @format
#' \describe{
#'   \item{train_header}{Train ID. This usually contains headcode information}
#'   \item{dt_origin}{Date and time of service origin}
#'   \item{wtt}{Scheduled date and time of event}
#'   \item{group}{Group name}
#'   \item{xi}{Start point}
#'   \item{xk}{End point}
#'   \item{duration}{Scheduled time between `xi` and `xk`}
#'   \item{type}{Type of travel, `moving` or `dwell`}
#'   \item{j}{Segment ID}
#'   \item{T_journey}{Total journey time across track section}
#' }
#'
#' @examples
#' timetable_specification
#'
#' @seealso [calculate_journey_specifications()] [match_ids()]
#'   [estimate_berth_lengths()]
#'
"timetable_specification"

#' Example timetable subset
#'
#' Processed timetable containing only relevant services travelling in one
#' direction specified by the Centrix data
#'
#' @seealso [wrangle_timetable()] [calculate_journey_specifications()]
#'   [match_ids()]
#'
#'   Data frame with 792 rows and 12 columns
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
#'   \item{group}{Train group, corresponds to its calling pattern}
#' }
#'
#' @examples
#' timetable_subset
#'
"timetable_subset"
