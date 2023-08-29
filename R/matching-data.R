#' Example of a wrangled timetable
#'
#' Timetable containing only relevant services travelling in a single direction
#' with calling patterns labelled.
#'
#' A data frame with 792 rows and 12 columns
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
#'   \item{group}{Group name corresponding to calling pattern}
#' }
"timetable_groups"

#' Example of wrangled Centrix data
#'
#' Data frame containing wrangled Centrix data with group labels calculated
#' through K-means clustering.
#'
#' A data frame with 6,952 rows and 16 columns
#'
#' @format
#' \describe{
#'   \item{signal}{Signal ID}
#'   \item{berth}{Berth ID}
#'   \item{train_id}{Train ID}
#'   \item{aspect}{Signal aspect as train enters berth}
#'   \item{t_enters}{Date and time that train enters berth}
#'   \item{t_red_on}{Date and time that signal aspect changes to red}
#'   \item{t_enters_next}{Date and time that train enters next berth}
#'   \item{t_vacates}{Date and time that train vacates berth}
#'   \item{t_red_off}{Date and time that signal aspect changes from red}
#'   \item{TSAR}{Amount of time signal aspect is set to red}
#'   \item{T_onset}{Time between train entering berth and signal aspect changing
#'                  to red}
#'   \item{T_clear}{Amount of time taken for train to fully clear the berth}
#'   \item{T_offset}{Time between train vacating berth and signal aspect
#'                   changing from red}
#'   \item{T_travel}{Amount of time taken for train to travel the length of the
#'                   berth}
#'   \item{T_coach}{Amount of time taken for train to travel its own length}
#'   \item{group}{Group name corresponding to calling pattern}
#' }
"berth_events_groups"

#' Example ID Matching
#'
#' Data frame containing matched Centrix and timetable IDs
#'
#' Data frame with 42 rows and 3 columns
#'
#' @format
#' \describe{
#'   \item{train_id}{Centrix train ID}
#'   \item{train_header}{Timetable ID}
#'   \item{dt_origin}{Datetime that train originated}
#' }
#'
"id_matching"
