#' Example Centrix data
#'
#' An example of a data frame containing raw Centrix data. This is real data
#' containing 15 days of Centrix data. The data have been anonymised.
#'
#' A data frame containing 110,339 rows and 3 columns.
#'
#' @format
#' \describe{
#'   \item{asset}{Centrix asset ID, signal assets include state data}
#'   \item{dt}{Date and time of event}
#'   \item{transition}{Event transition data}
#' }
#'
#' @examples
#' raw_centrix
#'
"raw_centrix"

#' Example aspect events
#'
#' An example of Centrix data which have been pre-processed into aspect events.
#'
#' A data frame containing 32,512 rows and 4 columns.
#'
#' @format
#' \describe{
#'   \item{signal}{Signal ID}
#'   \item{dt}{Date and time of event}
#'   \item{aspect}{Signal aspect caused by event}
#'   \item{past_aspect}{Signal aspect preceding event}
#' }
#'
#' @examples
#' aspect_events
#'
"aspect_events"

#' Example track events
#'
#' An example of Centrix data which have been pre-processed into track events.
#'
#' A data frame containing 25,242 rows and 4 columns.
#'
#' @format
#' \describe{
#'   \item{track}{Track ID}
#'   \item{dt}{Date and time of event}
#'   \item{occupied}{TRUE if train enters track, else FALSE}
#'   \item{event}{'enters' if train enters track, else 'vacates'}
#' }
#'
#' @examples
#' track_events
#'
"track_events"

#' Example time windows
#'
#' An example of time windows calculated from Centrix data denoting time
#' intervals of valid train journeys.
#'
#' A data frame containing 681 rows and 2 columns
#'
#' @format
#' \describe{
#'   \item{window}{Window ID}
#'   \item{interval}{Window time interval}
#' }
#'
#' @examples
#' time_windows
#'
"time_windows"

#' Example of processed aspect events
#'
#' An example of aspect events which have been fully processed and validated by
#' the calculated time windows.
#'
#' A data frame containing 25,834 rows and 6 columns
#'
#' @format
#' \describe{
#'   \item{signal}{Signal ID}
#'   \item{dt}{Date and time of event}
#'   \item{aspect}{Signal aspect caused by event}
#'   \item{past_aspect}{Signal aspect preceding event}
#'   \item{window}{Window ID}
#'   \item{interval}{Window interval}
#' }
"valid_aspect_events"

#' Example of processed track events
#'
#' An example of track events which have been fully processed and validated by
#' the calculated time windows.
#'
#' A data frame containing 24,332 rows and 6 columns
#'
#' @format
#' \describe{
#'   \item{track}{Track ID}
#'   \item{dt}{Date and time of event}
#'   \item{occupied}{TRUE if train enters track, else FALSE}
#'   \item{event}{'enters' if train enters track, else 'vacates'}
#'   \item{window}{Window ID}
#'   \item{interval}{Window interval}
#' }
"valid_track_events"

#' Example of berth-level events
#'
#' An example of processed Centrix data containing TSAR and its sub-components.
#'
#' A data frame containing 6,952 rows and 15 columns
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
#' }
#'
#' @examples
#' berth_events
#'
"berth_events"
