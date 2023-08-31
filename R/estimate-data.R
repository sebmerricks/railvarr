#' Example berth lengths
#'
#' An example of a data frame containing estimated berth lengths.
#'
#' @seealso [estimate_berth_lengths()]
#'
#' A data frame containing 8 rows and 6 columns
#'
#' @format
#' \describe{
#'   \item{berth}{Berth ID}
#'   \item{median_travel}{Median travel time for each berth}
#'   \item{v.mph}{Expected speed across berth}
#'   \item{L.miles}{Estimated berth length in miles}
#'   \item{L.km}{Estimated berth length in kilometres}
#'   \item{L}{Estimated berth length in metres}
#' }
#'
#' @examples
#' berth_lengths
#'
"berth_lengths"

#' Example dwell times
#'
#' An example of a data frame containing estimated dwell times
#'
#' @seealso [estimate_dwell_times()]
#'
#' A data frame containing 1,581 rows and 22 columns
#'
#' @format
#' \describe{
#'   \item{signal}{Signal ID}
#'   \item{berth}{Berth ID}
#'   \item{station}{Station name}
#'   \item{train_id}{Train ID}
#'   \item{group}{Group name}
#'   \item{t_enters}{Date and time that train enters berth}
#'   \item{T_travel}{Berth travel time}
#'   \item{v_entry}{Velocity at entry to berth in m/s}
#'   \item{a_brake}{Braking capacity in m/s^2}
#'   \item{L1}{Distance from start of berth to station}
#'   \item{v_exit}{Velocity at exit of berth in m/s}
#'   \item{a_tract}{Acceleration in m/s^2}
#'   \item{L2}{Distance from station to end of berth}
#'   \item{T_brake}{Time spent braking}
#'   \item{S_brake}{Distance spent braking}
#'   \item{T_const_one}{Time spent moving at a constant speed before
#'                        braking for station}
#'   \item{T_move_one}{Total time moving before reaching station}
#'   \item{T_tract}{Time spent accelerating}
#'   \item{S_tract}{Distance spent accelerating}
#'   \item{T_const_two}{Time spent moving at a constant speed before
#'                        vacating berth}
#'   \item{T_move_two}{Total time moving after leaving station}
#'   \item{T_dwell}{Dwell time at station}
#' }
#'
#' @examples
#' dwell_times
#'
"dwell_times"

#' Example delays
#'
#' An example of a data frame containing estimated delays
#'
#' @seealso [estimate_delays()]
#'
#' A data frame containing 66 row and 8 columns
#'
#' @format
#' \describe{
#'   \item{`signal`}{Signal ID}
#'   \item{`train_id`}{Train ID (Centrix)}
#'   \item{`station`}{Station name}
#'   \item{`T_dwell`}{Dwell time in seconds}
#'   \item{`arrival_delay`}{Arrival delay in seconds, can be negative if the
#'                          train arrives early}
#'   \item{`departure_delay`}{Departure delay in seconds, can be negative if
#'                            the train departs early}
#'   \item{`excess_dwell_time`}{Difference between actual dwell time and
#'                              scheduled dwell time. Can be negative if the
#'                              train spends less time in the station than
#'                              scheduled}
#'   \item{`early`}{Logical vector indicating whether the train arrived at the
#'                  station earlier than scheduled}
#' }
#'
#' @examples
#' delays
#'
"delays"
