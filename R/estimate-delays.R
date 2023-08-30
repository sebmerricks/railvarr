#' Calculate delays from Centrix and timetable data
#'
#' Calculate delays from estimated dwell times and timetable data. This function
#' calculates arrival delay, departure delay, excess dwell time, and whether a
#' train was early or late. These are all estimates, as the dwell times on which
#' they are based are also estimates.
#'
#' @param dwell_times Data frame containing estimated dwell times. The structure
#'   is expected to match the output of [estimate_dwell_times()]. However, only
#'   6 columns are required for this function to work:
#'   \itemize{
#'     \item{`signal`}{Signal ID}
#'     \item{`train_id`}{Train ID (Centrix)}
#'     \item{`station`}{Station name}
#'     \item{`t_enters`}{Date and time that train enters berth}
#'     \item{`T_move_one`}{Moving time before coming to a stop at the station}
#'     \item{`T_dwell`}{Dwell time at the station}
#'   }
#' @param timetable_subset Processed timetable containing calling patterns. The
#'   structure is expected to match the output of [wrangle_timetable()].
#'   However, only 7 columns are required:
#'   \itemize{
#'     \item{`train_header`}{Train ID (timetable)}
#'     \item{`dt_origin`}{Date and time of train origin}
#'     \item{`geo`}{Name of location}
#'     \item{`group`}{Group name}
#'     \item{`event`}{Event name; anything other than 'Arrive' or 'Depart' is
#'                    ignored}
#'     \item{`wtt`}{Scheduled time of event}
#'     \item{`t`}{Actual time of event}
#'   }
#' @param id_matching Data frame containing a 1-1 mapping from Centrix IDs to
#'   timetable IDs. There should be 3 columns:
#'   \itemize{
#'     \item{`train_id`}{Train ID (Centrix)}
#'     \item{`train_header`}{Train ID (timetable)}
#'     \item{`dt_origin`}{Date and time of train origin}
#'   }
#'
#' @return A data frame containing 8 columns:
#'   \itemize{
#'     \item{`signal`}{Signal ID}
#'     \item{`train_id`}{Train ID (Centrix)}
#'     \item{`station`}{Station name}
#'     \item{`T_dwell`}{Dwell time in seconds}
#'     \item{`arrival_delay`}{Arrival delay in seconds, can be negative if the
#'                            train arrives early}
#'     \item{`departure_delay`}{Departure delay in seconds, can be negative if
#'                              the train departs early}
#'     \item{`excess_dwell_time`}{Difference between actual dwell time and
#'                                scheduled dwell time. Can be negative if the
#'                                train spends less time in the station than
#'                                scheduled}
#'     \item{`early`}{Logical vector indicating whether the train arrived at the
#'                    station earlier than scheduled}
#'   }
#'
#' @examples
#' data(dwell_times, timetable_subset, id_matching)
#' dwell_times
#' timetable_subset
#' id_matching
#' delays <- estimate_delays(dwell_times, timetable_subset, id_matching)
#' delays
#'
#'
#' @importFrom dplyr filter inner_join select mutate
#' @importFrom tidyr pivot_wider
#'
#' @export
#'
estimate_delays <- function(dwell_times,
                            timetable_subset,
                            id_matching) {
  timetable_matched <- timetable_subset %>%
    filter(event %in% c("Arrive", "Depart")) %>%
    inner_join(id_matching,
               by = c("train_header", "dt_origin")) %>%
    pivot_wider(
      id_cols = c("train_header", "dt_origin", "geo", "group", "train_id"),
      names_from = "event",
      values_from = c("wtt", "t")
    )

  calculate_events <- dwell_times %>%
    select("signal", "train_id", "station", "t_enters", "T_move_one",
           "T_dwell") %>%
    mutate(t_arrive = t_enters + T_move_one,
           t_depart = t_arrive + T_dwell) %>%
    select(-"t_enters", -"T_move_one")

  events_matched <- calculate_events %>%
    inner_join(timetable_matched %>%
                rename(station = geo),
              by = c("train_id", "station"))

  delays <- events_matched %>%
    mutate(arrival_delay = t_arrive - wtt_Arrive,
           departure_delay = t_depart - wtt_Depart,
           wtt_dwell = wtt_Depart - wtt_Arrive,
           excess_dwell_time = T_dwell - wtt_dwell,
           early = arrival_delay < 0) %>%
    select("signal", "train_id", "station", "T_dwell", "arrival_delay",
           "departure_delay", "excess_dwell_time", "early")

  return(delays)
}
