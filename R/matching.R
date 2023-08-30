#' Match Centrix and Timetable IDs
#'
#' @description `match_ids()` matches Centrix observations with timetabling
#' information. The result is a data set that contains a 1-1 mapping between
#' Centrix IDs and timetable IDs. This is a necessary step because the timings
#' used in the timetable are rounded to the nearest 30 seconds, resulting in much
#' lower granularity. There may also be other inaccuracies with the timetable.
#' This function results in data loss, the amount of which depends on how
#' accurate the matching needs to be.
#'
#' @details There are two methods for matching Centrix data with timetable data.
#' The method that is used depends on whether the train is a fast train (does not
#' stop at any stations present in the Centrix data set) or not.
#'
#' For fast trains, matches must be found at the nearest TIPLOC, e.g., a junction
#' preceding the Centrix track section. For these trains, the `lb` and `ub`
#' columns of the `match_mapping` control what is considered a match. If the
#' train passed through the junction at time `t_Pass` and the train entered the
#' Centrix track at time `t_enters`, a match is made if: \cr
#' `t_Pass > t_enters + lb & t_Pass < t_enters + ub` \cr
#' I.e., a match is made if the train passed through the junction less than `lb`
#' seconds before `t_enters` (assuming that `lb` is negative) and if the train
#' passed through the junction earlier than `t_enters + ub`. Thus, `lb` and `ub`
#' define a time window based on `t_enters` in which `t_Pass` must occur for a
#' match to be made.
#'
#' For stopping trains, matches can be made at stations, which is conceptually
#' simpler. Let's take the example of a train which stops at only one station.
#' The train enters the station's berth at time `t_enters`, arrives at the
#' station at `t_Arrive`, departs the station at `t_Depart` and vacates the
#' berth at time `t_vacates`. A match is made if: \cr
#' `t_enters + lb < t_Arrive & t_Depart < t_vacates + ub` \cr
#' I.e., a match is made if the train arrives at the station
#' after it enters the berth and if the train departs the station before it
#' vacates the berth. Similar to the fast trains, `t_enters` and `t_vacates`
#' define a time window in which `t_Arrive` and `t_Depart` must occur for a
#' match to be made. This window can be adjusted using `lb` and `ub`.
#'
#' @param berth_events_groups Data frame containing berth-level Centrix data.
#'   The structure is expected to conform to the following:
#'   \itemize{
#'     \item{`berth`}{[character()] Berth ID}
#'     \item{`train_id`}{[numeric()] Train ID}
#'     \item{`t_enters`}{[lubridate::POSIXct()] Date and time that the train
#'                       enters the berth}
#'     \item{`t_vacates`}{[lubridate::POSIXct()] Date and time that the train
#'                        vacates the berth}
#'     \item{`group`}{[character()] Group name, corresponding to the train's
#'                    calling pattern}
#'   }
#' @param timetable_groups Data frame containing Timetable data. The structure
#'   is expected to conform to the following:
#'   \itemize{
#'     \item{`train_header`}{[character()] Train ID}
#'     \item{`dt_origin`}{[lubridate::POSIXct()] Date and time that the service
#'                        originated}
#'     \item{`group`}{[character()] Group name, corresponding to the train's
#'                    calling pattern}
#'     \item{`geo`}{[character()] Name of TIPLOCs that trains pass through or
#'                  stop at}
#'     \item{`event`}{[character()] Type of timetable event, every element must
#'                    be one of 'Arrive', 'Depart', or 'Pass'}
#'     \item{`wtt`}{[lubridate::POSIXct()] Scheduled date and time of event}
#'     \item{`t`}{[lubridate::POSIXct()] Actual date and time of event}
#'   }
#' @param match_mapping Data frame containing instructions for the matching. It
#'   should match the following structure:
#'   \itemize{
#'     \item{`group`}{[character()] Group name, corresponding to the train's
#'                    calling pattern}
#'     \item{`berth`}{[character()] Berth ID}
#'     \item{`geo`}{[character()] Name of TIPLOC(s) or station(s) to match at}
#'     \item{`lb`}{[numeric()] Number of seconds to shift the start of the
#'                 matching window}
#'     \item{`ub`}{[numeric()] Number of seconds to shift the end of the
#'                 matching window}
#'   }
#'
#' @returns Data frame containing a 1-1 mapping between Centrix IDs and Timetable
#'   IDs.
#'
#' @examples
#' # Define the matching instructions
#' data(berth_events_groups, timetable_subset)
#' match_mapping <- dplyr::tribble(
#'   ~group, ~berth, ~geo, ~lb, ~ub,
#'   # match fast trains at berth "A" and geo "geo6"
#'   # move start of window 240 seconds before `t_enters`
#'   # move end of window 60 seconds after `t_enters`
#'   "fast", "A", "geo6", -240, 60,
#'   # match stopping-all trains at berths "A","D","F", and geos "geo110","geo111","geo112"
#'   # don't adjust window
#'   "stopping-all", "A", "geo110", 0, 0,
#'   "stopping-all", "D", "geo111", 0, 0,
#'   "stopping-all", "F", "geo112", 0, 0
#' )
#'
#' timetable_groups <- dplyr::filter(timetable_subset,
#'                                   event %in% c("Arrive", "Depart", "Pass"))
#'
#' matched_ids <- match_ids(berth_events_groups, timetable_groups, match_mapping)
#' matched_ids
#'
#' match_mapping <- dplyr::tribble(
#'   ~group, ~berth, ~geo, ~lb, ~ub,
#'   "fast", "A", "geo6", -240, 60,
#'   # move start of window for stopping-all trains to 60 seconds before `t_enters`
#'   "stopping-all", "A", "geo110", -60, 0,
#'   "stopping-all", "D", "geo111", -60, 0,
#'   "stopping-all", "F", "geo112", -60, 0
#' )
#' wider_window <- match_ids(berth_events_groups, timetable_groups, match_mapping)
#' # More matches are found with the wider window
#' wider_window
#'
#' @export
#'
match_ids <- function(berth_events_groups, timetable_groups, match_mapping) {
  validate_berth_matching(berth_events_groups)
  validate_timetable_matching(timetable_groups)
  validate_match_mapping(match_mapping)

  berth_matching <- preprocess_berth_matching(berth_events_groups,
                                              match_mapping)

  timetable_matching <- preprocess_timetable_matching(timetable_groups,
                                                      match_mapping)

  return(match_observations(berth_matching, timetable_matching))
}

#' @importFrom dplyr filter select inner_join anti_join mutate if_else arrange
#'   group_by distinct
match_observations <- function(berth_matching, timetable_matching) {
  pair_start <- berth_matching %>%
    select(-"end_hour") %>%
    inner_join(timetable_matching %>%
                 select(-"end_hour"),
               by = c("group", "berth", "lb", "ub", "date", "start_hour", "geo"),
               relationship = "many-to-many")

  pair_end <- berth_matching %>%
    select(-"start_hour") %>%
    inner_join(timetable_matching %>%
                 select(-"start_hour"),
               by = c("group", "berth", "lb", "ub", "date", "end_hour", "geo"),
               relationship = "many-to-many")

  paired <- bind_rows(
    pair_start,
    pair_end %>% anti_join(pair_start,
                           by = c("train_id", "train_header", "dt_origin"))
  )

  matched <- paired %>%
    mutate(is_match = if_else(
      is.na(.data$t_Pass),
      (.data$t_enters + .data$lb < .data$t_Arrive &
         .data$t_vacates + .data$ub > .data$t_Depart),
      (.data$t_enters + .data$lb < .data$t_Pass &
         .data$t_enters + .data$ub > .data$t_Pass)
    )) %>%
    select("train_id", "train_header", "dt_origin", "berth", "geo", "t_enters",
           "t_Pass", "t_Arrive", "t_Depart", "t_vacates", "lb", "ub",
           "is_match") %>%
    arrange(.data$train_id, .data$dt_origin, .data$train_header) %>%
    group_by(.data$train_id, .data$train_header, .data$dt_origin) %>%
    filter(any(.data$is_match)) %>%
    distinct(.data$train_id, .data$train_header, .data$dt_origin) %>%
    ungroup()

  return(matched)
}
