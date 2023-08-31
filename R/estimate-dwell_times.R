# Calculate time taken to decelerate from v to 0 with acceleration a
time_taken <- function(v, a) {
  return(v / a)
}

# Calculate distance traveled while decelerating from v to 0 with
# acceleration a
distance_travelled <- function(v, t) {
  return((v * t) / 2)
}

# Calculate time taken to travel given distance minus deceleration time
time_for_distance <- function(S, v, a) {
  t1 = S / v
  t2 = time_taken(v, 2 * a)
  return(t1 - t2)
}

# Calculate time taken to travel given distance with unknown end velocity
time_for_distance_unknown_velocity <- function(S, a) {
  v = sqrt(2 * a * S)
  return(time_taken(v, a))
}

#' @importFrom dplyr mutate case_when
calculate_dwell_times <- function(trains) {
  # Calculate moving time before reaching the station
  moving_time_one <- trains %>%
    mutate(T_brake = time_taken(v_entry, a_brake),
           S_brake = distance_travelled(v_entry, T_brake),
           T_const_one = case_when(
             S_brake >= L1 ~ 0,
             TRUE ~ (L1 - S_brake) / v_entry
           ),
           T_brake = case_when(
             T_const_one == 0 ~ time_for_distance_unknown_velocity(L1, a_brake),
             TRUE ~ T_brake
           ),
           T_move_one = T_brake + T_const_one)

  # Calculate moving time after leaving the station
  moving_time_two <- moving_time_one %>%
    mutate(T_tract = time_taken(v_exit, a_tract),
           S_tract = distance_travelled(v_exit, T_tract),
           T_const_two = case_when(
             S_tract >= L2 ~ 0,
             TRUE ~ (L2 - S_tract) / v_exit
           ),
           T_tract = case_when(
             T_const_two == 0 ~ time_for_distance_unknown_velocity(L2, a_tract),
             TRUE ~ T_tract
           ),
           T_move_two = T_const_two + T_tract)

  dwell_times <- moving_time_two %>%
    mutate(T_dwell = T_travel - T_move_one - T_move_two)

  return(dwell_times)
}

#' @importFrom dplyr inner_join left_join mutate group_by mutate ungroup select
#'   summarise if_else
preprocess_dwell_data <- function(berth_events_groups,
                                  berth_lengths,
                                  station_names,
                                  stopping_patterns,
                                  a_brake,
                                  a_tract,
                                  station_berth_lengths = NULL) {
  names <- berth_events_groups %>%
    left_join(
      station_names,
      by = "berth"
    )

  lengths <- names %>%
    left_join(
      berth_lengths,
      by = "berth"
    )

  if (is.null(station_berth_lengths)) {
    lengths <- lengths %>%
      mutate(L1 = L / 2,
             L2 = L / 2)
  } else {
    lengths <- lengths %>%
      left_join(
        station_berth_lengths,
        by = "station"
      )
  }

  speeds <- lengths %>%
    mutate(v_L = L / T_travel) %>%
    group_by(train_id) %>%
    mutate(v_entry = lag(v_L),
                  v_exit = lead(v_L)) %>%
    ungroup()

  average_speed <- speeds %>%
    select(signal, berth, station, train_id, group,
           t_enters, v_entry, v_L, v_exit) %>%
    filter(is.na(station)) %>%
    filter(grepl("stopping", group)) %>%
    summarise(median.v_L = median(v_L))

  acceleration <- speeds %>%
    mutate(a_brake = a_brake,
           a_tract = a_tract)

  dwell_data <- acceleration %>%
    mutate(v_entry = if_else(is.na(v_entry), average_speed[[1]], v_entry),
           v_exit = if_else(is.na(v_exit), average_speed[[1]], v_exit)) %>%
    select(signal, berth, station, train_id, group, t_enters,
           T_travel, v_entry, a_brake, L1, v_exit, a_tract, L2) %>%
    semi_join(stopping_patterns,
              by = c("group", "station"))

  return(dwell_data)
}

#' Estimate dwell times
#'
#' This function estimates dwell times from Centrix data
#'
#' @param berth_events_groups Berth-level Centrix data
#' @param berth_lengths Data frame with 2 columns:
#'   \itemize{
#'     \item{`berth`}{Berth ID}
#'     \item{`L`}{Berth length in metres}
#'   }
#' @param station_names Data frame with 2 columns:
#'   \itemize{
#'     \item{`berth`}{Berth ID}
#'     \item{`station`}{Station name}
#'   }
#'   This provides a 1-1 mapping from berth ID to station name. The station
#'   names should correspond to those in the timetable
#' @param stopping_patterns Data frame with 2 columns:
#'   \itemize{
#'     \item{`group`}{Group name}
#'     \item{`station`}{Station name}
#'   }
#'   This provides a list of stations that each group stops at.
#' @param a_brake Braking capacity in m/s^2
#' @param a_tract Acceleration in m/s^2
#' @param station_berth_lengths (Optional) Data frame with 3 columns:
#'   \itemize{
#'     \item{`station`}{Station name}
#'     \item{`L1`}{Distance from start of berth to station}
#'     \item{`L2`}{Distance from station to end of berth}
#'   }
#'   If station_berth_lengths is NULL (the default), then L1 and L2 will be
#'   equal to the berth length halved
#'
#' @return Data frame with 22 columns:
#'   \itemize{
#'     \item{`signal`}{Signal ID}
#'     \item{`berth`}{Berth ID}
#'     \item{`station`}{Station name}
#'     \item{`train_id`}{Train ID}
#'     \item{`group`}{Group name}
#'     \item{`t_enters`}{Date and time that train enters berth}
#'     \item{`T_travel`}{Berth travel time}
#'     \item{`v_entry`}{Velocity at entry to berth in m/s}
#'     \item{`a_brake`}{Braking capacity in m/s^2}
#'     \item{`L1`}{Distance from start of berth to station}
#'     \item{`v_exit`}{Velocity at exit of berth in m/s}
#'     \item{`a_tract`}{Acceleration in m/s^2}
#'     \item{`L2`}{Distance from station to end of berth}
#'     \item{`T_brake`}{Time spent braking}
#'     \item{`S_brake`}{Distance spent braking}
#'     \item{`T_const_one`}{Time spent moving at a constant speed before
#'                          braking for station}
#'     \item{`T_move_one`}{Total time moving before reaching station}
#'     \item{`T_tract`}{Time spent accelerating}
#'     \item{`S_tract`}{Distance spent accelerating}
#'     \item{`T_const_two`}{Time spent moving at a constant speed before
#'                          vacating berth}
#'     \item{`T_move_two`}{Total time moving after leaving station}
#'     \item{`T_dwell`}{Dwell time at station}
#'   }
#'
#' @examples
#' data(berth_events_groups, berth_lengths)
#'
#' # Set up station mapping
#' station_names <- dplyr::tribble(
#'   ~berth, ~station,
#'   "A", "geo110",
#'   "D", "geo111",
#'   "F", "geo112"
#' )
#'
#' # Specify stopping patterns
#' stopping_patterns <- dplyr::tribble(
#'   ~group, ~station,
#'   "stopping-all", "geo110",
#'   "stopping-all", "geo111",
#'   "stopping-all", "geo112",
#'   "stopping-geo112", "geo112"
#' )
#'
#' # Define braking and accelerating capacities
#' a_brake = 0.4
#' a_tract = 0.35
#'
#' # Calculate dwell times
#' dwell_times <- estimate_dwell_times(berth_events_groups,
#'                                     berth_lengths,
#'                                     station_names,
#'                                     stopping_patterns,
#'                                     a_brake,
#'                                     a_tract)
#'
#' # View dwell times
#' dwell_times
#'
#' # Summarise dwell times
#' # There are some negative values, which indicate outliers and potentially invalid observations
#' summary(dwell_times$T_dwell)
#'
#' @seealso [cluster_journeys()] [estimate_berth_lengths()] [estimate_delays()]
#'   [dwell_times]
#'
#' @export
#'
estimate_dwell_times <- function(berth_events_groups,
                                 berth_lengths,
                                 station_names,
                                 stopping_patterns,
                                 a_brake,
                                 a_tract,
                                 station_berth_lengths = NULL) {
  dwell_data <- preprocess_dwell_data(berth_events_groups,
                                      berth_lengths,
                                      station_names,
                                      stopping_patterns,
                                      a_brake,
                                      a_tract,
                                      station_berth_lengths)

  dwell_times <- calculate_dwell_times(dwell_data)

  return(dwell_times)
}
