new_dwell_event <- function(t_arrive = lubridate::POSIXct(),
                            t_depart = lubridate::POSIXct(),
                            T_dwell = lubridate::duration()) {
  if (!lubridate::is.POSIXct(t_arrive))
    rlang::abort("`t_arrive` must be a POSIXct object")
  if (!lubridate::is.POSIXct(t_depart))
    rlang::abort("`t_arrive` must be a POSIXct object")
  if (!lubridate::is.duration(T_dwell))
    rlang::abort("`t_arrive` must be a duration")

  vctrs::new_rcrd(list(t_arrive = t_arrive,
                       t_depart = t_depart,
                       T_dwell = T_dwell
                       ),
                  class = "railvarr_dwell")
}

#' @importFrom vctrs field
#' @importFrom dplyr lag lead
dwell_event <- function(berth = berth(),
                        tsar = tsar_event(),
                        a_brake = double(),
                        a_tract = double()) {
  # tsar_event: t_enters t_red_on t_enters_next t_red_off t_vacates
  # tsar_event: TSAR T_onset T_travel T_coach T_offset
  # calculate dwell times from tsar_event

  # next steps
  # implement dwell time calculations
  # once that's done, create a function that takes trains and berths and marks which ones are valid for dwell time calculation
  # i.e., a function that detects and marks stopping trains at the berths at which they stop
  # so, I give it a train with group='stopping' and stops='c,e' along with every event along its journey,
  # this function returns only those events where the trains stop, so at stations 'c' and 'e'

  v_L <- field(berth, "length") / field(tsar, "T_travel")
  v_entry <- lag(v_L)
  v_exit <- lead(v_L)

  # calculate v_entry
  # parameter a_brake
  # parameter L1
  # calculate v_exit
  # parameter a_tract
  # parameter L2
}

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
