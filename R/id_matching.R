preprocess_berths <- function(berth_events, train_classes) {
  berth_groups <- berth_events %>%
    inner_join(
      train_classes %>%
        select(train_id, group),
      by = "train_id"
    )

  berth_groups <- berth_groups %>%
    group_by(.data$train_id) %>%
    mutate(day = lubridate::date(first(.data$t_enters)),
           start_hour = lubridate::hour(first(.data$t_enters)),
           end_hour = lubridate::hour(last(.data$t_enters))) %>%
    ungroup()

  return(berth_groups)
}

preprocess_timetable <- function(timetable) {
  timetable_groups <- timetable %>%
    filter(!is.na(.data$t)) %>%
    group_by(.data$train_header) %>%
    mutate(day = lubridate::date(first(.data$t))) %>%
    mutate(start_hour = lubridate::hour(first(.data$t)),
           end_hour = lubridate::hour(last(.data$t))) %>%
    ungroup() %>%
    arrange(.data$dt_origin)

  return(timetable_groups)
}

fuzzy_less_than <- function(observed, rounded, tolerance = 1) {
  rounded.UB <- rounded + 29 * tolerance
  return(observed < rounded.UB)
}

fuzzy_greater_than <- function(observed, rounded, tolerance = 1) {
  rounded.LB <- rounded - 30 * tolerance
  return(observed > rounded.LB)
}

fuzzy_equals <- function(observed, rounded, tolerance = 1) {
  rounded.LB <- rounded - 30 * tolerance
  rounded.UB <- rounded + 29 * tolerance
  return(observed > rounded.LB & observed < rounded.UB)
}

pair_observations <- function(observed, timetable) {
  #is.null(get_station_mapping())?

  # Identify Duplicates
  duplicates <- timetable %>%
    group_by(.data$train_header, .data$geo, .data$event) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(.data$n > 1)
  # Remove Duplicates
  timetable <- timetable %>%
    anti_join(duplicates, by = "train_header")

  timetable_wider <- timetable %>%
    select("train_header", "geo", "wtt", "t", "event") %>%
    filter(.data$event %in% c("Pass", "Arrive", "Depart")) %>%
    pivot_wider(
      id_cols = c("train_header", "geo"),
      values_from = c("wtt", "t"),
      names_from = "event"
    ) %>%
    left_join(timetable %>%
                select("train_header", "geo", "day", "start_hour", "end_hour",
                       "group") %>%
                group_by(.data$train_header, .data$geo) %>%
                filter(row_number() == 1) %>%
                ungroup(),
              by = c("train_header", "geo"))

  aligned_observations <- observed %>%
    semi_join(timetable,
              by = c("day", "start_hour")) %>%
    semi_join(timetable,
              by = c("day", "end_hour")) %>%
    inner_join(get_network_map() %>%
                 select("signal", "geo"),
               by = "signal",
               relationship = "many-to-many") %>%
    left_join(timetable_wider %>%
                select(-"end_hour", -"group"),
              by = c("geo", "day", "start_hour"),
              relationship = "many-to-many") %>%
    left_join(timetable_wider %>%
                select(-"start_hour", -"group"),
              by = c("geo", "day", "end_hour"),
              relationship = "many-to-many") %>%
    pivot_longer(
      cols = matches("\\w\\W\\w"),
      names_to = c(".value", "name"),
      names_sep = -2L,
      names_repair = "minimal",
    ) %>%
    select("signal", "geo", "train_id", "train_header", "t_enters", "t_vacates",
           "t_Arrive", "t_Depart", "t_Pass", "group") %>%
    filter(!is.na(.data$train_id) & !is.na(.data$train_header)) %>%
    arrange(.data$train_id, .data$train_header, .data$t_enters) %>%
    group_by(.data$train_id, .data$train_header, .data$geo) %>%
    filter(row_number() == 1) %>%
    ungroup()

  paired_observations <- aligned_observations
  #    group_by(.data$train_id, .data$train_header, .data$geo) %>%
  #    mutate(geo = str_split_fixed(.data$geo, " |-", n = 3)[[1]])

  return(paired_observations)
}

match_pass <- function(t_enters, t_vacates, t_Pass, lb, ub,
                       operators, fuzzy_tolerance, tolerance) {
  if (operators == "fuzzy") {
    return(fuzzy_less_than(t_enters + tolerance*lb, t_Pass, fuzzy_tolerance) &
             fuzzy_greater_than(t_vacates + tolerance*ub, t_Pass, fuzzy_tolerance))
  } else {
    return(t_enters + tolerance*lb < t_Pass & t_vacates + tolerance*ub > t_Pass)
  }
}

match_stop <- function(t_enters, t_vacates, t_Arrive, t_Depart, lb, ub,
                       operators, fuzzy_tolerance, tolerance) {
  if (operators == "fuzzy") {
    return(fuzzy_less_than(t_enters + tolerance*lb, t_Arrive, fuzzy_tolerance) &
             fuzzy_greater_than(t_vacates + tolerance*ub, t_Depart,
                                fuzzy_tolerance))
  } else {
    return(t_enters + tolerance*lb < t_Arrive &
             t_vacates + tolerance*ub > t_Depart)
  }
}

match_group <- function(observed, timetable, match_map,
                        operators, fuzzy_tolerance, tolerance) {
  paired_observations <- pair_observations(observed, timetable)

  match_at <- paired_observations %>%
    inner_join(match_map, by = "geo")

  is_matched <- match_at %>%
    mutate(is_match = if_else(
      .data$event == "Pass",
      match_pass(.data$t_enters, .data$t_vacates, .data$t_Pass, .data$lb,
                 .data$ub, operators, fuzzy_tolerance, tolerance),
      match_stop(.data$t_enters, .data$t_vacates, .data$t_Arrive,
                 .data$t_Depart, .data$lb, .data$ub, operators, fuzzy_tolerance,
                 tolerance)
    )) %>%
    select(train_id, train_header, t_enters, t_vacates, t_Pass, t_Arrive,
           t_Depart, is_match, group)

  matched <- is_matched %>%
    filter(is_match) %>%
    group_by(.data$train_id) %>%
    mutate(tdiff = .data$t_Pass - .data$t_enters) %>%
    slice_min(order_by = .data$tdiff) %>%
    group_by(.data$train_header) %>%
    slice_min(order_by = .data$tdiff) %>%
    group_by(.data$train_id) %>%
    slice_min(order_by = .data$train_header) %>%
    arrange(.data$train_id) %>%
    select(-"is_match", -"tdiff") %>%
    distinct(train_id, train_header, .keep_all = TRUE)

  return(matched)
}

match_ids <- function(berth_groups,
                      timetable_groups,
                      group_map,
                      operators = "fuzzy",
                      fuzzy_tolerance = 1,
                      tolerance = 1) {
  matched_ids <- dplyr::tribble(~train_id, ~train_header)

  for (g in names(group_map)) {
    observed_group <- berth_groups %>%
      filter(.data$group == g)
    timetable_group <- timetable_groups %>%
      filter(.data$group == g)

    matched <- match_group(observed_group, timetable_group, group_map[[g]],
                           operators, fuzzy_tolerance, tolerance)

    matched_ids <- dplyr::bind_rows(matched_ids, matched)
  }

  return(matched_ids %>% arrange(train_id))
}

combine_ids <- function(ids, observed, timetable) {
  duplicates <- timetable %>%
    group_by(train_header, geo, event) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n > 1)

  ids_together <- timetable %>%
    select(train_header, geo, t, event) %>%
    anti_join(duplicates, by = c("train_header", "geo", "event")) %>%
    filter(event %in% c("Pass", "Arrive", "Depart")) %>%
    pivot_wider(
      id_cols = c("train_header", "geo"),
      values_from = "t",
      names_from = "event",
      names_glue = "t_{.name}"
    ) %>%
    inner_join(ids %>%
                 select("train_id", "train_header"),
               by = "train_header") %>%
    inner_join(observed %>%
                 select(train_id, signal, t_enters) %>%
                 inner_join(get_station_mapping(),
                            by = "signal",
                            relationship = "many-to-many"),
               by = c("geo", "train_id")) %>%
    arrange(train_id) %>%
    mutate(t = if_else(is.na(t_Pass), t_Arrive, t_Pass)) %>%
    select(train_id, train_header, geo, t_enters, t) %>%
    left_join(train_classes %>%
                select(train_id, group),
              by = "train_id")
  return(ids_together)
}

accuracy <- function(ids) {
  ids <- ids %>%
    mutate(tdiff = as.integer(.data$t_enters - .data$t),
           tdiff2 = .data$tdiff * .data$tdiff)

  ma = summary(ids$tdiff)[["Mean"]]
  msa = summary(ids$tdiff2)[["Mean"]]

  print(glue::glue(
    "Mean Accuracy: {ma}s      Mean Squared Accuracy: {msa}s^2"
  ))
}
