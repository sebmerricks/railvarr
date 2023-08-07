slope <- function(T_travel, previous_travel) {
  return((T_travel / previous_travel) - 1)
}

#' @importFrom dplyr filter select group_by mutate ungroup
kmeans_clusters <- function(berth_events,
                            k = 3L,
                            niter = 20L) {
  kmeans_input <- berth_events %>%
    filter(!is.na(.data$m)) %>%
    select("train_id", "m") %>%
    group_by(.data$train_id) %>%
    mutate(name = row_number()) %>%
    ungroup() %>%
    tidyr::pivot_wider(
      id_cols = train_id,
      names_prefix = "m",
      values_from = "m",
      names_from = "name"
    )

  out <- kmeans_input %>%
    select(-"train_id") %>%
    stats::kmeans(centers = k, iter.max = niter, nstart = 1)

  return(list(
    "clusters" = broom::augment(out, kmeans_input) %>% select("train_id", ".cluster"),
    "centroids" = broom::tidy(out)
  ))
}

#' @importFrom dplyr group_by mutate first select ungroup anti_join
#' @export
cluster_centrix <- function(berth_events,
                            outliers = NULL,
                            outlier_detection = "none",
                            k = 3L,
                            niter = 20L) {
  berth_events_slopes <- berth_events %>%
    group_by(.data$train_id) %>%
    mutate(
      t_origin = first(.data$t_enters),
      time_elapsed = as.integer(.data$t_enters - .data$t_origin)
    ) %>%
    select("signal", "berth", "train_id", "aspect", "T_travel", "time_elapsed") %>%
    mutate(m = purrr::pmap_dbl(list(.data$T_travel, lag(.data$T_travel)), slope)) %>%
    ungroup()

  if (outlier_detection == "manual" & !is.null(outliers)) {
    berth_events_slopes <- berth_events_slopes %>%
      anti_join(outliers, by = "train_id")
  } else if (outlier_detection == "boxplot") {
    # boxplot detection (threshold)
  }

  clusters <- kmeans_clusters(berth_events_slopes, k = k, niter = niter)

  berth_events_clusters <- berth_events %>%
    select("signal", "berth", "train_id", "aspect", "T_travel") %>%
    inner_join(clusters$clusters, by = "train_id")

  return(berth_events_clusters)
}
