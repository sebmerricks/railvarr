slope <- function(T_travel, previous_travel) {
  return((T_travel / previous_travel) - 1)
}

#' @importFrom dplyr filter select group_by mutate ungroup
kmeans_clusters <- function(berth_events, ...) {
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
    stats::kmeans(...)

  return(list(
    "clusters" = broom::augment(out, kmeans_input) %>% select("train_id", ".cluster"),
    "centroids" = broom::tidy(out)
  ))
}

#' Cluster Centrix Data
#'
#' Clusters Centrix data into groups based on travel times across berths.
#'
#' @param berth_events A data frame containing berth-level Centrix events
#'   containing the columns: signal, berth, train_id, aspect, T_travel,
#'   time_elapsed.
#' @param outliers A data frame containing the train IDs for any outliers. Only
#'   used if `outlier_detection` == "manual".
#' @param outlier_detection Which approach to use for outliers. Options are:
#' \itemize{
#'   \item{none} No outliers removed.
#'   \item{manual} Manually remove outliers using the `outliers` parameter.
#'   \item{boxplot} Unimplemented.
#' }
#' @inheritDotParams stats::kmeans
#'
#' @return A data frame containing train IDs and their clusters. Clusters are
#'   ordered based on total variance, highest variance first.
#'
#' @importFrom dplyr group_by mutate first select ungroup anti_join
#'
#' @export
cluster_centrix <- function(berth_events,
                            outliers = NULL,
                            outlier_detection = "none",
                            ...) {
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

  clusters <- kmeans_clusters(berth_events_slopes, ...)

  new_order <- clusters$centroids %>%
    tidyr::pivot_longer(
      cols = starts_with("m")
    ) %>%
    group_by(cluster) %>%
    summarise(variance = var(value)) %>%
    ungroup() %>%
    arrange(desc(variance)) %>%
    mutate(new_cluster = row_number())

  new_clusters <- clusters$clusters %>%
    rename(cluster = .cluster) %>%
    left_join(new_order, by = "cluster") %>%
    select("train_id", "new_cluster") %>%
    rename(cluster = new_cluster)

  berth_events_clusters <- berth_events %>%
    select("signal", "berth", "train_id", "aspect", "T_travel") %>%
    inner_join(new_clusters, by = "train_id")

  return(berth_events_clusters)
}

#' @import ggplot2
#' @export
plot_cluster_events <- function(cluster_events) {
  pclusters <- cluster_events %>%
    group_by(train_id) %>%
    mutate(x = row_number()) %>%
    ungroup() %>%
    ggplot() +
    geom_line(
      aes(x = signal, y = T_travel, group = train_id),
      alpha = .5,
      colour = "grey50"
    ) +
    ylab("Travel time") +
    xlab("Signal") +
    facet_wrap(vars(cluster), ncol = 2) +
    theme_bw()
  pclusters
}
