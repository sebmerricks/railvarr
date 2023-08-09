save_pclusters <- function(pclusters) {
  path <- tempfile(fileext = ".jpg")
  ggsave(
    path,
    pclusters,
    device = "jpeg",
    width = 7,
    height = 7
  )
  return(path)
}

expect_snapshot_plot <- function(name, pclusters) {
  name <- paste0(name, ".jpg")
  announce_snapshot_file(name = name)
  path <- save_pclusters(pclusters)
  expect_snapshot_file(path, name)
}

test_that("cluster_centrix works", {
  skip_on_cran()
  skip_on_ci()
  skip_if(is_checking())
  berth_events <- read_rds_test("berth_events.rds")
  cluster_events <- cluster_centrix(berth_events, k = 2L) %>%
    dplyr::filter(!is.na(.data$T_travel))
  pclusters <- plot_cluster_events(cluster_events)
  expect_snapshot_plot("pclusters", plot_cluster_events(cluster_events))
})
