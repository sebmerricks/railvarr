read_rds_demo <- function(file) {
  return(read_rds(
    system.file(
      glue::glue("demos/data/id-matching/{file}.rds"),
      package = "railvarr"
    )
  ))
}
