#' @export
get_class <- function(class_name) {
  return(system.file(paste0("R/class_definitions/", class_name, ".R"),
                     package = "railvarr"))
}
