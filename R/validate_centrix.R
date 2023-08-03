validate_centrix <- function(centrix) {
  stop_if_not(all(c("asset", "dt", "transition") %in% names(centrix)),
              msg = "Centrix data requires the columns 'asset', 'dt', and 'transition'.")
  assets <- centrix$asset
  stop_if_not(is.character(assets),
              msg = "Column 'asset' must be of type character().")
  stop_if_not(all(!is.na(
    stringr::str_extract(assets, "(S[0-9]+\\s[A-Z]+)|(T[A-Z]+(-[0-9])?)")
  )), msg = "Column 'asset' must match the regular expression
    '(S[0-9]+\\s[A-Z]+)|(T[A-Z]+(-[0-9])?)'
    e.g., 'S123 RGE', 'S123 HHGE', 'TABC-1', 'TXYZ'")
  dts <- centrix$dt
  stop_if_not(lubridate::is.POSIXct(dts),
              msg = "Column 'dt' must be of type lubridate::POSIXct().")
  transitions <- centrix$transition
  stop_if_not(is.character(transitions),
              msg = "Column 'transition' must be of type character().")
  stop_if_not(all(transitions %in% c("DN to UP", "UP to DN")),
              msg = "Every element in column 'transition' must be equal to either 'DN to UP' or 'UP to DN'.")
}
