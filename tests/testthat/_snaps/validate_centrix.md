# validate_centrix errors with appropriate messages for incorrect data structure

    Code
      validate_centrix(bad_names)
    Error <simpleError>
      Centrix data requires the columns 'asset', 'dt', and 'transition'.

---

    Code
      validate_centrix(bad_asset_type)
    Error <simpleError>
      Column 'asset' must be of type character().

---

    Code
      validate_centrix(bad_asset_structure)
    Error <simpleError>
      Column 'asset' must match the regular expression
          '(S[0-9]+\s[A-Z]+)|(T[A-Z]+(-[0-9])?)'
          e.g., 'S123 RGE', 'S123 HHGE', 'TABC-1', 'TXYZ'

---

    Code
      validate_centrix(bad_dt_type)
    Error <simpleError>
      Column 'dt' must be of type lubridate::POSIXct().

---

    Code
      validate_centrix(bad_transition_type)
    Error <simpleError>
      Column 'transition' must be of type character().

---

    Code
      validate_centrix(bad_transition_structure)
    Error <simpleError>
      Every element in column 'transition' must be equal to either 'DN to UP' or 'UP to DN'.

