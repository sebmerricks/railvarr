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

# validate_asset_map errors with appropriate messages for incorrect data structure

    Code
      validate_asset_map(bad_names)
    Error <simpleError>
      Asset map requires the columns 'signal', 'berth', 'track', and 'event'.

---

    Code
      validate_asset_map(bad_signal_type)
    Error <simpleError>
      Column 'signal' must be of type character().

---

    Code
      validate_asset_map(bad_signal_structure)
    Error <simpleError>
      Column 'signal' must match the regular expression 'S[0-9]+'
                    e.g., 'S1', 'S123'.

---

    Code
      validate_asset_map(bad_berth_type)
    Error <simpleError>
      Column 'berth' must be of type character().

---

    Code
      validate_asset_map(bad_berth_structure)
    Error <simpleError>
      Column 'berth' must match the regular expression '[A-Z]+'
                    e.g., 'A', 'ABC'.

---

    Code
      validate_asset_map(bad_track_type)
    Error <simpleError>
      Column 'track' must be of type character().

---

    Code
      validate_asset_map(bad_track_structure)
    Error <simpleError>
      Column 'track' must match the regular expression 'T[A-Z]+(-[0-9])?'
                    e.g., 'TA', 'TABC', 'TXYZ-1'.

---

    Code
      validate_asset_map(bad_event_type)
    Error <simpleError>
      Column 'event' must be of type character().

---

    Code
      validate_asset_map(bad_event_structure)
    Error <simpleError>
      Every element in column 'event' must be equal to either 'enters' or 'vacates'.

# validate_state_mapping errors with appropriate messages for incorrect data structure

    Code
      validate_state_mapping(bad_names)
    Error <simpleError>
      State mapping requires the columns 'state' and 'aspect'.

---

    Code
      validate_state_mapping(bad_state_type)
    Error <simpleError>
      Column 'state' must be of type character().

---

    Code
      validate_state_mapping(bad_aspect_type)
    Error <simpleError>
      Column 'aspect' must be of type factor().

