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

