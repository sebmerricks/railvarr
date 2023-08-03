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

