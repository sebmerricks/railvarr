# So far, I have only implemented a berth object and a tsar event object.
#
# The berth object combines berth-related data into a single object, improving
# general usability. Specifically, a berth object contains a berth ID, a signal
# ID, a signal aspect, a station name, and a berth length. As a pretty addition,
# signal aspects are coloured when viewed inside a tibble. This object improves
# usability, because it shrinks 5 columns into 1 without losing anything.
#
# Another example of where berth objects could be useful is in the asset map.
# The user doesn't have to worry about whether they have created their map
# correctly, because it would be impossiblet create a malformed map due to the
# validation capabilities of the objects involved. The current state of the
# berth object would be inappropriate for this, however, because it is needs an
# aspect and knows nothing about the entry and vacate events which are encoded
# in the asset map. Alternatively, the asset map could be changed to fit the
# berth object. The possibilities are endless.
#
# One thing I haven't thought about yet is how you would do something like
# filter the data to a specific station, because that station would be somewhat
# hidden away inside the berth object. In that case, you would have to create a
# new berth object to filter by, or implement a custom equality method that
# allows for testing by a specified attribute. This equality method would take
# in the berth object, the object to test against, and the field to test, and
# would return TRUE or FALSE. However, it would have to be made clear to the
# user how to use this method.
#
# The tsar event object is very useful because it shrinks 10 columns into 1. The
# constructor also calculates TSARs, which is very useful, as it means those
# calculations don't need to be part of a wider algorithm. However, where it
# becomes less useful is when you actually need to access the fields. Unlike
# berths, which are rather static, the fields in a tsar object, specifically
# T_travel, are quite important. But to access these fields you need to use the
# vctrs::field() function. Users would need to be made aware of this.
#
# However, these problems aren't necessarily important because you can learn how
# to use these objects effectively and further quality of life methods like
# custom equality methods could be implemented. Furthermore, I could see another
# potential object in a stop at a station. Called something like a dwell object.
# This would be formed of an arrival time, a departure time, and a dwell time.
# The constructor of such an object could even take in a tsar object and perform
# the dwell time estimation. This would potentially simplify the dwell time
# estimation algorithm.
#
# TL;DR
# There is a learning curve for these objects, both for the developer and the
# user. They need to be extremely well documented in order for the user to be
# able to learn how to use them effectively. However, they have the potential
# to be very powerful and they could simplify important parts of the processing
# pipeline. Furthermore, they improve the robustness of the pipeline, as they
# effectively come shipped with validation capabilities. Certainly, it is
# trivial to add validation functions, as each validation step is encapsulated
# by the object it belongs to.

library(tidyverse)
load_all()
data(berth_events_groups, asset_map)

station_map <- dplyr::tribble(
  ~berth, ~station,
  "A", "geo110",
  "D", "geo111",
  "F", "geo112"
)

# Create berth object
berths <- berth_events_groups %>%
  left_join(station_map, by = "berth") %>%
  mutate(berth = berth(berth, signal, aspect, station, NA)) %>%
  select(-signal, -aspect, -station)

berths

# Create tsar object
tsars <- berths %>%
  mutate(tsar = tsar_event(t_enters, t_red_on, t_enters_next, t_vacates,
                           t_red_off)) %>%
  select(train_id, group, berth, tsar)

# tsar shows t_enters first. TSAR is highlighted in red, T_travel is in blue.
# In order, it is printed as:
# t_enters (TSAR: T_onset T_travel T_coach T_offset)
tsars
