
<!-- README.md is generated from README.Rmd. Please edit that file -->

# railvarr

<!-- badges: start -->

[![R-CMD-check](https://github.com/sebmerricks/railvarr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sebmerricks/railvarr/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of `railvarr` is to …

## Installation

You can install the development version of `railvarr` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sebmerricks/railvarr")
```

## Pipeline

`railvarr` implements 5 key processing steps for the analysis of Centrix
data.

### 1. Centrix Processing

In this step, the raw Centrix data are converted into human-readable
berth-level observations, including labeling train IDs. This step also
removes anomalies and invalid data. Up to around 5% of the data can be
lost. The main function you will need for this step is
[wrangle_centrix()](https://sebmerricks.github.io/railvarr/reference/wrangle_centrix.html).
There are three inputs for this step:

#### 1. Raw Centrix data

Centrix observations consist of four data points:

- `asset` a character string containing the asset ID
- `state` a character string representing the state of the asset
- `dt` the date and time at which the event took place
- `transition` the state transition that caused the event

For example, a train entering track `'TA-1'` on the 2nd January 2020 at
17:03 would produce the following Centrix observation:

    #> # A tibble: 1 × 4
    #>   asset state dt                  transition
    #>   <chr> <chr> <dttm>              <chr>     
    #> 1 TA-1  TR    2020-01-02 17:03:00 UP to DN

The state `'TR'` is simply a placeholder, because this observation
relates to a track asset. The state component is only relevant for
signal events, in which case it represents the signal aspect. The
transition `'UP to DN'` represents a train entering the track, while a
transition of `'DN to UP'` would represent a train exiting the track.

#### 2. Asset mapping

The asset mapping is a representation of the track section. Currently,
the track is represented by a linear data frame. However, this does not
support more complex track layouts like junctions. The asset map should
contain a 1-1 mapping from signal ID to berth name, a 1-many mapping
from berth name to track ID, and an extra `event` column. For example, a
simple track section with two berths, two signals, and three tracks
would be:

    #> # A tibble: 6 × 4
    #>   signal berth track event  
    #>   <chr>  <chr> <chr> <chr>  
    #> 1 S1     A     TA-1  enters 
    #> 2 S1     A     TA-1  vacates
    #> 3 S1     A     TA-2  enters 
    #> 4 S1     A     TA-2  vacates
    #> 5 S2     B     TB    enters 
    #> 6 S2     B     TB    vacates

The `event` column represents the fact that there are separate Centrix
observations for trains entering and exiting a track. Therefore, each
track can have both an `'enters'` and `'vacates'` event associated with
it. In order to calculate berth travel times, it is necessary to know
when a train entered the following berth. However, if we used the asset
map given above, we would not include data for the `'enters'` event at
signal `'S3'`. Therefore, we would not be able to calculate travel times
for `'S2'`. If these travel time calculations are important, it is
recommended to include the extra event in the asset map:

    #> # A tibble: 1 × 4
    #>   signal berth track event 
    #>   <chr>  <chr> <chr> <chr> 
    #> 1 S3     C     TC    enters

#### 3. State mapping

The state mapping provides a 1-1 mapping from signal state codes to
signal aspects. This is standardised, so `railvarr` provides a default
mapping that you can access by running

``` r
data(state_mapping)
state_mapping
#> # A tibble: 4 × 2
#>   state aspect
#>   <chr> <fct> 
#> 1 RGE   R     
#> 2 HGE   Y     
#> 3 HHGE  YY    
#> 4 DGE   G
```

See [this
website](https://wiki.openraildata.com/index.php/Signalling_Nomenclature#Signals)
for other codes.

### 2. Centrix Clustering

Raw Centrix data contains no information about train types or stopping
patterns. However, knowledge of these stopping patterns is useful for
analysis and it is pre-requisite to the ID matching process. `railvarr`
provides functionality for clustering by travel time. Specifically,
K-Means Clustering is used. Currently, this implementation does not
support outlier detection. However, you can perform your own outlier
detection and give a list of outliers to the clustering function in
order to remove those observations from the analysis. See
[cluster_journeys()](https://sebmerricks.github.io/railvarr/reference/cluster_journeys.html)
for more information.

### 3. Timetable Processing

This step filters the timetable to only include relevant services,
travelling in the correct direction. It also calculates and attaches
calling patterns. The main `railvarr` function you will need for this is
[wrangle_timetable()](https://sebmerricks.github.io/railvarr/reference/wrangle_timetable.html).
Three inputs are required:

#### 1. Raw Timetable

Your timetable must match the expected structure. This consists of 8
data points:

- `train_header` the train identifier, usually some from of
  [headcode](https://www.rail-record.co.uk/understanding-a-headcode/)
- `dt_origin` the date and time at which the train originated; this
  forms a train’s unique identifier when combined with the
  `train_header`
- `geo` the location at which the timetable event is taking place,
  usually stations and TIPLOcs like junctions
- `event` the type of event; this can only be one of the following:
  `"Pass"`, `"Arrive"`, `"Depart"`, `"Originate"`, `"Terminate"`
- `wtt` the scheduled date and time of the event
- `t` the actual date and time of the event
- `delay` the difference between the actual time and the scheduled time
  of the event
- `allow` the timetable delay allowance; this indicates whether delays
  should be expected

For example, a train with the headcode `'1A23'` originating at 14:17 on
the 23rd May 2020 could have the following timetable entry for the
station `'Example'`

    #> # A tibble: 2 × 8
    #>   train_header dt_origin           geo     event  wtt                
    #>   <chr>        <dttm>              <chr>   <chr>  <dttm>             
    #> 1 1A23         2020-05-23 14:17:00 Example Arrive 2020-05-23 14:37:00
    #> 2 1A23         2020-05-23 14:17:00 Example Depart 2020-05-23 14:37:30
    #> # ℹ 3 more variables: t <dttm>, delay <dbl>, allow <dbl>

This timetable entry indicates that the train arrived at the station 1
minute early and departed 30 seconds early. However, it is important to
note that timings in the timetable are usually rounded, either to the
nearest 15 or 30 seconds.

#### 2. Stations

You should provide a list of stations that you are interested in. For
example, a train may stop at many stations on its journey, but your
Centrix data only covers a small subset of that journey. Therefore, you
should provide a list of stations that completely encompasses your
Centrix data. This ensures that all relevant services are included in
the processed timetable. However, you may need to ensure that no
irrelevant services are accidentally included. If you are using
[wrangle_timetable()](https://sebmerricks.github.io/railvarr/reference/wrangle_timetable.html),
the order in which you define this list is important, because it
specifies the direction in which trains should be travelling. This
allows for the filtering of services by direction. You can avoid this by
using the lower level functions directly, but these functions do not
currently come with input validation, so discretion is advised.

#### 3. Stopping Stations

This is a list of only those stations which are covered by the Centrix
data. For example, imagine you have the following track layout:

\|——–$$Stop 1$$——–(Centrix start)\|———-$$Stop 2$$———–\|(Centrix
end)———–$$Stop 3$$————\|

The first list should include all three stations in order to capture all
trains which use the track included in the Centrix data. However,
because only Station 2 is actually included in the Centrix data, this
second list should contain only this station. This second list is used
to calculate calling patterns, so any trains which do not stop at any of
the stations in the list will be labeled as `'fast'` trains. In the
above example, trains which stop at Station 1 and Station 3, but not
Station 2, would be labeled as `'fast'`.

### 4. ID Matching

The ID matching algorithm matches Centrix IDs with corresponding
timetable IDs. This is necessary because raw Centrix data contain no
identification information. Furthermore, there can be mismatches, where
Centrix says there are trains present when the timetable doesn’t, and
vice-versa. An extra input is required here, containing instructions for
the matching process. For more details, see
[match_ids()](https://sebmerricks.github.io/railvarr/reference/match_ids.html)
and the [ID Matching section of the Get Started
page](https://sebmerricks.github.io/railvarr/articles/railvarr.html#id-matching).

### 5. Dwell Time and Delays

Before calculating dwell times, you need to know the length of each
berth. This could come from a data source, but we have found that such
data is not always reliable. Therefore, `railvarr` provides
functionality for estimating berth lengths. This can be done using the
[estimate_berth_lengths()](https://sebmerricks.github.io/railvarr/reference/estimate_berth_lengths.html)
function. This function first requires running the processed timetable
through the
[calculate_journey_specifications()](https://sebmerricks.github.io/railvarr/reference/calculate_journey_specifications.html).
In order to estimate berth lengths, you must also have an estimate for
the total length of the track. This can be done by, for example,
measuring it on google maps. See the [berth lengths section of the Get
Started
page](https://sebmerricks.github.io/railvarr/articles/railvarr.html#berth-lengths).

Once you are in possession of berth lengths, you can calculate dwell
times using the
[estimate_dwell_times()](https://sebmerricks.github.io/railvarr/reference/estimate_dwell_times.html)
function. For more details, see the [dwell times section of the Get
Started
page](https://sebmerricks.github.io/railvarr/articles/railvarr.html#dwell-times-1).
Once you have dwell times, you can then [estimate
delays](https://sebmerricks.github.io/railvarr/articles/railvarr.html#delays)

<figure>
<img src="man/figures/README-pipeline.PNG"
alt="Diagram of the processing pipeline that railvarr provides" />
<figcaption aria-hidden="true">Diagram of the processing pipeline that
<code>railvarr</code> provides</figcaption>
</figure>

# Example Data

`railvarr` comes with some example data to help get you started. To see
a list of these data sets head to [this
page](https://sebmerricks.github.io/railvarr/reference/index.html#data).
Each data set represents a separate stage of the processing pipeline.
