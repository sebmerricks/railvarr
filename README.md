<!-- README.md is generated from README.Rmd. Please edit that file -->

# railvarr

<!-- badges: start -->

[![R-CMD-check](https://github.com/sebmerricks/railvarr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sebmerricks/railvarr/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of `railvarr` is to ...

## Installation

You can install the development version of railvarr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sebmerricks/railvarr")
```

## Pipeline

`railvarr` implements 5 key processing steps for the analysis of Centrix data.

### 1. Centrix Processing

In this step, the raw Centrix data are converted into human-readable berth-level observations. This step also removes anomalies and invalid data. Up to around 5% of the data can be lost. There are three inputs for this step:

#### 1. Raw Centrix data

Centrix observations consist of four data points:

-   `asset` a character string containing the asset ID
-   `state` a character string representing the state of the asset
-   `dt` the date and time at which the event took place
-   `transition` the state transition that caused the event

For example, a train entering track `'TA-1'` on the 2nd January 2020 at 17:03 would produce the following Centrix observation:

```         
#> # A tibble: 1 × 4
#>   asset state dt                  transition
#>   <chr> <chr> <dttm>              <chr>     
#> 1 TA-1  TR    2020-01-02 17:03:00 UP to DN
```

The state `'TR'` is simply a placeholder, because this observation relates to a track asset. The state component is only relevant for signal events, in which case it represents the signal aspect. The transition `'UP to DN'` represents a train entering the track, while a transition of `'DN to UP'` would represent a train exiting the track.

#### 2. Asset mapping

The asset mapping is a representation of the track section. Currently, the track is represented by a linear data frame. However, this does not support more complex track layouts like junctions. The asset map should contain a 1-1 mapping from signal ID to berth name, a 1-many mapping from berth name to track ID, and an extra `event` column. For example, a simple track section with two berths, two signals, and three tracks would be:

```         
#> # A tibble: 6 × 4
#>   signal berth track event  
#>   <chr>  <chr> <chr> <chr>  
#> 1 S1     A     TA-1  enters 
#> 2 S1     A     TA-1  vacates
#> 3 S1     A     TA-2  enters 
#> 4 S1     A     TA-2  vacates
#> 5 S2     B     TB    enters 
#> 6 S2     B     TB    vacates
```

The `event` column represents the fact that there are separate Centrix observations for trains entering and exiting a track. Therefore, each track can have both an `'enters'` and `'vacates'` event associated with it. In order to calculate berth travel times, it is necessary to know when a train entered the following berth. However, if we used the asset map given above, we would not include data for the `'enters'` event at signal `'S3'`. Therefore, we would not be able to calculate travel times for `'S2'`. If these travel time calculations are important, it is recommended to include the extra event in the asset map:

```         
#> # A tibble: 1 × 4
#>   signal berth track event 
#>   <chr>  <chr> <chr> <chr> 
#> 1 S3     C     TC    enters
```

#### 3. State mapping

The state mapping provides a 1-1 mapping from signal state codes to signal aspects. This is standardised, so `railvarr` provides a default mapping that you can access by running

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

See [this website](https://wiki.openraildata.com/index.php/Signalling_Nomenclature#Signals) for other codes.

### 2. Centrix Clustering

### 3. Timetable Processing

### 4. ID Matching

### 5. Dwell Time and Delays

<figure>

<img src="man/figures/README-pipeline.PNG" alt="Diagram of the processing pipeline that railvarr provides"/>

<figcaption aria-hidden="true">

Diagram of the processing pipeline that railvarr provides

</figcaption>

</figure>

## Getting Started

The first step is to massage your raw data into the correct structure, as railvarr provides no reading functionality.

### Centrix

A detailed overview of what is expected for Centrix data can be found in [wrangle_centrix()](https://sebmerricks.github.io/railvarr/reference/wrangle_centrix.html). Here is an example of a valid Centrix data frame:

``` r
data(raw_centrix)
head(raw_centrix, 10)
#> # A tibble: 10 × 3
#>    asset   dt                  transition
#>    <chr>   <dttm>              <chr>     
#>  1 TA-1 TR 2000-01-01 06:13:48 UP to DN  
#>  2 S1 HHGE 2000-01-01 06:13:49 UP to DN  
#>  3 S1 HGE  2000-01-01 06:13:49 UP to DN  
#>  4 S1 RGE  2000-01-01 06:13:49 DN to UP  
#>  5 TA-1 TR 2000-01-01 06:25:29 DN to UP  
#>  6 TA-1 TR 2000-01-01 06:25:31 UP to DN  
#>  7 TA-1 TR 2000-01-01 06:25:43 DN to UP  
#>  8 S1 HHGE 2000-01-01 06:25:48 DN to UP  
#>  9 S1 HGE  2000-01-01 06:25:48 DN to UP  
#> 10 S1 RGE  2000-01-01 06:25:48 UP to DN
```

## Timetable

A detailed overview of what is expected for Timetable data can be found in [wrangle_timetable()](https://sebmerricks.github.io/railvarr/reference/wrangle_timetable.html). Here is an example of a valid Timetable data frame:

``` r
data(timetable)
head(timetable, 10)
#> # A tibble: 10 × 11
#>    train_header dt_origin           geo    event     wtt                
#>    <chr>        <dttm>              <chr>  <chr>     <dttm>             
#>  1 168H         2000-01-01 12:05:00 geo1   Originate 2000-01-01 12:20:00
#>  2 168H         2000-01-01 12:05:00 geo18  Arrive    2000-01-01 12:23:00
#>  3 168H         2000-01-01 12:05:00 geo18  Depart    2000-01-01 12:24:00
#>  4 168H         2000-01-01 12:05:00 geo2   Arrive    2000-01-01 12:28:00
#>  5 168H         2000-01-01 12:05:00 geo2   Depart    2000-01-01 12:29:00
#>  6 168H         2000-01-01 12:05:00 geo3   Arrive    2000-01-01 12:35:30
#>  7 168H         2000-01-01 12:05:00 geo3   Depart    2000-01-01 12:36:30
#>  8 168H         2000-01-01 12:05:00 geo104 Arrive    2000-01-01 12:39:00
#>  9 168H         2000-01-01 12:05:00 geo104 Depart    2000-01-01 12:39:30
#> 10 168H         2000-01-01 12:05:00 geo4   Arrive    2000-01-01 12:41:30
#> # ℹ 6 more variables: t <dttm>, delay <dbl>, allow <dbl>, allow_perf <dbl>,
#> #   allow_path <dbl>, allow_eng <dbl>
```
