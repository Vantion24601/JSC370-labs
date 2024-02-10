Lab 05 - Data Wrangling
================
Elaine Dai
2024-2-7

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/JSC370-2024/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages),
    `mgcv`, `ggplot2`, `leaflet`, `kableExtra`.

``` r
# install.packages("webshot2")
```

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.9-0. For overview type 'help("mgcv-package")'.

``` r
library(ggplot2)
library(leaflet)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
library(tidyr)
library(webshot2)
```

``` r
fn <- "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz"
if (!file.exists("met_all_2023.gz"))
  download.file(fn, destfile = "met_all_2023.gz")
met <- data.table::fread("met_all_2023.gz")
```

2.  Load the met data from
    <https://github.com/JSC370/JSC370-2024/main/data/met_all_2023.gz> or
    (Use
    <https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE, LAT, LON)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

# Read in the met data and fix lat, lon, temp
met$lon <- met$lon / 1000
met$lat <- met$lat / 1000
met$temp <- met$temp / 10
met$atm.press <- met$atm.press / 10
```

3.  Merge the data as we did during the lecture. Use the `merge()` code
    and you can also try the tidy way with `left_join()`

``` r
met <- merge(
  x = met,
  y = stations,
  by.x = "USAFID",
  by.y = "USAF",
  all.x = TRUE,
  all.y = FALSE
)

# met <- left_join(met, stations, by = c("USAFID" = "USAF"))

head(met[, list(USAFID, WBAN, STATE)], n=4)
```

    ##    USAFID  WBAN STATE
    ## 1: 690150 93121    CA
    ## 2: 690150 93121    CA
    ## 3: 690150 93121    CA
    ## 4: 690150 93121    CA

## Question 1: Identifying Representative Stations

Across all weather stations, which stations have the median values of
temperature, wind speed, and atmospheric pressure? Using the
`quantile()` function, identify these three stations. Do they coincide?

``` r
# Find the average for each station
met_average <- met |>
  group_by(USAFID, STATE) |>
  summarise(
    across(
      c(temp, wind.sp, atm.press, lon, lat),
      function(x) mean(x, na.rm = TRUE)
    ), 
    .groups = "drop"
  )

met_average
```

    ## # A tibble: 1,852 × 7
    ##    USAFID STATE  temp wind.sp atm.press    lon   lat
    ##     <int> <chr> <dbl>   <dbl>     <dbl>  <dbl> <dbl>
    ##  1 690150 CA     26.7    46.5     1009. -116.   34.3
    ##  2 720110 TX     28.9    33.2      NaN   -98.7  30.8
    ##  3 720113 MI     20.6    30.8      NaN   -83.2  42.5
    ##  4 720120 SC     24.4    36.4      NaN   -80.7  32.2
    ##  5 720137 IL     21.4    31.9      NaN   -88.4  41.4
    ##  6 720151 TX     27.6    39.4      NaN  -104.   30.4
    ##  7 720169 MO     23.5    37.8      NaN   -91    38.6
    ##  8 720170 IL     23.5    27.5      NaN   -88.8  37.2
    ##  9 720172 AR     23.6    24.1      NaN   -94.2  34.5
    ## 10 720175 AR     25.9    28.0     1011.  -91.8  33.6
    ## # ℹ 1,842 more rows

``` r
# Find the median value
met_median <- met_average |>
  summarise(
    across(
      c(temp, wind.sp, atm.press),
      function(x) quantile(x, probs = .5, na.rm = TRUE)
    )
  )
met_median
```

    ## # A tibble: 1 × 3
    ##    temp wind.sp atm.press
    ##   <dbl>   <dbl>     <dbl>
    ## 1  21.6    35.0     1012.

``` r
# Find the station with the median values
med_temp_st <- met_average |> filter(temp == met_median$temp)
print(med_temp_st)
```

    ## # A tibble: 1 × 7
    ##   USAFID STATE  temp wind.sp atm.press   lon   lat
    ##    <int> <chr> <dbl>   <dbl>     <dbl> <dbl> <dbl>
    ## 1 723122 SC     21.6    34.6       NaN -82.4  34.8

``` r
med_ws_st <- met_average |> filter(wind.sp == met_median$wind.sp)
print(med_ws_st)
```

    ## # A tibble: 1 × 7
    ##   USAFID STATE  temp wind.sp atm.press   lon   lat
    ##    <int> <chr> <dbl>   <dbl>     <dbl> <dbl> <dbl>
    ## 1 721044 TX     26.9    35.0       NaN -96.4  32.9

``` r
met_average |> filter(atm.press == met_median$atm.press)
```

    ## # A tibble: 0 × 7
    ## # ℹ 7 variables: USAFID <int>, STATE <chr>, temp <dbl>, wind.sp <dbl>,
    ## #   atm.press <dbl>, lon <dbl>, lat <dbl>

``` r
# No exact match, so choose the closest one
med_atmp_st <- met_average |>
  mutate(diff = abs(atm.press - met_median$atm.press)) |>
  arrange(diff) |>
  head(1)
  
print(med_atmp_st)
```

    ## # A tibble: 1 × 8
    ##   USAFID STATE  temp wind.sp atm.press   lon   lat    diff
    ##    <int> <chr> <dbl>   <dbl>     <dbl> <dbl> <dbl>   <dbl>
    ## 1 723095 NC     23.1    36.1     1012. -77.0  35.1 0.00364

The station with median temperature is SC, the station with median wind
speed is TX, and the station with median atmosphere pressure is NC. They
do not coincide.

Next identify the stations have these median values.

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Identifying Representative Stations per State

Now let’s find the weather stations by state with closest temperature
and wind speed based on the euclidean distance from these medians.

``` r
# Add the euclidean distance for each station to met_average
met_new <- met_average |>
  mutate(
    distance = sqrt((temp - met_median$temp)^2 + (wind.sp - met_median$wind.sp)^2)
  )

# Identify the closest station for each state
closest_stations <- met_new |>
  group_by(STATE) |>
  filter(distance == min(distance)) |>
  select(STATE, USAFID, lat, lon)

closest_stations
```

    ## # A tibble: 36 × 4
    ## # Groups:   STATE [36]
    ##    STATE USAFID   lat   lon
    ##    <chr>  <int> <dbl> <dbl>
    ##  1 NJ    720581  40.6 -74.2
    ##  2 ND    720855  48.8 -97.6
    ##  3 MN    722006  43.6 -96.2
    ##  4 FL    722067  30.2 -81.9
    ##  5 RI    722151  41.4 -71.8
    ##  6 MS    722165  35.0 -89.8
    ##  7 GA    722185  34.3 -83.8
    ##  8 IA    722331  42.5 -93.2
    ##  9 LA    722485  32.5 -93.7
    ## 10 TX    722587  33.6 -95.5
    ## # ℹ 26 more rows

Knit the doc and save it on GitHub.

## Question 3: In the Geographic Center?

For each state, identify which station is closest to the geographic
mid-point (median) of the state. Combining these with the stations you
identified in the previous question, use `leaflet()` to visualize all
~100 points in the same figure, applying different colors for the
geographic median and the temperature and wind speed median.

``` r
# Find the median location for each state
state_midpoint <- met_average |>
  group_by(STATE) |>
  summarise(
    median_lat = median(lat, na.rm = TRUE),
    median_lon = median(lon, na.rm = TRUE)
  )
state_midpoint
```

    ## # A tibble: 48 × 3
    ##    STATE median_lat median_lon
    ##    <chr>      <dbl>      <dbl>
    ##  1 AL          32.6      -86.6
    ##  2 AR          35.3      -92.7
    ##  3 AZ          33.5     -112. 
    ##  4 CA          35.7     -120. 
    ##  5 CO          39.1     -105. 
    ##  6 CT          41.4      -72.8
    ##  7 DE          39.1      -75.5
    ##  8 FL          28.5      -81.8
    ##  9 GA          32.6      -83.5
    ## 10 IA          41.8      -93.5
    ## # ℹ 38 more rows

``` r
# Calculate the distance from each station to the midpoint of corresponding state
met_avg_with_mp <- met_average |>
  left_join(state_midpoint, by = "STATE") |>
  mutate(
    distance_to_midpoint = sqrt((lat - median_lat)^2 + (lon - median_lon)^2)
  )
```

``` r
# Identify the closest station to the midpoint for each state
closest_to_mp <- met_avg_with_mp |>
  group_by(STATE) |>
  filter(distance_to_midpoint == min(distance_to_midpoint)) |>
  select(STATE, USAFID, lat, lon)
closest_to_mp
```

    ## # A tibble: 48 × 4
    ## # Groups:   STATE [48]
    ##    STATE USAFID   lat    lon
    ##    <chr>  <int> <dbl>  <dbl>
    ##  1 WV    720328  39    -80.3
    ##  2 AR    720401  35.6  -92.4
    ##  3 KY    720448  37.6  -84.8
    ##  4 LA    720468  30.6  -92.1
    ##  5 VA    720498  37.4  -77.5
    ##  6 ND    720867  48.4 -100. 
    ##  7 MO    720869  38.9  -92.7
    ##  8 OH    720928  40.3  -83.1
    ##  9 GA    722175  32.6  -83.6
    ## 10 NC    722201  35.6  -79.1
    ## # ℹ 38 more rows

``` r
all_stations <- bind_rows(
  closest_to_mp |>
    mutate(category = 'Geographic Midpoint'),
  closest_stations |>
    mutate(category = 'Temperature & Wind Speed Median')
)
all_stations
```

    ## # A tibble: 84 × 5
    ## # Groups:   STATE [48]
    ##    STATE USAFID   lat    lon category           
    ##    <chr>  <int> <dbl>  <dbl> <chr>              
    ##  1 WV    720328  39    -80.3 Geographic Midpoint
    ##  2 AR    720401  35.6  -92.4 Geographic Midpoint
    ##  3 KY    720448  37.6  -84.8 Geographic Midpoint
    ##  4 LA    720468  30.6  -92.1 Geographic Midpoint
    ##  5 VA    720498  37.4  -77.5 Geographic Midpoint
    ##  6 ND    720867  48.4 -100.  Geographic Midpoint
    ##  7 MO    720869  38.9  -92.7 Geographic Midpoint
    ##  8 OH    720928  40.3  -83.1 Geographic Midpoint
    ##  9 GA    722175  32.6  -83.6 Geographic Midpoint
    ## 10 NC    722201  35.6  -79.1 Geographic Midpoint
    ## # ℹ 74 more rows

``` r
leaflet() |>
  addTiles() |>
  addCircleMarkers(
    data = all_stations,
    lng = ~lon, 
    lat = ~lat, 
    color = ~case_when(
      category == 'Geographic Midpoint' ~ 'blue',
      category == 'Temperature & Wind Speed Median' ~ 'red',
    ),
    popup = ~paste(STATE, USAFID, sep = "<br>"),
    radius = 4
  )
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Knit the doc and save it on GitHub.

## Question 4: Summary Table with `kableExtra`

Generate a summary table using `kable` where the rows are each state and
the columns represent average temperature broken down by low, median,
and high elevation stations.

Use the following breakdown for elevation:

- Low: elev \< 93
- Mid: elev \>= 93 and elev \< 401
- High: elev \>= 401

``` r
met_elev <- met |>
  mutate(
    elev_cat = case_when(
      elev < 93 ~ "Low",
      elev >= 93 & elev < 401 ~ "Mid",
      elev >= 401 ~ "High",
      TRUE ~ NA_character_  # for any missing elevation data
    )
  ) |>
  select(STATE, USAFID, elev, elev_cat, temp)
head(met_elev)
```

    ##    STATE USAFID elev elev_cat temp
    ## 1:    CA 690150  696     High 28.9
    ## 2:    CA 690150  696     High 28.9
    ## 3:    CA 690150  696     High 26.1
    ## 4:    CA 690150  696     High 23.9
    ## 5:    CA 690150  696     High 23.3
    ## 6:    CA 690150  696     High 22.2

``` r
temp_by_elevcat <- met_elev |>
  group_by(STATE, elev_cat) |>
  summarise(
    average_temp = mean(temp, na.rm = TRUE),
    .groups = 'drop'
  )
temp_by_elevcat
```

    ## # A tibble: 101 × 3
    ##    STATE elev_cat average_temp
    ##    <chr> <chr>           <dbl>
    ##  1 AL    Low              25.1
    ##  2 AL    Mid              23.8
    ##  3 AR    High             23.7
    ##  4 AR    Low              25.6
    ##  5 AR    Mid              24.4
    ##  6 AZ    High             23.9
    ##  7 AZ    Low              29.3
    ##  8 AZ    Mid              30.4
    ##  9 CA    High             18.1
    ## 10 CA    Low              18.3
    ## # ℹ 91 more rows

``` r
temp_pivot <- temp_by_elevcat |>
  pivot_wider(
    names_from = elev_cat,
    values_from = average_temp
  )
temp_pivot
```

    ## # A tibble: 48 × 4
    ##    STATE   Low   Mid  High
    ##    <chr> <dbl> <dbl> <dbl>
    ##  1 AL     25.1  23.8  NA  
    ##  2 AR     25.6  24.4  23.7
    ##  3 AZ     29.3  30.4  23.9
    ##  4 CA     18.3  18.8  18.1
    ##  5 CO     NA    NA    15.2
    ##  6 CT     19.4  18.8  NA  
    ##  7 DE     21.4  NA    NA  
    ##  8 FL     26.6  NA    NA  
    ##  9 GA     24.8  23.2  NA  
    ## 10 IA     NA    22.3  22.0
    ## # ℹ 38 more rows

``` r
kable(temp_pivot, format = "html", caption = "Average Temperature by Elevation Category Per State") |>
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<caption>
Average Temperature by Elevation Category Per State
</caption>
<thead>
<tr>
<th style="text-align:left;">
STATE
</th>
<th style="text-align:right;">
Low
</th>
<th style="text-align:right;">
Mid
</th>
<th style="text-align:right;">
High
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
AL
</td>
<td style="text-align:right;">
25.07106
</td>
<td style="text-align:right;">
23.79775
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
AR
</td>
<td style="text-align:right;">
25.58698
</td>
<td style="text-align:right;">
24.40578
</td>
<td style="text-align:right;">
23.723926
</td>
</tr>
<tr>
<td style="text-align:left;">
AZ
</td>
<td style="text-align:right;">
29.28585
</td>
<td style="text-align:right;">
30.38057
</td>
<td style="text-align:right;">
23.892609
</td>
</tr>
<tr>
<td style="text-align:left;">
CA
</td>
<td style="text-align:right;">
18.25508
</td>
<td style="text-align:right;">
18.77071
</td>
<td style="text-align:right;">
18.148808
</td>
</tr>
<tr>
<td style="text-align:left;">
CO
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
15.184075
</td>
</tr>
<tr>
<td style="text-align:left;">
CT
</td>
<td style="text-align:right;">
19.37249
</td>
<td style="text-align:right;">
18.78433
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
DE
</td>
<td style="text-align:right;">
21.40611
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
FL
</td>
<td style="text-align:right;">
26.61484
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
GA
</td>
<td style="text-align:right;">
24.80529
</td>
<td style="text-align:right;">
23.23841
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
IA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
22.26228
</td>
<td style="text-align:right;">
21.992787
</td>
</tr>
<tr>
<td style="text-align:left;">
ID
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
16.415667
</td>
</tr>
<tr>
<td style="text-align:left;">
IL
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
22.11707
</td>
<td style="text-align:right;">
20.843173
</td>
</tr>
<tr>
<td style="text-align:left;">
IN
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
20.12731
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
KS
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
24.16196
</td>
<td style="text-align:right;">
22.098776
</td>
</tr>
<tr>
<td style="text-align:left;">
KY
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
21.36103
</td>
<td style="text-align:right;">
20.178196
</td>
</tr>
<tr>
<td style="text-align:left;">
LA
</td>
<td style="text-align:right;">
27.61819
</td>
<td style="text-align:right;">
26.09414
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
MA
</td>
<td style="text-align:right;">
17.44477
</td>
<td style="text-align:right;">
17.59058
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
MD
</td>
<td style="text-align:right;">
21.25462
</td>
<td style="text-align:right;">
20.62255
</td>
<td style="text-align:right;">
20.648332
</td>
</tr>
<tr>
<td style="text-align:left;">
ME
</td>
<td style="text-align:right;">
15.23159
</td>
<td style="text-align:right;">
15.43930
</td>
<td style="text-align:right;">
15.329681
</td>
</tr>
<tr>
<td style="text-align:left;">
MI
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
18.54432
</td>
<td style="text-align:right;">
17.977982
</td>
</tr>
<tr>
<td style="text-align:left;">
MN
</td>
<td style="text-align:right;">
22.66275
</td>
<td style="text-align:right;">
21.15523
</td>
<td style="text-align:right;">
19.931963
</td>
</tr>
<tr>
<td style="text-align:left;">
MO
</td>
<td style="text-align:right;">
25.79654
</td>
<td style="text-align:right;">
23.77652
</td>
<td style="text-align:right;">
23.300286
</td>
</tr>
<tr>
<td style="text-align:left;">
MS
</td>
<td style="text-align:right;">
26.34285
</td>
<td style="text-align:right;">
24.66682
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
MT
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
16.293015
</td>
</tr>
<tr>
<td style="text-align:left;">
NC
</td>
<td style="text-align:right;">
22.82945
</td>
<td style="text-align:right;">
21.21073
</td>
<td style="text-align:right;">
18.046833
</td>
</tr>
<tr>
<td style="text-align:left;">
ND
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
21.79236
</td>
<td style="text-align:right;">
20.415848
</td>
</tr>
<tr>
<td style="text-align:left;">
NE
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
23.48598
</td>
<td style="text-align:right;">
21.048920
</td>
</tr>
<tr>
<td style="text-align:left;">
NH
</td>
<td style="text-align:right;">
17.78844
</td>
<td style="text-align:right;">
16.77731
</td>
<td style="text-align:right;">
7.243417
</td>
</tr>
<tr>
<td style="text-align:left;">
NJ
</td>
<td style="text-align:right;">
19.96563
</td>
<td style="text-align:right;">
19.31963
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
NM
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
22.448418
</td>
</tr>
<tr>
<td style="text-align:left;">
NV
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
20.849170
</td>
</tr>
<tr>
<td style="text-align:left;">
NY
</td>
<td style="text-align:right;">
18.75621
</td>
<td style="text-align:right;">
18.31489
</td>
<td style="text-align:right;">
15.887585
</td>
</tr>
<tr>
<td style="text-align:left;">
OH
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
19.43774
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
OK
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
25.07676
</td>
<td style="text-align:right;">
24.000040
</td>
</tr>
<tr>
<td style="text-align:left;">
OR
</td>
<td style="text-align:right;">
15.20318
</td>
<td style="text-align:right;">
16.39100
</td>
<td style="text-align:right;">
16.711553
</td>
</tr>
<tr>
<td style="text-align:left;">
PA
</td>
<td style="text-align:right;">
20.34185
</td>
<td style="text-align:right;">
19.40527
</td>
<td style="text-align:right;">
17.286934
</td>
</tr>
<tr>
<td style="text-align:left;">
RI
</td>
<td style="text-align:right;">
17.88116
</td>
<td style="text-align:right;">
17.46589
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
SC
</td>
<td style="text-align:right;">
23.68407
</td>
<td style="text-align:right;">
22.38995
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
SD
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
22.79495
</td>
<td style="text-align:right;">
20.639922
</td>
</tr>
<tr>
<td style="text-align:left;">
TN
</td>
<td style="text-align:right;">
25.81362
</td>
<td style="text-align:right;">
22.89642
</td>
<td style="text-align:right;">
19.457179
</td>
</tr>
<tr>
<td style="text-align:left;">
TX
</td>
<td style="text-align:right;">
28.74462
</td>
<td style="text-align:right;">
28.08021
</td>
<td style="text-align:right;">
26.500393
</td>
</tr>
<tr>
<td style="text-align:left;">
UT
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
19.754720
</td>
</tr>
<tr>
<td style="text-align:left;">
VA
</td>
<td style="text-align:right;">
21.34826
</td>
<td style="text-align:right;">
20.49998
</td>
<td style="text-align:right;">
17.954522
</td>
</tr>
<tr>
<td style="text-align:left;">
VT
</td>
<td style="text-align:right;">
NaN
</td>
<td style="text-align:right;">
16.89971
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
WA
</td>
<td style="text-align:right;">
15.25193
</td>
<td style="text-align:right;">
17.80542
</td>
<td style="text-align:right;">
16.810354
</td>
</tr>
<tr>
<td style="text-align:left;">
WI
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
19.56563
</td>
<td style="text-align:right;">
17.994615
</td>
</tr>
<tr>
<td style="text-align:left;">
WV
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
19.31079
</td>
<td style="text-align:right;">
17.492150
</td>
</tr>
<tr>
<td style="text-align:left;">
WY
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
13.748173
</td>
</tr>
</tbody>
</table>

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, first create a
  lazy table. Filter out values of atmospheric pressure outside of the
  range 1000 to 1020. Examine the association between temperature (y)
  and atmospheric pressure (x). Create a scatterplot of the two
  variables using ggplot2. Add both a linear regression line and a
  smooth line.

``` r
median_per_station <- met |>
  group_by(USAFID) |>
  summarise(
    across(c(temp, atm.press), function(x) quantile(x, probs = .5, na.rm = TRUE))
  )
mps_lz <- lazy_dt(median_per_station)
mps_lz
```

    ## Source: local data table [1,852 x 3]
    ## Call:   `_DT1`
    ## 
    ##   USAFID  temp atm.press
    ##    <int> <dbl>     <dbl>
    ## 1 690150  26.7     1009.
    ## 2 720110  28         NA 
    ## 3 720113  20         NA 
    ## 4 720120  24         NA 
    ## 5 720137  21.1       NA 
    ## 6 720151  28.0       NA 
    ## # ℹ 1,846 more rows
    ## 
    ## # Use as.data.table()/as.data.frame()/as_tibble() to access results

``` r
mps_lz <- mps_lz |>
  filter(atm.press >= 1000 & atm.press <= 1020) |>
  collect()
```

``` r
ggplot(mps_lz, aes(x = atm.press, y = temp)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  geom_smooth(se = FALSE, color = "red") +
  labs(title = "Temperature vs. Atmosphere Pressure",
       x = "Atmosphere Pressure", y = "Temperature")
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.

``` r
linear_model <- lm(temp ~ atm.press, data = mps_lz)
summary(linear_model)
```

    ## 
    ## Call:
    ## lm(formula = temp ~ atm.press, data = mps_lz)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.3696  -2.6173   0.1844   2.3012  11.6394 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 1175.93898   58.53149   20.09   <2e-16 ***
    ## atm.press     -1.14162    0.05785  -19.73   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.762 on 1083 degrees of freedom
    ## Multiple R-squared:  0.2645, Adjusted R-squared:  0.2638 
    ## F-statistic: 389.4 on 1 and 1083 DF,  p-value: < 2.2e-16

``` r
spline_model <- gam(temp ~ s(atm.press, bs = "cr"), data = mps_lz)
summary(spline_model)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## temp ~ s(atm.press, bs = "cr")
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  20.9378     0.1117   187.5   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##               edf Ref.df     F p-value    
    ## s(atm.press) 8.67  8.968 51.52  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.297   Deviance explained = 30.2%
    ## GCV = 13.647  Scale est. = 13.526    n = 1085

``` r
mps_lz <- mps_lz |>
  mutate(
    lm_pred = predict(linear_model),
    sp_pred = predict(spline_model)
  ) |>
  collect()
mps_lz
```

    ## # A tibble: 1,085 × 5
    ##    USAFID  temp atm.press lm_pred   sp_pred
    ##     <int> <dbl>     <dbl>   <dbl> <dbl[1d]>
    ##  1 690150  26.7     1009.    23.6      22.7
    ##  2 720175  25       1011.    21.9      21.8
    ##  3 720198  13.9     1014.    17.9      17.1
    ##  4 720269  29.3     1006.    27.8      27.6
    ##  5 720306  23.3     1012.    20.7      21.2
    ##  6 720333  17       1014.    18.8      19.4
    ##  7 720334  21       1012.    20.0      19.9
    ##  8 720361  25       1012     20.6      20.9
    ##  9 720362  22.2     1006.    27.2      28.0
    ## 10 720363  23.3     1012.    21.2      22.1
    ## # ℹ 1,075 more rows

``` r
ggplot(mps_lz, aes(x = atm.press, y = temp)) +
  geom_point() +
  geom_line(aes(y = lm_pred, color = "Linear Model")) +
  geom_line(aes(y = sp_pred, color = "Spline Model")) +
  labs(title = "Temperature vs. Atmosphere Pressure: Linear vs. Spline Model",
       x = "Atmosphere Pressure", y = "Temperature") +
  scale_color_manual("", values = c("Linear Model" = "blue", "Spline Model" = "red"))
```

![](README_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

The spline model has a higher deviance explained (30.2% vs. 26.45%) and
a higher adjusted R-squared (0.297 vs. 0.2638) compared to the linear
model, suggesting that the spline model has better explanatory power and
fits the data better. The spline model is more complex due to its
non-linear nature. Both models show significant relationships between
atmosphere pressure and temperature, but the spline model’s significant
smooth term and edf suggests that the relationship is non-linear. Hence
the spline model is the best fit.

## Deliverables

- .Rmd file (this file)

- link to the .md file (with all outputs) in your GitHub repository
