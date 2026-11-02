# Using R to Track Flights and Weather Across Salt Lake City's Flight Network

This repository contains supporting materials for the November 6, 2026, [R Govys webinar](https://community.amstat.org/discussion/r-govys-webinar-november-6-1-pm-et), *Using R to Track Flights and Weather Across Salt Lake City's Flight Network*, presented by [Aleksey Kulyukin](https://orcid.org/0009-0009-4396-9878) from [Utah State University](https://www.usu.edu/)'s [Department of Mathematics & Statistics](https://artsci.usu.edu/math-stats/).

The presentation explores Salt Lake City International Airport's flight network, historical flight activity and diversions, and weather observations at airports connected to Salt Lake City. This repository brings together the interactive applications, companion website, R scripts, and research data used to explore these topics.

## `apps/`: Interactive exploration

The [`apps/`](apps/) directory contains the source code and supporting resources for three R Shiny applications. Each application has been updated with new features and additional data since the original versions were published alongside the following article:

Kulyukin, A., Swapp, C., and Symanzik, J. (2026). [Tracking and Mapping Change in Salt Lake City's Flight Network, 1987–2026](https://doi.org/10.1080/09332480.2026.2725530). *CHANCE*, 39(3), 30–41.

The [original *CHANCE* companion website](https://alkulak.github.io/slcflights_apps/) provides access to the applications accompanying the article, enabling comparison with the updated versions presented here.

Each updated application examines a different aspect of Salt Lake City's flight network:

- **SLC Flight Network Explorer** shows how the airports, routes, and airlines connected to Salt Lake City have changed over time.
- **SLC Flight Volume Explorer** examines daily flight volumes, including flights scheduled to arrive at or depart from Salt Lake City and flights that reached Salt Lake City through diversions.
- **SLC Airport & Weather Station Explorer** connects the flight network to historical weather observations. It identifies weather stations associated with airports and shows how the availability of hourly and daily weather reports varies across stations and time periods.

## `website/`: Webinar companion website

The [`website/`](website/) directory contains the Quarto source for the [webinar companion website](https://alkulak.github.io/rgovys_talk_2026/).

The website provides a single entry point to all three updated Shiny applications. Visitors can use the applications directly without downloading the repository or installing R.

GitHub Actions renders the Quarto website and publishes it through GitHub Pages.

## Presentation slides

The [`slides/`](slides/) directory contains the slidedeck for the webinar.

## `scripts/`: Flight and weather data processing

The [`scripts/`](scripts/) directory contains R code for combining flight records from the U.S. [Bureau of Transportation Statistics (BTS) Reporting Carrier On-Time Performance](http://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FGJ&QO_fu146_anzr=b0-gvzr) dataset with weather observations from NOAA's [Global Historical Climatology Network-Hourly (GHCNh)](https://www.ncei.noaa.gov/products/global-historical-climatology-network-hourly) dataset.

The starting point for the flight data is [`slcflights`](https://github.com/AlKulAK/slcflights), an R package for building and querying a local database of commercial domestic flights involving Salt Lake City International Airport (SLC). It includes flights for which SLC was the scheduled origin or destination, as well as flights that reached SLC through diversions.

The scripts extend these flight records with weather information:

- [`diversion_processing.R`](scripts/diversion_processing.R) reconstructs flight movements associated with diversions.
- [`GHCNh_station_manifest.R`](scripts/GHCNh_station_manifest.R) identifies relevant weather stations and their historical relationships to airports.
- [`GHCNh_downloader.R`](scripts/GHCNh_downloader.R) retrieves the necessary NOAA weather files.
- [`GHCNh_processing.R`](scripts/GHCNh_processing.R) prepares the weather observations.
- [`flight_weather_linkage.R`](scripts/flight_weather_linkage.R) and [`diversion_weather_linkage.R`](scripts/diversion_weather_linkage.R) connect flight events with weather observations at the relevant airports.

These scripts provide the working foundation for **`slcweather`**, a developing companion R package. Whereas `slcflights` organizes the flight data, `slcweather` is intended to provide reusable tools for acquiring and processing GHCNh observations, identifying appropriate weather stations, and linking weather records to flights. The package remains under development, but the scripts in this repository already implement these capabilities.

### `scripts/den_2023/`: Denver diversion chronology

The [`scripts/den_2023/`](scripts/den_2023/) directory contains a worked example of a flight-weather reconstruction.

On June 21, 2023, 19 flights scheduled to arrive at Denver International Airport (DEN) diverted through Salt Lake City International Airport (SLC). Most were airborne as [severe thunderstorms](https://www.weather.gov/media/bou/2023DenverClimateSummary.pdf) brought heavy rain, hail, strong wind gusts, and reduced visibility to the Denver area. The reconstruction follows their recorded movements alongside weather observations at the airports involved.

The directory contains two files:

- [`build_presentation_timeline.R`](scripts/den_2023/build_presentation_timeline.R) reconstructs the chronology from BTS flight records and NOAA GHCNh weather observations.
- [`den_2023_full_chronology.json`](scripts/den_2023/den_2023_full_chronology.json) contains the resulting machine-readable chronology.

The JSON includes **149 flight events** and **912 weather observations** across **16 airports**, ordered chronologically. Weather records retain all 52 GHCNh meteorological elements, associated measurement and source attributes, and original weather-report remarks.

This example illustrates what becomes possible when flight and weather records are examined together at the level of individual events. It provides a more detailed view of the diversion episode than daily flight or weather summaries alone.

### Reproducing the Denver chronology

The JSON is included in the repository and can be inspected without rebuilding the underlying data. To generate it independently, use R 4.1 or later and install the necessary packages.

Install the CRAN dependencies:

```r
install.packages(c(
  "arrow", "curl", "dplyr", "jsonlite", "lubridate",
  "lutz", "pak", "readr", "rlang", "sf", "tibble", "tidyr"
))
```

Install [`slcflights`](https://github.com/AlKulAK/slcflights) from GitHub:

```r
pak::pak("AlKulAK/slcflights")
```

Build the local BTS flight database through 2023:

```r
slcflights::build_slcflights_db(
  until = "2023-12",
  confirm = TRUE
)
```

Building the database requires substantial time and disk space. Once the database is ready, run the following command from the root directory of the cloned repository:

```r
source("scripts/den_2023/build_presentation_timeline.R")
```

The script prepares the flight events, identifies the required weather stations, downloads NOAA GHCNh data as needed, and generates:

```text
scripts/den_2023/den_2023_full_chronology.json
```

The NOAA files are cached locally for subsequent use.

To inspect the output and verify the expected record counts:

```r
path <- "scripts/den_2023/den_2023_full_chronology.json"

chronology <- jsonlite::read_json(path)

stopifnot(
  length(chronology$events) == 1061L,
  chronology$metadata$flight_count == 19L,
  chronology$metadata$flight_event_count == 149L,
  chronology$metadata$weather_observation_count == 912L,
  chronology$metadata$ghcnh_element_count == 52L
)
```
