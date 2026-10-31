# Using R to Track Flights and Weather Across Salt Lake City's Flight Network

This repository contains supporting materials for the November 6, 2026, R Govys webinar, *Using R to Track Flights and Weather Across Salt Lake City's Flight Network*, presented by [Aleksey Kulyukin](https://orcid.org/0009-0009-4396-9878) of the [Department of Mathematics & Statistics](https://artsci.usu.edu/math-stats/) at [Utah State University](https://www.usu.edu/).

The presentation examines Salt Lake City International Airport's flight network, flight-volume patterns and diversions, and the availability of weather observations at airports connected to Salt Lake City.

## Interactive applications

Three R Shiny applications accompany the presentation. They are accessible through the [companion website](https://alkulak.github.io/rgovys_talk_2026/).

- **[SLC Flight Network Explorer](https://9ixde5-aleksey-kulyukin.shinyapps.io/slc-flight-network-2/)** explores historical connections between Salt Lake City and other airports, including routes and airlines.
- **[SLC Flight Volume Explorer](https://9ixde5-aleksey-kulyukin.shinyapps.io/slc-flight-volume-2/)** examines daily main and diverted flight activity at Salt Lake City.
- **[SLC Airport & Weather Station Explorer](https://9ixde5-aleksey-kulyukin.shinyapps.io/slc-weather-explorer/)** explores airport-to-weather-station relationships and the availability of daily and hourly weather reports over time.

## Website

The [`website/`](website/) directory contains the Quarto source for the companion website. GitHub Actions renders the website and publishes it through GitHub Pages.

## Related publication

Kulyukin, A., Swapp, C., and Symanzik, J. (2026). [Tracking and Mapping Change in Salt Lake City's Flight Network, 1987–2026](https://doi.org/10.1080/09332480.2026.2725530). *CHANCE*, 39(3), 30–41.
