# BTS Flight Counts Through July 2026

The November 13, 2026, Graphics Group presentation reports **235,010,356 nationwide flight records** from October 1987 through July 2026.

Two BTS webpages were captured as PDFs on November 12, 2026. Both PDFs are included in this directory:

1. [Reporting Carrier On-Time Performance: Table Profile](<OST_R_BTS_Transtats Data Table.pdf>) ([BTS webpage](https://www.transtats.bts.gov/TableInfo.asp?QO_fu146_anzr=b0-gvzr&V0s1_b0yB=D&gnoyr_VQ=FGJ)): **235,618,745 records** through August 2026.
2. [Flight Delays at a Glance: Monthly](<OST_R_BTS_Transtats Home Drill Chart Month.pdf>) ([BTS webpage](https://www.transtats.bts.gov/HomeDrillChart_Month.asp)): **608,389 flight operations** in August 2026, with All Airports, All Carriers, and Arrivals selected.

Subtracting the August count gives the nationwide total through July 2026:

**235,618,745 − 608,389 = 235,010,356**

The presentation also reports **7,995,631 SLC-related flights**: 7,992,084 main flights and 3,547 diverted flights. These counts come from the local [`slcflights`](https://github.com/AlKulAK/slcflights) database and can be reproduced in R:

```r
slcflights::status_slcflights_db()

counts <- lapply(
  list(
    Main = slcflights::open_main(),
    Diverted = slcflights::open_div()
  ),
  \(x) x |>
    dplyr::summarise(flights = sum(Flights)) |>
    dplyr::collect()
) |>
  dplyr::bind_rows(.id = "population")

dplyr::bind_rows(
  counts,
  dplyr::summarise(
    counts,
    population = "Total",
    flights = sum(flights)
  )
)
```

The reported counts correspond to the database through July 2026. Check its coverage using `status_slcflights_db()` before comparing results.
