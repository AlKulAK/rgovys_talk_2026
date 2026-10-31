# SLC Flight Volume Explorer
# 10.03.2026 version
#
# Necessary runtime packages:
# dplyr, ggplot2, grid, plotly, shiny
#
# The deployed app uses precomputed .rds files in data/ and does not
# import slcflights directly. It depends on slcflights indirectly because
# the compute pipeline uses slcflights to build those .rds files before
# deployment.
#
# Plotting helpers live in R/plots.R and are loaded automatically by
# Shiny when the app directory is run.

# --------------------------------------------------
# App-level display constants and helpers
# --------------------------------------------------

main_plot_height <- "480px"
div_plot_height <- "480px"
default_outlier_threshold <- 750

make_tab_title <- function(label, dates) {
  if (length(dates) == 0L) {
    stop("Cannot build a tab title from an empty date vector.")
  }

  if (anyNA(dates)) {
    stop(
      "Cannot build a tab title when `FlightDate` contains missing values."
    )
  }

  date_range <- range(dates)

  paste0(
    label,
    " (",
    format(date_range[1], "%B %Y"),
    " - ",
    format(date_range[2], "%B %Y"),
    ")"
  )
}

# --------------------------------------------------
# App-level attribution and navigation
# --------------------------------------------------

app_home_url <- "https://alkulak.github.io/slcflights_apps/"
university_url <- "https://www.usu.edu/"
department_url <- "https://artsci.usu.edu/math-stats/"

app_authors <- list(
  list(
    name = "Aleksey Kulyukin",
    url = "https://orcid.org/0009-0009-4396-9878"
  ),
  list(
    name = "Cameron Swapp",
    url = "https://orcid.org/0009-0004-9019-1097"
  ),
  list(
    name = "Juergen Symanzik",
    url =
      "https://artsci.usu.edu/math-stats/directory/faculty/symanzik-juergen"
  )
)

external_link <- function(label, url) {
  shiny::tags$a(
    label,
    href = url,
    target = "_blank",
    rel = "noopener noreferrer"
  )
}

external_link_html <- function(label, url) {
  paste0(
    "<a href=\"",
    htmltools::htmlEscape(url, attribute = TRUE),
    "\" target=\"_blank\" rel=\"noopener noreferrer\">",
    htmltools::htmlEscape(label),
    "</a>"
  )
}

author_link <- function(author) {
  external_link(
    label = author$name,
    url = author$url
  )
}

author_link_html <- function(author) {
  external_link_html(
    label = author$name,
    url = author$url
  )
}

make_author_links <- function(authors) {
  links <- vapply(
    authors,
    author_link_html,
    character(1)
  )

  n_links <- length(links)

  if (n_links == 0L) {
    return("")
  }

  if (n_links == 1L) {
    return(links[[1]])
  }

  if (n_links == 2L) {
    return(
      paste0(
        links[[1]],
        " and ",
        links[[2]]
      )
    )
  }

  paste0(
    paste0(
      links[seq_len(n_links - 1L)],
      collapse = ", "
    ),
    ", and ",
    links[[n_links]]
  )
}

make_credits_tab <- function() {
  talk_title <- paste(
    "Using R to Track Flights and Weather Across Salt Lake City’s",
    "Flight Network"
  )

  article_title <- paste(
    "Tracking and Mapping Change in Salt Lake City's Flight",
    "Network, 1987–2026."
  )

  credits_text <- paste0(
    "The SLC Flight Volume Explorer was developed by ",
    author_link_html(app_authors[[1]]),
    " at ",
    external_link_html(
      label = "Utah State University",
      url = university_url
    ),
    "'s ",
    external_link_html(
      label = "Department of Mathematics & Statistics",
      url = department_url
    ),
    ". This version was prepared for the R Govys talk <em>",
    htmltools::htmlEscape(talk_title),
    "</em>. It builds on the SLC Flight Volume Explorer developed by ",
    "Kulyukin et al. for the <em>CHANCE</em> article <em>",
    htmltools::htmlEscape(article_title),
    "</em>"
  )

  shiny::tabPanel(
    title = "Credits",
    shiny::tags$div(
      style = paste(
        "max-width: 900px;",
        "margin-left: auto;",
        "margin-right: auto;",
        "font-size: 15px;",
        "line-height: 1.5;"
      ),
      shiny::tags$p(
        shiny::HTML(credits_text)
      )
    )
  )
}

# --------------------------------------------------
# Load precomputed app data
# --------------------------------------------------

main_daily <- readRDS("data/main_daily.rds")
div_daily <- readRDS("data/div_daily.rds")

main_tab_title <- make_tab_title(
  label = "Main Flights",
  dates = main_daily$FlightDate
)

div_tab_title <- make_tab_title(
  label = "Diverted Flights",
  dates = div_daily$FlightDate
)

# --------------------------------------------------
# User interface
# --------------------------------------------------

app_ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style(
      shiny::HTML(
        "
      .container-fluid {
        padding-left: 8px;
        padding-right: 8px;
      }

      h2 {
        font-size: 24px;
        margin-top: 8px;
        margin-bottom: 6px;
      }

      .nav-tabs {
        margin-bottom: 4px;
      }

      .nav-tabs > li > a {
        padding-top: 6px;
        padding-bottom: 6px;
        color: #555555;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color: #000000;
        font-weight: 600;
        background-color: #f2f2f2;
        border-bottom-color: transparent;
      }

      .nav-tabs > li:not(.active) > a:hover {
        color: #000000;
        background-color: #f7f7f7;
      }
      "
      )
    )
  ),
  shiny::tags$h2(
    "SLC Flight Volume Explorer",
    style = "text-align: center;"
  ),
  shiny::tabsetPanel(
    shiny::tabPanel(
      title = main_tab_title,
      plotly::plotlyOutput(
        outputId = "main_plot",
        height = main_plot_height
      )
    ),
    shiny::tabPanel(
      title = div_tab_title,
      shiny::sliderInput(
        inputId = "div_threshold",
        label = "Outlier threshold",
        min = 100,
        max = 1000,
        value = default_outlier_threshold,
        step = 1,
        ticks = FALSE,
        post = "%",
        width = "100%"
      ),
      plotly::plotlyOutput(
        outputId = "div_plot",
        height = div_plot_height
      )
    ),
    shiny::tabPanel(
      title = "Notes",
      shiny::tags$div(
        style = paste(
          "max-width: 900px;",
          "margin-left: auto;",
          "margin-right: auto;",
          "font-size: 15px;",
          "line-height: 1.5;"
        ),
        shiny::tags$h3("Main flights"),
        shiny::tags$p(
          paste(
            "The dark blue long-dashed line marks 100%:",
            "that year's average daily main-flight count."
          )
        ),
        shiny::tags$p(
          paste(
            "Each dot is one calendar date/category observation;",
            "holiday dates also remain in their weekday categories."
          )
        ),
        shiny::tags$h3("Diverted flights"),
        shiny::tags$p(
          paste(
            "The dark blue long-dashed line marks 100%:",
            "that year's average daily diverted-flight count.",
            "The light blue short-dashed line marks the selected",
            "outlier threshold. Red dots are observations above",
            "the selected threshold."
          )
        ),
        shiny::tags$p(
          paste(
            "Each dot is one calendar date/category observation;",
            "holiday dates also remain in their weekday categories."
          )
        )
      )
    ),
    make_credits_tab()
  )
)

# --------------------------------------------------
# Server logic
# --------------------------------------------------

app_server <- function(input, output, session) {
  output$main_plot <- plotly::renderPlotly({
    plot_main_date_level(main_daily)
  })

  output$div_plot <- plotly::renderPlotly({
    plot_div_date_outliers(
      plot_df = div_daily,
      threshold = input$div_threshold
    )
  })
}

shiny::shinyApp(
  ui = app_ui,
  server = app_server
)
