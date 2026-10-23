# EU plant-pest interceptions explorer (TRACES, species-level identifications)
#
#   shiny::runApp()   from the project root, after Rscript R/update.R

library(shiny)
library(bslib)
library(DBI)
library(dplyr)
library(stringr)
library(DT)
library(plotly)

series_col <- "#2a78d6"
ink_muted <- "#52514e"

# data -----------------------------------------------------------------------

con <- dbConnect(RSQLite::SQLite(), "data/europhyt.db", extended_types = TRUE)
findings <- dbReadTable(con, "findings")
consignments <- dbReadTable(con, "consignments")
commodities <- dbReadTable(con, "finding_commodities")
taxa <- dbReadTable(con, "taxa")
countries <- dbReadTable(con, "countries")
product_types <- dbReadTable(con, "product_types")
snapshot <- dbReadTable(con, "snapshot")
dbDisconnect(con)

commodity_by_finding <- commodities |>
  group_by(finding_id) |>
  summarise(commodity = paste(unique(commodity_name), collapse = ", "),
            commodity_eppo = paste(unique(eppocode), collapse = ", "))

interceptions <- findings |>
  left_join(consignments, by = "ched_id") |>
  left_join(commodity_by_finding, by = "finding_id") |>
  left_join(select(taxa, eppocode, pest_eppo_name = eppo_name, pest_order = order,
                   pest_family = family), by = c("pest_eppo" = "eppocode")) |>
  left_join(select(countries, iso2, origin = name), by = c("origin_iso2" = "iso2")) |>
  left_join(select(countries, iso2, wp_origin = name), by = c("wp_origin_iso2" = "iso2")) |>
  left_join(product_types, by = "product_type_code") |>
  mutate(
    # undecided consignments fall back to their declaration date
    date = coalesce(decision_date, declaration_date),
    year = as.integer(format(date, "%Y")),
    origin = coalesce(origin, wp_origin),
    pest = coalesce(pest_eppo_name, pest_name),
    commodity = if_else(item == "object", coalesce(wood_packaging_type, "wood packaging"), commodity),
    # one lower-case string per row for the species / EPPO code search
    search_text = str_to_lower(paste(pest_eppo, pest, pest_name, commodity, commodity_eppo, sep = " | "))
  ) |>
  arrange(desc(date))

years <- range(interceptions$year, na.rm = TRUE)
member_states <- sort(unique(na.omit(interceptions$member_state)))
origins <- sort(unique(na.omit(interceptions$origin)))


# helpers --------------------------------------------------------------------

bar_layout <- function(p, xlab = "", ylab = "") {
  p |>
    layout(
      xaxis = list(title = list(text = xlab), gridcolor = "#e9e8e4", zeroline = FALSE, color = ink_muted),
      yaxis = list(title = list(text = ylab), gridcolor = "#e9e8e4", zeroline = FALSE, color = ink_muted,
                   automargin = TRUE),
      hoverlabel = list(bgcolor = "white", font = list(color = "#0b0b0b")),
      margin = list(t = 10, r = 10), bargap = 0.25
    ) |>
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
}

top_n_bar <- function(df, var, n = 15, label) {
  d <- df |> count(.data[[var]], name = "n", sort = TRUE) |> filter(!is.na(.data[[var]])) |>
    slice_head(n = n)
  if (!nrow(d)) return(plotly_empty())
  plot_ly(d, x = ~n, y = ~factor(.data[[var]], levels = rev(.data[[var]])),
          type = "bar", orientation = "h", marker = list(color = series_col),
          text = ~n, textposition = "outside", cliponaxis = FALSE,
          textfont = list(color = ink_muted),
          hovertemplate = paste0("%{y}<br>%{x} ", label, "<extra></extra>")) |>
    bar_layout(xlab = "Interceptions")
}


# ui -------------------------------------------------------------------------

ui <- page_sidebar(
  title = "EU plant-pest interceptions",
  theme = bs_theme(version = 5, primary = series_col),
  sidebar = sidebar(
    width = 300,
    textInput("q", "Search species or EPPO code",
              placeholder = "e.g. Bemisia tabaci, TOBRFV, Citrus"),
    radioButtons("q_in", NULL, inline = TRUE,
                 choices = c("Pest" = "pest", "Commodity" = "commodity", "Both" = "both"),
                 selected = "both"),
    helpText("Separate several terms with commas. Matches names and EPPO codes, case-insensitive."),
    sliderInput("years", "Year", min = years[1], max = years[2], value = years,
                step = 1, sep = ""),
    selectizeInput("origin", "Country of origin", choices = origins, multiple = TRUE,
                   options = list(placeholder = "All")),
    selectizeInput("ms", "Intercepting member state", choices = member_states, multiple = TRUE,
                   options = list(placeholder = "All")),
    checkboxGroupInput("item", "Intercepted on", inline = TRUE,
                       choices = c("Plants & products" = "plant", "Wood packaging / objects" = "object"),
                       selected = c("plant", "object")),
    hr(),
    p(class = "small text-muted",
      "Source: DG SANTE TRACES plant interceptions dashboard, data to ",
      paste0(format(as.Date(snapshot$extracted_to), "%d %b %Y"), "."),
      "Only pests identified to species level (or below).")
  ),
  tags$style(HTML("table.dataTable { font-size: 0.85rem; }")),
  layout_columns(
    fill = FALSE,
    value_box("Interceptions", textOutput("n_findings"), theme = "light", max_height = "110px"),
    value_box("Consignments", textOutput("n_consignments"), theme = "light", max_height = "110px"),
    value_box("Pest species", textOutput("n_pests"), theme = "light", max_height = "110px"),
    value_box("Origin countries", textOutput("n_origins"), theme = "light", max_height = "110px")
  ),
  navset_card_underline(
    nav_panel("Table", DTOutput("table")),
    nav_panel("Over time", plotlyOutput("trend", height = "420px")),
    nav_panel("Top pests", plotlyOutput("top_pests", height = "520px")),
    nav_panel("Top origins", plotlyOutput("top_origins", height = "520px")),
    nav_panel("Top commodities", plotlyOutput("top_commodities", height = "520px")),
    nav_panel("Pest summary", DTOutput("pest_summary"))
  )
)


# server ---------------------------------------------------------------------

server <- function(input, output, session) {

  filtered <- reactive({
    d <- interceptions |>
      filter(is.na(year) | between(year, input$years[1], input$years[2]),
             item %in% input$item)
    if (length(input$origin)) d <- filter(d, origin %in% input$origin)
    if (length(input$ms)) d <- filter(d, member_state %in% input$ms)

    terms <- str_to_lower(str_trim(str_split_1(input$q, ",")))
    terms <- terms[nzchar(terms)]
    if (length(terms)) {
      field <- switch(input$q_in,
        pest = str_to_lower(paste(d$pest_eppo, d$pest, d$pest_name)),
        commodity = str_to_lower(paste(d$commodity, d$commodity_eppo)),
        both = d$search_text
      )
      hit <- Reduce(`|`, lapply(terms, \(t) str_detect(field, fixed(t))))
      d <- d[hit, ]
    }
    d
  })

  fmt <- \(x) format(x, big.mark = " ")
  output$n_findings <- renderText(fmt(nrow(filtered())))
  output$n_consignments <- renderText(fmt(n_distinct(filtered()$ched_id)))
  output$n_pests <- renderText(fmt(n_distinct(filtered()$pest_eppo)))
  output$n_origins <- renderText(fmt(n_distinct(na.omit(filtered()$origin))))

  output$table <- renderDT({
    filtered() |>
      transmute(Date = date, `Member state` = member_state, `CHED ref` = ched_ref,
                Origin = origin, `Pest EPPO` = pest_eppo, Pest = pest,
                Order = pest_order, Family = pest_family,
                Commodity = commodity, `Commodity EPPO` = commodity_eppo,
                `CN code` = coalesce(cn_code, wp_cn_code), `Product type` = product_type,
                `Net kg` = net_weight_kg) |>
      datatable(
        rownames = FALSE, filter = "top", extensions = "Buttons",
        class = "compact stripe hover nowrap",
        options = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
                       buttons = c("copy", "csv", "excel"),
                       order = list(list(0, "desc")))
      )
  }, server = FALSE)

  output$trend <- renderPlotly({
    d <- filtered() |> filter(!is.na(date)) |>
      count(month = as.Date(format(date, "%Y-%m-01")), name = "n")
    if (!nrow(d)) return(plotly_empty())
    plot_ly(d, x = ~month, y = ~n, type = "bar", marker = list(color = series_col),
            hovertemplate = "%{x|%b %Y}<br>%{y} interceptions<extra></extra>") |>
      bar_layout(ylab = "Interceptions per month")
  })

  output$top_pests <- renderPlotly(top_n_bar(filtered(), "pest", label = "interceptions"))
  output$top_origins <- renderPlotly(top_n_bar(filtered(), "origin", label = "interceptions"))
  output$top_commodities <- renderPlotly(top_n_bar(filtered(), "commodity", label = "interceptions"))

  output$pest_summary <- renderDT({
    filtered() |>
      group_by(`Pest EPPO` = pest_eppo, Pest = pest, Order = pest_order, Family = pest_family) |>
      summarise(Interceptions = n(),
                Consignments = n_distinct(ched_id),
                `Origin countries` = n_distinct(origin, na.rm = TRUE),
                `Top origins` = paste(head(names(sort(table(origin), decreasing = TRUE)), 3), collapse = ", "),
                `Top commodities` = paste(head(names(sort(table(commodity), decreasing = TRUE)), 3), collapse = ", "),
                `First seen` = min(date, na.rm = TRUE), `Last seen` = max(date, na.rm = TRUE),
                .groups = "drop") |>
      arrange(desc(Interceptions)) |>
      datatable(rownames = FALSE, filter = "top", extensions = "Buttons",
                class = "compact stripe hover nowrap",
                options = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
                               buttons = c("copy", "csv", "excel")))
  }, server = FALSE)
}

shinyApp(ui, server)
