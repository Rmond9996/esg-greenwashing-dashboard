library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)

esg <- read_csv("data/esg_greenwashing.csv", show_col_types = FALSE) |>
  mutate(
    across(c(net_zero_target_set, sbti_committed, emissions_disclosed,
             third_party_verified), ~ factor(.x, levels = c("No", "Yes"))),
    greenwashing_status = factor(
      if_else(greenwashing_flag == 1, "Flagged", "Not flagged"),
      levels = c("Not flagged", "Flagged")
    )
  )

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
)

ui <- page_sidebar(
  title = div(class = "brand-title", "ESG Greenwashing Monitor"),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0f766e",
    success = "#16a34a",
    danger = "#dc2626"
  ),
  fillable = TRUE,
  sidebar = sidebar(
    width = 290,
    h5("Filter the analysis"),
    sliderInput(
      "year", "Reporting year",
      min = min(esg$year), max = max(esg$year),
      value = range(esg$year), sep = ""
    ),
    selectizeInput(
      "sector", "Sector", choices = sort(unique(esg$sector)),
      selected = unique(esg$sector), multiple = TRUE
    ),
    selectizeInput(
      "country", "Country", choices = sort(unique(esg$country)),
      selected = unique(esg$country), multiple = TRUE
    ),
    selectizeInput(
      "company", "Company", choices = sort(unique(esg$company)),
      selected = unique(esg$company), multiple = TRUE
    ),
    radioButtons(
      "flag", "Greenwashing status",
      choices = c("All", "Flagged", "Not flagged"), inline = FALSE
    ),
    actionButton("reset", "Reset filters", class = "btn-outline-primary w-100"),
    hr(),
    p(class = "small text-muted",
      "Greenwashing flags are indicators in the source data, not findings of misconduct.")
  ),
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  layout_column_wrap(
    width = 1/4,
    value_box(title = "Companies", value = textOutput("companies", inline = TRUE),
              showcase = bsicons::bs_icon("buildings"), theme = "primary"),
    value_box(title = "Revenue represented", value = textOutput("revenue", inline = TRUE),
              showcase = bsicons::bs_icon("currency-dollar"), theme = "light"),
    value_box(title = "Scope 1 + 2 emissions", value = textOutput("emissions", inline = TRUE),
              showcase = bsicons::bs_icon("cloud-haze2"), theme = "light"),
    value_box(title = "Flag rate", value = textOutput("flag_rate", inline = TRUE),
              showcase = bsicons::bs_icon("flag"), theme = "danger")
  ),
  navset_card_tab(
    nav_panel(
      "Overview",
      layout_columns(
        card(card_header("Emissions and ESG score over time"), plotlyOutput("trend", height = "390px")),
        card(card_header("Greenwashing flags by sector"), plotlyOutput("sector_flags", height = "390px")),
        col_widths = c(8, 4)
      ),
      layout_columns(
        card(card_header("ESG score vs carbon intensity"), plotlyOutput("scatter", height = "430px")),
        card(card_header("Disclosure and assurance profile"), plotlyOutput("assurance", height = "430px")),
        col_widths = c(7, 5)
      )
    ),
    nav_panel(
      "Company explorer",
      layout_columns(
        card(
          card_header("Select a company"),
          selectInput("focus_company", NULL, choices = sort(unique(esg$company))),
          uiOutput("company_profile")
        ),
        card(card_header("Company emissions trajectory"), plotlyOutput("company_trend", height = "420px")),
        col_widths = c(4, 8)
      )
    ),
    nav_panel(
      "Data explorer",
      card(
        card_header("Filtered company-year observations"),
        downloadButton("download", "Download filtered CSV", class = "btn-primary mb-3"),
        DTOutput("table")
      )
    ),
    nav_panel(
      "About",
      div(
        class = "about-copy",
        h3("About this dashboard"),
        p("This dashboard explores ESG performance and potential greenwashing indicators across Energy, Utilities and Industrials companies from 2010 to 2024."),
        h5("How to interpret it"),
        tags$ul(
          tags$li("Emissions are reported in million tonnes of CO2 equivalent (MtCO2e)."),
          tags$li("Carbon intensity is tonnes of CO2 equivalent per USD million of revenue."),
          tags$li("The flag rate is the share of selected company-year observations where greenwashing_flag equals 1."),
          tags$li("Association in the charts should not be interpreted as causation or proof of wrongdoing.")
        ),
        h5("Data quality note"),
        p("Year-on-year Scope 1 change is unavailable for each company's first observation because no prior year exists.")
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateSliderInput(session, "year", value = range(esg$year))
    updateSelectizeInput(session, "sector", selected = unique(esg$sector))
    updateSelectizeInput(session, "country", selected = unique(esg$country))
    updateSelectizeInput(session, "company", selected = unique(esg$company))
    updateRadioButtons(session, "flag", selected = "All")
  })

  filtered <- reactive({
    req(input$sector, input$country, input$company)
    out <- esg |>
      filter(
        between(year, input$year[1], input$year[2]),
        sector %in% input$sector,
        country %in% input$country,
        company %in% input$company
      )
    if (input$flag != "All") out <- filter(out, greenwashing_status == input$flag)
    out
  })

  output$companies <- renderText(n_distinct(filtered()$company))
  output$revenue <- renderText(dollar(sum(filtered()$revenue_usd_bn), suffix = "bn", accuracy = 1))
  output$emissions <- renderText(number(sum(filtered()$total_s1_s2_mt_co2e), suffix = " Mt", accuracy = 0.1))
  output$flag_rate <- renderText(percent(mean(filtered()$greenwashing_flag), accuracy = 0.1))

  output$trend <- renderPlotly({
    trend <- filtered() |>
      group_by(year) |>
      summarise(
        emissions = sum(total_s1_s2_mt_co2e),
        esg_score = weighted.mean(esg_score_0_100, revenue_usd_bn),
        .groups = "drop"
      )
    p <- ggplot(trend, aes(year)) +
      geom_col(aes(y = emissions, text = paste0("Year: ", year, "<br>Emissions: ", comma(emissions), " Mt")), fill = "#0f766e", alpha = .82) +
      geom_line(aes(y = esg_score * max(emissions) / 100, text = paste0("Year: ", year, "<br>ESG score: ", round(esg_score, 1))), color = "#f59e0b", linewidth = 1.2) +
      geom_point(aes(y = esg_score * max(emissions) / 100), color = "#f59e0b", size = 2) +
      scale_x_continuous(breaks = pretty_breaks()) +
      scale_y_continuous(labels = comma, name = "Scope 1 + 2 emissions (MtCO2e)",
                         sec.axis = sec_axis(~ . * 100 / max(trend$emissions), name = "Revenue-weighted ESG score")) +
      labs(x = NULL)
    ggplotly(p, tooltip = "text") |> layout(legend = list(orientation = "h"))
  })

  output$sector_flags <- renderPlotly({
    dat <- filtered() |>
      count(sector, greenwashing_status) |>
      group_by(sector) |>
      mutate(share = n / sum(n)) |>
      ungroup()
    p <- ggplot(dat, aes(sector, share, fill = greenwashing_status,
                         text = paste0(sector, "<br>", greenwashing_status, ": ", percent(share, accuracy = .1), "<br>Observations: ", n))) +
      geom_col() +
      scale_fill_manual(values = c("Not flagged" = "#94a3b8", "Flagged" = "#dc2626")) +
      scale_y_continuous(labels = percent) +
      labs(x = NULL, y = "Share of observations", fill = NULL)
    ggplotly(p, tooltip = "text")
  })

  output$scatter <- renderPlotly({
    p <- ggplot(filtered(), aes(
      carbon_intensity_tco2e_per_musd, esg_score_0_100,
      color = greenwashing_status, size = revenue_usd_bn,
      text = paste0("Company: ", company, "<br>Year: ", year,
                    "<br>ESG score: ", esg_score_0_100,
                    "<br>Carbon intensity: ", carbon_intensity_tco2e_per_musd)
    )) +
      geom_point(alpha = .68) +
      scale_color_manual(values = c("Not flagged" = "#0f766e", "Flagged" = "#dc2626")) +
      scale_size_continuous(range = c(2, 12), labels = label_number(suffix = "bn")) +
      labs(x = "Carbon intensity (tCO2e per USDm)", y = "ESG score", color = NULL, size = "Revenue")
    ggplotly(p, tooltip = "text")
  })

  output$assurance <- renderPlotly({
    dat <- filtered() |>
      summarise(
        `Net-zero target` = mean(net_zero_target_set == "Yes"),
        `SBTi committed` = mean(sbti_committed == "Yes"),
        `Emissions disclosed` = mean(emissions_disclosed == "Yes"),
        `Third-party verified` = mean(third_party_verified == "Yes")
      ) |>
      pivot_longer(everything(), names_to = "indicator", values_to = "share")
    p <- ggplot(dat, aes(reorder(indicator, share), share,
                         text = paste0(indicator, ": ", percent(share, accuracy = .1)))) +
      geom_col(fill = "#0f766e") +
      coord_flip() +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      labs(x = NULL, y = "Share of observations")
    ggplotly(p, tooltip = "text")
  })

  output$company_profile <- renderUI({
    x <- esg |> filter(company == input$focus_company) |> arrange(year)
    latest <- slice_tail(x, n = 1)
    tags$div(
      class = "company-profile",
      h3(latest$company),
      p(strong("Ticker: "), latest$ticker),
      p(strong("Sector: "), latest$sector),
      p(strong("Country: "), latest$country),
      p(strong("Latest ESG score: "), latest$esg_score_0_100),
      p(strong("Latest CDP score: "), latest$cdp_climate_score),
      p(strong("Latest flag status: "), as.character(latest$greenwashing_status))
    )
  })

  output$company_trend <- renderPlotly({
    x <- esg |>
      filter(company == input$focus_company) |>
      select(year, `Scope 1` = scope1_emissions_mt_co2e,
             `Scope 2` = scope2_emissions_mt_co2e,
             `Scope 3` = scope3_emissions_mt_co2e) |>
      pivot_longer(-year, names_to = "scope", values_to = "emissions")
    p <- ggplot(x, aes(year, emissions, color = scope,
                       text = paste0("Year: ", year, "<br>", scope, ": ", emissions, " Mt"))) +
      geom_line(linewidth = 1) + geom_point() +
      scale_y_log10(labels = comma) +
      scale_x_continuous(breaks = pretty_breaks()) +
      labs(x = NULL, y = "Emissions (MtCO2e, log scale)", color = NULL)
    ggplotly(p, tooltip = "text")
  })

  output$table <- renderDT({
    datatable(
      filtered() |>
        select(year, company, ticker, sector, country, revenue_usd_bn,
               total_s1_s2_mt_co2e, scope3_emissions_mt_co2e,
               carbon_intensity_tco2e_per_musd, esg_score_0_100,
               cdp_climate_score, greenwashing_status),
      filter = "top", rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE)
    ) |>
      formatRound(c("revenue_usd_bn", "total_s1_s2_mt_co2e",
                    "scope3_emissions_mt_co2e", "carbon_intensity_tco2e_per_musd",
                    "esg_score_0_100"), 2)
  })

  output$download <- downloadHandler(
    filename = function() paste0("filtered-esg-data-", Sys.Date(), ".csv"),
    content = function(file) write_csv(filtered(), file)
  )
}

shinyApp(ui, server)
