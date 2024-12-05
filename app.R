library(wbstats)
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(bslib)

# Data Cleaning
icelanddata <- wb_data(country = "ISL", 
                       indicator = c("SP.POP.TOTL", "NY.GDP.MKTP.CD"))
icelanddata <- icelanddata %>%
  select(-iso2c, -iso3c)

# UI
ui <- fluidPage(
  theme = bs_theme(
    version = 4, 
    bootswatch = "cerulean", 
    bg = "#f7f9fc", 
    fg = "#333333", 
    primary = "#007bff", 
    base_font = font_google("Roboto")
  ),
  
  titlePanel(div(style = "color: #007bff; font-size: 24px; font-weight: bold;", "Exploring Iceland - A Shiny App")),
  
  fluidRow(
    column(3, 
           div(
             style = "background-color: #f7f9fc; padding: 15px; 
             border: 1px solid #ddd; border-radius: 8px;",
             h3("Project Sections", style = "color: #007bff;"),
             actionLink("link_intro", "1. General Introduction"),
             br(),
             actionLink("link_population", "2. Population Projection"),
             br(),
             actionLink("link_comparison", "3. Comparison with Greenland"),
             br(),
             actionLink("link_swot", "4. SWOT Analysis")
           )
    ),
    column(9,
           uiOutput("dynamic_content")
    )
  )
)

# Server
server <- function(input, output, session) {
  active_section <- reactiveVal("intro")
  
  observeEvent(input$link_intro, {
    active_section("intro")
  })
  
  observeEvent(input$link_population, {
    active_section("population")
  })
  
  observeEvent(input$link_comparison, {
    active_section("comparison")
  })
  
  observeEvent(input$link_swot, {
    active_section("swot")
  })
  
  output$dynamic_content <- renderUI({
    if (active_section() == "intro") {
      tabsetPanel(
        tabPanel("Introduction to Iceland",
                 p("Iceland is a Nordic island country between the 
                   North Atlantic and Arctic Oceans, on the 
                   Mid-Atlantic Ridge between North America and Europe. 
                   Its capital and largest city is Reykjavík, 
                   which is home to about 36% of the country's roughly 380,000 
                   residents. 86% of the residents are Icelanders. 
                   The official language of the country is Icelandic. 
                   The government structure is unitary parliamentry republic, 
                   which means that political power is entrusted to the 
                   parliament with confidence."),
                 p("Iceland is on a rift between tectonic plates, 
                   and its geologic activity includes geysers and 
                   frequent volcanic eruptions. The interior consists of 
                   a volcanic plateau with sand and lava fields, 
                   mountains and glaciers, and many glacial rivers 
                   flow to the sea through the lowlands."),
                 p("In 2022, Iceland was the eighth-most productive country 
                   in the world per capita (US$78,837). About 85 percent of 
                   the total primary energy supply in Iceland is derived from 
                   domestically produced renewable energy sources. The economy 
                   now in Iceland is more dependent on tourism, and other 
                   important sectors are: fish and fish products, aluminium, 
                   and ferrosilicon.")
        ),
        tabPanel("Iceland Map",
                 leafletOutput("iceland_map", height = "500px")
        ),
        tabPanel("Photos",
                 h3("Beautiful Views of Iceland", style = "color: #007bff;"),
                 fluidRow(
                   column(6, 
                          img(src = "icelandimage1.jpg", 
                              alt = "Beautiful Iceland View", 
                              style = "max-width: 100%; height: auto; 
                              border: 1px solid #ddd; border-radius: 8px;")),
                   column(6, 
                          img(src = "iceland image2.jpeg",
                              style = "max-width: 100%; height: auto; 
                              border: 1px solid #ddd; border-radius: 8px;")),
                   column(6, 
                          img(src = "icelandimage3.webp",
                              style = "max-width: 100%; height: auto; 
                              border: 1px solid #ddd; border-radius: 8px;")),
                   column(6, 
                          img(src = "icelandimage4.jpg",
                              style = "max-width: 100%; height: auto; 
                              border: 1px solid #ddd; border-radius: 8px;"))
                 )
        )
      )
    } else if (active_section() == "population") {
      tagList(
        h3("Projection of Iceland Population Data", style = "color: #007bff;"),
        plotOutput("population_plot", height = "400px")
      )
    } else if (active_section() == "comparison") {
      tagList(
        h3("Comparison of Iceland and Greenland", style = "color: #007bff;"),
        tableOutput("comparison_table")
      )
    } else if (active_section() == "swot") {
      tagList(
        h3("SWOT Analysis", style = "color: #007bff;"),
        p("**Strengths:** Natural beauty, geothermal energy."),
        p("**Weaknesses:** Small population, high dependency on imports."),
        p("**Opportunities:** Tourism growth, renewable energy development."),
        p("**Threats:** Climate change, economic vulnerabilities.")
      )
    }
  })
  
  # Render Map for General Introduction
  output$iceland_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -19.0208, lat = 64.9631, zoom = 6) %>%
      addMarkers(lng = -21.895, lat = 64.1355, 
                 popup = "Reykjavik - Capital of Iceland")
  })
  
  # Render Population Projection Plot
  output$population_plot <- renderPlot({
    ggplot(iceland_population, aes(x = Year, y = Population)) +
      geom_line(color = "#007bff", size = 1) +
      geom_point(color = "#ff4d4d", size = 2) +
      labs(title = "Population Projection of Iceland",
           x = "Year", y = "Population") +
      theme_minimal(base_family = "Roboto") +
      theme(plot.title = element_text(color = "#007bff", size = 16, 
                                      face = "bold"))
  })
  
  # Render Comparison Table
  output$comparison_table <- renderTable({
    data.frame(
      Metric = c("Area (sq km)", "Population", "GDP (USD Billion)", "Capital"),
      Iceland = c("103,000", "370,000", "27", "Reykjavik"),
      Greenland = c("2,166,086", "56,000", "3", "Nuuk")
    )
  })
}

# Run the App
shinyApp(ui, server)


