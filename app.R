library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(bslib)
library(shinydashboard)

# UI
ui <- dashboardPage(
  skin = "blue",  # Dashboard theme color
  dashboardHeader(title = "Exploring Iceland"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("General Introduction", tabName = "intro", icon = icon("globe")),
      menuItem("GDP Projection", tabName = "population", icon = icon("chart-line")),
      menuItem("Comparison with Greenland", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("SWOT Analysis", tabName = "swot", icon = icon("tasks"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body {
          font-size: 15px; /* Adjust the overall font size */
        }
        h1 {
          font-size: 28px; /* Larger size for h1 headers */
        }
        p {
          font-size: 23px; /* Adjust paragraph text size */
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "intro",
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
                            img(src = "icelandimage3.webp",
                                style = "max-width: 100%; height: auto; 
                                border: 1px solid #ddd; border-radius: 8px;")),
                     column(6, 
                            img(src = "icelandimage4.jpg",
                                style = "max-width: 100%; height: auto; 
                                border: 1px solid #ddd; border-radius: 8px;")),
                     column(6, 
                            img(src = "icelandimage2.png",
                                style = "max-width: 100%; height: auto; 
                                border: 1px solid #ddd; border-radius: 8px;"))
                   )
          )
        )
      ),
      tabItem(
        tabName = "population",
        tabsetPanel(
          tabPanel("Iceland GDP Data Plot",
                   tags$img(src = "gdpdata.png", width = "80%"),
                   tags$img(src = "adftest1.png", width = "80%"),
                   p("From the plot, it can be seen that the GDP has an obvious upward trend for the last 60 years, and its trend is similar to an exponential function."),
                   p("Also, based on the ADF test result, p-value is larger than 0.05, indicating the series is not statistically significant, and some transformations need to be done.")),
          tabPanel("GDP Data After Differencing",
                   tags$img(src = "gdp2.png", width = "80%"),
                   tags$img(src = "adftest2.png", width = "80%"),
                   p("Based on the ADF test result above, the p-value is 0.01, so we can reject the null hypothesis, showing that the series is stationary.")),
          tabPanel("ACF and PACF for New GDP Data",
                   tags$img(src = "acfpacf.png", width = "80%"),
                   p("Based on the plot above, both ACF and PACF have significant spike at lag order 1, and since there is first order differencing performed on GDP data, the first model to try is ARIMA(1, 1, 1).")),
          tabPanel("Assess Model Fit",
                   tags$img(src = "modelfit.png", width = "80%"),
                   tags$img(src = "ljungboxtest.png", width = "80%"),
                   p("Based on the Ljung-Box test result, p-value is greater than 0.05, indicating that there is no significant autocorrelation between residuals, and the residuals behave like white noise. Based on the ACF plot in the lower left part, most of the spikes are not statistically significant, suggesting again no autocorrelation between residuals. The histogram on the lower right shows the normality of residuals. Based on the model diagnostics above, ARIMA(1, 1, 1) model can be used to predict the GDP in Iceland.")),
          tabPanel("Population Prediction",
                   tags$img(src = "prediction.png", width = "80%"),
                   p("Based on the forecast values above, the GDP is predicted to decrease and become gradually stable at around $700,000,000 per year for the next 15 years."))
        )
        
      ),
      tabItem(
        tabName = "comparison",
        h3("Comparison of Iceland and Greenland", style = "color: #007bff;"),
        tableOutput("comparison_table")
      ),
      tabItem(
        tabName = "swot",
        h3("SWOT Analysis", style = "color: #007bff;"),
        p("**Strengths:** Natural beauty, geothermal energy."),
        p("**Weaknesses:** Small population, high dependency on imports."),
        p("**Opportunities:** Tourism growth, renewable energy development."),
        p("**Threats:** Climate change, economic vulnerabilities.")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Render Map for General Introduction
  output$iceland_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -19.0208, lat = 64.9631, zoom = 6) %>%
      addMarkers(lng = -21.895, lat = 64.1355, 
                 popup = "Reykjavik - Capital of Iceland")
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