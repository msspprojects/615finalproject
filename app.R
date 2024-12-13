library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(bslib)
library(shinydashboard)
library(geosphere)

locations <- data.frame(
  name = c("Iceland", "Reykjavik", "Greenland", "Nuuk"),
  lat = c(64.9631, 64.1265, 71.7069, 64.1726),
  lng = c(-19.0208, -21.8174, -42.6043, -51.7214),
  type = c("country", "capital", "country", "capital")
)
reykjavik_coords <- as.numeric(locations[locations$name == "Reykjavik", c("lng", "lat")])
nuuk_coords <- as.numeric(locations[locations$name == "Nuuk", c("lng", "lat")])

distance_km <- distHaversine(reykjavik_coords, nuuk_coords) / 1000


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
      tabItem(tabName = "comparison",
        tabsetPanel(
          tabPanel("Map and Basic Information",
                   leafletOutput("comparisonmap"),
                   tableOutput("comparison_table")),
          tabPanel("Comparison of GDP and Population",
                   tags$img(src = "comparison.png", width = "80%"),
                   p("Based on the comparison above, for GDP, Greenland and Iceland almost started at the same point, but the growth rate for Iceland over the years is much higher than Greenland. For population data, although Greenland has 20 times area than Iceland, Greenland has a lot lower population than Iceland. For both GDP and population, Greenland's data almost did not change in the past 60 years compared to the growth of Iceland.")),
          tabPanel("Comparison of Life Expectancy",
                   tags$img(src = "comparison2.png", width = "80%"),
                   p("Based on the plot above, it is clear that people in Iceland have longer life expectancy compared to people in Greenland. For the most recent data, the average life expectancy of Iceland is 82 years old, compared to 72 years old in Greenland.")),
          tabPanel("Comparison of Intentional Homicides",
                   tags$img(src = "comparison3.png", width = "80%"),
                   p("Based on the graph above, Greenland obviously has more violence incidences than Iceland for the past 60 years, reaching even 30 cases per 100000 people at around 2000, about 30 times the number of cases in Iceland. For the most recent data, in 2016, Greenland has 5 cases of homicide per 100000 people, while Iceland only has one case."),
                   p("In conclusion, from the comparison of a few different perspectives, compared to Greenland, Iceland had more people, better economy, and longer life expectancy and low intentional homicide show better social security and national happiness. Thus, Iceland is a better country to live in than Greenland."))
        )
      ),
      tabItem(
        tabName = "swot",
        div(
          class = "swot-content",
          h2("SWOT Analysis of Iceland"),
          h3("Strength"),
          tags$ul(
            tags$li("Based on the GDP analysis above, Iceland GDP continues increasing, suggesting the economic prosperity. Also, based on the CIA World Factbook, Iceland is ranked 20th for GDP per capita across all countries in the world."),
            tags$li("Based on the life expectancy analysis before, the mean life expectancy of people in Iceland is 82, suggesting the overall better life quality and healthcare for old people. Additionally, according to CIA World Factbook, Iceland's urbanization rate is 94%, reflecting the focus on economic activities, such as service, tourism and technology."),
            tags$li("Based on the CIA World Book, 70% of the country's electricity comes from hydroeletricity, and 20% comes from geothermal energy, making Iceland a global leader in sustainable development.")
          ),
          h3("Weakness"),
          tags$img(src = "icelandpopulation.png", width = "80%"),
          tags$ul(
            tags$li("Iceland's economy is heavily relied on the tourism and fishing, making it vulnerable to demand swings and volcanic activity."),
            tags$li("As the median age plot shown above, Iceland is expected to have an aging population, which may lead to shrinkage of labor force and more expenditure on healthcare.")
          ),
          h3("Opportunities"),
          tags$ul(
            tags$li("As previously mentioned, since Iceland relies heavily on tourism, with the recovery of economy followed by the post-pendemic era, the visitors in Iceland will increase and further boost its economy."),
            tags$li("Surprisingly, because of the cold weather in Iceland, it has become favorable place for building data center in recent years. With more big tech companies, such as Meta, Google and Apple started building data centers in Nordic countries, it can stimulate the economic growth and rapid development of Iceland.")
          ),
          h3("Threats"),
          tags$ul(
            tags$li("Global warming is likely to have a great impact on Iceland, since 11% of the land area of Iceland is covered in glaciers, and global warming is gradually melting those glaciers, impacting both marine life and fish industry, which is among one of major economic pillars that Iceland has."),
            tags$li("Since data centers built in Iceland are hungry for energy, this may cause shortage in electricity generation because some of the energy generated is reserved for powering eletric cars. This shortage may lead to less electric cars to a turn to traditional fossil fuels to generate energy, which may further damage the environment.")
          )
        ),
        tags$style(HTML("
          .swot-content h2 {
            font-size: 32px;
          }
          .swot-content h3 {
            font-size: 28px;
          }
          .swot-content p, .swot-content li {
            font-size: 23px;
          }
        "))
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
  
  output$comparisonmap <- renderLeaflet({
    leaflet(locations) %>%
      addTiles() %>%
      # Add markers for capitals
      addMarkers(
        data = locations[locations$type == "capital",],
        lng = ~lng, 
        lat = ~lat, 
        popup = ~name
      ) %>%
      addPolylines(
        lng = c(reykjavik_coords[1], nuuk_coords[1]),
        lat = c(reykjavik_coords[2], nuuk_coords[2]),
        color = "blue",
        weight = 2
      ) %>%
      addLabelOnlyMarkers(
        lng = mean(c(reykjavik_coords[1], nuuk_coords[1])) + 0.5,
        lat = mean(c(reykjavik_coords[2], nuuk_coords[2])) + 0.5,
        label = paste0(round(distance_km, 2), " km"),
        labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE)
      ) %>%
      # Set view to center between Iceland and Greenland
      setView(
        lng = mean(locations$lng), 
        lat = mean(locations$lat), 
        zoom = 4
      )
  })
  
  # Render Comparison Table
  output$comparison_table <- renderTable({
    data.frame(
      Metric = c("Area (sq km)", "Capital"),
      Iceland = c("103,000", "Reykjavik"),
      Greenland = c("2,166,086", "Nuuk")
    )
  })
}

# Run the App
shinyApp(ui, server)