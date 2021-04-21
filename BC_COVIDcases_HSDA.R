# BC COVID data by Health Service Delivery Area
# Mark Johnson, University of British Columbia, https://twitter.com/ecohydrologist 
# Institute for Resources, Environment and Sustainability and Department of Earth, Ocean and Atmospheric Sciences
# Last updated 2021-04-21

library(tidyverse)
library(shiny)
library(ggiraph)
library(shinythemes)

# cases reported by HSDA
HSDAcases <-
  read.csv("http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv")

# generate time stamp for data access and plot
dataDate <-
  paste0("Figure updated: ", Sys.Date(), ", Latest HSDA data posted by BCCDC: ", max(HSDAcases$Date))

# wrangle data to remove non-HSDA data
HSDAcases.hsdas <- subset(HSDAcases, HSDA != "All" & HSDA != "Out of Canada" & HSDA != "Unknown")

# prep the list of HSDAs to use in Shiny
HSDAs <- HSDAcases.hsdas$HSDA %>%
  unique() %>%
  sort() %>%
  as.list()

# population by HSDA (obtained for 2019 from https://bcstats.shinyapps.io/popApp/; downloaded to Github)
HSDApop <- read.csv("https://raw.githubusercontent.com/markjohnsonubc/COVID-19_cases_BC_HSDA/main/Population_Estimates_HSDA_2019.csv")
names(HSDApop) <- c("Region", "HSDA", "Year", "Gender", "Total")

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head( tags$style(type="text/css", "text {font-family: sans-serif}")),
  
  titlePanel("COVID-19 cases by BC Health Service Delivery Area (HSDA)"),
  
  sidebarLayout(
    sidebarPanel( 
      helpText(tags$h4("Select HSDA")),
      selectInput(
        "hsda",
        label = "List of HSDAs",
        choices = HSDAs,
        selected = "Vancouver"
      ),
      helpText(p("Health Service Delivery Area")),
      hr(),
      helpText(tags$h4("Select date range")),
        dateRangeInput(
        "daterange",
        "Date range:",
        start  = "2021-02-01",
        end    = Sys.Date(),
        min    = "2020-01-29",
        max    = Sys.Date(),
        format = "yyyy-mm-dd",
        separator = " - "
      ),
      helpText(p("default Feb 1, 2021 - present")),
      helpText(p("max Jan 29, 2020 - present")),
    ),
    mainPanel(
      girafeOutput(outputId = "colPlot")
    )
  ),
  h5("Info:"),
  p("The most localized COVID-19 case numbers for British Columbia are available at the HSDA level."),
  h5("Data sources:"),
  p("COVID-19 cases by HSDA: http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv"),
  p("Graphed as cases per 100k people based on HSDA-level population data from: https://bcstats.shinyapps.io/popApp/"),
  p("Note: 7-day moving average data by HSDA available at https://bccdc.shinyapps.io/covid19_global_epi_app/"),
  p("Also note: HSDA-level data is not released every day. To find your HSDA, see https://catalogue.data.gov.bc.ca/dataset/health-service-delivery-area-boundaries"),
  h5("Code"),
  p("Code used to generate this visualization tool is available on Github https://github.com/markjohnsonubc/COVID-19_cases_BC_HSDA."),
  p("Visualization prepared by Mark Johnson, University of British Columbia. Twitter: @ecohydrologist")
)


# Define server logic ----
server <- function(input, output) {
  output$colPlot <-
    renderGirafe({
      
      HSDAcases$tt <- c(paste0(HSDAcases$Date, "\n Daily cases = ", HSDAcases$Cases_Reported, "\n Cases/100k = ", 
                               round(HSDAcases$Cases_Reported/(subset(HSDApop,HSDA =={input$hsda})$Total/1e5),digits=1), "\n HSDA=",{input$hsda}))
      
      p <- ggplot(
        subset(HSDAcases, HSDA == {input$hsda} & 
                 Date >= {input$daterange[1]} &
                 Date <= {input$daterange[2]}),
        aes(
          x = as.Date(Date),
          y = Cases_Reported / (subset(HSDApop, HSDA == {input$hsda})$Total / 1e5))
      ) +
        geom_col_interactive(aes(tooltip = tt)) +
        labs(
          x = "",
          y = "Daily cases per 100,000 people",
          title = paste0("Daily COVID-19 cases, ", {input$hsda}," HSDA"),
          subtitle = dataDate
        ) 
      girafe(ggobj = p)
    })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)