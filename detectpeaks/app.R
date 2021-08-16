#
# test your peak detection values
# 
#
# by: Stephan Lange
#
#    
#

library(shiny)
#### import data ####
running.system <- 2

# 1 - windows
# 2 - linux AWI

## read paths and allowed variables
if (running.system == 1) {
    yearlyDatasetPaths <- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_auto_noflag.csv",
                                   stringsAsFactors = FALSE, strip.white = TRUE)
    allowedVariables   <- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedVariables.csv",
                                   stringsAsFactors = FALSE, strip.white = TRUE)
    filterbasepath     <- "N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/filter.files/"
    checkbasepath      <- "N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/check.files/"
    # read file for modification of style of shiny-app
    source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/additionals_shiny/appCSS.R")
    pp <- read.table(file = paste0("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/peak.parameter.csv"),
                     sep = ",", dec = ".", header = TRUE, encoding = "ISO-8859-1")
} else if (running.system == 2) {
    yearlyDatasetPaths <- read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_AWI_noflag.csv", stringsAsFactors = FALSE,
                                   strip.white = TRUE)
    allowedVariables <- read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedVariables.csv", stringsAsFactors = FALSE,
                                 strip.white = TRUE)
    filterbasepath     <- "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/filter.files/"
    checkbasepath      <- "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/check.files/"
    # read file for modification of style of shiny-app
    source("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/additionals_shiny/appCSS.R")
    pp <- read.table(file = paste0("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/peak.parameter.csv"),
                     sep = ",", dec = ".", header = TRUE, encoding = "ISO-8859-1")
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    #titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("station", "Choose a station:", selected = "TVC",
                        choices = sort(unique(yearlyDatasetPaths$station))),
            selectInput("dataset", "Choose a dataset:", selected = "BaMet2009",
                        choices = yearlyDatasetPaths$dataset[yearlyDatasetPaths$station == "Bayelva"]),
            selectInput("variable", "Choose a variable:",  choices = NULL),
            selectInput("year", "Choose a year:",
                        choices = sort(unique(yearlyDatasetPaths$year), decreasing = TRUE), selected = NULL),
            
            sliderInput("pp1", "Number of bins:",
                        min = 1,max = 50,   value = 30),
            sliderInput("pp2", "Number of bins:",
                        min = 1,max = 50,   value = 30),
            sliderInput("pp3", "Number of bins:",
                        min = 1,max = 50,   value = 30),
            sliderInput("pp4", "Number of bins:",
                        min = 1,max = 50,   value = 30),
            sliderInput("pp6", "Number of bins:",
                        min = 1,max = 50,   value = 30),
            sliderInput("pp7", "Number of bins:",
                        min = 1,max = 50,   value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("lv0_plot"),
           plotOutput("result_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observeEvent(list(input$dataset, input$station), {
        # update dataset list
        datasets <- yearlyDatasetPaths$dataset[yearlyDatasetPaths$station == input$station]
        if (input$dataset %in% datasets) {
            # double update to trigger input$dataset invalidation
            updateSelectInput(session, "dataset", choices = datasets, selected = input$dataset)
        } else {
            updateSelectInput(session, "dataset", choices = datasets)
            return()
        }
        
        # update variable list
        varlist <- allowedVariables$variable[allowedVariables$dataset == input$dataset]
        ## If variable was selected, update filter list, read data and draw overview plot, else abort
        if (input$variable %in% varlist) {
            # double update to trigger input$variable invalidation
            updateSelectInput(session, "variable", choices = varlist, selected = input$variable)
        } else {
            updateSelectInput(session, "variable", choices = varlist)
            return()
        }
    })
    #browser()
    output$lv0_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
    })
    output$result_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
