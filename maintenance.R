#############################################################################
##
##   Shiny App
##
##   App to manually flag maintenance periods
##
##   it includes:
##   selection of
##   - a station
##   - a dataset
##   - a variable
##   - a year
##
##   control plot for one variable at a time:
##   - the time series of one year
##   - the option to zoom in within the year
##
##   written by:  stephan.lange@awi.de
##                christian.lehr@awi.de
##
##   last check: 2020-01-27
##   checked by: christian.lehr@awi.de
##
#############################################################################
##
## open issues:
##
##
##
#############################################################################
##
## last modification:
##
##  2021-03-23 SL convert to git strcture
##  2020-09-14 CL change of wording: "created" ==> "flagging_date", "creator" ==> "flagged_by"
##
###############################################################################
##
## comments:
##
###############################################################################

library(dplyr)
library(shiny)
library(shinyjs)
library(DT)
library(rbokeh)

#### import data ####
running.system <- 1
#
# 1 - windows
# 2 - linux AWI

## read paths and allowed variables
if (running.system == 1) {
  # read paths and allowed variables for windows
  yearlyDatasetPaths <- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_auto.csv",
                                 stringsAsFactors = FALSE, strip.white = TRUE)
  allowedVariables   <- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedVariables.csv",
                                 stringsAsFactors = FALSE, strip.white = TRUE)
  maintenancebasepath     <- "N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/"
  # read file for modification of style of shiny-app
  source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/additionals_shiny/appCSS.R")
} else if (running.system == 2) {
  # read paths and allowed variables for linux
  yearlyDatasetPaths <- read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_AWI.csv", stringsAsFactors = FALSE,
                                 strip.white = TRUE)
  allowedVariables <- read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedVariables.csv", stringsAsFactors = FALSE,
                               strip.white = TRUE)
  maintenancebasepath     <- "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/"
  # read file for modification of style of shiny-app
  source("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/additionals_shiny/appCSS.R")
}

# color ramp for flags
flagcolors <- rainbow(20)
#flagcolors[1:8] <- "darkorchid4"
flagcolors[2] <- "#AC1116"
flagcolors[3] <- "#9346A4"
flagcolors[4] <- "#71D7DA"
flagcolors[5] <- "#4B9158"
flagcolors[6] <- "#9A632D"

###############################
#### helper functions for busy/error button feedbacks
#
# © https://github.com/daattali/advanced-shiny/tree/master/busy-indicator
#
# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      hidden(
        icon("spinner", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    hidden(
      div(class = "btn-err",
        div(icon("exclamation-circle"),
            tags$b("Error: "),
            span(class = "btn-err-msg")
        )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })

  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade", time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

#### server logic ####
server <- shinyServer(function(input, output, session) {
  # Initialize variable to save application state ==> adapted from sparcviewer app
  store <- reactiveValues(selectedDaterange = c(0, 1), variables = allowedVariables,
                          currentData = data.frame())

  # holds original data
  currentData <- data.frame()
  # holds temporary flagged data
  flaggedData <- data.frame()

  # define maintenance files and paths analogue to filter files and paths
  # use the same directory for saving of maintenance files
  getMaintenancepath <- function() {
    stationprefix <- switch(input$station,
                            "Bayelva" = "Ba",
                            "Kurungnakh" = "Ku",
                            "Samoylov" = "Sa",
                            "Sardakh" = "Sd",
                            "TVC" = "TVC")
    return(paste0(maintenancebasepath, stationprefix, "_maintenance_", input$year, ".dat"))
  }

  getMaintenancelist <- function() {
    maintenancepath <- getMaintenancepath()
    if (file.exists(maintenancepath))
      return(read.table(maintenancepath, stringsAsFactors = FALSE, strip.white = TRUE,
                        sep = ",", dec = ".", header = T))
    else return(NULL)
  }

  redrawMaintenanceTable <- function() {
    ## Load maintenance
    maintenancelist <- getMaintenancelist()
    ## Option list for data tables
    optionList <- list(orderClasses = TRUE, searching = FALSE, paging = FALSE, selection = "single")

    # if year has a maintenancelist, draw table
    if (!is.null(maintenancelist)) {
    ################# display all maintenance dates of the current year
    ################# of the selected datasets
      output$maintenancetable <- DT::renderDataTable(maintenancelist[maintenancelist$variable %in% input$variables, ], editable = TRUE,
                                                     options = optionList, rownames = FALSE, caption = NULL, # "Maintenance",
                                                     colnames = c('from', 'to', 'dataset', 'variable', 'flag',
                                                                  'flagging date', 'flagged by', 'comments'))
     } else output$maintenancetable <- DT::renderDataTable(data.frame(
      Maintenance = "No maintenance available for this year"), options = list(searching = FALSE))
  }

  # filter files and paths
  getFilterpath <- function() {
    stationprefix <- switch(input$station,
                            "Bayelva" = "Ba",
                            "Kurungnakh" = "Ku",
                            "Samoylov" = "Sa",
                            "Sardakh" = "Sd",
                            "TVC" = "TVC")
    return(paste0(maintenancebasepath, stationprefix, "_filter_", input$year, ".dat"))
  }

  getFilterlist <- function() {
    filterpath <- getFilterpath()
    if (file.exists(filterpath))
      return(read.table(filterpath, stringsAsFactors = FALSE, strip.white = TRUE,
                        sep = ",", dec = ".", header = T))
    else return(NULL)
  }


  updateCurrentData <- function(x) {

  ## Get data
  path <- yearlyDatasetPaths$path[(yearlyDatasetPaths$dataset == input$dataset) &
                                  (yearlyDatasetPaths$year == input$year)]
  # path will be empty if dataset changed and input$variable did not update
  if (length(path) == 0) return()
  # get names of selected variable and associated flag
  variablename <- input$variable
  flagname <- paste0(variablename, "_fl")
  # read data and save in global variable
  currentData <<- read.table(file = path, stringsAsFactors = FALSE, strip.white = TRUE, sep = ",", dec = ".", header = T)
  currentData$UTC <<- as.POSIXct(currentData$UTC, tz = 'UTC')
  currentData <<- currentData[, c("UTC", variablename, flagname)]

  ##########################
  ##########################
  # integrate the maintenance flagging
  #
  # for displaying also the data which is flagged due to maintenance
  # in the overview and zoom plot
  # combine flagged data from ****_YEAR_lv1.dat and STATION_filter_YEAR.dat
  # with STATION_maintenance_YEAR.dat
  maintenancelist <- getMaintenancelist()
  # flag the dates which are according to STATION_maintenance_YEAR.dat within the maintenance period with flag 3
  # index of dataset and variable
  ind.ds.var <- which((maintenancelist$dataset %in% input$dataset) & (maintenancelist$variable %in% input$variable))
  if (nrow(maintenancelist[ind.ds.var, ]) > 0) {

    maintenancelist$from <- as.POSIXct(maintenancelist$from, tz = 'UTC') #,origin = "1970-01-01 00:00.00 UTC")
    maintenancelist$to <- as.POSIXct(maintenancelist$to, tz = 'UTC') #,origin = "1970-01-01 00:00.00 UTC")
    # index of existing combinations of station and variable in maintenancelist
    # ind <- which((maintenancelist$dataset %in% input$dataset) & (maintenancelist$variable %in% input$variable))

    for (i in ind.ds.var) {
      currentData[((currentData$UTC >= maintenancelist$from[i]) & (currentData$UTC <= maintenancelist$to[i])), flagname] <- 3
    }
    #####
    # check
    # write.table(x = currentData, file = paste(maintenancebasepath, "flaggedData_test.dat", sep = ""), sep = ",", dec = ".", row.names = FALSE)
    #####
  }


  d <- currentData
  # reshape for plotting with rBokeh, ggplot2 etc...,
  # variable column indices: 2:(2 + length(variablenames) - 1)
  # flag column indices: (2 + length(variablenames)):ncol(d))
  d.m <- reshape(d, direction = "long",
                 idvar = "UTC",
                 varying = list(2:(2 + length(variablename) - 1),
                                (2 + length(variablename)):ncol(d)),
                 timevar = "variable",
                 times = names(d)[2:(2 + length(variablename) - 1)],
                 v.names = c("value", "flag"))
  d.m$dataset <- input$dataset #dataset
  newdata <- d.m #rbind(newdata, d.m)

  newdata$UTC <- as.POSIXct(newdata$UTC, tz = 'UTC', origin = '1970-01-01' )
  store$currentData <- newdata


  #########################
  # initialize flaggedData
  flaggedData <<- currentData
  #########################

  # check
  # write.table(x = currentData, file = paste(maintenancebasepath, "flaggedData_test2.dat", sep = ""), sep = ",", dec = ".", row.names = FALSE)
  #write(x = ind, file = paste(maintenancebasepath, "ind.dat", sep = ""))
  #


  }

  redrawBokehplot <- function() {
    output$bokehplot <- renderRbokeh({
        # renderPlot({
        timing <- proc.time()

        # from sparcviewer
        # validate(need( !is.null(store$currentData),
        #                   # OLD: nrow(store$currentData) > 0,
        #                   paste("No data available for ",
        #                         apply(store$variables[store$variables$selected,
        #                                               c("dataset", "variable")],
        #                               1, function(x) paste(x, collapse = ":")))))
        #
        # validate(need( !is.null(store$currentData),
        #                   # OLD: nrow(store$currentData) > 0,
        #                   paste("No variable selected")))

        #variablename <- isolate(input$variable)
        variablename <- input$variable
        flagname <- paste0(variablename, "_fl")

        # validate needs
        validate(need(variablename, "Please select a variable."))

      # validation
      # validate(need(variablename != "", "Please select a variable"))
      # validate(need(!all(is.na(flaggedData[, variablename])), "All data is NA."))
      # validate(need(nrow(flaggedData) > 0, "No data available."))

      dm <- store$currentData #flaggedData #
      #dm <- dm[!is.na(dm$value), ]

      validate(need( ncol(dm) > 0,
                     paste("No variable selected")))

      # uncomment test <<- dm  and check with str(test) outside shiny ==> the data frame has some attributes
      # create copy without attributes
      dm <- dm[, 1:ncol(dm)]

      # ##########################
      # ##########################
      # # integrate the maintenance flagging  ==> in the current version not necessary any more
      # #
      # # for displaying also the data which is flagged due to maintenance
      # # in the overview and zoom plot
      # # combine flagged data from ****_YEAR_lv1.dat and STATION_filter_YEAR.dat
      # # with STATION_maintenance_YEAR.dat
      # maintenancelist <- getMaintenancelist()
      # # flag the dates which are according to STATION_maintenance_YEAR.dat within the maintenance period with 3
      # if (nrow(maintenancelist[(maintenancelist$dataset %in% input$dataset) & (maintenancelist$variable %in% input$variable), ]) > 0) {
      #
      #   maintenancelist$from <- as.POSIXct(maintenancelist$from, tz = 'UTC') #,origin = "1970-01-01 00:00.00 UTC")
      #   maintenancelist$to <- as.POSIXct(maintenancelist$to, tz = 'UTC') #,origin = "1970-01-01 00:00.00 UTC")
      #   # index of existing combinations of station and variable in maintenancelist
      #   ind <- which((maintenancelist$dataset %in% input$dataset) & (maintenancelist$variable %in% input$variable))
      #
      #   for (i in ind) {
      #     dm[((dm$UTC >= maintenancelist$from[i]) & (dm$UTC <= maintenancelist$to[i])), flagname] <- 3
      #   }
      # }
      # ##########################
      # ##########################
      test <<- dm
      x <- dm

      # create plot
      fig.width <- 1400
      p <-  figure(width = fig.width, height = 800, title = variablename, toolbar_location = "above", legend_location = "top_left") %>%
#              ly_lines(UTC, value, data = x) %>%
              ly_points(UTC, value, data = x,
                        glyph = 21, size = 10, legend = FALSE,
                        line_color = "white", line_width = 4, line_alpha = 0, fill_color = "lightgray", fill_alpha = 0,
                        #glyph = 21, size = 8, legend = FALSE,
                        #color = "lightgray", alpha = 0.2,
                        hover = list(UTC, value, flag, variable)) %>% #list(UTC, value)) %>% #
              ly_points(UTC, value, data = x, size = 3, col = "black", legend = FALSE) %>%
              ly_lines(UTC, value, data = x) %>%
              ### Can't use the data argument together with legend and color parameters,
              ## see issue bokeh/rbokeh/issues/132, adding everything manually
              ## style of points either : size = 10, glyph = 21 or size = 12, line_width = 3, glyph = 1,
              ly_points(x[x$flag == 2, "UTC"], x[x$flag == 2, "value"], size = 12, line_width = 3, glyph = 1,# system error
                        legend = "flag 2, system error", color = flagcolors[2]#, #"red",
                        #hover = subset(x, flag == 2)
                        ) %>%
              ly_points(x[x$flag == 3, "UTC"], x[x$flag == 3, "value"], size = 12, line_width = 3, glyph = 1, # maintenance
                         legend = "flag 3, maintenance", color = flagcolors[3]) %>%
              ly_points(x[x$flag == 4, "UTC"], x[x$flag == 4, "value"], size = 12, line_width = 3, glyph = 1, # physical limits
                        legend = "flag 4, physical limits", color = flagcolors[4]) %>%
              ly_points(x[x$flag == 5, "UTC"], x[x$flag == 5, "value"], size = 12, line_width = 3, glyph = 1, # gradient (peaks)
                        legend = "flag 5, gradient (peaks)", color = flagcolors[5]) %>%
              ly_points(x[x$flag == 6, "UTC"], x[x$flag == 6, "value"], size = 12, line_width = 3, glyph = 1, # plausibility
                        legend = "flag 6, plausibility", color = flagcolors[6]) %>%
              ly_points(x[x$flag == 7, "UTC"], x[x$flag == 7, "value"], size = 12, line_width = 3, glyph = 1, # shift
                        legend = "flag 7, shift", color = flagcolors[7]) %>%
              ly_points(x[x$flag == 8, "UTC"], x[x$flag == 8, "value"], size = 12, line_width = 3, glyph = 1, # snow covered
                        legend = "flag 8, snow covered", color = flagcolors[8]) %>%

              # there was an error related to the format of the dataframe (dataframe with attributes)
              # ==> see reformating of dataframe above
              ly_abline(h = 0, color = "red", type = 2, legend = FALSE) %>%
              tool_crosshair()
#      pp <<- p

      cat(file = stderr(), "Time spent on renderRbokeh", (proc.time() - timing)[3], "s\n")
      p

    })
  }


  # update dataset which is displayed in the plots
  observeEvent(list(input$year, input$variable, input$dataset, input$station), {
    # update years list
    years <- sort(unique(yearlyDatasetPaths$year[yearlyDatasetPaths$dataset == input$dataset]), decreasing = TRUE)
    if (input$year %in% years) {
      # double update to trigger input$dataset invalidation
      updateSelectInput(session, "year", choices = years, selected = input$year)
    } else {
      updateSelectInput(session, "year", choices = years)
    }

    # update dataset list
    datasets <- yearlyDatasetPaths$dataset[(yearlyDatasetPaths$year == input$year) &
                                           (yearlyDatasetPaths$station == input$station)]
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

    redrawMaintenanceTable()

    updateCurrentData()

    ## redraw plot
    redrawBokehplot()
  })


  #### write maintenance periods to file ###
  observeEvent(input$buttonSaveMaintenance, ignoreInit = TRUE, {
    withBusyIndicatorServer("buttonSaveMaintenance", {
      # check for name
      if (input$flagged_by == "") {
        stop("Please enter your initials to save a maintenance.")
      }

      # check if all maintenance parameters are valid and save result with error message
      if (input$from == "") {
        stop("Please enter the beginning of the maintenance period.")
      }
      if (input$to == "") {
        stop("Please enter the end of the maintenance period.")
      }

      ########################
      flagToBeSet <- 3

      ## read old maintenance list
      maintenancelist <- getMaintenancelist()
      # check if maintenancelist exists and if not, create one!
      if (is.null(maintenancelist)) {
        warning("No maintenancelist exists for that year! Creating one...")
        maintenancelist <- data.frame(from = character(0), to = character(0), dataset = character(0),
                                 variable = character(0), flag = character(0),
                                 flagging_date = character(0), flagged_by = character(0),
                                 comments = character(0),
                                 stringsAsFactors = FALSE)
      }
      # construct maintenance line and new maintenancelist for all selected variables:
      maintenancelistNew <- maintenancelist
      for (i in 1:length(input$variables)) {
        # maintenance file
        maintenanceline <- c(input$from, input$to, #valideDaterange,
                            input$dataset, input$variables[i],
                            flagToBeSet, as.character(Sys.time()), input$flagged_by, input$comments)
        maintenancelistNew[nrow(maintenancelistNew) + 1, ] <- maintenanceline
      }

      # write new maintenance list to file
      maintenancepath <- getMaintenancepath()
      write.table(maintenancelistNew, file = maintenancepath, row.names = FALSE, sep = ",", dec = ".")

      redrawMaintenanceTable()

      updateCurrentData()

      ## redraw plot
      redrawBokehplot()
    })
  })

  #### Deletes selected maintenance dates
  observeEvent(input$buttonDeleteMaintenance, ignoreInit = TRUE, {
    withBusyIndicatorServer("buttonDeleteMaintenance", {
      ### Delete selected Maintenance
      # read maintenance list
      maintenancelist <- getMaintenancelist()
      # sanity check
      if (length(input$maintenancetable_rows_selected) < 1 || is.null(maintenancelist)) {
        stop("No maintenance dates selected.")
      }
      # get selected rows ==> input$variables from selection of checkbox
      selectedRows <- maintenancelist[maintenancelist$variable %in% input$variables, ][input$maintenancetable_rows_selected, ]
      # get indexes of selected rows
      selectedRowIdx <- numeric()
      for (i in 1:nrow(maintenancelist)) {
        # slow for large selections, vectorize to optimise
        for (j in 1:nrow(selectedRows)) {
          if (all(maintenancelist[i, ] == selectedRows[j, ], na.rm = TRUE))
            selectedRowIdx <- c(selectedRowIdx, i)
        }
      }
      if (length(selectedRowIdx) < 1) {
        stop("Can't find index of selected rows, aborting...")
        return()
      }
      # remove maintenance from file
      maintenancepath <- getMaintenancepath()
      write.table(maintenancelist[-selectedRowIdx, ], file = maintenancepath, row.names = FALSE, sep = ",", dec = ".")
      redrawMaintenanceTable()

      updateCurrentData()

      ## redraw plot
      redrawBokehplot()

    })
  })

  # update variable checkbox choices on dataset change
  observeEvent(input$dataset, {
    choiceVariables <- store$variables[(store$variables$dataset == input$dataset), "variable"]
    # select variableS

    selectedVariables <<- choiceVariables

    updateCheckboxGroupInput(session, "variables", choices = choiceVariables, selected = selectedVariables)
    selectedVariables <- input$variables

  })

  ## select no variables ==> deselect all variables
  observeEvent(input$resetVariables, {
    # reset variable choices
    store$variables$selected <- FALSE
    updateCheckboxGroupInput(session, "variables", selected = character(0))
    selectedVariables <<- store$variables[(store$variables$dataset == input$dataset) & store$variables$selected, "variable"]
    redrawMaintenanceTable()
  })

  ## select all variables
  observeEvent(input$selectAllVariables, {
    if (input$selectAllVariables == 0) return(NULL)
    else if (input$selectAllVariables > 0) {
    # adapted from example https://stackoverflow.com/questions/28829682/r-shiny-checkboxgroupinput-select-all-checkboxes-by-click

    selectedVariables <<- store$variables[(store$variables$dataset == input$dataset), "variable"]
    updateCheckboxGroupInput(session, "variables", #,"Variable:", choices = variables
                             selected = selectedVariables)

    redrawMaintenanceTable()
    }
  })

  # ## check selection of variables
  # output$selected_var <- renderText({
  #   selectedVariables <- input$variables
  #   paste(selectedVariables)
  # })

  session$allowReconnect(TRUE)
})


#### user interface ####
ui <- shinyUI(
  fluidPage(
    p(" "),
    # enable shinyjs
    useShinyjs(),
    # add custom CSS
    tags$style(appCSS),
    tags$head(tags$style(HTML(
      "#variables {-webkit-column-count: 2; /* Chrome, Safari, Opera */
            -moz-column-count: 3;    /* Firefox */
            column-count: 3;}
            #resetVariables {text-align: center; margin-bottom: 1.4em;}
            #selectAllVariables {text-align: center; margin-bottom: 1.4em;}
            .col-sm-4 {width: 750px;}"))),
    # Sidebar
    fluidRow(
      column(3, wellPanel(

        selectInput("station", "Choose a station:", selected = "Bayelva", # "Samoylov",
                    choices = sort(unique(yearlyDatasetPaths$station))),

        selectInput("dataset", "Choose a dataset:", selected = "BaSnow2019cs", #SaMet2002",
                    choices = yearlyDatasetPaths$dataset[(yearlyDatasetPaths$year == 2019) & #2018) &
                                                         (yearlyDatasetPaths$station == "Bayelva")]), #Samoylov")]),

        selectInput("year", "Choose a year:", selected = 2019, #2018,
                    choices = sort(unique(yearlyDatasetPaths$year), decreasing = TRUE)),

        tags$hr(),
        selectInput("variable", "Variable for visual control plots:", choices = NULL),

        tags$hr(),
        tags$strong("Maintenance flag the selected variables and time period:"),
        checkboxGroupInput("variables", label = NULL, choices = NULL),

        actionButton("resetVariables", "Select None"),
        actionButton("selectAllVariables", "Select All"), ## adapted from example https://stackoverflow.com/questions/28829682/r-shiny-checkboxgroupinput-select-all-checkboxes-by-click

        textInput("flagged_by", "Your name:", placeholder = "Please enter your Initials!"),
        textInput("from", "From:", placeholder = "1828-10-14 00:00:00"),
        textInput("to", "To:", placeholder = "1828-11-03 00:00:00"),
        textInput("comments", "Comments:", placeholder = ""),

        tags$hr(),

        withBusyIndicatorUI(
          actionButton("buttonSaveMaintenance", "Save current maintenance period",
                       icon = icon("refresh"),
                       style = 'white-space:normal;color:black;background-color:green')
        ),

        withBusyIndicatorUI(
          actionButton("buttonDeleteMaintenance", "Delete maintenance period selected in table",
                       icon = icon("eraser"),
                       style = 'white-space:normal;color:red;background-color:orange')
        )
      )),

        column(9,
              dataTableOutput("maintenancetable"),
               #rbokehOutput("bokehplot", width = "100%", height = "100%")
              rbokehOutput("bokehplot", height = 800, width = 1400)

      )
    )
  ))


#### run ####
#options(shiny.reactlog = FALSE)
runApp(shinyApp(ui = ui, server = server))
# for app.R in shiny-server:
# shinyApp(ui = ui, server = server)
