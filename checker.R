###.........................................................................
##
##   Shiny App
##
##   App to display for each station, each dataset and each year which variables have been checked:
##   - not at all,
##   - once,
##   - twice or
##   - thrice
##
##   check 1 and 2: separate check of distinct years of the time series of a variable
##   check 3: check of the complete series of a variable for anomalies / differences between the years
##
##   written by:  stephan.lange@awi.de
##                christian.lehr@awi.de
##   last modified: 2021-03-23
##
##   last check: 2020-02-05
##   checked by: christian.lehr@awi.de
##
###.........................................................................
##
## open issues:
##   - 
##
##
###.........................................................................
##
## last modification:
##  2021-05-14 SL add run buttun to create/update checkfiles 
##  2021-05-14 SL new pathes to git structure
##
## - display only the years that were selected in the trendviewer (rs) app to perform the complete check.
##    ==> because the check for the complete series is always for the complete series, irrespective of the begin and end of the check period
##
###.........................................................................
##
## comments:
##
###.........................................................................

library("shiny")
library("shinyjs")
library("DT")
library("tidyr")
library("shinyBS")
library("shinybusy")
library("shinyWidgets")

#### import data ####
# select running system:
# 1 - windows
# 2 - linux AWI

running.system <- 1

## read paths for
## (a) data files, allowed variables, filter files, check files, etc.
## (b) the file with the definition of the style of the shiny-apps
if (running.system == 1) {
  # read paths and allowed variables for windows
  yearlyDataPath <<- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_auto.csv",
                                 stringsAsFactors = FALSE, strip.white = TRUE)
  allowedVariables   <<- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedVariables.csv",
                                 stringsAsFactors = FALSE, strip.white = TRUE)
  filterbasepath     <<- "N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/filter.files/"
  checkbasepath      <<- "N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/check.files/"
  scriptpath         <<- "N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/"
  # read file for modification of style of shiny-app
  source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/additionals_shiny/appCSS.R")
} else if (running.system == 2) {
  # read paths and allowed variables for linux
  yearlyDataPath <<- read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_AWI.csv", stringsAsFactors = FALSE,
                                 strip.white = TRUE)
  allowedVariables <<- read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedVariables.csv", stringsAsFactors = FALSE,
                               strip.white = TRUE)
  filterbasepath     <<- "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/filter.files/"
  checkbasepath      <<- "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/check.files/"
  scriptpath         <<- "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/"
    # read file for modification of style of shiny-app
  source("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/additionals_shiny/appCSS.R")
}

###.........................................................................

stations <- c("Bayelva" = "Ba","Kurungnakh" = "Ku", "Samoylov" = "Sa", "Sardakh" = "Sd", "TVC" = "TVC")
datasets <- vector("list", length(stations))
# Define which years to check:
# The check period starts with the first year and ends with the last complete year before the recent year.
recent.year <- as.numeric(format(Sys.Date(), "%Y"))
check.end <- recent.year  # for a check of the script use only recent.year (without - 1)
# special condition for Samoylov ==> here only the period until two years before the recent year can be checked
# check.end.s <- recent.year - 2
#years <- list(1998:check.end, 2013:check.end, 1998:check.end.s, 2009:check.end, 2016:check.end)
years <- list(1998:check.end, 2013:check.end, 1998:check.end, 2009:check.end, 2016:check.end)

for (i in 1:length(datasets)) {
  datasets[[i]] <- trimws(as.character(unique(yearlyDataPath$dataset[which(yearlyDataPath$station == names(stations)[i])])), "l")
}

###.........................................................................
# #### helper functions for busy/error button feedbacks
# #
# # https://github.com/daattali/advanced-shiny/tree/master/busy-indicator
# #
# # Set up a button to have an animated loading indicator and a checkmark
# # for better user experience
# # Need to use with the corresponding `withBusyIndicator` server function
# withBusyIndicatorUI <- function(button) {
#   id <- button[['attribs']][['id']]
#   div(
#     `data-for-btn` = id,
#     button,
#     span(
#       class = "btn-loading-container",
#       hidden(
#         icon("spinner", class = "btn-loading-indicator"),
#         icon("check", class = "btn-done-indicator")
#       )
#     ),
#     hidden(
#       div(class = "btn-err",
#           div(icon("exclamation-circle"),
#               tags$b("Error: "),
#               span(class = "btn-err-msg")
#           )
#       )
#     )
#   )
# }
# 
# # Call this function from the server with the button id that is clicked and the
# # expression to run when the button is clicked
# withBusyIndicatorServer <- function(buttonId, expr) {
#   # UX stuff: show the "busy" message, hide the other messages, disable the button
#   loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
#   doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
#   errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
#   shinyjs::disable(buttonId)
#   shinyjs::show(selector = loadingEl)
#   shinyjs::hide(selector = doneEl)
#   shinyjs::hide(selector = errEl)
#   on.exit({
#     shinyjs::enable(buttonId)
#     shinyjs::hide(selector = loadingEl)
#   })
# 
#   # Try to run the code when the button is clicked and show an error message if
#   # an error occurs or a success message if it completes
#   tryCatch({
#     value <- expr
#     shinyjs::show(selector = doneEl)
#     shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade", time = 0.5))
#     value
#   }, error = function(err) { errorFunc(err, buttonId) })
# }
# 
# # When an error happens after a button click, show the error
# errorFunc <- function(err, buttonId) {
#   errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
#   errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
#   errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
#   shinyjs::html(html = errMessage, selector = errElMsg)
#   shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
# }
# 

###.........................................................................
#### server logic ####
server <- shinyServer(function(input, output, session) {
  observeEvent(input$b.utton, {# process button
    show_modal_progress_line(color = "#06afce",text = "Starting computation",height = "40px")
    start_time <- Sys.time()     
    update_modal_progress(0.1,
                          text = paste("loading existing files"))
    Sys.sleep(3)# to see the last comment
    try(source(paste0(scriptpath, "additionals_shiny/create_update_checkfiles.R")))
    
    
    end_time <- Sys.time()     
    update_modal_progress(1,
                          text = paste("Finished within ",round(end_time-start_time,0)," sec\n"))
    
    Sys.sleep(3)# to see the last comment
    remove_modal_progress()
  })


  ###.........................................................................
  # create one table with all combinations of stations, years, datasets and variables.
  df.0 <- data.frame("station" = character(0), "dataset" = character(0), "variable" = character(0), "check1" = character(0), "controller1" = character(0), "check2" = character(0), "controller2" = character(0), "year" = character(0))

  ###.........................................................................
  # loop for yearly checks of series
  for (i in 1:length(stations)) {
    for (k in 1:length(years[[i]])) {
      # check whether check file of this year exists. If not, throw an error.
      # ==> Create a new check file for the respective year with the script "create_empty_checkfiles.R"
      checkpath.yr <- paste(checkbasepath, stations[i], "_check_", years[[i]][k], ".dat", sep = "")
      # validate(
      #     need(file.exists(checkpath.yr), paste(checkpath.yr, 'does not exist'))
      #     )
      if (file.exists(checkpath.yr)) {
        dat.1      <- read.table(file = checkpath.yr, sep = ",", dec = ".", header = TRUE)
        dat.1$year <- years[[i]][k]
      }
      else {
        # create entries for the year with the non existing check file based on the selection of variables of the year before.
        # ==> choose year before and replace check values with NA
        dat.1      <- read.table(file = paste(checkbasepath, stations[i], "_check_", years[[i]][k - 1], ".dat", sep = ""), sep = ",", dec = ".", header = TRUE)
        dat.1[, c("check1", "controller1", "check2", "controller2")] <- NA
        dat.1$year <- years[[i]][k]
      }
      df.0       <- rbind(df.0, dat.1)
    }
  }

  # re-order columns of data frame
  df.0 <- df.0[, c("station", "dataset", "variable", "year", "check1", "controller1", "check2", "controller2")]

  # convert data format of columns of data.frame to character format
  # https://stackoverflow.com/questions/29035508/r-add-character-vector-to-data-frame-row
  indx       <- sapply(df.0, is.factor)
  df.0[indx] <- lapply(df.0[indx], as.character)
  ###.........................................................................

  # create one table with all combinations of stations, years, datasets and variables
  # for the two annual checks (Check 1 and 2) + the check of the complete series (check 3)
  df.1 <- df.0
  df.1[, "controller3"] <- df.1[, "check3"] <- character(0)
  #df.1 <- data.frame("station" = character(0), "dataset" = character(0), "variable" = character(0), "check1" = character(0), "controller1" = character(0), "check2" = character(0), "controller2" = character(0), "year" = character(0))

  # loop for checks of complete series
  for (i in 1:length(stations)) {
    # read file for check of complete series of stations[i]
    dat.2 <- read.table(paste(checkbasepath, stations[i], "_check_complete.dat", sep = ""), sep = ",", dec = ".", header = TRUE)
    # convert data format of columns to character format
    indx <- sapply(dat.2, is.factor)
    dat.2[indx] <- lapply(dat.2[indx], as.character)
    # loop through all combinations of datasets and variables
    for (j in 1:nrow(dat.2)) {
      # if there was a control of the complete series
      if (!is.na(dat.2$controller3[j])) {
        # condition 1 to 2: select the respective combination of dataset, variable
        # condition 3 to 4: select only the years that were selected in the trendviewer (rs) app to perform the complete check.
        #                   ==> uncomment condition 3 and 4, because the check for the complete series is always for the complete series, irrespective of the begin and end of the check period
        # condition 5: select only the complete years which are before the check date of the complete series.
        #             select only the first 4 characters of dat.2$check3[j] ==> YYYY for the comparison
        # condition 6: select only the years which are before the check.end ==> that is the last complete year before the running year ==> recent.year
        #               ==> not needed any more
        ind.2 <- which((df.1$dataset == dat.2$dataset[j]) &
                         (df.1$variable == dat.2$variable[j]) &
                         # (df.1$year >= dat.2$begin[j]) & ## ==> condition 3 uncommented ==> see notes above
                         # (df.1$year <= dat.2$end[j]) &  ## ==> condition 4 uncommented ==> see notes above
                         # (dat.2$end[j] <= as.numeric(substr(dat.2$check3[j], 1, 4))) &
                         (df.1$year < as.numeric(substr(dat.2$check3[j], 1, 4))) )#&
                         #(df.1$year <= check.end)) # ==> condition 6 uncommented
        # and assign date of check and name of the controller to the selection
        df.1$check3[ind.2] <- dat.2$check3[j]
        df.1$controller3[ind.2] <- dat.2$controller3[j]
      }
    }
  }

  # function to subset the data according to station (e.g. "Bayelva") and dataset (e.g. "BaSoil2009")
  # + layout of the table in the shiny-app
  redrawCheckTable <- function() {
    # subsetting of data
    # Exchange the following line df.2 <- ....
    df.2 <- df.1[which( (df.1$station == input$station) & (df.1$dataset == input$dataset) ), ]
    # with one of the following lines for manual check of the function
    # df.2 <- df.1[ which(df.1$station == "Bayelva" & df.1$dataset == "BaSoil2009" ), ]
    # df.2 <- df.1[ which(df.1$station == "Sardakh" & df.1$dataset == "SdHole2009" ), ]
    df.3 <- df.2[, c("variable", "year")]
    df.3$sum <- as.numeric(!is.na(df.2$check1)) + as.numeric(!is.na(df.2$check2)) + as.numeric(!is.na(df.2$check3))
    sort.ing <- df.3$variable # for not loosing the sorting
    df.4 <- spread(df.3, year, sum)
    row.names(df.4) <- df.4[, 1]# for not loosing the sorting
    #        browser()
    df.4 <- df.4[sort.ing[1:length(df.4[, 1])], ]# for not loosing the sorting
    #        browser()
    df.4 <- df.4[, c(1, rev(2:ncol(df.4)))]

    # layout of the table in the shiny-app
    output$checktable <- DT::renderDataTable({
      datatable(df.4, selection = list(target = 'cell'), class = 'cell-border stripe', escape = FALSE, rownames = FALSE,
                options = list(pageLength = 150, searchHighlight = TRUE, ordering = 0, dom = "t",#===>option to show only table
                               search = list(search = ''))) %>% formatStyle(
                                 names(df.4),
                                 backgroundColor = styleEqual(c(0, 1, 2, 3), c('#ffffff', '#ecf9f2', '#b3e6cc', '#39ac73')),
                                 color = styleEqual(c(0, 1, 2, 3), c('#ffffff', '#ecf9f2', '#b3e6cc', '#39ac73'))
                               )
    })

    ###.........................................................................
    # for output$clickIndex1 and output$clickIndex2 the functions validate(need(...)) and req(...) are used to omit error messages which are related to missing input during the start of the application. (When the application is started the first time no cells are selected.)
    output$clickIndex1 <- renderText({
      # if no cells are selected ask the user to select a cell
      # https://shiny.rstudio.com/reference/shiny/latest/validate.html
      validate(
        need(input$checktable_cells_selected, 'Please select a cell!')
      )
      # index of selected cell
      cell.ind <- input$checktable_cells_selected
      # display error message if variable column is selected
      if (0 %in% cell.ind[, 2]) {
        paste0('Please deselect variable column!')
      }
      else {
        # selected year
        year.ind <- names(df.4)[cell.ind[, 2] + 1]
        # selected variable
        var.ind <- df.4$variable[cell.ind[, 1]]
        # row index of combination of selected variable and year in df.2
        row.ind <- rep(NA, nrow(cell.ind))
        for (ii in 1:nrow(cell.ind)) {
          row.ind[ii] <- which((df.2$variable == var.ind[ii]) & (df.2$year == year.ind[ii]))
        }
        # date of check 1, select only the first 10 characters ==> YYYY-MM-DD
        date.check1 <- substr(df.2$check1[row.ind], 1, 10)
        # controller of check 1
        controller.check1 <- df.2$controller1[row.ind]
        # line to check the code
        # paste0("check1 = ", date.check1,", ", controller.check1," || year ", year.ind  ,", var ", var.ind,  "\n")
        # final line to be displayed
        paste0(#"check1 = ",
          date.check1,", ", controller.check1,"\n")
      }
    })

    output$clickIndex2 <- renderText({
      # if no cells are selected stop running this section
      # https://shiny.rstudio.com/articles/req.html
      req(input$checktable_cells_selected)
      # # if no cells are selected ask the user to select a cell
      # # https://shiny.rstudio.com/reference/shiny/latest/validate.html
      # validate(
      #     need(input$checktable_cells_selected, 'Please select a cell!')
      # )
      # index of selected cell
      cell.ind <- input$checktable_cells_selected
      # display error message if variable column is selected
      if (0 %in% cell.ind[, 2]) {
        paste0('Please deselect variable column!')
      }

      else {
        # selected year
        year.ind <- names(df.4)[cell.ind[, 2] + 1]
        # selected variable
        var.ind <- df.4[, 1][cell.ind[, 1]]
        # row index of combination of selected variable and year in df.2
        row.ind <- rep(NA, nrow(cell.ind))
        for (ii in 1:nrow(cell.ind)) {
          row.ind[ii] <- which((df.2$variable == var.ind[ii]) & (df.2$year == year.ind[ii]))
        }
        # date of check 2, select only the first 10 characters ==> YYYY-MM-DD
        date.check2 <- substr(df.2$check2[row.ind], 1, 10)
        # controller of check 2
        controller.check2 <- df.2$controller2[row.ind]
        # line to check the code
        # paste0("check2 = ", date.check2,", ", controller.check2, " || year ", year.ind  ,", var ", var.ind, "\n")
        # final line to be displayed
        paste0(#"check2 = ",
          date.check2,", ", controller.check2, "\n")
      }
    })

    output$clickIndex3 <- renderText({
      # if no cells are selected stop running this section
      # https://shiny.rstudio.com/articles/req.html
      req(input$checktable_cells_selected)
      # # if no cells are selected ask the user to select a cell
      # # https://shiny.rstudio.com/reference/shiny/latest/validate.html
      # validate(
      #     need(input$checktable_cells_selected, 'Please select a cell!')
      # )
      # index of selected cell
      cell.ind <- input$checktable_cells_selected
      # display error message if variable column is selected
      if (0 %in% cell.ind[, 2]) {
        paste0('Please deselect variable column!')
      }
      else {
        # selected year
        year.ind <- names(df.4)[cell.ind[, 2] + 1]
        # selected variable
        var.ind <- df.4[, 1][cell.ind[, 1]]
        # row index of combination of selected variable and year in df.2
        row.ind <- rep(NA, nrow(cell.ind))
        for (ii in 1:nrow(cell.ind)) {
          row.ind[ii] <- which((df.2$variable == var.ind[ii]) & (df.2$year == year.ind[ii]))
        }
        # date of check 3, select only the first 10 characters ==> YYYY-MM-DD
        date.check3 <- substr(df.2$check3[row.ind], 1, 10)
        # controller of check 2
        controller.check3 <- df.2$controller3[row.ind]
        # line to check the code
        # paste0("check3 = ", date.check3,", ", controller.check3, " || year ", year.ind  ,", var ", var.ind, "\n")
        # final line to be displayed
        paste0(#"check3 = ",
          date.check3,", ", controller.check3,"\n")
      }
    })
  }

  # function to select which station and dataset is used for the subsetting of the data
  observeEvent(list(input$year, input$variable, input$dataset, input$station), {
    # update dataset list
    datasets <- yearlyDataPath$dataset[yearlyDataPath$station == input$station]
    if (input$dataset %in% datasets) {
      # double update to trigger input$dataset invalidation
      updateSelectInput(session, "dataset", choices = datasets, selected = input$dataset)
    } else {
      updateSelectInput(session, "dataset", choices = datasets)
      return()
    }
    # update the displayed table
    redrawCheckTable()
  })
  session$allowReconnect(TRUE)
})


###.........................................................................
#### user interface ####
ui <- shinyUI(
  fluidPage(
    p(" "),
    # enable shinyjs
    useShinyjs(),
    # add custom CSS
    tags$style(appCSS),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(#'#ffffff', '#ecf9f2', '#b3e6cc', '#39ac73'
        # legend:
        # convert colors from rgb to hexadecimal: ==> see in the header
        p("ZERO checks", style = 'white-space:normal;background-color:#ffffff'),
        p("Checked once", style = 'white-space:normal;background-color:#ecf9f2'),
        p("Checked twice", style = 'white-space:normal;background-color:#b3e6cc'),
        p("Checked thrice", style = 'white-space:normal;background-color:#39ac73'),

        # drop down menus to selecet the station and the dataset to be displayed
        selectInput("station", "Choose a station:", selected = "Sardakh", #"Samoylov",
                    choices = sort(unique(yearlyDataPath$station))),
        #bsTooltip('station', 'This button will inflate a balloon'),
        selectInput("dataset", "Choose a dataset:", selected = "SdHole2009",##"SaMet2002",
                    choices = yearlyDataPath$dataset[yearlyDataPath$year == 2018 & yearlyDataPath$station == "Sardakh"]),#"Samoylov"]),

        tags$hr(),

        # check info boxes
        p("check 1"),
        verbatimTextOutput("clickIndex1"),#, placeholder = "Please select a cell"),
        p("check 2"),
        verbatimTextOutput("clickIndex2"),
        p("check of complete series"),
        verbatimTextOutput("clickIndex3"),
        hr(),
        p("for new years and new stations, refresh check files"),
        actionBttn(
          inputId = "b.utton",
          label = "Refresh check files",
          style = "float", 
          color = "danger"
        )
      ),

      mainPanel(
        dataTableOutput("checktable")
      )
    )
  )
)


###.........................................................................
#### run ####
# options(shiny.reactlog = FALSE)
runApp(shinyApp(ui = ui, server = server))
# for app.R in shiny-server:
# shinyApp(ui = ui, server = server)

