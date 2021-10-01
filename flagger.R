#############################################################################
##
##   Shiny App
##
##   App to view the observed time series and their data quality flags
##
##   It includes:
##
##   - a tool to manually flag data
##   - a check button to confirm that the respective year is checked and ready for publication
##   ==> the check button saves the name of the controller and the date of the check
##
##   written by:  stephan.lange@awi.de
##                christian.lehr@awi.de
##
##   last check: 2020-02-06
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
## 2021-03-23, SL: new pathes to git structure
## 2020-09-14, CL: change of wording: "created" ==> "flagging_date", "creator" ==> "flagged_by"
## 2020-03-20, CL:  - format date columns to the default date format "UTC" for correct match of dates read from the filter (flag 6) tables and the date format of the plots
##                  - Selection of filter data points (flag 6) changed from "<" and ">" to "<=" and ">="
## 2020-04-06, CL:  - Button Delete filters ==> replacement of index of variable ==> which(filterlist$variable == input$variable)
##                  with index of combination of variable and dataset ==> which((filterlist$dataset == input$dataset) & (filterlist$variable == input$variable))
## 2020-02-06, CL:  - if maintenance periods exist, mark them in the plots
##                    ==> separate tables are used. "maint" for Bayelva and "maint_sa" for Samoylov, Sardakh and Kurungnakh
## 2020-02-05, CL:  - display data points marked as flag 6 in the filter table of the selected year (e.g. "Ba_filter_2018.dat") in both plots
##                  - display selected entries from table in overview plot
##                  replacement of index of variable ==> which(checklist$variable == variablename)
##                  with index of combination of variable and dataset ==> which((checklist$dataset == input$dataset) & (checklist$variable == variablename))
## 2020-02-03, CL:  - replacement of index of variable ==> which(checklist$variable == input$variable)
##                  with index of combination of variable and dataset ==> which((checklist$dataset == input$dataset) & (checklist$variable == input$variable))
##
###############################################################################
##
## comments:
##
###############################################################################

library(shiny)
library(shinyjs)
library(DT)



## read paths and allowed variables
if (.Platform$OS.type == "windows") {
  yearlyDatasetPaths <- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_auto.csv",
                                 stringsAsFactors = FALSE, strip.white = TRUE)
  allowedVariables   <- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedVariables.csv",
                                 stringsAsFactors = FALSE, strip.white = TRUE)
  allowedcompVariables   <- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedcompVariables.csv",
                                 stringsAsFactors = FALSE, strip.white = TRUE)
  #db.path            <- "N:/sparc/LTO/R_database/database_R/Sa_02_Lvl0_Lvl1/"
  filterbasepath     <- "N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/filter.files/"
  checkbasepath      <- "N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/check.files/"
  # maintenance period Bayelva
  maint <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)
  # maintenance period Samoylov
  maint_sa <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/sa_maintance.txt", sep = "\t", header = T)
  # read file for modification of style of shiny-app
  source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/additionals_shiny/appCSS.R")
} else {
  yearlyDatasetPaths <- read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_AWI.csv", stringsAsFactors = FALSE,
                                 strip.white = TRUE)
  allowedVariables <- read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedVariables.csv", stringsAsFactors = FALSE,
                               strip.white = TRUE)
  allowedcompVariables   <- read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedcompVariables.csv",
                                     stringsAsFactors = FALSE, strip.white = TRUE)
  
  #db.path            <- "/sparc/LTO/R_database/database_R/Sa_02_Lvl0_Lvl1/"
  filterbasepath     <- "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/filter.files/"
  checkbasepath      <- "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/check.files/"
  # maintenance period Bayelva
  maint <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)
  # maintenance period Samoylov
  maint_sa <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/sa_maintance.txt", sep = "\t", header = T)
  # read file for modification of style of shiny-app
  source("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/additionals_shiny/appCSS.R")
}

# color ramp for flags
flagcolors <- rainbow(20)
flagcolors[2] <- "#AC1116"
flagcolors[3] <- "#9346A4"
flagcolors[4] <- "#71D7DA"
flagcolors[5] <- "#4B9158"
flagcolors[6] <- "#9A632D"
flagcolors[7] <- "orchid"
flagcolors[8] <- "aquamarine"

origin <- "1970-01-01 00:00:00"

# same colours like in sparcviewer
# flagcolors[2:8] <- c("red", "thistle", "skyblue", "darkblue", "palegreen", "orchid", "aquamarine")

# color for maintenance periods
# color <- rgb(190, 190, 190, alpha = 70, maxColorValue = 255)

###############################
#### helper functions for busy/error button feedbacks
#
# https://github.com/daattali/advanced-shiny/tree/master/busy-indicator
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
  # holds original data
  currentData <- data.frame()
  # holds temporary flagged data
  flaggedData <- data.frame()
  # holds comparison data
  comparisonData <- data.frame()

  ######## plots
  ## overview plot
  redrawOverviewPlot <- function() {
    output$overviewplot <- renderPlot({
      # get names of selected variable and associated flag
      variablename <- isolate(input$variable)
      flagname <- paste0(variablename, "_fl")

      # validation
      validate(need(variablename != "", "Please select a variable"))
      validate(need(!all(is.na(flaggedData[, variablename])), "All data is NA."))
      validate(need(nrow(flaggedData) > 0, "No data available."))

      # extract flagged and regular values from working copy
      flaggedData.noflag <- flaggedData[flaggedData[, flagname] == 0, ]
      flaggedData.flag <- flaggedData[flaggedData[, flagname] != 0, ]

      ## Calculate y-axis limits, 5% of range above and below all values
      ## that don't have flag 2|4
      datarange <- range(flaggedData[(flaggedData[, flagname] != 2) &
                                     (flaggedData[, flagname] != 4), variablename], na.rm = T)
      ylim <- datarange + diff(datarange) * c(-1, 1) * 0.05

      if (length(na.omit(ylim)) <= 1) {
        ylim = c(-2000, 2000)
      }
      # set plot margins
      par(mar = c(4, 3, 1, 3))
      plot(x = flaggedData$UTC, y = flaggedData[, variablename], xlab = "", ylab = "",
           ylim = ylim, type = "n", xaxt = "n")
      # draw x-axis
      xTicks <- axis.POSIXct(1, at = seq(min(flaggedData$UTC), max(flaggedData$UTC), by = "month"))

      ##########
      # plot maintenance period ==> adapted from plot_maintenance
      if (input$station == "Bayelva") {
        # if the current year appears in the table of the maintenance periods, mark the maintenance period as grey rectangle in the plot
        ind.yr <- grep(input$year, maint$start)
        if (length(ind.yr) > 0) {
          rect(xleft = as.numeric(strptime(maint$start[ind.yr], format = "%d.%m.%Y")), xright = as.numeric(strptime(maint$end[ind.yr], format = "%d.%m.%Y")),
              ybottom = -150000, ytop = 150000, col = rgb(190, 190, 190, alpha = 70, maxColorValue = 255), border = "transparent")
        }
      }

      if (input$station %in% c("Samoylov", "Sardakh", "Kurungnakh")) {
        # if the current year appears in the table of the maintenance periods, mark the maintenance period as grey rectangle in the plot
        ind.yr <- grep(input$year, maint_sa$start)
        if (length(ind.yr) > 0) {
        rect(xleft = as.numeric(strptime(maint_sa$start[ind.yr], format = "%d.%m.%Y")), xright = as.numeric(strptime(maint_sa$end[ind.yr], format = "%d.%m.%Y")),
             ybottom = -150000, ytop = 150000, col = rgb(190, 190, 190, alpha = 70, maxColorValue = 255), border = "transparent")
        }
      }
      ##########

      # draw x-axis grid aligning with date ticks
      abline(v = xTicks, col = "lightgray", lty = "dotted")
      # draw y-axis grid
      grid(nx = NA, ny = NULL)
      #browser()
      if (nrow(comparisonData) > 0) {
        if(input$comparisonrange == TRUE){
          legend("topleft", paste("comparison:", input$comparisonVariable,
                                  input$comparisonDataset, input$comparisonYear),
                 lty = 2, col = "red")
          lines(x = comparisonData$UTC, y = comparisonData$value, col = "red")
          pos.comp.axis<-c(range(na.omit(comparisonData$value)),round(mean(na.omit(comparisonData$value)),2),range(na.omit(comparisonData$value)))
          axis(4,at=pos.comp.axis,labels=pos.comp.axis,col="red",col.axis="red")
          #browser()
        }else if(input$comparisonrange == FALSE){
          legend("topleft", paste("comparison:", input$comparisonVariable,
                                  input$comparisonDataset, input$comparisonYear),
                 lty = 2, col = "red")
          #browser()
          comparisonData$valuez <- comparisonData$value-mean(range(na.omit(comparisonData$value)))
          comp.ratio <- diff(datarange)/diff(range(na.omit(comparisonData$value)))
          pos.comp.axis <- c(range(na.omit(comparisonData$valuez))[1]*comp.ratio,round(mean(na.omit(comparisonData$valuez)),2),range(na.omit(comparisonData$valuez))[2]*comp.ratio)
          lab.comp.axis <- c(range(na.omit(comparisonData$value))[1],round(mean(na.omit(comparisonData$value)),2),range(na.omit(comparisonData$value))[2])
          lines(x = comparisonData$UTC, y = comp.ratio*(comparisonData$value-mean(range(na.omit(comparisonData$value)))), col = "red")
          axis(4,at=pos.comp.axis,labels=lab.comp.axis,col="red",col.axis="red")
          
          #
     }}
      
      # plot regular values
      points(x = flaggedData.noflag$UTC, y = flaggedData.noflag[, variablename],
             type = "p", pch = ".", cex = 0.2)
      # plot flagged values
      for (flag in unique(flaggedData.flag[, flagname])) {  #2:8 { sort(as.character(unique(flaggedData.flag[, flagname])))) {
        if ( length(flaggedData.flag[, flagname] == flag) > 0 ) {
          points(x = flaggedData.flag$UTC[flaggedData.flag[, flagname] == flag],
                 y = flaggedData.flag[flaggedData.flag[, flagname] == flag, variablename],
                 col = flagcolors[flag], pch = 19)
        }
      }

      # add flag 6 points from filter table
      filterlist <- getFilterlist()
      if (!is.null(filterlist)) {
        # format date columns to the default date format "UTC"
        filterlist$from <- as.POSIXct(filterlist$from, origin = origin, tz = "UTC")
        filterlist$to <- as.POSIXct(filterlist$to, origin = origin, tz = "UTC")
        # index of combination of selected variable and selected dataset
        f.ind.ds.var <- which((filterlist$dataset == input$dataset) & (filterlist$variable == variablename))
        filterlist.var <- filterlist[f.ind.ds.var, ] #[which(filterlist$variable == variablename),]
        selectedFilters <- filterlist.var
        for (ii in 1:nrow(selectedFilters)) {
          selectedPoints <- flaggedData[(flaggedData$UTC >= selectedFilters[ii, ]$from) &
                                          (flaggedData$UTC <= selectedFilters[ii, ]$to) &
                                          (flaggedData[, variablename] >= selectedFilters[ii, ]$min) &
                                          (flaggedData[, variablename] <= selectedFilters[ii, ]$max), ]
          points(x = selectedPoints$UTC, y = selectedPoints[, variablename], pch = 1, col = flagcolors[6], cex = 2)
        }
      }

      # highlight points marked by selected filter
      if (!is.null(input$filtertable_rows_selected)) {
        filterlist <- getFilterlist()
        if (!is.null(filterlist)) {
          # format date columns to the default date format "UTC"
          filterlist$from <- as.POSIXct(filterlist$from, origin = origin, tz = "UTC")
          filterlist$to <- as.POSIXct(filterlist$to, origin = origin, tz = "UTC")
          # index of combination of selected variable and selected dataset
          f.ind.ds.var <- which((filterlist$dataset == input$dataset) & (filterlist$variable == variablename))
          filterlist.var <- filterlist[f.ind.ds.var, ] #[which(filterlist$variable == variablename),]
          selectedFilters <- filterlist.var[input$filtertable_rows_selected, ]
          for (iii in 1:nrow(selectedFilters)) {
            selectedPoints <- flaggedData[(flaggedData$UTC >= selectedFilters[iii, ]$from) &
                                          (flaggedData$UTC <= selectedFilters[iii, ]$to) &
                                          (flaggedData[, variablename] >= selectedFilters[iii, ]$min) &
                                          (flaggedData[, variablename] <= selectedFilters[iii, ]$max), ]
            points(x = selectedPoints$UTC, y = selectedPoints[, variablename], pch = 3, col = "red")
          }
        }
      }
    })
  }

  # zoom plot
  redrawZoomplot <- function() {
    output$zoomplot <- renderPlot({
      variablename <- isolate(input$variable)
      flagname <- paste0(variablename, "_fl")
      overviewbrush <- isolate(input$overviewplotBrush)
      # validate needs
      validate(need(overviewbrush, "Draw a rectangle in the overview plot to zoom in."))
      validate(need(variablename, "Please select variables."))

      # Extract flagged data
      flaggedData.noflag <- flaggedData[flaggedData[, flagname] == 0, ]
      flaggedData.flag <- flaggedData[flaggedData[, flagname] != 0, ]
      
      datarange <- range(flaggedData[(flaggedData[, flagname] != 2) &
                                       (flaggedData[, flagname] != 4), variablename], na.rm = T)
      # set plot margins
      par(mar = c(4, 3, 1, 3))
      # plot
     
      plot(x = flaggedData.noflag$UTC,
           y = flaggedData.noflag[, variablename],
           type = "p", pch = 20, cex = .4, xlab = "date", ylab = "", xaxt = "n",
           xlim = c(overviewbrush$xmin, overviewbrush$xmax),
           ylim = c(overviewbrush$ymin, overviewbrush$ymax))
      lines(flaggedData.noflag$UTC, flaggedData.noflag[, variablename], col = "gray50")
      # draw x-axis
      xTicks <- axis.POSIXct(1, at = as.POSIXct(seq(overviewbrush$xmin, overviewbrush$xmax, length.out = 6),
                                                origin = origin, tz = "UTC"))

      ##########
      # plot maintenance period ==> adapted from plot_maintenance
      if (input$station == "Bayelva") {
        # if the current year appears in the table of the maintenance periods, mark the maintenance period as grey rectangle in the plot
        ind.yr <- grep(input$year, maint$start)
        if (length(ind.yr) > 0) {
        rect(xleft = as.numeric(strptime(maint$start[ind.yr], format = "%d.%m.%Y")), xright = as.numeric(strptime(maint$end[ind.yr], format = "%d.%m.%Y")),
             ybottom = -1500, ytop = 1500, col = rgb(190, 190, 190, alpha = 70, maxColorValue = 255), border = "transparent")
        }
      }

      if (input$station %in% c("Samoylov", "Sardakh", "Kurungnakh")) {
        # if the current year appears in the table of the maintenance periods, mark the maintenance period as grey rectangle in the plot
        ind.yr <- grep(input$year, maint_sa$start)
        if (length(ind.yr) > 0) {
        rect(xleft = as.numeric(strptime(maint_sa$start[ind.yr], format = "%d.%m.%Y")), xright = as.numeric(strptime(maint_sa$end[ind.yr], format = "%d.%m.%Y")),
             ybottom = -1500, ytop = 1500, col = rgb(190, 190, 190, alpha = 70, maxColorValue = 255), border = "transparent")
        }
      }
      ##########

      # draw grid aligning with x axis ticks
      abline(v = xTicks, col = "lightgray", lty = "dotted")
      # draw y-axis grid
      grid(nx = NA, ny = NULL)

      # plot comparison data if present
      if (nrow(comparisonData) > 0) {
        if(input$comparisonrange == TRUE){
          legend("topleft", paste("comparison:", input$comparisonVariable,
                                  input$comparisonDataset, input$comparisonYear),
                 lty = 1, col = "red")
          lines(x = comparisonData$UTC, y = comparisonData$value, col = "red")
          pos.comp.axis<-c(range(na.omit(comparisonData$value)),round(mean(na.omit(comparisonData$value)),2),range(na.omit(comparisonData$value)))
          axis(4,at=pos.comp.axis,labels=pos.comp.axis,col="red",col.axis="red")
          
          #browser()
        }else if(input$comparisonrange == FALSE){
          legend("topleft", paste("comparison:", input$comparisonVariable,
                                  input$comparisonDataset, input$comparisonYear),
                 lty = 1, col = "red")
          comparisonData$valuez <- comparisonData$value-mean(range(na.omit(comparisonData$value)))
          comp.ratio <- diff(datarange)/diff(range(na.omit(comparisonData$value)))
          pos.comp.axis <- c(range(na.omit(comparisonData$valuez))[1]*comp.ratio,round(mean(na.omit(comparisonData$valuez)),2),range(na.omit(comparisonData$valuez))[2]*comp.ratio)
          lab.comp.axis <- c(range(na.omit(comparisonData$value))[1],round(mean(na.omit(comparisonData$value)),2),range(na.omit(comparisonData$value))[2])
          lines(x = comparisonData$UTC, y = comp.ratio*(comparisonData$value-mean(range(na.omit(comparisonData$value)))), col = "red")
          axis(4,at=pos.comp.axis,labels=lab.comp.axis,col="red",col.axis="red")
          
          #browser()
        }}
      

      # add flagged points
      for (flag in unique(flaggedData.flag[, flagname])) {
        if (flag == 20) break
        points(x = flaggedData.flag$UTC[flaggedData.flag[, flagname] == flag],
               y = flaggedData.flag[flaggedData.flag[, flagname] == flag, variablename],
               col = flagcolors[flag], pch = 3)
      }

      # add flag 6 points from filter table
      filterlist <- getFilterlist()
      if (!is.null(filterlist)) {
        # format date columns to the default date format "UTC"
        filterlist$from <- as.POSIXct(filterlist$from, origin = origin, tz = "UTC")
        filterlist$to <- as.POSIXct(filterlist$to, origin = origin, tz = "UTC")
        # index of combination of selected variable and selected dataset
        f.ind.ds.var <- which((filterlist$dataset == input$dataset) & (filterlist$variable == variablename))
        filterlist.var <- filterlist[f.ind.ds.var, ] #[which(filterlist$variable == variablename),]
        selectedFilters <- filterlist.var
        for (iiii in 1:nrow(selectedFilters)) {
          selectedPoints <- flaggedData[(flaggedData$UTC >= selectedFilters[iiii, ]$from) &
                                          (flaggedData$UTC <= selectedFilters[iiii, ]$to) &
                                          (flaggedData[, variablename] >= selectedFilters[iiii, ]$min) &
                                          (flaggedData[, variablename] <= selectedFilters[iiii, ]$max), ]
          points(x = selectedPoints$UTC, y = selectedPoints[, variablename], pch = 1, col = flagcolors[6], cex = 2)
        }
      }

      # add selected points
      if (!is.null(isolate(input$zoomplotBrush))) {
        zoombrush <- isolate(input$zoomplotBrush)
        selectedPoints <- flaggedData[(flaggedData$UTC > zoombrush$xmin) &
                                        (flaggedData$UTC < zoombrush$xmax) &
                                        (flaggedData[, variablename] > zoombrush$ymin) &
                                        (flaggedData[, variablename] < zoombrush$ymax), 1:2]
        points(x = selectedPoints$UTC, y = selectedPoints[, variablename], pch = 20, cex = 0.4)
        points(x = selectedPoints$UTC, y = selectedPoints[, variablename], col = rgb(0.2, 0.2, 1, 0.5))
      }

      # highlight points marked by selected filter
      if (!is.null(input$filtertable_rows_selected)) {
        filterlist <- getFilterlist()
        if (!is.null(filterlist)) {
          # format date columns to the default date format "UTC"
          filterlist$from <- as.POSIXct(filterlist$from, origin = origin, tz = "UTC")
          filterlist$to <- as.POSIXct(filterlist$to, origin = origin, tz = "UTC")
          # index of combination of selected variable and selected dataset
          f.ind.ds.var <- which((filterlist$dataset == input$dataset) & (filterlist$variable == variablename))
          filterlist.var <- filterlist[f.ind.ds.var, ]
          #filterlist.var <- filterlist[which(filterlist$variable == variablename), ]
          selectedFilters <- filterlist.var[input$filtertable_rows_selected, ]
          for (iiiii in 1:nrow(selectedFilters)) {
            selectedPoints <- flaggedData[(flaggedData$UTC >= selectedFilters[iiiii, ]$from) &
                                            (flaggedData$UTC <= selectedFilters[iiiii, ]$to) &
                                            (flaggedData[, variablename] >= selectedFilters[iiiii, ]$min) &
                                            (flaggedData[, variablename] <= selectedFilters[iiiii, ]$max), ]
            points(x = selectedPoints$UTC, y = selectedPoints[, variablename], pch = 1, col = "red")
          }
        }
      }
    })
  }

  # zoom window
  observeEvent(list(input$overviewplotBrush, input$zoomplotBrush), ignoreInit = TRUE, {
    ## Refilter if overviewplotBrush or zoomplotBrush changes
    ##    Could be optimized by:
    ##      - splitting into observers for each specific brush
    ##      - resetting data only when necessary
    if (is.null(input$overviewplotBrush)) {
      session$resetBrush("zoomplotBrush")
    }
    flaggedData <<- currentData
    #flaggedData$UTC <<- as.POSIXct(flaggedData$UTC, origin = origin, tz = 'UTC')
    applyManualFilter()
    ### redraw overviewplot and zoomplot if overview or zoomplot brush change ###
    redrawOverviewPlot()
    redrawZoomplot()
  })

  ####### filters
  # filter files and paths
  getFilterpath <- function() {
    stationprefix <- switch(input$station,
                            "Bayelva" = "Ba",
                            "Kurungnakh" = "Ku",
                            "Samoylov" = "Sa",
                            "Sardakh" = "Sd",
                            "TVC" = "TVC")
    return(paste0(filterbasepath, stationprefix, "_filter_", input$year, ".dat"))
  }

  getFilterlist <- function() {
    filterpath <- getFilterpath()
    if (file.exists(filterpath))
      return(read.table(filterpath, stringsAsFactors = FALSE, strip.white = TRUE,
                        sep = ",", dec = ".", header = T))
    else return(NULL)
  }

  redrawFilterTable <- function() {
    ## Load filter
    filterlist <- getFilterlist()
    ## Option list for data tables
    optionList <- list(orderClasses = TRUE, searching = TRUE, paging = FALSE, selection = "single")
    # if year has a filterlist, draw table
    if (!is.null(filterlist)) {
      ## if a filter exists for a selected variable and the selected dataset ==> show variable filters
      f.ind.ds.var <- which((filterlist$dataset == input$dataset) & (filterlist$variable == input$variable))
      # if (nrow(filterlist[filterlist$variable == input$variable, ])) {
      #   output$filtertable <- DT::renderDataTable(filterlist[filterlist$variable == input$variable, ],
      #                                             options = optionList, rownames = FALSE)
      if (nrow(filterlist[f.ind.ds.var, ])) {
        output$filtertable <- DT::renderDataTable(filterlist[f.ind.ds.var, ],
                                                  options = optionList, rownames = FALSE)
      } else output$filtertable <- DT::renderDataTable(data.frame(
        "Manual Flag" = "No plausibility flag for selected variable in this year"), options = list(searching = FALSE))
    } else output$filtertable <- DT::renderDataTable(data.frame(
      "Manual Flag" = "No plausibility flag for selected variable in this year"), options = list(searching = FALSE))
  }

  manualFilter <- function(d, min, max) {
    # return which data is within bounds
    min <- ifelse(!is.na(min) && is.numeric(min), min, -Inf)
    max <- ifelse(!is.na(max) && is.numeric(max), max, Inf)
    dataWithinBounds <- (d > min & d < max)
    dataWithinBounds[is.na(dataWithinBounds)] <- FALSE
    return(dataWithinBounds)
  }

  applyManualFilter <- function() {
    ### flags points selected in zoomplot ###
    # get names of selected variable and associated flag
    variablename <- input$variable
    flagname <- paste0(variablename, "_fl")
    zoombrush <- input$zoomplotBrush
    # abort if no zoomplotBrush
    if (is.null(zoombrush)) return()
    # get points selected in zoomplot
    selectedPoints <- flaggedData[flaggedData$UTC > zoombrush$xmin &
                                    flaggedData$UTC < zoombrush$xmax, ]
    dataWithinBounds <- manualFilter(selectedPoints[, variablename],
                                     zoombrush$ymin, zoombrush$ymax)
    # assign flag
    filterflag <- 20
    flaggedData[flaggedData$UTC %in% selectedPoints$UTC, flagname][dataWithinBounds] <<- filterflag
  }

  # get and display filters
  observeEvent(list(input$year, input$variable, input$dataset, input$station), {
    # update years list
    years <- sort(unique(yearlyDatasetPaths$year[yearlyDatasetPaths$station == input$station]), decreasing = TRUE)
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

    redrawFilterTable()
    # redrawCheckTable()
    ## Get data
    path <- yearlyDatasetPaths$path[(yearlyDatasetPaths$dataset == input$dataset) &
                                      (yearlyDatasetPaths$year == input$year)]
    # path will be empty if dataset changed and input$variable did not update
    if (length(path) == 0) return()
    # get names of selected variable and associated flag
    variablename <- input$variable
    flagname <- paste0(variablename, "_fl")
    # read data and save in global variable
    currentData <<- read.csv(path)
    currentData$UTC <<- as.POSIXct(currentData$UTC, origin = origin, tz = 'UTC')
    currentData <<- currentData[, c("UTC", variablename, flagname)]
    # initialize flaggedData
    flaggedData <<- currentData
    ## reset brushes
    session$resetBrush("overviewplotBrush")
    session$resetBrush("zoomplotBrush")
    ## redraw plot
    redrawOverviewPlot()
    redrawZoomplot()
  })

  # save filters
  observeEvent(input$buttonSaveFilter, ignoreInit = TRUE, {
    #### write filter to file ###
    withBusyIndicatorServer("buttonSaveFilter", {
      # check for name
      if (input$flagged_by == "") {
        stop("Please enter your name to save a filter.")
      }
      # check if all filter parameters are valid and save result with error message
      if (is.null(input$zoomplotBrush)) {
        stop("Can't save filter. Please select points in the zoomplot.")
      }
      # gather filter parameters and context information
      filtername <- "minmaxmanual"
      min <- input$zoomplotBrush$ymin
      max <- input$zoomplotBrush$ymax
      validDaterange <- as.POSIXct(c(input$zoomplotBrush$xmin, input$zoomplotBrush$xmax),
                                   origin = origin, tz = 'UTC')
      # round to half hour
      validDaterange <- round.POSIXt(validDaterange, units = "mins")
      validDaterange$min <- c(ceiling(validDaterange[1]$min / 30) * 30,
                              floor(validDaterange[2]$min / 30) * 30)
      # Convert to string
      validDaterange <- as.character(validDaterange)
      flagToBeSet <- 6
      ## read old filter list
      filterlist <- getFilterlist()
      # check if filterlist exists and if not, create one!
      if (is.null(filterlist)) {
        warning("No filterlist exists for that year! Creating one...")
        filterlist <- data.frame(from = character(0), to = character(0), dataset = character(0),
                                 variable = character(0), filtername = character(0),
                                 min = character(0), max = character(0), flag = character(0),
                                 flagging_date = character(0), flagged_by = character(0),
                                 stringsAsFactors = FALSE)
      }
      # construct filter line and new filterlist
      filterline <- c(validDaterange, input$dataset, input$variable,
                      filtername, min, max, flagToBeSet,
                      as.character(Sys.time()), input$flagged_by)
      filterlistNew <- filterlist
      filterlistNew[nrow(filterlistNew) + 1, ] <- filterline
      # validate filter line by comparing names and dimensions
      if (any(names(filterlist) != names(filterlistNew)) |
          ncol(filterlist) != length(filterline)         |
          ncol(filterlist) != ncol(filterlistNew)        |
          nrow(filterlist) != nrow(filterlistNew) - 1) {
        stop("BUG: Output filter structure not valid, aborting.")
      }
      # write new filter list to file
      filterpath <- getFilterpath()
      write.table(filterlistNew, file = filterpath, row.names = FALSE, sep = ",", dec = ".")

      redrawFilterTable()
      # redrawCheckTable()
      #pause()
      ### Reset zoomplotBrush
      session$resetBrush("zoomplotBrush")
    })
  })

  # delete filters
  observeEvent(input$buttonDeleteFilter, ignoreInit = TRUE, {
    #### Deletes selected filter
    withBusyIndicatorServer("buttonDeleteFilter", {
      ### Delete selected Filter
      # read filter list
      filterlist <- getFilterlist()
      # sanity check
      if (length(input$filtertable_rows_selected) < 1 || is.null(filterlist)) {
        stop("No filter selected.")
      }

      # get selected rows: index of combination of selected variable and selected dataset !
      f.ind.ds.var <- which((filterlist$dataset == input$dataset) & (filterlist$variable == input$variable))
      selectedRows <- filterlist[f.ind.ds.var, ][input$filtertable_rows_selected, ]
      # OLD: if only variables are selected ==> the selection can be wrong if the variable appears several times in the dataset
      # selectedRows <- filterlist[filterlist$variable == input$variable, ][input$filtertable_rows_selected, ]

      # get indexes of selected rows
      selectedRowIdx <- numeric()
      for (i in 1:nrow(filterlist)) {
        # slow for large selections, vectorize to optimise
        for (j in 1:nrow(selectedRows)) {
          if (all(filterlist[i, ] == selectedRows[j, ], na.rm = TRUE))
            selectedRowIdx <- c(selectedRowIdx, i)
        }
      }
      if (length(selectedRowIdx) < 1) {
        stop("Can't find index of selected rows, aborting...")
        return()
      }
      # remove filter from file
      filterpath <- getFilterpath()
      write.table(filterlist[-selectedRowIdx, ], file = filterpath, row.names = FALSE,
                  sep = ",", dec = ".")
      redrawFilterTable()
    })
  })

  ########## checks
  # define check files and paths analogue to filter files and paths
  # use the same directory for saving of check files
  getCheckpath <- function() {
    stationprefix <- switch(input$station,
                            "Bayelva" = "Ba",
                            "Kurungnakh" = "Ku",
                            "Samoylov" = "Sa",
                            "Sardakh" = "Sd",
                            "TVC" = "TVC")
    return(paste0(checkbasepath, stationprefix, "_check_", input$year, ".dat"))
  }

  getChecklist <- function() {
    checkpath <- getCheckpath()
    if (file.exists(checkpath))
      return(read.table(checkpath, stringsAsFactors = FALSE, strip.white = TRUE,
                        sep = ",", dec = ".", header = T))
    else return(NULL)
  }

  redrawCheckTable <- function() {
    ## Load check
    checklist <- getChecklist()
    ## Option list for data tables
    optionList <- list(orderClasses = TRUE, searching = TRUE, paging = FALSE, selection = "single")
    # if year has a checklist, draw table
    if (!is.null(checklist)) {
      ## if a check exists for a selected variable and the selected dataset ==> show checks
      c.ind.ds.var <- which((checklist$dataset == input$dataset) & (checklist$variable == input$variable))
      # c.ind.ds.var <- which((checklist$variable == input$variable))
      if (nrow(checklist[c.ind.ds.var, ])) {
        output$checktable <- DT::renderDataTable(checklist[c.ind.ds.var, ], editable = TRUE,
                                                 options = optionList, rownames = FALSE)
      } else output$checktable <- DT::renderDataTable(data.frame(
        Check = "No check available for selected variable"), options = list(searching = FALSE))
    } else output$checktable <- DT::renderDataTable(data.frame(
      Check = "No check available for this year"), options = list(searching = FALSE))
  }

  # get and display checks
  observeEvent(list(input$year, input$variable, input$dataset, input$station), {
    check.list <- getChecklist()
    ## if a check exists for a selected variable and the selected dataset ==> show checks
    c.ind.ds.var <- which((check.list$dataset == input$dataset) & (check.list$variable == input$variable))
    validate(
      need(nrow(check.list[c.ind.ds.var, ]) > 0, 'Please select a cell!')
    )
    outz <<- check.list[c.ind.ds.var, ]
    # validate(
    #   need(nrow(check.list[check.list$variable == input$variable, ]) > 0, 'Please select a cell!')
    # )
    # outz <<- check.list[check.list$variable == input$variable, ]
    #if (nrow(check.list[check.list$variable == input$variable, ])) {

    ####
    # if (nrow(check.list[check.list$variable == input$variable, ])>0) {
    #     outz <<- check.list[check.list$variable == input$variable, ]}
    # validate(
    #     need(outz, 'Please select a cell!')
    # )
    output$check1  <- renderText({
      paste(outz$controller1, outz$check1)
    })
    output$check2 <- renderText({
      paste(outz$controller2, outz$check2)
    })
  })

  # save checks
  observeEvent(input$buttonSaveCheck, ignoreInit = TRUE, {
    #### write check to file ###
    withBusyIndicatorServer("buttonSaveCheck", {
      # check for name
      if (input$flagged_by == "") {
        stop("Please enter your name to confirm the check.")
      }
      # check if all check parameters are valid and save result with error message
      #    if (is.null(input$zoomplotBrush)) {
      #         stop("Can't save check. Please select points in the zoomplot.")
      #     }
      # gather check parameters and context information
      # checkname <- "minmaxmanual"
      # min <- input$zoomplotBrush$ymin
      # max <- input$zoomplotBrush$ymax
      # validDaterange <- as.POSIXlt(c(input$zoomplotBrush$xmin, input$zoomplotBrush$xmax),
      #                              origin = '1970-01-01')
      # # round to half hour
      # validDaterange <- round.POSIXt(validDaterange, units = "mins")
      # validDaterange$min <- c(ceiling(validDaterange[1]$min / 30) * 30,
      #                         floor(validDaterange[2]$min / 30) * 30)
      # # Convert to string
      # validDaterange <- as.character(validDaterange)
      # flagToBeSet <- 6
      # ## read old check list
      checklist <- getChecklist()
      # # check if checklist exists and if not, create one!
      if (is.null(checklist)) {
        warning("No checklist exists for that year! Creating one...")
        checklist <- data.frame(station = character(0), dataset = character(0), variable = character(0), check1 = character(0), controller1 = character(0), check2 = character(0), controller2 = character(0))
      }

      # construct check line and new checklist
      # index of variable and dataset to be checked
      ind.check1 <- which((input$dataset == checklist$dataset) & (input$variable == checklist$variable))
      # ind.check1 <- which(input$variable == checklist$variable)
      # case 1: if there is no first check, add new entries for the first check
      if (is.na(checklist$check1[ind.check1])) {
        checklist$check1[ind.check1] <- as.character(Sys.time())#(Sys.Date())
        checklist$controller1[ind.check1] <- input$flagged_by
        checklistNew <- checklist
      }
      # case 2: if there is already a first check, add new entries for the second check
      else{
        checklist$check2[ind.check1] <- as.character(Sys.time())#(Sys.Date())
        checklist$controller2[ind.check1] <- input$flagged_by
        checklistNew <- checklist
      }

      # validate check line by comparing names and dimensions
      if (any(names(checklist) != names(checklistNew)) |
          #  ncol(checklist) != length(checkline)         |
          ncol(checklist) != ncol(checklistNew)        #|
          # nrow(checklist) != nrow(checklistNew) - 1
      ) {
        stop("BUG: Output check structure not valid, aborting.")
      }
      # write new check list to file
      checkpath <- getCheckpath()
      write.table(checklistNew, file = checkpath, row.names = FALSE, sep = ",", dec = ".")

      # redrawCheckTable()
      # read updated check list
      check.list <- getChecklist()
      # index of variable and dataset to be checked
      c.ind.ds.var.new <- which((check.list$dataset == input$dataset) & (check.list$variable == input$variable))
      # c.ind.ds.var.new <- which(check.list$variable == input$variable)
      validate(
        need(nrow(check.list[c.ind.ds.var.new, ]) > 0, 'Please select a cell!')
      )
      outz <<- check.list[c.ind.ds.var.new, ]
      output$check1  <- renderText({
        paste(outz$controller1, outz$check1)
      })
      output$check2 <- renderText({
        paste(outz$controller2, outz$check2)
      })
      #browser()
    })
  })

  # delete check 1
  observeEvent(input$buttonDeleteCheck1, ignoreInit = TRUE, {
    # read check list
    checklist <- getChecklist()

    # index of variable and dataset to be checked
    ind.check <- which((input$dataset == checklist$dataset) & (input$variable == checklist$variable))
    # ind.check <- which(input$variable == checklist$variable)
    # case 1: delete values of first check
    if (!is.na(checklist$check1[ind.check])) {
      checklist$check1[ind.check] <- NA
      checklist$controller1[ind.check] <- NA
      checklistNew <- checklist
    } else {
      stop("No checks to delete")
      return()
      }

    # remove check from file
    checkpath <- getCheckpath()
    write.table(checklistNew, file = checkpath, row.names = FALSE, sep = ",", dec = ".")
    # redrawCheckTable()
    # read updated check list
    check.list <- getChecklist()
    # index of variable and dataset to be checked
    c.ind.ds.var.new <- which((check.list$dataset == input$dataset) & (check.list$variable == input$variable))
    # c.ind.ds.var.new <- which(check.list$variable == input$variable)
    validate(
      need(nrow(check.list[c.ind.ds.var.new, ]) > 0, 'Please select a cell!')
    )
    outz <<- check.list[c.ind.ds.var.new, ]
    output$check1 <- renderText({
      paste(outz$controller1, outz$check1)
    })

  })

  # delete check 2
  observeEvent(input$buttonDeleteCheck2, ignoreInit = TRUE, {
    # read check list
    checklist <- getChecklist()

    # index of variable and dataset to be checked
    ind.check <- which((input$dataset == checklist$dataset) & (input$variable == checklist$variable))
    # ind.check <- which(input$variable == checklist$variable)
    # case 2: delete values of first check
    if (!is.na(checklist$check2[ind.check])) {
      checklist$check2[ind.check] <- NA
      checklist$controller2[ind.check] <- NA
      checklistNew <- checklist
    } else {
      stop("No checks to delete")
      return()
    }

    # remove check from file
    checkpath <- getCheckpath()
    write.table(checklistNew, file = checkpath, row.names = FALSE, sep = ",", dec = ".")
    # redrawCheckTable()
    # read updated check list
    check.list <- getChecklist()
    # index of variable and dataset to be checked
    c.ind.ds.var.new <- which((check.list$dataset == input$dataset) & (check.list$variable == input$variable))
    # c.ind.ds.var.new <- which(check.list$variable == input$variable)
    validate(
      need(nrow(check.list[c.ind.ds.var.new, ]) > 0, 'Please select a cell!')
    )
    outz <<- check.list[c.ind.ds.var.new, ]

    output$check2 <- renderText({
      paste(outz$controller2, outz$check2)
    })

  })



  ##########
  ## comparison Data
  comparisonModal <- function(failed = FALSE) {
    modalDialog(
      if (failed)
        div(tags$b("Please select a variable", style = "color: red;")),

      selectInput("comparisonStation", "Choose a station:",
                  choices = sort(unique(yearlyDatasetPaths$station)), selected = input$station),

      selectInput("comparisonYear", "Choose a year:",
                  choices = sort(unique(yearlyDatasetPaths$year), decreasing = TRUE), selected = input$year),

      selectInput("comparisonDataset", "Choose a dataset:", choices = input$dataset),

      selectInput("comparisonVariable", "Choose a variable:", choices = NULL),
      checkboxInput("comparisonrange", "Same Axis", TRUE),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("OKcomparisonModal", "Add comparison data")
      ),
      title = "Add comparison data (lv0)",
      easyClose = TRUE,
      size = "s"
    )
  }

  # Show modal "Add comparison data"
  observeEvent(input$showComparisonDataDialog, {
    showModal(comparisonModal())
  })

  # When OK button is pressed, attempt to load the comparison data and redraw plots.
  # If successful, remove the modal. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$OKcomparisonModal, {
    if (is.null(input$comparisonVariable)) {
      ## Check inputs
      showModal(comparisonModal(failed = TRUE))
    } else {
      ## Get data and redraw overviewplot
      path <- yearlyDatasetPaths$path[(yearlyDatasetPaths$dataset == input$comparisonDataset) &
                                      (yearlyDatasetPaths$year == input$comparisonYear)]
      path<-gsub("LTO/level1", "LTO/level0", path)
      path<-gsub("lv1.dat", "lv0.dat", path)
      #browser()
      # path will be empty if dataset changed and input$variable did not update
      if (length(path) == 0) return()
      # get names of selected variable and associated flag
      variablename <- input$comparisonVariable
      flagname <- paste0(variablename, "_fl")
      # read data and save in global variable
      comparisonData <<- read.csv(path)
      # format date column to the default date format "UTC"
      # ATTENTION: This time as POSIXlt ==> technically this is a list format which allows to access the year with "$year"
      comparisonData$UTC <<- as.POSIXlt(comparisonData$UTC, origin = origin, tz = 'UTC')
      ## change year of comparison data to that of currently selected one
      comparisonData$UTC$year <<- rep(as.integer(input$year) - 1900, nrow(comparisonData))
      #comparisonData <<- comparisonData[, c("UTC", variablename, flagname)]
      comparisonData <<- comparisonData[, c("UTC", variablename)]
      comparisonData <<- reshape(comparisonData, direction = "long",
                                 idvar = "UTC",
                                 varying = list(variablename),#list(variablename, flagname),
                                 timevar = "variable",
                                 times = variablename,
                                 v.names = c("value"))
      # remove short streaks of NA (< 12) in order to plot lines properly
      r <- rle(is.na(comparisonData$value)) # run length encoding
      comparisonData <<- comparisonData[!rep(r$values & r$lengths < 12, r$lengths), ]


      redrawOverviewPlot()
      redrawZoomplot()
      removeModal()
    }
  })

  ## Handle modal dialog choices
  observeEvent(list(input$comparisonYear, input$comparisonVariable, input$comparisonDataset,
                    input$comparisonStation), ignoreInit = TRUE, {
                      # update years list
                      years <- sort(unique(yearlyDatasetPaths$year[yearlyDatasetPaths$station == input$comparisonStation]))
                      if (input$comparisonYear %in% years) {
                        # double update to trigger input$comparisonDataset invalidation
                        updateSelectInput(session, "comparisonYear", choices = years,
                                          selected = input$comparisonYear)
                      } else {
                        updateSelectInput(session, "comparisonYear", choices = years)
                      }

                      # update dataset list
                      datasets <- yearlyDatasetPaths$dataset[(yearlyDatasetPaths$year == input$comparisonYear) &
                                                             (yearlyDatasetPaths$station == input$comparisonStation)]
                      if (input$comparisonDataset %in% datasets) {
                        # double update to trigger input$comparisonDataset invalidation
                        updateSelectInput(session, "comparisonDataset", choices = datasets,
                                          selected = input$comparisonDataset)
                      } else {
                        updateSelectInput(session, "comparisonDataset", choices = datasets)
                        return()
                      }

                      # update variable list
                      varlist <- allowedcompVariables$variable[allowedcompVariables$dataset == input$comparisonDataset]
                      ## If variable was selected, update filter list, read data and
                      ## draw overview plot, else abort
                      if (input$comparisonVariable %in% varlist) {
                        # double update to trigger input$comparisonVariable invalidation
                        updateSelectInput(session, "comparisonVariable", choices = varlist,
                                          selected = input$comparisonVariable)
                      } else {
                        updateSelectInput(session, "comparisonVariable", choices = varlist)
                        return()
                      }
                    })

  # Remove comparison data and redraw overviewplot
  observeEvent(input$removeComparisonData, {
    comparisonData <<- data.frame()
    redrawOverviewPlot()
  })

  ##################
  ## update data base
  observeEvent(input$buttonRunScript, ignoreInit = TRUE, {
    ### Run background script
    print("runscriptButton pressed")
    ## Inform user about action, block shiny user interface
    ## source update script
    ## Update data
    datasetname <- with(allowedVariables, dataset[variable %in% input$variable])
    datapath <- with(yearlyDatasetPaths, path[(dataset == datasetname) & (year == input$year)])
    #print(datasetname)
    if (running.system == 1) {
      ## source update script
      p.1 <<- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_win.txt", sep = "\t", header = T)
      source("N:/sparc/LTO/R_database//Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
      aktuell <<- input$year
      station <<- datasetname
      var.name <<- paste0(input$variable, "_fl")
      try(source(paste0(with(yearlyDatasetPaths, db.path[(dataset == datasetname) & (year == input$year)]))))
    } else if (running.system %in% c(2, 3, 4)) {#momentan nur 2
      p.1 <<- read.table("/sparc/LTO/R_database//Time_series_preprocessing/required-scripts-and-files/settings/path_linux.txt", sep = "\t", header = T)
      source("/sparc/LTO/R_database//Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
      aktuell <<- input$year
      station <<- datasetname
      var.name <<- paste0(input$variable, "_fl")
      try(source(paste0(with(yearlyDatasetPaths, db.path[(dataset == datasetname) & (year == input$year)]))))
    }

    # get names of selected variable and associated flag
    variablename <- input$variable
    flagname <- paste0(variablename, "_fl")
    # read data and save in global variable
    currentData <<- read.csv(datapath)
    currentData$UTC <<- as.POSIXct(currentData$UTC, origin = origin, tz = 'UTC')
    currentData <<- currentData[, c("UTC", variablename, flagname)]
    # initialize flaggedData
    flaggedData <<- currentData

    ## redraw overview plot
    redrawOverviewPlot()
    redrawZoomplot()
  })
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
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(

        # h3("SPARC filter tool"),

        selectInput("station", NULL, #Station:",#"Choose a station:",
                    selected = "Bayelva",
                    choices = sort(unique(yearlyDatasetPaths$station))),

        selectInput("year", NULL, #"Year:", #"Choose a year:",
                    selected = 2020,
                    choices = sort(unique(yearlyDatasetPaths$year))),

        selectInput("dataset", NULL, #"Dataset:", #"Choose a dataset:",
                    selected = "BaMet2009",
                    choices = yearlyDatasetPaths$dataset[(yearlyDatasetPaths$year == 2020) &
                                                           (yearlyDatasetPaths$station == "Bayelva")]),

        selectInput("variable", NULL, #"Choose a variable:",
                    NULL),

        actionButton("showComparisonDataDialog", "Add comparison data (lv0)"),

        tags$br(),
        tags$br(),

        actionButton("removeComparisonData", "Remove comparison data"),
        #actionButton("uih", "uih"),
        tags$hr(),

        textInput("flagged_by", NULL, # "Your name:",# for check & filter:",
                  placeholder = "Please enter your Initials!"),

      #  tags$hr(),

       # p(strong("Manual flag (6):"), "Choose data points in zoom plot"),

        withBusyIndicatorUI(
          actionButton("buttonSaveFilter", "Save flag 6",
                       icon = icon("refresh"),
                       style = 'white-space:normal;color:black;background-color:green')
        ),

        withBusyIndicatorUI(
        actionButton("buttonDeleteFilter", "Delete flag 6",
                     icon = icon("eraser"),
                     style = 'white-space:normal;color:black;background-color:orange')
        ),

        tags$hr(),

      #withBusyIndicatorUI(
        actionButton("buttonSaveCheck", "Check",
                     icon = icon("refresh"),
                     style = 'white-space:normal;color:black;background-color:violet'),

      #),

        verbatimTextOutput("check1"),
        verbatimTextOutput("check2"),

        actionButton("buttonDeleteCheck1", "Delete Check 1",
                     icon = icon("eraser"),
                     style = 'white-space:normal;color:black;background-color:orange'),

        actionButton("buttonDeleteCheck2", "Delete Check 2",
                     icon = icon("eraser"),
                     style = 'white-space:normal;color:black;background-color:orange'),

        tags$hr(),

        actionButton("buttonRunScript", "Refresh DB",
                     icon = icon("database"),
                     style = 'color:black;background-color:green'),

      tags$hr(),
      p("System Error (2)", style = 'white-space:normal;background-color:#AC1116'),
      p("Maintenance (3)", style = 'white-space:normal;background-color:#9346A4'),
      p("Physical Limits (4)", style = 'white-space:normal;background-color:#71D7DA'),
      p("Gradient (5)", style = 'white-space:normal;background-color:#4B9158'),
      p("Plausibility (6)", style = 'white-space:normal;background-color:#9A632D'),
      p("Decreased accuracy (7)", style = 'white-space:normal;background-color: orchid'),
      p("Snow covered (8)", style = 'white-space:normal;background-color: aquamarine')
   #

      ),

      mainPanel(plotOutput("overviewplot", height = 300,
                           brush = brushOpts(id = "overviewplotBrush", resetOnNew = FALSE)),

                plotOutput("zoomplot", height = 400,
                           brush = brushOpts(id = "zoomplotBrush", resetOnNew = FALSE)),

                #dataTableOutput("checktable"),
                dataTableOutput("filtertable")
      )
    )
  ))


#### run ####
#options(shiny.reactlog = FALSE)
# runApp(shinyApp(ui = ui, server = server))
# for app.R in shiny-server:
shinyApp(ui = ui, server = server)