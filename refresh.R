library(shiny)
library(shinybusy)
library(shinyWidgets)

# todo ------
#
# - select dayshift
# - show result wikiplot
# - show all input tables (dsn correction, ...)
# - implement overviewplots(windows to linux)

running.system <- 1

if (running.system == 1) {
  # read paths and allowed variables for windows
  p.1 <<- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_win.txt", sep = "\t", header = T)
  p.1maint <<- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)
  
  yearlyDatasetPaths <- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_auto.csv",
                                 stringsAsFactors = FALSE, strip.white = TRUE)
  allowedVariables   <- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedVariables.csv",
                                 stringsAsFactors = FALSE, strip.white = TRUE)
  filterbasepath     <- "N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/filter.files/"
  checkbasepath      <- "N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/check.files/"
  # read file for modification of style of shiny-app
  source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/additionals_shiny/appCSS.R")
  source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
  
} else if (running.system == 2) {
  # read paths and allowed variables for linux
  p.1 <<- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
  p.1maint <<- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)
  
  yearlyDatasetPaths <- try(read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_AWI.csv", stringsAsFactors = FALSE,
                                     strip.white = TRUE))
  allowedVariables <- try(read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/allowedVariables.csv", stringsAsFactors = FALSE,
                                   strip.white = TRUE))
  filterbasepath     <- "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/filter.files/"
  checkbasepath      <- "/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/check.files/"
  # read file for modification of style of shiny-app
  source("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/additionals_shiny/appCSS.R")
  source("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
  
}
i.nput <- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_auto.csv")
i.nput$station<-as.character(i.nput$station)
i.nput$dataset<-as.character(i.nput$dataset)
recent.year <<- as.numeric(format(Sys.Date(), "%Y"))
s.tation.names<-c("Bayelva"="Ba","Samoylov"="Sa","Sardakh"="Sd","TVC"="TVC","Kurungnakh"="Ku")
ui <- fluidPage(
  tags$style(appCSS),
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      ### sidebarpanel -----
      helpText("1. Copy new raw files to /sparc/data/LTO/Raw/..."),
      pickerInput(
        inputId = "s.tation",
        label = "Select Research Station", 
        choices = unique(i.nput$station),
        selected = c("Samoylov")
      ),
      pickerInput(
        inputId = "d.ataset",
        label = "Select dataset", 
        choices = unique(i.nput$dataset[i.nput$station=="Samoylov"])
        
      ),
      pickerInput(
        inputId = "y.ears",
        label = "Select/deselect years", 
        choices = unique(i.nput$year[i.nput$dataset=="SaMet2002"]),
        selected = c(2006),
        options = list(
          `actions-box` = TRUE), 
        multiple = TRUE
      ),
      helpText("2. Select processing steps:"),
      awesomeCheckboxGroup(
        inputId = "Id001",
        label = "Select processing steps", 
        choices = c("LV0", "O2A", "LV1", "Wikiplots","Overviewplots"),
        selected = c("LV0", "LV1", "Wikiplots"),
        inline = TRUE,
        status = "danger"
      ),
      helpText("Overviewplots not implemented yet"),
      actionBttn(
        inputId = "b.utton",
        label = "Go",
        style = "float", 
        color = "danger"
      ),
      verbatimTextOutput("text", placeholder = TRUE)
    ),
    
    
    mainPanel(
      ### main panel -----
     # verbatimTextOutput("text", placeholder = TRUE),
      #verbatimTextOutput("text", placeholder = TRUE)
      # add_busy_spinner(spin = "fading-circle"),
      # add_busy_bar(color = "red", height = "8px"),
      dataTableOutput('p.hyslimits')
    )
  )
)










server <- function(input, output, session) {
  output$text <- renderText({
    paste("Press Go to start!", sep="\n")
  })
  observeEvent(input$s.tation, {
    updatePickerInput(
      session = session,
      inputId = "d.ataset",
      label = "Select dataset",
      choices = unique(i.nput$dataset[i.nput$station==input$s.tation]),
      selected = unique(i.nput$dataset[i.nput$station==input$s.tation])[2],
      choicesOpt = NULL,
      options = NULL,
      clearOptions = FALSE
    )
    output$p.hyslimits <- renderDataTable(
      read.csv(paste0(p.1$w[p.1$n == "settings.p"],"phys.limits/phys.limit_",s.tation.names[input$s.tation],".csv"))
    )
    
    
  })
  observeEvent(input$d.ataset, {
    updatePickerInput(
      session = session,
      inputId = "y.ears",
      label = "Select dataset",
      choices = rev(unique(i.nput$year[i.nput$dataset==input$d.ataset])),
      selected = rev(unique(i.nput$year[i.nput$dataset==input$d.ataset]))[1],
      choicesOpt = NULL,
      options = NULL,
      clearOptions = FALSE
    )
    # if(input$d.ataset=="SaSoil2002"){
    #   
    # }
    
  })

    
    
  observeEvent(input$b.utton, {# process button
    show_modal_progress_line(color = "#DF0101",text = "Starting computation")
    start_time <- Sys.time()  
    m<-0
    for(i in input$Id001){# checkbox options
      for(j in rev(input$y.ears))# use earlier years first
        
        if(i=="LV0"){
          ### LV0 ---------------------- 
          runyear <<- j;origin <<- "1970-01-01"
          run.year <<-  as.numeric(j)
          if(input$d.ataset %in% c("BaSnow2019sr","BaSnow2019cs","BaSoil2009") ){
            cat("BaSnow2019 files not yet implemented\n")
          }else{
            try(source(paste0(p.1$w[p.1$n == "script.p"], "r-level-0-scripts/",input$s.tation,"/RAW_to_LV0_",input$d.ataset,".R")))
          }
          m <- m + 1 # index for progressbar
          update_modal_progress(value=(((0.1*m)/(length(input$y.ears)*length(input$Id001))*10)),
                                text = paste("Processing level0 for year",j,"done"))

          
          
          
        }else if(i=="O2A"){
          ### O2A ----------------------
          if(input$d.ataset %in% c("BaMet2009","BaHole2009","BaSoil2009") ){
            station <<- input$d.ataset 
            day.shift <<- 40 ## optional selectabel
            run.year <<-  as.numeric(j)  #
            try(source(paste0(p.1$w[p.1$n == "script.p"], "r-level-0-scripts/O2A/LV0_to_O2A_dataflow.R")))
            m <- m + 1
            update_modal_progress(value=(((0.1*m)/(length(input$y.ears)*length(input$Id001))*10)),
                                  text = paste("Processing O2A files for year",j,"done"))
            Sys.sleep(1)
          }else{
            m <- m + 1
            update_modal_progress(value=(((0.1*m)/(length(input$y.ears)*length(input$Id001))*10)),
                                  text = paste("O2A files for",input$d.ataset,"not yet implemented"))
            Sys.sleep(1)
          }
          
          

        }else if(i=="LV1"){
          ### LV1 ---------------------- 
          station <<- input$d.ataset 
          run.year <<- as.numeric(j) #
          #browser()
          try(source(paste0(p.1$w[p.1$n == "script.p"], "r-level-1-scripts/LV0_to_LV1_",s.tation.names[input$s.tation],"All.R")))
          m <- m + 1
          update_modal_progress(value=(((0.1*m)/(length(input$y.ears)*length(input$Id001))*10)),
                                text = paste("Processing level1 for year",j,"done"))
          Sys.sleep(1)

        }else if(i=="Wikiplots"){
          ### wikiplots ----------------------
          if(input$d.ataset %in% c("BaHole2009","BaHole2015","BaHole2021","BaMet2009","BaSnow2013","BaSnow2019cs","BaSoil2017",
                                   "SaHole2006","SaHole2010","SaHole2018","SaMet2002","Sapond2006","SaPond2014",
                                   "SaSnow2012","SaSnow2016","SaSoil2002","SaSoil2012",
                                   "SdHole2009","SdHole20091","SdHole20092","TVCSoil2016") ){
            run.year <<-  as.numeric(j)  #
            try(source(paste0(p.1$w[p.1$n == "script.p"], "required-scripts-and-files/additionals/r-wikiplot-scripts/"
                              ,"/LV1_plots_",input$d.ataset,".R")))
            m <- m + 1
            update_modal_progress(value=(((0.1*m)/(length(input$y.ears)*length(input$Id001))*10)),
                                  text = paste("Wikiplots for year",j,"generated"))
            Sys.sleep(1)
          }else if(input$d.ataset %in% c("BaSoil2009")){# special case for Basoil; 2 scripts for dataset plots
            run.year <<-  as.numeric(j)  #
            try(source(paste0(p.1$w[p.1$n == "script.p"], "required-scripts-and-files/additionals/r-wikiplot-scripts/"
                              ,"/LV1_plots_",input$d.ataset,"_temp.R")))
            try(source(paste0(p.1$w[p.1$n == "script.p"], "required-scripts-and-files/additionals/r-wikiplot-scripts/"
                              ,"/LV1_plots_",input$d.ataset,"_tdr.R")))
            m <- m + 1
            update_modal_progress(value=(((0.1*m)/(length(input$y.ears)*length(input$Id001))*10)),
                                  text = paste("Wikiplots for year",j,"generated"))
            Sys.sleep(1)
          }else{
            m <- m + 1
            update_modal_progress(value=(((0.1*m)/(length(input$y.ears)*length(input$Id001))*10)),
                                  text = paste("Wikiplots for",input$d.ataset,"not yet implemented"))
            Sys.sleep(1)
          }
          
          

        }else if(i=="Overviewplots"){
          ### overviewplots ----------------------  
          runyear <<-  as.numeric(j) 
          if(input$d.ataset %in% c("Nothing") ){
            
            m <- m + 1
            update_modal_progress(value=(((0.1*m)/(length(input$y.ears)*length(input$Id001))*10)),
                                text = paste("Overviewplots for year",j,"generated"))
          }else{
            m <- m + 1
            update_modal_progress(value=(((0.1*m)/(length(input$y.ears)*length(input$Id001))*10)),
                                  text = paste("Overviewplots for",input$d.ataset,"not yet implemented"))
            
          }
          
          
          
         }
    }
    end_time <- Sys.time()     
    cat(paste("finished within ",round(end_time-start_time,0)," sec\n"))
    update_modal_progress(value=(((0.1*m)/(length(input$y.ears)*length(input$Id001))*10)),
                          text = paste("Finished within ",round(end_time-start_time,0)," sec\n"))
    
    Sys.sleep(3)
    remove_modal_progress()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
