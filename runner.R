
library(shiny)
library(magick)
library(zoo)
library(caTools)
library(shinybusy)
library(shinyWidgets)

# todo ------
#
# - select dayshift / default / yearly files
# - show result wikiplot or pathes
# - implement overviewplots (windows to linux)
# - mit.peak.detection <- 1 (default and optional)
# - allowed variables and yearlydatapath

running.system <- 2

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
  i.nput <- read.csv("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_auto.csv")
  
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
  i.nput <- read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_auto.csv")
  
}
#i.nput <- read.csv("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings_shiny/yearlyDataPath_auto.csv")
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
        selected = c("Bayelva")
      ),
      pickerInput(
        inputId = "d.ataset",
        label = "Select dataset", 
        choices = unique(i.nput$dataset[i.nput$station=="Bayelva"]),
        selected = c("BaSnow2013")
        
      ),
      pickerInput(
        inputId = "y.ears",
        label = "Select/deselect years", 
        choices = unique(i.nput$year[i.nput$dataset=="SaMet2002"]),
        selected = c(2020),
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
      p("Approximitly processing time"), 
      textOutput("t.ime"),
      br(),
      actionBttn(
        inputId = "b.utton",
        label = "Start processing",
        style = "float", 
        color = "danger"
      ),
      h4("Comming features"),
      p("- add allowedvariables script",br(),
        "- add yearlydatapath script",br(),
        "- links to outputs",br(),
        "- links to input tables",br(),
        "- statistics for level 1 data"),
      actionBttn(
        inputId = "u.pdatepath",
        label = "create_yearly_dataset_p.R",
        # style = "float", 
        color = "danger"
      ),
      actionBttn(
        inputId = "u.pdatevars",
        label = "create_allowed_v.R",
        # style = "float", 
        color = "danger"
      )
    ),
    
    
    mainPanel(### main panel -----
              fluidRow(
                
                column(3,
                       helpText("Not implemented: Kurungnagk, Samoylov(only a few) and all old stations!!"),
                       h3("Zero curtain zone"),
                       dataTableOutput('zero.curt')
                ),
                column(9,   
                       helpText("see DSN corrections below"),   
                       h3("Physical limits"),
                       dataTableOutput('p.hyslimits'),
                       h3("Dsn corrections"),
                       dataTableOutput('dsn.corr')
                )
                
              ))
  )
)










server <- function(input, output, session) {
  # A notification ID (warnings)
  n.ote1 <- NULL
  
  
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
      read.csv(paste0(p.1$w[p.1$n == "settings.p"],"phys.limits/phys.limit_",s.tation.names[input$s.tation],".csv")),
      #extensions = 'Scroller', 
      options = list(
        # deferRender = TRUE,
        # scrollY = 200,
        # #scroller = TRUE,
        # scrollCollapse = TRUE,
        pageLength = 10,
        dom = 'frtiS')# tp
    ) 
    if(s.tation.names[input$s.tation]%in%c("Ba","Sa","TVC")){
      output$zero.curt <- renderDataTable(
        read.csv(paste0(p.1$w[p.1$n == "settings.p"],"zero.curtain/zero.curtain.doy_",s.tation.names[input$s.tation],".dat"),
                 stringsAsFactors=F, na.strings="NA"),
        options = list(order = list(list(0, 'desc')),
                       pageLength = 12,
                       dom = 'tp')
      )  
    }else{
      output$zero.curt <-NULL
    }
    
    
    
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
    
    if(input$d.ataset%in%c("SaSnow2012","SaSoil2002")){
      if (!is.null(n.ote1))
        return()
      n.ote1 <<- showNotification(
        paste("Please update SaMet2002 first!",input$d.ataset,
              "station uses Tair values of SaMet2002."),
        duration = 0, type="error")
    }else{
      removeNotification(n.ote1)
      n.ote1 <<- NULL
    }
    
    if(input$d.ataset%in%c("SaSnow2012","BaSnow2019")){
      output$dsn.corr <- renderDataTable(
        read.csv(paste0(p.1$w[p.1$n == "settings.p"],"DSNcorr.files/",input$d.ataset,"_DSN_correction.dat")))
    }else{
      output$dsn.corr <- NULL
    }
  })
  observeEvent(c(input$y.ears,input$Id001), {
    req(input$y.ears)
    # time calculation ----
    if("LV0"%in%input$Id001){l.0<-4}else{l.0<-0}
    if("LV1"%in%input$Id001){l.1<-12}else{l.1<-0}
    if("Wikiplots"%in%input$Id001){l.w<-1}else{l.w<-0}
    if("Overviewplots"%in%input$Id001){l.ov<-350}else{l.ov<-0}
    if("O2A"%in%input$Id001){l.o2a<-2}else{l.o2a<-0}
    n<-length(input$y.ears)
    #browser()
    if(n>=1){    
      v<-ncol(read.table(paste0(p.1$w[p.1$n == "LV0.p"],input$d.ataset,"/00_full_dataset/",input$d.ataset,"_",rev(input$y.ears)[1],"_lv0.dat"),sep = ",", dec = ".", header = T))
    }else{v<-0}
    
    apr.time <- n*v*l.0 + n*v*l.1 + n*v*l.w + n*v*l.ov + n*v*l.o2a
    if(apr.time>=3600){
      output$t.ime<-renderText({paste(apr.time %/% 3600," h",(apr.time %% 3600) %/% 60," min",(apr.time %% 3600) %% 60," sec")})
    }else if(apr.time>=60){
      output$t.ime<-renderText({paste(apr.time %/% 60," min",apr.time %% 60," sec")})
    }else{
      output$t.ime<-renderText({paste(apr.time," sec")})
    }
    
  })
  observeEvent(input$u.pdatepath, {# process button
    show_modal_progress_line(color = "#06afce",text = "Update available data pathes ",height = "40px")
    try(source(paste0(p.1$w[p.1$n == "script.p"], "required-scripts-and-files/additionals_shiny/create_yearly_dataset_p.R")))
    remove_modal_progress()
  })
  observeEvent(input$u.pdatevars, {# process button
    show_modal_progress_line(color = "#06afce",text = "Update available variables ",height = "40px")
    try(source(paste0(p.1$w[p.1$n == "script.p"], "required-scripts-and-files/additionals_shiny/create_allowed_v.R")))
    remove_modal_progress()
  })
  
  
  
  observeEvent(input$b.utton, {# process button
    show_modal_progress_line(color = "#06afce",text = "Starting computation",height = "40px")
    start_time <- Sys.time()  
    m<-0
    for(i in input$Id001){# checkbox options
      for(j in rev(input$y.ears))# use earlier years first
        
        if(i=="LV0"){
          ### LV0 ---------------------- 
          runyear <<- j;origin <<- "1970-01-01"
          run.year <<-  as.numeric(j)
          if(input$d.ataset %in% c("BaSnow2019sr","BaSnow2019cs")){# case BaSnow2019 ... 1 script for 2 stations ----
            try(source(paste0(p.1$w[p.1$n == "script.p"], "r-level-0-scripts/Bayelva/RAW_to_LV0_BaSnow2019.R")))
          }else if(input$d.ataset %in% c("BaSoil2009") ){# case BaSoil2009 ... 2 scripts for 1 station ----
            try(source(paste0(p.1$w[p.1$n == "script.p"], "r-level-0-scripts/Bayelva/RAW_to_LV0_BaSoil2009_temp.R")))
            Sys.sleep(1)
            try(source(paste0(p.1$w[p.1$n == "script.p"], "r-level-0-scripts/Bayelva/RAW_to_LV0_BaSoil2009_tdr.R")))
          }else if(input$d.ataset %in% c("BaSoil1998","BaMet1998","BaEddy2007","SaSoil1998","SaMet1998",
                                         "KuLucky22014","KuLucky12013","KuLucky2013") ){# case old stations ----
            cat("not implemented yet")
          }else if(input$d.ataset %in% c("KuLucky22014","KuLucky2013") ){# case KuLucky ... 1 script for 2 stations ----
            try(source(paste0(p.1$w[p.1$n == "script.p"], "r-level-0-scripts/Kurungnakh/RAW_to_LV0_KuLucky22014_KuLucky2013.R")))
          }else if(input$d.ataset %in% c("TVCSoil2016") ){# case TVCSoil2016 ... tricky name TrailValleyCreek ----
            try(source(paste0(p.1$w[p.1$n == "script.p"], "r-level-0-scripts/TrailValleyCreek/RAW_to_LV0_",input$d.ataset,".R")))
          }else{# standard case ----
            try(source(paste0(p.1$w[p.1$n == "script.p"], "r-level-0-scripts/",input$s.tation,"/RAW_to_LV0_",input$d.ataset,".R")))
          }
          m <- m + 1 # index for progressbar
          update_modal_progress(value=(((0.1*m)/(length(input$y.ears)*length(input$Id001))*10)),
                                text = paste("Processing level0 for year",j,"done"))
          
          ##................................................................................................................................          
          ##................................................................................................................................          
          ##................................................................................................................................          
          ##................................................................................................................................          
          
          
          
        }else if(i=="O2A"){
          ### O2A ----------------------
          if(input$d.ataset %in% c("BaMet2009","BaHole2009","BaSoil2009","BaSnow2013") ){
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
          ##................................................................................................................................          
          ##................................................................................................................................          
          ##................................................................................................................................          
          ##................................................................................................................................          
          
          
          
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
          ##................................................................................................................................          
          ##................................................................................................................................          
          ##................................................................................................................................          
          ##................................................................................................................................          
          
        }else if(i=="Wikiplots"){
          ### wikiplots ----------------------
          if(input$d.ataset %in% c("BaHole2009","BaHole2015","BaHole2021","BaMet2009","BaSnow2013","BaSnow2019cs","BaSoil2017",
                                   "SaHole2006","SaHole2010","SaHole2018","SaMet2002","Sapond2006","SaPond2014",
                                   "SaSnow2012","SaSnow2016","SaSoil2002","SaSoil2012",
                                   "SdHole2009","SdHole20091","SdHole20092",
                                   "KuQ12013",
                                   "TVCSoil2016") ){# standard case ----
            run.year <<-  as.numeric(j)  #
            try(source(paste0(p.1$w[p.1$n == "script.p"], "required-scripts-and-files/additionals/r-wikiplot-scripts/"
                              ,"/LV1_plots_",input$d.ataset,".R")))
            m <- m + 1
            update_modal_progress(value=(((0.1*m)/(length(input$y.ears)*length(input$Id001))*10)),
                                  text = paste("Wikiplots for year",j,"generated"))
            Sys.sleep(1)
          }else if(input$d.ataset %in% c("BaSoil2009")){# special case for Basoil; 2 scripts for dataset plots ----
            run.year <<-  as.numeric(j)  #
            try(source(paste0(p.1$w[p.1$n == "script.p"], "required-scripts-and-files/additionals/r-wikiplot-scripts/"
                              ,"/LV1_plots_",input$d.ataset,"_temp.R")))
            try(source(paste0(p.1$w[p.1$n == "script.p"], "required-scripts-and-files/additionals/r-wikiplot-scripts/"
                              ,"/LV1_plots_",input$d.ataset,"_tdr.R")))
            m <- m + 1
            update_modal_progress(value=(((0.1*m)/(length(input$y.ears)*length(input$Id001))*10)),
                                  text = paste("Wikiplots for year",j,"generated"))
            Sys.sleep(1)
          }else if(input$d.ataset %in% c("BaSnow2019sr")){# special case for BaSnow2019sr ... run BaSnow2019cs instead ----
            run.year <<-  as.numeric(j)  #
            try(source(paste0(p.1$w[p.1$n == "script.p"], "required-scripts-and-files/additionals/r-wikiplot-scripts/"
                              ,"/LV1_plots_BaSnow2019cs.R")))            
            m <- m + 1
            update_modal_progress(value=(((0.1*m)/(length(input$y.ears)*length(input$Id001))*10)),
                                  text = paste("Wikiplots for year",j,"generated"))
            Sys.sleep(1)
          }else{# not implemented yet ----
            m <- m + 1
            update_modal_progress(value=(((0.1*m)/(length(input$y.ears)*length(input$Id001))*10)),
                                  text = paste("Wikiplots for",input$d.ataset,"not yet implemented"))
            Sys.sleep(1)
          }
          ##................................................................................................................................          
          ##................................................................................................................................          
          ##................................................................................................................................          
          ##................................................................................................................................          
          
          
        }else if(i=="Overviewplots"){
          ### overviewplots ----------------------  
          
          if(input$d.ataset %in% c("BaHole2009","BaHole2015","BaHole2021","BaMet2009","BaSnow2013","BaSnow2019cs","BaSoil2017","BaSoil2009") ){
            station <<- input$d.ataset 
            run.year <<-  as.numeric(j)  #
            try(source(paste0(p.1$w[p.1$n == "script.p"], "required-scripts-and-files/additionals/OverviewPlots_test.R")))
            m <- m + 1
            update_modal_progress(value=(((0.1*m)/(length(input$y.ears)*length(input$Id001))*10)),
                                  text = paste("Processing O2A files for year",j,"done"))
            Sys.sleep(1)
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
    
    Sys.sleep(3)# to see the last comment
    remove_modal_progress()
  })
  
}
##................................................................................................................................          
##................................................................................................................................          
##................................................................................................................................          
##................................................................................................................................          
##................................................................................................................................          

#### run ####
options(shiny.reactlog = FALSE)
# runApp(shinyApp(ui = ui, server = server))
# for app.R in shiny-server:
shinyApp(ui = ui, server = server)
