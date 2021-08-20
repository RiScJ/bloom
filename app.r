
################################################################################
################################  INCLUDE  ##################################### 
################################################################################

library(shiny)
library(DT)
library(tidyverse)


################################################################################
###############################  CONSTANTS  ####################################
################################################################################

LBM_PER_KG <- 2.20462
KG_PER_LBM <- 0.453592

NEW_JOURNAL_TYPE_WELCOME <- "Hello! Take some notes on me!"




################################################################################
###############################  LOAD STORED  ##################################
###############################  DATAFRAMES   ##################################
################################################################################



# Read in the file which contains the list of all the plant varieties we have
# defined
plant_definitions <- readRDS("data/pd_df.rds")


# Read in the files which contain the running of counter of plants in each bed
# This is maintained as a dirt-simple way of ensuring no two plants are ever
# confused with one another
n_plants <- readRDS("data/n_plants.rds")
n_A <- readRDS("data/n_A.rds")




# Read in the ledger containing the data of all plants ever grown in the garden
plant_ledger <- readRDS("data/plant_ledger.rds")


# Read in the files which contain the data of the plants which are currently 
# in each of the beds
plants_A <- readRDS("data/A.rds")


#
harvest_log <- readRDS("data/harvest_log.rds")


################################################################################
###############################   FUNCTION   ###################################
###############################  DEFINITIONS ###################################
################################################################################

# What follows is ridiculous I know ok I will address it later but I just wanna
# get this up and running as soon as possible so for the time being it is 
# honestly just easier for me to rewrite the same functions like nine times ok

# Add a new plant to bed A
addPlant2A <- function(type, variety, track, planted) {
  if (track) {
    track <- 0
  } else {
    track <- NA
  }
  n_A <<- n_A + 1
  n_plants <<- n_plants + 1
  tempdf <- data.frame(paste(toString(n_plants), "A", toString(n_A)), type, 
                             variety)
  colnames(tempdf) <- c("ID", "Plant", "Variety")
  plants_A <<- rbind(plants_A, tempdf)
  
  tempde <- data.frame(track, track, track, planted, as.Date(NA))
  
  colnames(tempde) <- c("Units Produced", "Mass Produced [kg]", 
                        "Mass Produced [lbm:ozm]", "Planting Date", 
                        "Removal Date")
  
  tempdf <- cbind(tempdf, tempde)
  plant_ledger <- rbind(plant_ledger, tempdf)
  saveRDS(plants_A, "data/A.rds")
  saveRDS(n_A, "data/n_A.rds")
  saveRDS(n_plants, "data/n_plants.rds")
  saveRDS(plant_ledger, "data/plant_ledger.rds")
}

resolveIndices <- function(df) {
  strtoi(strsplit(as.character(unlist(df[1])), split = " ")[[1]][1])
}

kg_to_lbm_ozm <- function(kg) {
  lbms <- kg * LBM_PER_KG
  
  lbm <- floor(lbms)
  ozms <- lbms - lbm
  ozm <- floor(ozms * 16)
  
  lbm_ozm <- paste(toString(lbm), ":", toString(ozm))
}


################################################################################
##############################     PLANT     ###################################
##############################  DEFINITIONS  ###################################
################################################################################

# The plant definitions dataframe is an object which stores all the plant types
# and varieties we have defined. It is a sparse matrix, with the column names
# carrying the names of plant types, and the row names carrying names of plant
# varieties. If a plant type can have a certain variety, the element of the
# dataframe which is in that type's column and that variety's row will be a 
# logical TRUE. Otherwise, FALSE. The entire first row is TRUE for all plant
# types, because it represents a generic variety. 

newJournal_type_type <- function(type) {
  dir.create(file.path(paste("journal/type/", type, sep = "")))
  saveRDS(NEW_JOURNAL_TYPE_WELCOME, paste("journal/type/", type, 
                                          "/generic.rds", sep = ""))
}

newJournal_type_variety <- function(type, variety) {
  saveRDS(NEW_JOURNAL_TYPE_WELCOME, paste("journal/type/", type, "/", 
                                          variety, ".rds", sep = ""))
}

# Add a new plant type to the plant definitions
addPD_type <- function(type) {
  tempdf <- data.frame(c(TRUE, logical(nrow(plant_definitions) - 1)))
  colnames(tempdf) <- type
  plant_definitions <<- cbind(plant_definitions, tempdf)
  saveRDS(plant_definitions, "data/pd_df.rds")
  newJournal_type_type(type)
}

# Add a new plant variety
addPD_variety <- function(type, variety) {
  if(is.na(match(variety, rownames(plant_definitions)))) {
    tempdf <- data.frame(t(unlist(logical(ncol(plant_definitions)))))
    colnames(tempdf) <- colnames(plant_definitions)
    rownames(tempdf) <- variety
    tempdf[match(type, colnames(plant_definitions))] <- TRUE
    plant_definitions <<- rbind(plant_definitions, tempdf)
  } else {
    plant_definitions[match(type, colnames(plant_definitions)), 
                      match(variety, rownames(plant_definitions))] <<- TRUE 
  }
  saveRDS(plant_definitions, "data/pd_df.rds")
  newJournal_type_variety(type, variety)
}

# Reshape the data into a format which can be displayed in a datatable
# This will have two columns, type, and variety. 
formPD_dt <- function() {
  tempdf <- as.data.frame(matrix(ncol = 2, 
                                 nrow = sum(unlist(plant_definitions))))
  colnames(tempdf) <- c("Type", "Variety")
  counter <- 1
  for (type in 1:ncol(plant_definitions)) {
    tempdf[counter, 1] <- colnames(plant_definitions)[type]
    tempdf[counter, 2] <- ""
    counter <- counter + 1
    if (length(which(plant_definitions[,type] %in% TRUE)) - 1) {
      for (variety in which(plant_definitions[,type] %in% TRUE)[2:length(
        which(plant_definitions[,type] %in% TRUE)
      )]) {
        tempdf[counter, 1] <- ""
        tempdf[counter, 2] <- rownames(plant_definitions[variety,])
        counter <- counter + 1
      }
    }
  }
  tempdf
}

resolveTypeVariety <- function(r) {
  tempdf <- formPD_dt()
  k <- r
  while (tempdf[k, 1] == "") {
    k <- k - 1
  }
  if (k == r) {
    variety <- "generic"
    type <- tempdf[r, 1]
  } else {
    variety <- tempdf[r, 2]
    type <- tempdf[k, 1]
  }
  c(type, variety)
}

writeTypeVariety <- function(r) {
  typevar <- resolveTypeVariety(r)
  type <- typevar[1]
  variety <- typevar[2]
  written <- paste(type, variety, sep = ", ")
}

retrieveJournal <- function(r) {
  typevar <- resolveTypeVariety(r)
  type <- typevar[1]
  variety <- typevar[2]
  journal <- readRDS(paste("journal/type/", type, "/", variety, ".rds", 
                           sep = ""))
}

stowJournal <- function(txt, r) {
  typevar <- resolveTypeVariety(r)
  type <- typevar[1]
  variety <- typevar[2]
  saveRDS(txt, paste("journal/type/", type, "/", variety, ".rds", sep = ""))
}


################################################################################
#############################  USER INTERFACE  #################################
################################################################################

ui <- navbarPage("My Garden",
  tabPanel("Plants",
    navlistPanel(widths=c(1,11),
      tabPanel("Overview"),
      tabPanel("Beds",
        tabsetPanel(
          tabPanel("A",
            br(),
            fluidRow(
              column(6, align="left", DTOutput("dt_plants_A")),
              column(4, align="left", 
                actionButton("btn_addPlant_A", "Add a new plant to this bed"),
                br(), br(),
                actionButton("btn_removePlant_A", "Remove selected plants")
              )
            )
          ),
          tabPanel("B"),
          tabPanel("C"),
          tabPanel("D"),
          tabPanel("E"),
          tabPanel("F"),
          tabPanel("G")
        )
      ),
      tabPanel("Plants",
        fluidRow(
          column(6, align="left", DTOutput("dt_pl"))
        )
      ),
      tabPanel("Plant Definitions",
        fluidRow(
          column(6, align="left", DTOutput("dt_pd")),
          column(4, align="left",
            actionButton("btn_addPD_type", "Define a new plant type"),
            br(), br(),
            actionButton("btn_addPD_variety", "Define a new plant variety"),
            br(), br(),
            br(), br(),
            br(), br(),
            br(), br(),
            actionButton("btn_notatePD", 
            "Make a note about the selected plant definition")
          )         
        )         
      )
    )
  ),
  tabPanel("Logging",
    mainPanel(
      fluidRow(
        column(2, dateInput("date_logHarvested", label = "Date harvested:")),
        column(2, numericInput("num_logUnits", "Units:", min = 1, step = 1, value = 1)),
        column(2, selectInput("select_logType", label = "Type:", 
                    choices = colnames(plant_definitions))),
        column(2, selectInput("select_logVariety", label = "Variety:",
                              choices = "")),
        column(2, numericInput("num_logMass", label = "Mass [g]:", value = 0)),
        column(2, textInput("text_log_plantID", label = "Associate to a plant?",
                            placeholder = "Plant ID"))
      ),
      fluidRow(
        column(12, br(), actionButton("btn_log_submit", "Submit", 
                                      width = "100%"), br(), br(), br())
      ),
      fluidRow(
        column(12, DTOutput("dt_hl"))
      )
    )         
  ),
  tabPanel("Analysis"),
  tabPanel("Help")
)

################################################################################
#################################  SERVER  #####################################
################################################################################

server <- function(input, output, session) {
  observeEvent(input$btn_addPlant_A, {
    showModal(modalDialog(
      tags$h3("What kind of plant?"),
      selectInput("addPlant_A_select_type", choices = colnames(
        plant_definitions), label = "Plant type:"),
      selectInput("addPlant_A_select_variety", choices = "", 
                  label = "Plant variety:"),
      checkboxInput("addPlant_A_check", 
                    label = "Will you be tracking this plant individually?"),
      dateInput("addPlant_A_date_planted", label = "Date of planting:"),
      footer=tagList(
        actionButton("addPlant_A_submit", 'Submit'),
        modalButton("Cancel")
      )
    ))
  })
  
  varietyData_add_A <- reactive({
    rownames(plant_definitions[which(plant_definitions[,match(
      input$addPlant_A_select_type, colnames(plant_definitions))] %in% TRUE),])
  })
  
  observe({
    updateSelectInput(session, "addPlant_A_select_variety", 
                      choices = varietyData_add_A())
  })
  
  varietyData_log <- reactive({
    rownames(plant_definitions[which(plant_definitions[,match(
      input$select_logType, colnames(plant_definitions))] %in% TRUE),])
  })
  
  observe({
    updateSelectInput(session, "select_logVariety", choices = varietyData_log())
  })
  
  observeEvent(input$addPlant_A_submit, {
    removeModal()
    new_plant_type <- input$addPlant_A_select_type
    new_plant_variety <- input$addPlant_A_select_variety
    new_plant_track <- as.integer(input$addPlant_A_check)
    planted <- input$addPlant_A_date_planted
    addPlant2A(new_plant_type, new_plant_variety, new_plant_track, planted)
    output$dt_plants_A <- DT::renderDT({
      DT::datatable(reactiveValues(data = plants_A)$data)
    })
    output$dt_pl <- DT::renderDT({
      DT::datatable(reactiveValues(data = plant_ledger)$data, 
                    selection = "single", rownames = FALSE)
    })
  })
  
  observeEvent(input$btn_removePlant_A, {
    showModal(modalDialog(
      tags$h3("To maintain accurate recordkeeping, please choose a reason:"),
      dateInput("removePlant_A_dead_date", label = "Date of removal:"),
      footer = tagList(
        actionButton("removePlant_A_dead", 
                     "The plant has died or stopped producing"),
        actionButton("removePlant_A_accident", 
                     "The plant was added accidentally"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$removePlant_A_dead, {
    removeModal()
    plant_ledger[resolveIndices(plants_A[
      input$dt_plants_A_rows_selected]), 8] <- input$removePlant_A_dead_date
    plants_A <<- plants_A[-input$dt_plants_A_rows_selected,]
    saveRDS(plants_A, "data/A.rds")
    output$dt_plants_A <- DT::renderDT({
      DT::datatable(reactiveValues(data = plants_A)$data)
    })
    output$dt_pl <- DT::renderDT({
      DT::datatable(reactiveValues(data = plant_ledger)$data, 
                    selection = "single", rownames = FALSE)
    })
  })
  
  observeEvent(input$removePlant_A_accident, {
    removeModal()
    plant_ledger <<- plant_ledger[-resolveIndices(plants_A[
      input$dt_plants_A_rows_selected,]),]
    plants_A <<- plants_A[-input$dt_plants_A_rows_selected,]
    saveRDS(plants_A, "data/A.rds")
    n_A <<- n_A - length(input$dt_plants_A_rows_selected)
    n_plants <<- n_plants - length(input$dt_plants_A_rows_selected)
    saveRDS(n_A, "data/n_A.rds")
    saveRDS(n_plants, "data/n_plants.rdS")
    saveRDS(plant_ledger, "data/plant_ledger.rds")
    output$dt_plants_A <- DT::renderDT({
      DT::datatable(reactiveValues(data = plants_A)$data)
    })
    output$dt_pl <- DT::renderDT({
      DT::datatable(reactiveValues(data = plant_ledger)$data, 
                    selection = "single", rownames = FALSE)
    })
  })
  
  observeEvent(input$btn_addPD_type, {
    showModal(modalDialog(
      tags$h3("What type of plant do you want to add?"),
      textInput("addPD_type_input", label = NULL, 
                placeholder = "Enter the name of the type here"),
      footer = tagList(
        actionButton("addPD_type_submit", "Submit"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$addPD_type_submit, {
    removeModal()
    addPD_type(input$addPD_type_input)
    output$dt_pd <- DT::renderDT({
      DT::datatable(reactiveValues(data = formPD_dt())$data, selection = "single",
                    rownames = FALSE)
    })
  })
  
  observeEvent(input$btn_addPD_variety, {
    showModal(modalDialog(
      tags$h3("What type of plant do you want to add a variety for?"),
      selectInput("addPD_variety_select_type", choices = colnames(
        plant_definitions), label = NULL
      ),
      br(),
      textInput("addPD_variety_input", label = NULL, 
                placeholder = "Enter the name of the variety here"),
      footer = tagList(
        actionButton("addPD_variety_submit", "Submit"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$addPD_variety_submit, {
    removeModal()
    addPD_variety(input$addPD_variety_select_type, input$addPD_variety_input)
    output$dt_pd <- DT::renderDT({
      DT::datatable(reactiveValues(data = formPD_dt())$data, selection = "single",
                    rownames = FALSE)
    })
  })
  
  observeEvent(input$btn_notatePD, {
    showModal(modalDialog(
      tags$h3(paste("Journal: ", writeTypeVariety
                    (input$dt_pd_row_last_clicked))),
      textAreaInput("notatePD_textarea", label = NULL,
                    value = retrieveJournal(input$dt_pd_row_last_clicked),
                    width = 500,
                    height = 400),
      footer = tagList(
        actionButton("notatePD_submit", "Save"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$notatePD_submit, {
    removeModal()
    stowJournal(input$notatePD_textarea, input$dt_pd_row_last_clicked)
  })
  
  observeEvent(input$btn_log_submit, {
    hd <- input$date_logHarvested
    nu <- input$num_logUnits
    ty <- input$select_logType
    va <- input$select_logVariety
    ma <- input$num_logMass
    id <- input$text_log_plantID
    
    ma <- ma / 1000
    
    mi <- kg_to_lbm_ozm(ma)
        
    df <- data.frame(hd, nu, ty, va, ma, mi, id)
    colnames(df) <- c("Harvest Date", "Units", "Type", "Variety", "Mass [kg]", 
                      "Mass [lbm:ozm]", "Plant ID")
    harvest_log <<- rbind(harvest_log, df)
    saveRDS(harvest_log, "data/harvest_log.rds")
    output$dt_hl <- DT::renderDT({
      DT::datatable(reactiveValues(data = harvest_log)$data,
                    selection = "none", rownames = FALSE)
    })
    output$dt_pl <- DT::renderDT({
      DT::datatable(reactiveValues(data = plant_ledger)$data, 
                    selection = "single", rownames = FALSE)
    })
    if (id != "") {
      r <- strtoi(strsplit(as.character(unlist(df[7])), split=" ")[[1]][1])
      plant_ledger[r, 4] <<- plant_ledger[r, 4] + nu
      plant_ledger[r, 5] <<- plant_ledger[r, 5] + ma
      plant_ledger[r, 6] <<- kg_to_lbm_ozm(plant_ledger[r, 5])
      saveRDS(plant_ledger, "data/plant_ledger.rds")
    }
    
  })
  
  output$dt_plants_A <- DT::renderDT({
    DT::datatable(reactiveValues(data = plants_A)$data)
  })
  
  output$dt_pd <- DT::renderDT({
    DT::datatable(reactiveValues(data = formPD_dt())$data, selection = "single",
                  rownames = FALSE)
  })
  
  output$dt_pl <- DT::renderDT({
    DT::datatable(reactiveValues(data = plant_ledger)$data, 
                  selection = "single", rownames = FALSE)
  })
  
  output$dt_hl <- DT::renderDT({
    DT::datatable(reactiveValues(data = harvest_log)$data,
                  selection = "none", rownames = FALSE)
  })
  
}

################################################################################

shinyApp(ui = ui, server = server)

################################################################################

# by RiScJ
# Started 2021-07-29
# Last updated 2021-07-30
# GPLv3


































