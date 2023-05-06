#' @import roxygen2 shiny dplyr ggplot2 reactable tidyverse skimr
#' @importFrom data.table fread
NULL

#' NCPMGUI()
#'
#' @return GUI
#' @export
#' @description All GUI code
#'
#' @examples
#' NCPMGUI()
NCPMGUI <- function () {

  ##### UI part
  #### NOTE = PERCEPTUAL = COGNITIVE and COGNITIVE = PERCEPTUAL
  ui <- fluidPage(

    # CSS
    tags$head(
      tags$style(HTML("
                  .btn {
                    display: inline-block;
                    height: 50px;
                    width: 200px;
                    border: 1px solid green;
                    background-color: cyan;
                  }

                    "))
    ),
    titlePanel("Novice - Cognitive Performance Model (N-CPM)"),
    navlistPanel(id = "tabs",
                 tabPanel("Home",
                          fluidPage(
                            fluidRow(
                              column(12,
                                     fluidRow(
                                       column(6,
                                              tags$img(height=208, width=312, src="https://static9.depositphotos.com/1074452/1184/i/600/depositphotos_11843259-stock-photo-novice-expert-keys-show-amateur.jpg")),
                                       column(6)
                                     )
                              )
                            ),
                            fluidRow(
                              column(4,"Model Overview:
    The purpose of this model is to
    predict novices' task performance and cognitive workload.
    Click on the help tab or upload a file to get started."),
                   column(8,fileInput("dataset2", "Choose CSV File", # This is for testing data. We can switch this to the build a scenario part at the end.
                                      accept = c(
                                        "text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")
                   ),
                   actionButton("uploaded","File uploaded?")))
               )
      ),
      tabPanel("Develop a Scenario",

               fluidPage(
                 fluidRow(
                   column(2,radioButtons('cognitive','Perceptual Operators', choices = c('Look','Read','Search', 'Saccade','Hear','custom'), selected = character(0))),
                   column(2,radioButtons('perceptual','Cognitive Operators', choices = c('Attend','Initiate','Ignore','Recall','Store','Think','Verify','custom'), selected = character(0))),
                   column(2,radioButtons('motor','Motor Operators', choices = c('Drag','Grasp','Hands','Keystroke','Point','Swipe','Tap','Touch','Turn','Type','Write','Reach','Flick','Zoom in','Zoom out','Say', 'custom'), selected = character(0))),                   column(2,radioButtons('chunk', 'Chunks', choices = c('Plate Number','Street name','Road Name','custom'), selected = character(0))),
                    column(2, radioButtons('system','System',choices =c('Wait'), selected = character(0))),
                   column(2,tableOutput("Code")),

                 )
               ),
               textOutput("result"),
               textOutput("Description"),
               textOutput("chunkresult"),
               # This only shows up if a custom operator is chosen
               uiOutput("customop"),
               # DW 10/16/21 for inputting time.
               uiOutput("customtimeop"),
               uiOutput("custombuttonop"),
               # the next part will only show up if custom chunk is chosen
               uiOutput("customtext"),
               uiOutput("custombutton"),

               textInput("desc","Describe the use of the operator"),
               checkboxInput("parallel","Parallel?", FALSE),
               checkboxInput("goal","Add Goal?", FALSE),
               actionButton("add","Add new line to Code"),
               actionButton("same","Add to current line"),
               actionButton("reset","Remove current selections"),
               actionButton("undo","Remove last line of code"),
               actionButton("moveedit","Move to editing"), # moves code to editing phase.
      ),
      tabPanel("Novice and Expert Comparison",
               "The novice performance can be compared to",
               "the expert performance here. Click a tab to",
               "view each comparison.",
               tabsetPanel( # DW 9/8/21 uioutput
                 tabPanel("Task completion time", uiOutput("Table_tct")),
                 tabPanel("Memory chunks", uiOutput("Table_mem")),
                 tabPanel("Number of operators", uiOutput("Table_oper"))
               )

      ),
      tabPanel("Edit a Scenario",
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("newLine"), HTML("<br/>"),
                   uiOutput("insert"), HTML("<br/>"),
                   uiOutput("delete"), HTML("<br/>"),
                   actionButton("done","Finished?"), HTML("<br/>"),
                   uiOutput("load")
                 ),
                 mainPanel(uiOutput("editor"))
               )
      ),
      tabPanel("Results Summary", # this is the "all" button

               fluidPage(
                 fluidRow(
                   h3("Results"),
                   "Task Completion Time (seconds):",
                   textOutput("Time"),
                   "Memory Load:",
                   textOutput("Load"),
                   "Perceptual Operators:",
                   textOutput("Perc"),
                   "Cognitive Operators:",
                   textOutput("Cog"),
                   "Motor Operators:",
                   textOutput("Motor")
                 ),
                 fluidRow(
                   h3("Chunk information"),
                   tableOutput("contents")
                 ),
                 fluidRow(
                   h4(" * Note * "),
                   tabPanel("Column information ", "Chunk Number: Added chunk's number", HTML("<br/>"),
                            "Chunk_Name: Added chunk's name", HTML("<br/>"),
                            "Chunk_Arrival_Time: The time when chunk was added to working memory (milliseconds)", HTML("<br/>"),
                            "Chunk_Elapsed_Time: The time that chunk remained in working memory (milliseconds)", HTML("<br/>"),
                            "Rehearsal: The number of rehearsals of chunk in working memory", HTML("<br/>"),
                            "Probability_of_Recall: The probability to recall specfiic chunk", HTML("<br/>")),
                 )
               )
      ),

      tabPanel("Help", "Click on each tab for more information.",
               tabsetPanel(
                 tabPanel("Scenario Development", "The help page for developing scenarios goes here.", HTML("<br/>"),
                          tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/qHGuHSaEoI8", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA),
                          ),
                 tabPanel("Scenario Edit", "The help page for editing a scenario goes here.", HTML("<br/>"),
                          tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/6mcy2GIQGRo", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                          ),
                 tabPanel("Glossary", h2("Glossary"), # DW new
                          shiny::dataTableOutput("glossary"))
               ))
    )
  )

  ###################################
  # SERVER

  #' Shiny Server
  #'
  #' @param input input
  #' @param output output
  #' @param session session
  #'
  #' @return contents for ui
  #' @export
  #'
  #' @examples
  #' no example
  server = shinyServer(function(input, output,session) {

    oper_set_shiny <- 0

    #####################
    ### RESULTS SUMMARY - Novice

    # DW 9/8/21 hiding tabs and new button
    hideTab(inputId = "tabs", target = "Results Summary")
    hideTab(inputId = "tabs", target = "Novice and Expert Comparison")
    observeEvent(input$uploaded,{
      showTab(inputId = "tabs", target = "Results Summary")
      showTab(inputId = "tabs", target = "Novice and Expert Comparison")
    })

    ###############################################################################################################
    # RESULTS START FROM HERE #####################################################################################
    ###############################################################################################################

    # Task completion time (TCT)
    # Global input for novices
    GlobalInput <- reactive({

      inFile <- input$dataset2

      if(is.null(inFile))
      {
        data <- as.data.frame(v$code, header=FALSE)
      }
      else
      {
        data <- read.csv(inFile$datapath, header = FALSE)
      }

      if(is.null(input$confirmop))
        data <- NCPMcalc(data, oper_set)
      else
        data <- NCPMcalc(data, oper_set_shiny)

      data

    })
    # Global input for experts
    GlobalInput_exp <- reactive({

      inFile <- input$dataset2

      if(is.null(inFile))
      {
        data <- as.data.frame(v$code, header=FALSE)
      }
      else
      {
        data <- read.csv(inFile$datapath, header = FALSE)
      }
      if(is.null(input$confirmop))
        data <- NCPMcalc_exp(data, oper_set)
      else
        data <- NCPMcalc_exp(data, oper_set_shiny)

      data

    })
    output$Time <- renderText({
      GlobalInput()[[2]]
    })
    output$Load <- renderText({
      GlobalInput()[[4]]
    })
    output$Perc <- renderText({
      GlobalInput()[[5]][[1]]
     })
    output$Cog <- renderText({
      GlobalInput()[[5]][[2]]
    })
    output$Motor <- renderText({
      GlobalInput()[[5]][[3]]
    })
    output$contents <- renderTable({
      GlobalInput()[[1]]
    })

    ############################
    # Nov vs. Exp - TCT - DATA TABLE
    output$Table_tct <- renderTable({

      inFile <- input$dataset2

      if(is.null(inFile))
        data<-as.data.frame(v$code, header=FALSE)
      else
        data <- read.csv(inFile$datapath, header = TRUE)

      if(is.null(input$confirmop))
        data_nov <- GlobalInput()
      else
        data_nov <- GlobalInput()

      data_exp <- GlobalInput_exp()
      data_exp[[2]]

      a<-c()
      b<-c()

      tableTCT <- data.frame(a, b)

      tableTCT[1,1] <- data_nov[[2]]
      tableTCT[1,2] <- data_exp[[2]]
      colnames(tableTCT)[1]<-"Novice"
      colnames(tableTCT)[2]<-"Expert"
      rownames(tableTCT) <- c("Task completion time (seconds)")

      tableTCT
    }, rownames = TRUE)

    # Nov vs. Exp - Memory load - DATA TABLE
    output$Table_mem <- renderTable({

      inFile <- input$dataset2

      if(is.null(inFile))
        data<-as.data.frame(v$code, header=FALSE)
      else
        data <- read.csv(inFile$datapath, header = TRUE)

      if(is.null(input$confirmop))
        data_nov <- GlobalInput()
      else
        data_nov <- GlobalInput()
      data_nov[[4]]

      data_exp <- GlobalInput_exp()
      data_exp[[4]]

      a<-c()
      b<-c()

      tableTCT <- data.frame(a, b)

      tableTCT[1,1] <- data_nov[[4]]
      tableTCT[1,2] <- data_exp[[4]]
      colnames(tableTCT)[1]<-"Novice"
      colnames(tableTCT)[2]<-"Expert"
      rownames(tableTCT) <- c("Memory chunks (count)")

      tableTCT
    }, rownames = TRUE)

    # Nov vs. Exp - Number of Operators - DATA TABLE
    output$Table_oper <- renderTable({

      inFile <- input$dataset2

      if(is.null(inFile))
        data<-as.data.frame(v$code, header=FALSE)
      else
        data <- read.csv(inFile$datapath, header = TRUE)

      if(is.null(input$confirmop))
        data_nov <- GlobalInput()
      else
        data_nov <- GlobalInput()
      data_nov[[5]]

      data_exp <- GlobalInput_exp()
      data_exp[[5]]

      a<-c()
      b<-c()

      tableTCT <- data.frame(a, b)

      tableTCT[1,1] <- data_nov[[5]][1]
      tableTCT[1,2] <- data_exp[[5]][1]
      tableTCT[2,1] <- data_nov[[5]][2]
      tableTCT[2,2] <- data_exp[[5]][2]
      tableTCT[3,1] <- data_nov[[5]][3]
      tableTCT[3,2] <- data_exp[[5]][3]

      colnames(tableTCT)[1]<-"Novice"
      colnames(tableTCT)[2]<-"Expert"
      rownames(tableTCT) <- c("Perceptual (count)", "Cognitive (count)", "Motor (count)")
      tableTCT

    }, rownames=TRUE)

    ###############################################################################################################
    # RESULTS END HERE ############################################################################################
    ###############################################################################################################

    ###########################################################################################################################
    # DEVELOP A SCENARIO STARTS FROM HERE #####################################################################################
    ###########################################################################################################################

    ############################
    # DEVELOP A SCENARIO
    # DW 10/16/21 added op
    v <- reactiveValues(current_selection = "", parallel = "", chunk = "", code = "",op = "")

    observeEvent(input$parallel,{
      if(input$parallel)
      {
        v$parallel <- "Also: "
      }
      else
      {
        v$parallel <- ""
      }
    })

    observeEvent(input$goal,{ # controls goal
      if(input$goal)
      {
        v$goal <- "Goal: "
      }
      else
      {
        v$goal <- ""
      }
    })

    observeEvent(input$chunk,{
      v$chunk <- paste("<",input$chunk,">", sep = "")
      output$chunkresult <- renderText({
        paste("Your chunk is ", input$chunk)
      })
      if (input$chunk == 'custom')
      {
        output$customtext <- renderUI({ textInput("customtext", "Input the chunk:")})
        output$custombutton <- renderUI ({
          actionButton("confirm", label = "Confirm Custom Chunk Input")
        })
      }
      else
      {
        output$customtext <- renderUI({NULL})
        output$custombutton <- renderUI({NULL})
      }

    })

    # dealing with operator inputs
    observeEvent(input$cognitive,{
      v$current_selection <- input$cognitive
      output$result <- renderText({
        paste("You chose ", v$current_selection)
      })
      updateRadioButtons(session,
                         'perceptual',
                         selected = character(0))
      updateRadioButtons(session,
                         'motor',
                         selected = character(0))
      updateRadioButtons(session,
                         'system',
                         selected = character(0))

      if (input$cognitive == 'custom')
      {
        v$op <- "custom"
        output$customop <- renderUI({ textInput("customtextop", "Input the operator:")})

        output$customtimeop <- renderUI({ textInput("customtimeop", "Input time to complete operator (ms):")})
        output$custombuttonop <- renderUI ({
          actionButton("confirmop", label = "Confirm Custom Operator")
        })

      }
      else
      {
        output$customop <- renderUI({NULL})
        output$custombuttonop <- renderUI({NULL})
        v$op <- ""

        output$customtimeop <- renderUI({NULL})
      }

      if (input$cognitive == 'Look')
      {
        output$Description <- renderText({paste("Look: Look at an item at a known position")})
      }
      else if(input$cognitive == 'Read')
      {
        output$Description <- renderText({paste("Read: Time to read and count labelled words")})
      }
      else if(input$cognitive == 'Search')
      {
        output$Description <- renderText({paste("Search: Search for an item at an unknown position")})
      }
      else if(input$cognitive == 'Saccade')
      {
        output$Description <- renderText({paste("Saccade: A single rapid eye movement")})
      }
      else if(input$cognitive == 'Hear')
      {
        output$Description <- renderText({paste("Hear: Listen to someone speaking. Label should be the text of the speech")})
      }
      # else if (input$cognitive == 'Say')
      # {
      #   output$Description <- renderText({paste("Say: Speech. Label should be the text of speech")})
      # }
      else
      {
        output$Description <- renderText({paste("")})
      }
    })
    observeEvent(input$perceptual,{
      v$current_selection <- input$perceptual
      output$result <- renderText({
        paste("You chose ", v$current_selection)
      })
      updateRadioButtons(session,
                         'cognitive',
                         selected = character(0))
      updateRadioButtons(session,
                         'motor',
                         selected = character(0))
      updateRadioButtons(session,
                         'system',
                         selected = character(0))
      if (input$perceptual == 'custom')
      {
        v$op <- "perceptual"
        output$customop <- renderUI({ textInput("customtextop", "Input the operator:")})
        output$customtimeop <- renderUI({ textInput("customtimeop", "Input time to complete operator (ms):")})

        output$custombuttonop <- renderUI ({
          actionButton("confirmop", label = "Confirm Custom Operator")
        })
      }
      else
      {
        # hiding boxes
        output$customop <- renderUI({NULL})
        output$custombuttonop <- renderUI({NULL})
        v$op <- ""
        output$customtimeop <- renderUI({NULL})
      }
      if(input$perceptual == 'Attend')
      {
        output$Description <- renderText({paste("Attend: Shifting of attention to stimuli")})
      }
      else if(input$perceptual == 'Initiate')
      {
        output$Description <- renderText({paste("Initiate: Initiate motor process")})
      }
      else if(input$perceptual == 'Ignore')
      {
        output$Description <- renderText({paste("Ignore: Removes item from working memory")})
      }
      else if(input$perceptual == 'Mental')
      {
        output$Description <- renderText({paste("Mental: Generic operator for thinking")})
      }
      else if(input$perceptual == 'Recall')
      {
        output$Description <- renderText({paste("Recall: Retrieve information from long term memory or working memory")})
      }
      else if(input$perceptual == 'Store')
      {
        output$Description <- renderText({paste("Store: Place item in working memory")})
      }
      else if(input$perceptual == 'Think')
      {
        output$Description <- renderText({paste("Think: Generic operator for thinking")})
      }
      else if(input$perceptual == 'Verify')
      {
        output$Description <- renderText({paste("Verify: Generic operator for thinking")})
      }
      else
      {
        output$Description <- renderText({paste("")})
      }
    })
    observeEvent(input$motor,{
      v$current_selection <- input$motor
      output$result <- renderText({
        paste("You chose ", v$current_selection)
      })
      updateRadioButtons(session,
                         'perceptual',
                         selected = character(0))
      updateRadioButtons(session,
                         'cognitive',
                         selected = character(0))
      updateRadioButtons(session,
                         'system',
                         selected = character(0))
      if (input$motor == 'custom')
      {
        v$op <- "motor"
        output$customop <- renderUI({ textInput("customtextop", "Input the operator:")})
        output$customtimeop <- renderUI({ textInput("customtimeop", "Input time to complete operator (ms):")})

        output$custombuttonop <- renderUI ({
          actionButton("confirmop", label = "Confirm Custom Operator")
        })
      }
      else
      {
        output$customop <- renderUI({NULL})
        output$custombuttonop <- renderUI({NULL})
        v$op <- ""
        output$customtimeop <- renderUI({NULL})
      }
      if (input$motor == 'Drag')
      {
        output$Description <- renderText({paste("Drag: Drag an item across a screen, associated with touchscreen devices")})
      }
      else if (input$motor == 'Grasp')
      {
        output$Description <- renderText({paste("Grasp: Act of reaching with the hand and grasping an object")})
      }
      else if (input$motor == 'Hands')
      {
        output$Description <- renderText({paste("Hands: Move hands to position")})
      }
      else if (input$motor == 'Keystroke')
      {
        output$Description <- renderText({paste("Keystroke: Press a single keyboard key")})
      }
      else if (input$motor == 'Point')
      {
        output$Description <- renderText({paste("Point: Move cursor via mouse")})
      }
      else if (input$motor == 'Swipe')
      {
        output$Description <- renderText({paste("Swipe: One swipe gesture")})
      }
      else if (input$motor == 'Tap')
      {
        output$Description <- renderText({paste("Tap: Touch a series of virtual buttons")})
      }
      else if (input$motor == 'Touch')
      {
        output$Description <- renderText({paste("Touch: Press a virtual button")})
      }
      else if (input$motor == 'Turn')
      {
        output$Description <- renderText({paste("Turn: One turn of a knob or dial")})
      }
      else if (input$motor == 'Type')
      {
        output$Description <- renderText({paste("Type: Press a series of keyboard keys")})
      }
      else if (input$motor == 'Write')
      {
        output$Description <- renderText({paste("Write: Time to write a single word")})
      }
      else if (input$motor == 'Wait')
      {
        output$Description <- renderText({paste("Wait: User waiting for system. Modify time by adding x seconds at end of line")})
      }
      else if (input$motor == 'Reach')
      {
        output$Description <- renderText({paste("Reach: Move a hand to a display")})
      }
      else if (input$motor == 'Flick')
      {
        output$Description <- renderText({paste("Flick: Flick a screen")})
      }
      else if (input$motor == 'Zoom in')
      {
        output$Description <- renderText({paste("Zoom in: Zoom in on the screen")})
      }
      else if (input$motor == 'Zoom out')
      {
        output$Description <- renderText({paste("Zoom out: Zoom out from the screen")})
      }
      else if (input$motor == 'Say')
      {
        output$Description <- renderText({paste("Say: Speech. Label should be the text of speech")})
      }
      else
      {
        output$Description <- renderText({paste("")})
      }
    })
    observeEvent(input$system,{
      v$current_selection <- input$system
      output$result <- renderText({
        paste("You chose ", v$current_selection)
      })
      updateRadioButtons(session,
                         'perceptual',
                         selected = character(0))
      updateRadioButtons(session,
                         'motor',
                         selected = character(0))
      updateRadioButtons(session,
                         'cognitive',
                         selected = character(0))
      output$Description <- renderText({paste("Wait: User waiting for system. Modify time by adding x seconds at end of line")})
    })


    observeEvent(input$confirm,{
      v$chunk <- paste("<",input$customtext,">", sep = "")
      output$chunkresult <- renderText({
        paste("You chose ", input$customtext)
      })

    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("code.csv", sep = "")
      },
      content = function(file) {
        write.csv(codelines$df, file, row.names = FALSE)
      }
    )

    # initial reactive value
    codelines <- reactiveValues()
    codelines$df <- data.frame(Code = numeric(0))

    newEntry <- observe({
      if(input$add > 0) {
        updateRadioButtons(session,
                           'perceptual',
                           selected = character(0))
        updateRadioButtons(session,
                           'cognitive',
                           selected = character(0))
        updateRadioButtons(session,
                           'motor',
                           selected = character(0))
        updateRadioButtons(session,
                           'system',
                           selected = character(0))
        updateRadioButtons(session,
                           'chunk',
                           selected = character(0))
        newLine <- isolate(c(input$desc))
        isolate(codelines$df[nrow(codelines$df)+1,] <- c(paste(v$goal,v$parallel,v$current_selection,v$chunk,input$desc))) # this is where you put the code in
        v$current_selection <- c("")
        v$chunk <- c("")
        output$result <- renderText({
          paste("")
        })
        output$chunkresult <- renderText({
          paste("")
        })
        output$Description <- renderText({
          paste("")
        }) # DW - 9/8/21, made custom stuff go away when needed
        output$customtext <- renderUI({NULL})
        output$custombutton <- renderUI({NULL})
        output$customop <- renderUI({NULL})
        output$custombuttonop <- renderUI({NULL})
        output$customtimeop <- renderUI({NULL})
      }
    })

    # Keeping updated the developed scenario
    observeEvent(input$dataset,{
       v$code <- codelines$df
    })

    rv <- reactiveValues(edit = data.frame(Code = numeric(0)))

    # This just blanks the line. Much simpler than removing line outright.
    # There's a couple ways to make this better. For now it's just the blanking.
    observeEvent(input$undo,{
      newLine <- isolate(c(input$desc))
      isolate(codelines$df[nrow(codelines$df),] <- c(""))
    })

    # for adding on to the same line
    observeEvent(input$same,{
      updateRadioButtons(session,
                         'perceptual',
                         selected = character(0))
      updateRadioButtons(session,
                         'cognitive',
                         selected = character(0))
      updateRadioButtons(session,
                         'motor',
                         selected = character(0))
      updateRadioButtons(session,
                         'system',
                         selected = character(0))
      updateRadioButtons(session,
                         'chunk',
                         selected = character(0))
      newLine <- isolate(c(input$desc))
      isolate(codelines$df[nrow(codelines$df),] <- c(paste(codelines$df[nrow(codelines$df),],v$goal,v$parallel,v$current_selection,v$chunk,input$desc)))
      v$current_selection <- c("")
      v$chunk <- c("")

      output$result <- renderText({ # DW 9/11/21 Bug fix
        paste("")
      })
      output$chunkresult <- renderText({
        paste("")
      })
      output$Description <- renderText({
        paste("")
      })
      # DW new, made custom stuff go away when needed
      output$customtext <- renderUI({NULL})
      output$custombutton <- renderUI({NULL})
      output$customop <- renderUI({NULL})
      output$custombuttonop <- renderUI({NULL})
      output$customtimeop <- renderUI({NULL})
    })

    # for removing selections
    observeEvent(input$reset,{
      updateRadioButtons(session,
                         'perceptual',
                         selected = character(0))
      updateRadioButtons(session,
                         'cognitive',
                         selected = character(0))
      updateRadioButtons(session,
                         'motor',
                         selected = character(0))
      updateRadioButtons(session,
                         'system',
                         selected = character(0))
      updateRadioButtons(session,
                         'chunk',
                         selected = character(0))
      v$current_selection <- c("")
      v$chunk <- c("")

      output$result <- renderText({ # DW 9/11/21 Bug fix
        paste("")
      })
      output$chunkresult <- renderText({
        paste("")
      })
      output$Description <- renderText({
        paste("")
      })
    })

    # Shows the progress of developed scenario on the right side of "Develop a Scneario" page
    output$Code <- renderTable({codelines$df})

    ###########################################################################################################################
    # DEVELOP A SCENARIO ENDS HERE ############################################################################################
    ###########################################################################################################################

    ###########################################################################################################################
    # EDIT A SCENARIO STARTS FROM HERE ########################################################################################
    ###########################################################################################################################

    #############################
    # EDIT A SCENARIO
    observeEvent(input$moveedit, { # moves user to edit a scenario
      rv$edit <- codelines$df
      v$code <- codelines$df # DW 9/8/21 moves Junho button effects
      output$editor <- renderUI({
        reactableOutput("editTable")
      })

      data_filtered <- reactive({
        rv$edit
      })
      output$editTable <- renderReactable({
        reactable(data_filtered(),
                  selection = "multiple",
                  onClick = "select")

      })
      updateNavlistPanel(session, "tabs", selected = "Edit a Scenario")
      # DW 9/8/21 Bring tabs back
      showTab(inputId = "tabs", target = "Results Summary")
      showTab(inputId = "tabs", target = "Novice and Expert Comparison")
    })

    # rendering buttons
    output$newLine <- renderUI({
      textInput("newLine","Rewrite the line you want to edit.")
    })

    output$insert <- renderUI({
      actionButton(
        "insert",
        label = "REPLACE LINE(S)"
      )
    })

    output$delete <- renderUI({
      actionButton(
        "delete",
        label = "DELETE LINE(S)"
      )
    })

    # Note that this has to be a separate process from building.
    observeEvent(input$insert,{
      data_filtered <- reactive({
        rv$edit
      })
      df <- data_filtered()
      table_selected <- reactive(getReactableState("editTable", "selected"))
      df[table_selected(),] <- input$newLine
      updateReactable("editTable", data = df)
      # DW 2/9/22 - Intentionally added TWO SPACES before all replaced lines to be used for the calculation
      rv$edit[rv$edit %in% df[table_selected(), "Code"],] <- paste("  ",input$newLine)
      rv$edit <- df
    })

    final <- reactiveValues(code = data.frame(Code = numeric(0)))

    observeEvent(input$delete,{ # Blanks the line to be removed later.
      data_filtered <- reactive({
        rv$edit
      })
      df <- data_filtered()
      table_selected <- reactive(getReactableState("editTable", "selected"))

      df[table_selected(),] <- ""

      updateReactable("editTable", data = df)
      rv$edit[rv$edit %in% df[table_selected(), "Code"],] <- ""
      rv$edit <- df
    })
    observeEvent(input$done,{ # IT WORKS WOW !!!
      final$code <- subset(rv$edit, Code != "")
      rv$edit <- final$code
      v$code <- rv$edit
      output$load <- renderUI({
        downloadButton(
          "Download", "Download Data"
        )
      })
    })

    output$Download <- downloadHandler( # Now in editing side
      filename = function() {
        paste("code.csv", sep = "")
      },
      content = function(file) {
        write.csv(rv$edit, file, row.names = FALSE)
      }
    )

    # downloads the code into a csv if you click the download button
    observeEvent(input$Download,{
      v$code <- function(file) {
        write.csv(rv$edit, file, row.names = FALSE)
      }
    })

    # NEW LOCATION OF JUNHO VARIABLE
    # The code breaks if you don't include this but I don't remember how it works?
    # v$code <- function(file) {
    #   write.csv(rv$edit, file, row.names = FALSE)
    # }

    # These are for custom operators
    oper_set_new <- reactiveValues()
    oper_set_new$c <- data.frame(Code=numeric(0))

    observeEvent(input$confirmop,{
      v$current_selection <- paste(input$customtextop, sep = "")

      test_time <- input$customtimeop
      test_opname <- input$customtextop

      oper_set_new$c <- NewOper(test_opname, test_time, oper_set_new$c)

      oper_set <- rbind(oper_set, oper_set_new$c)

      oper_set_shiny <<- oper_set

      output$result <- renderText({
        paste("You chose ", input$customtextop)
      })

    })

    ###########################################################################################################################
    # EDIT A SCENARIO ENDS HERE ###############################################################################################
    ###########################################################################################################################

    ############################################
    # HELP
    output$glossary = shiny::renderDataTable({
      glossaryGUI
    })
  })
  shinyApp(ui, server)
}

#' NewOper
#'
#' @param op_name operator name
#' @param op_time operator time
#'
#' @return
#' @export
#'
#' @examples
#' NewOper(a, b)
NewOper <- function(op_name, op_time,oper_set_new){

  oper_set_new <- rbind(oper_set_new , c("Custom", op_name, as.numeric(op_time)))

  colnames(oper_set_new)[1]<-"Category_1"
  colnames(oper_set_new)[2]<-"Category_2"
  colnames(oper_set_new)[3]<-"TCT"

  return(oper_set_new)
}

# For Novice
#' NCPMcalc
#'
#' @param scenario scenario file
#'
#' @description Runs N-CPM model
#' @return all outcome
#' @export
#'
#' @examples
#' NCPMcalc(sce, "Novice")
NCPMcalc <- function(scenario, time_library){

  ans = RunMain(scenario, time_library, "Novice")

  return(ans)
}

# For Expert
#' NCPMcalc_expert
#'
#' @param scenario scenario file
#'
#' @description Runs N-CPM model in an expert's performance
#' @return all outcome - expert
#' @export
#'
#' @examples
#' NCPMcalc_exp(sce, "Expert")
NCPMcalc_exp <- function(scenario, time_library){

  ans = RunMain(scenario, time_library, "Expert")

  return(ans)
}
