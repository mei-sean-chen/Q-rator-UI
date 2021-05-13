library(shiny)
library(Qrator)
library(tibble)
options(shiny.maxRequestSize = 60*1024^2)

helperNames <- c("Errors", "Parent-Homozygous Markers",
                 "Marker Summary (Parents)",
                 "Marker Summary (Input Set)",
                 "Positive Marker Set",
                 "Negative Marker Set")

outputNames <- c('.dat', '.map','Marker Count Per Group')



ui <- fluidPage(

  navbarPage(title = "Q-rator UI",

             tabPanel("About",
                      fluidRow(
                        column(width = 6, offset = 3, h2(("Input Q-rator"))),
                        column(width = 6, offset = 3, p(code("FlexQTL Input Q-rator"), " is an ", code("R"), "language script & graphical user interface that automates assembly
                        of files for FlexQTL and other genetic datasets, written for ", code("R"),  " version 4.0.2+. It requires several input files and creates several output files,
                        which we will enumerate below."))
                        ),
                      br(),
                      fluidRow(
                        column(width = 6, offset = 3, h4(("Required Files"))),
                        column(width = 6, offset = 3, p(strong("Map File:"), "Group and loci for every marker in your data set, must be
                                                        converted to a comma-separated value format. See the following image for
                                                        the top 15 cells of an example map-as-CSV.")),
                        br()
                      ),
                      HTML('<center><img src="intmap.png"></center>'),
                      br(),
                      fluidRow(
                        column(width = 6, offset = 3, p(strong("Phenotype Data:"), "Data collected on a set of individuals. Consider which
                                                        columns of data you wish to include in the analysis. Data format must be comma-separated
                                                        value.")),
                        br()
                      ),
                      fluidRow(
                        column(width = 6, offset = 3, p(strong("Master Data Set:"), "Marker data on every individual in your program. The set
                                                        of all individuals in this file must be a superset of the set of all individuals in
                                                        your phenotype data. (Ask Ashley if programs will usually keep one of these lying around)")),
                        br()
                      ),
                      fluidRow(
                        column(width = 6, offset = 3, h4(("Features & Navigation"))),
                        column(width = 6, offset = 3, p("Navigate ", code("Q-rator"), " by the upper navbar. The tabs are organized in sequential
                                                        order. Though you may visit any tab at any time, tab 'Config.' will not
                                                        be useful until you have uploaded the correct files at the 'File Input' tab.")),
                        br(),
                        column(width = 6, offset = 3, p("Upload files to File Input tab. Pick assembly settings at Config. tab. Also
                                                        provided there are several tables which summarize changes in the data set as you apply settings.
                                                        Once you have chosen your preferred settings, load them with the 'Load Config' button on the sidebar.
                                                        "))
                      )

                      ),

             tabPanel("File Input",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput(inputId = "master","Master data set"), # fileinput() function is used to get the file upload contorl option
                           helpText("Set of all individuals with marker data, as CSV"),
                           tags$hr(),
                           fileInput(inputId = "map","Map"), # fileinput() function is used to get the file upload contorl option
                           helpText("Marker group and location map, as CSV"),
                           tags$hr(),
                           fileInput(inputId = "phen","Phenotype Data"), # fileinput() function is used to get the file upload contorl option
                           helpText("Input individuals with phenotype data, as CSV"),
                           tags$hr(),
                         ),
                         mainPanel(
                           uiOutput("tb1")
                         )
                       )
                      ),
             tabPanel("Config.",
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("Settings")),
                          sliderInput('resolution', 'Resolution cM (WIP)',
                                      min=1, max=20, value=5),
                          checkboxInput(inputId = "remove_homozygous_markers", label = 'Remove homozygous markers', value = FALSE),
                          checkboxInput(inputId = "by_locus", "Remove by marker location", FALSE),
                          checkboxGroupInput(
                            "phen_check",
                            "Phenotype Data",
                            choices = NULL,
                            selected = NULL,
                            inline = FALSE,
                            width = NULL,
                            choiceNames = NULL,
                            choiceValues = NULL
                          ),
                          textInput('add', "Add markers"),
                          textInput('remove', "Remove markers"),
                          actionButton('load', "Load Config."),
                          hr(),
                          selectInput('render_helpers', 'Display Helper Tables', helperNames, selected = "Errors"),
                          hr(),
                          # selectInput('pick_dl', 'Choose File', outputNames, selected = ".dat"),
                          h5(strong("ugly download buttons")),
                          fluidRow(
                            column(width=1, offset=0,
                                   downloadButton('downloadDat',".dat")
                                   ),
                            column(width=1, offset=1,
                                   downloadButton('downloadMap',".map")
                                   )
                          ),
                          fluidRow(
                            column(width=1, offset=0,
                                   downloadButton('downloadErrors',"errors")
                            ),
                          column(width=1, offset=2,
                                 downloadButton('downloadGC',"group count")
                          )
                          ),
                        ),
                        mainPanel(
                          uiOutput('tb2')
                        )
                      )
                    )
             # tabPanel("Deploy",
             #          sidebarLayout(
             #            sidebarPanel(
             #              # checkboxInput(inputId = ".dat", label = '.dat', value = TRUE),
             #              # checkboxInput(inputId = ".map", ".map", TRUE),
             #              # checkboxInput("count", "Marker count by group", TRUE),
             #              # textInput('export_path', "Export to this filepath"),
             #              #C:/Users/Sean/Downloads/makeD package trials/exports
             #              # h4(strong("File Names")),
             #              # textInput('.dat_name', ".dat file name"),
             #              # textInput('.map_name', ".map file name"),
             #              
             #            ),
             #            mainPanel(
             #              uiOutput("LOL"))
             #          )
             #        )
             )
)


server <- function(session, input,output){
  #Cleaning up inputs before doing anything else
        master <- reactive({
          file1 <- input$master
          if(is.null(file1)){return()}
          master_cleaned <- read.csv(file=file1$datapath, fileEncoding = "UTF-8-BOM")
          master_cleaned[, colSums(is.na(master_cleaned)) == 0]
        })

        map <- reactive({
          file2 <- input$map
          if(is.null(file2)){return()}
          read.csv(file2$datapath, fileEncoding = "UTF-8-BOM")
        })

        phen <- reactive({
          file3 <- input$phen
          if(is.null(file3)){return()}
          file3 <- read.csv(file3$datapath, fileEncoding = "UTF-8-BOM")
          return(file3)
        })
        
        phen_colnames <- reactive({
          colnames(phen())
        })
        
        observeEvent(phen(), {
          l <- length(phen_colnames())
          updateCheckboxGroupInput(session, "phen_check", choices = phen_colnames()[-1], selected=phen_colnames()[-1])
        })

        errors <- reactive({
          if(is.null(map()) | is.null(phen()) | is.null(master())){
            return()
          }
          else{
            makeD_get_errors(phen(), master())
          }
        })

        p_summary <- reactive({
          if(is.null(map()) | is.null(phen()) | is.null(master())){
            return()
          }
          else{
            parent_SNP_summary(phen(), master(), map(), 2, 7)
          }
        })

        p_h_markers <- reactive({
          if(is.null(map()) | is.null(phen()) | is.null(master())){
            return()
          }
          else{
            return(parent_SNPs_culled(phen(), master(), map(), 2, 7))
          }
        })
        input_summary <- eventReactive(input$load, {
          if(is.null(map()) | is.null(phen()) | is.null(master())){
            return()
          }
          else if(input$remove_homozygous_markers == FALSE &
             input$by_locus == FALSE){
            input_SNP_summary(phen(), master(), map(), 2, 7)
          }
          else if(input$remove_homozygous_markers == TRUE &
                  input$by_locus == FALSE){
            temp_summary <- input_SNP_summary(phen(), master(), map(), 2, 7)
            temp_parent_summary <- parent_SNP_summary(phen(), master(), map(), 2, 7)
            p_homozygous_vector <- to_extract_parent_homozygous(temp_parent_summary)
            temp_summary <- subtraction_input_summary(temp_summary, p_homozygous_vector)
            return(temp_summary)
          }
          else if(input$remove_homozygous_markers == FALSE &
                  input$by_locus == TRUE){
            temp_summary <- input_SNP_summary(phen(), master(), map(), 2, 7)
            temp_summary <- cull_by_locus(temp_summary)
            return(temp_summary)
          }
          else{
            temp_summary <- input_SNP_summary(phen(), master(), map(), 2, 7)
            temp_parent_summary <- parent_SNP_summary(phen(), master(), map(), 2, 7)
            p_homozygous_vector <- to_extract_parent_homozygous(temp_parent_summary)
            temp_summary <- subtraction_input_summary(temp_summary, p_homozygous_vector)
            temp_summary <- cull_by_locus(temp_summary)
            return(temp_summary)
          }
        })

        positive_marker_set <- reactive({
          if(is.null(map()) | is.null(phen()) | is.null(master())){
            return()
          }
          else{
            included_markers(input_summary())
          }
        })

        negative_marker_set <- reactive({
          if(is.null(map()) | is.null(phen()) | is.null(master())){
            return()
          }
          else{
            excluded_markers(input_summary())
          }
        })
        
        dat_file <- eventReactive(input$load, {
          if(is.null(input_summary())){
            return()
          }
          else{
            phen_len <- length(phen_colnames())-1
            phen_filter <- rep(F, phen_len)
            phen_names <- phen_colnames()[-1]
            for(x in 1:length(phen_filter)){
              if(phen_names[x] %in% input$phen_check){
                phen_filter[x] <- T
              }
            }
            temp_out <- update_out_by_summary(phen(), master(), 2, 7, input_summary())
            temp_out_filter <- c(rep(T,5), phen_filter, rep(T, length(temp_out)-(5+phen_len)))
            return(temp_out[,temp_out_filter])
          }
        })

        map_file <- reactive({
          if(is.null(input_summary())){
            return()
          }
          else{
            return(map_final(input_summary(), map()))
          }
        })
        
        group_count <- reactive({
          if(is.null(input_summary())){
            return()
          }
          else{
            return(marker_count(input_summary()))
          }
        })

  output$errors_frame <- renderTable({
    errors()
  })

  output$parent_summary <- renderTable({
      p_summary()
  })

  output$ph_markers_table <- renderTable({
      p_h_markers()
  })

  output$phen_frame <- renderTable({
    phen()
  })

  output$input_summary_frame <- renderTable({
    input_summary()
  })

  output$positive_marker_set_frame <- renderTable({
    positive_marker_set()
  })

  output$negative_marker_set_frame <- renderTable({
    negative_marker_set()
  })

  output$master_confirmation <- renderText({
    if(is.null(master())){
      return("No master data set has been uploaded.")
    }
    "The set is too big to render here, but it has been successfully received."
  })

  output$map_frame <- renderTable({
    map()
  })

  output$missing_map <- renderText({
    "No map file has been uploaded."
  })

  output$missing_phen_data <- renderText({
    "No phenotype data has been uploaded."
  })

  output$tb1 <- renderUI({
    tabsetPanel(tabPanel("Map", {
      if(is.null(map())){
        verbatimTextOutput("missing_map")
      }
      else {
        tableOutput("map_frame")
      }
    }),
    tabPanel("Phenotype Data",  {
      if(is.null(map())){
        verbatimTextOutput("missing_phen_data")
      }
      else {
        tableOutput("phen_frame")
      }
    }),
    tabPanel("Master Data Set", verbatimTextOutput("master_confirmation"))
    )
  })

  output$tb2 <- renderUI({
    if(input$render_helpers == "Errors"){
      tableOutput("errors_frame")
    }
    else if(input$render_helpers == "Marker Summary (Parents)"){
      tableOutput("parent_summary")
    }
    else if(input$render_helpers == "Parent-Homozygous Markers"){
      tableOutput("ph_markers_table")
    }
    else if(input$render_helpers == "Marker Summary (Input Set)"){
      tableOutput("input_summary_frame")
    }
    else if(input$render_helpers == "Positive Marker Set"){
      tableOutput("positive_marker_set_frame")
    }
    else if(input$render_helpers == "Negative Marker Set"){
      tableOutput("negative_marker_set_frame")
    }
  })
  
  
  
  output$downloadDat <- downloadHandler(
    filename = function(){
      paste("dat", ".csv", sep="")
    },
    content = function(file){
      write.table(dat_file(), file, row.names = FALSE, col.names = FALSE, sep=',', na="")
    }
  )
  
  output$downloadErrors <- downloadHandler(
    filename = function(){
      paste("errors", ".csv", sep="")
    },
    content = function(file){
      write.table(errors(), file, row.names = FALSE, col.names = TRUE, sep=',', na="")
    }
  )
  
  output$downloadMap <- downloadHandler(
    filename = function(){
      paste("map", ".csv", sep="")
    },
    content = function(file){
      write.table(map_file(), file, row.names = FALSE, col.names = TRUE, sep=',', na="")
    }
  )
  
  output$downloadGC <- downloadHandler(
    filename = function(){
      paste("marker count by group", ".csv", sep="")
    },
    content = function(file){
      write.table(group_count(), file, sep = "\t", row.names = FALSE, col.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
