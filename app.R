library(shiny)
library(Qrator)
library(tibble)
library(shinyFiles)
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
                        column(width = 6, offset = 3, h3(("Required Files"))),
                        column(width = 6, offset = 3, p(strong("Map File:"), "intmap11_20k")),
                      ),
                      fluidRow(
                        column(width = 6, offset = 3, p(strong("Master Data Set:"), "20k_8koverlap")),
                      ),
                      fluidRow(
                        column(width = 6, offset = 3, p(strong("Phenotype Data:"), "The formatting of your phenotype data must follow these rules.
                                                        List all individuals in column A with column name 'Index'. 
                                                        Data for each individual goes in columns B and onward. 
                                                        Each column of phenotype data must have a descriptive column name in Row 1. 
                                                        Pictured is an example phenotype data file.")),
                        br()
                      ),
                      HTML('<center><img src="phenotype_data_example.png"></center>'),
                      fluidRow(
                        column(width = 6, offset = 3, h3(("Features & Navigation"))),
                        column(width = 6, offset = 3, p("Navigate ", code("Q-rator"), " by the upper navbar. The tabs are organized in sequential
                                                        order.")),
                        br(),
                        column(width = 6, offset = 3, p("Upload files to File Input tab.")),
                        br(),
                        column(width = 6, offset = 3, p("Pick assembly settings at Config. tab and load them with the 'Load Config' 
                                                        button on the sidebar. All settings are explained in detail in the Glossary.")),
                        br(),
                        column(width = 6, offset = 3, p("Verify applied settings by checking the data summary tables. You can pick which table to 
                                                        render in the main panel via the dropdown menu titled 'Display Helper Tables'. All summary
                                                        tables are explained in detail in the Glossary.")),
                        br(),
                        column(width = 6, offset = 3, p("Once you have confirmed your settings, provide a descriptive name for your current session
                                                        of Q-rator and click the 'Export' button. All necessary files will be exported with file 
                                                        names based on the session name you provided. Files are explained in detail in the Glossary."))
                      ),
                      fluidRow(
                        column(width = 6, offset = 3, h3(("Glossary - Settings"))),
                        column(width = 6, offset = 3, p(strong("Resolution (cM):"), "This slider controls the resolution of your map. Provide an input
                                                        integer between 1 and 10, and this integer becomes the scale of your map. For instance, setting
                                                        the slider to 5 cM will subset markers every 5 cM beginning with the first marker in group 1.
                                                        Setting the slider to 0 is the max resolution and will keep every marker in your map.
                                                        Default value is 0. ")),
                        br(),
                        column(width = 6, offset = 3, p(strong("Remove homozygous markers:"), "Set this flag to filter out all markers for which all parents
                                                        of your input individuals are homozygous (AA, BB, CC, etc).  ")),
                        br(),
                        column(width = 6, offset = 3, p(strong("Remove by marker location:"), "Set this flag to filter out markers according to these rules:
                                                        if more than one marker shares the same locus within a Group, the first marker in the map is kept in
                                                        the data set, and the other(s) is/are filtered out.")),
                        br(),
                        column(width = 6, offset = 3, p("Example: In group 1 of the intmap11_20k, 5 SNPs exist at locus 24.104 ")),
                        br(),
                      ),
                      HTML('<center><img src="marker_location_redundancies.png"></center>'),
                      br(),
                      fluidRow(
                        column(width = 6, offset = 3, p("Setting this flag will remove SNPs 870, 2101, 1562, and 1563.")),
                        br(),
                        column(width = 6, offset = 3, p(strong("Phenotype data:"), "This group of flags determines which columns
                                                        of phenotype data you wish to keep in the output data file.")),
                        br(),
                        column(width = 6, offset = 3, p(strong("Add markers:"), "Correct any previous subsets on the markers in your data set.
                                                        If you have removed a marker from the data set by applying a setting, you may add it
                                                        back into the data set by providing its ID in the text field. To apply the addition,
                                                        press the 'Load Config.' button.")),
                        br(),
                        column(width = 6, offset = 3, p(strong("Remove markers:"), "If you wish to remove a marker in your data set, 
                                                        provide its ID in the text field. To apply the removal, press the 'Load Config.' 
                                                        button.")),
                        br(),
                        column(width = 6, offset = 3, p(strong("Session ID:"), "Provide a descriptive name for the current session of Q-rator.
                                                        The provided text will be used to name all exported files. You will be unable to export
                                                        any files unless a session ID is provided."))
                      ),
                      fluidRow(
                        column(width = 6, offset = 3, h3(("Glossary - Summary Tables"))),
                        column(width = 6, offset = 3, p(strong("Errors"), "is a list of all individuals that were present in the input phenotype data file,
                                                        but not present in the 20k_8koverlap file. If there were no clerical errors in the creation of your
                                                        phenotype data file, then it may well be empty")),
                        br(),
                        column(width = 6, offset = 3, p(strong("Parent homozygous markers"), "is a subset of the master data set which shows the set of all
                                                        parents of input individuals, and all markers for which the parents are homozygous.")),
                        br(),
                        column(width = 6, offset = 3, p(strong("Marker Summary (Parents)"), "is a data frame showing allele frequencies for every marker
                                                        calculated for the set of all parents of input individuals.")),
                        br(),
                        column(width = 6, offset = 3, p(strong("Marker Summary (Input Set)"), "is a data frame showing allele frequencies for every marker 
                                                        calculated for the set of all input individuals, group and locus for every marker, and whether or 
                                                        not the marker is included in your final data set depending on the settings you applied. Column 'Included' 
                                                        takes binary values: 1 if the marker is included in the data set, 0 if it has been removed.")),
                        br(),
                        column(width = 6, offset = 3, p(strong("Positive Marker Set"), "is a subset of the input set marker summary. Only rows which have value
                                                        of 1 in the 'Included' column are present. If default settings are applied, then this set is equal to 
                                                        the marker summary input set.")),
                        br(),
                        column(width = 6, offset = 3, p(strong("Negative Marker Set"), "is a subset of the input set marker summary. Only rows which have value of 0
                                                        in the 'Included' column are present. If default settings are applied, then this data frame is empty.")),
                        br(),
                      ),
                      fluidRow(
                        column(width = 6, offset = 3, h3(("Glossary - Exported Files"))),
                        column(width = 6, offset = 3, p(strong("Note:"), "Note: these are generic names. If you provided a session ID under the settings,
                                                        it will be applied to all the exported files. For instance, if the session ID is 'test1', then the 
                                                        errors file will be named", code("test1_errors.csv"), "the dat file will be called ", code('test1_dat.csv'),
                                                        " and so on.", code("Q-rator"), " cannot handle files types such as '.map' or '.dat', so all files will
                                                        be exported as CSVs and must be converted by the user." )),
                        br(),
                        column(width = 6, offset = 3, p(code("errors.csv"), "is an exported version of the errors table")),
                        br(),
                        column(width = 6, offset = 3, p(code("dat.csv"), "is the sheet to be used in FlexQTL, matching individuals to their pedigrees, 
                                                        phenotype data, and marker data. Must be converted manually to DAT as it is a CSV by default")),
                        br(),
                        column(width = 6, offset = 3, p(code("map.csv"), "is the map file corresponding to ", code("dat.csv"), " with those SNPs present
                                                        in ", code("dat.csv"), " represented in column A, and their Group and location in column B. Must be converted to 
                                                        a .map file as it is a CSV by default.")),
                        br(),
                        column(width = 6, offset = 3, p(code("marker_count.csv"), "provides a count of SNPs per group based on the SNPs present in dat.csv. 
                                                        Row number corresponds to group number, so the integer on row 3 is the count of group 3 SNPs present in ", 
                                                        code("dat.csv."))),
                        br(),
                        br(),
                        br(),
                        br()
                      ),

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
                          sliderInput('resolution', 'Resolution (cM)',
                                      min=0, max=10, value=0),
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
                          textInput('sessionID', "Session ID"),
                          
                          h5(strong("Export Files")),
                          shinyDirButton('folder', 'Select a folder', 'Please select a folder', FALSE),
                          actionButton('export', "Export")
                        ),
                        mainPanel(
                          uiOutput('tb2')
                        )
                      )
                    )
             )
)


server <- function(session, input,output){
  
  volumes = getVolumes()()
  
  shinyDirChoose(input, 'folder', roots=volumes, session = session)

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
            parent_SNP_summary(phen(), master(), map())
          }
        })

        p_h_markers <- reactive({
          if(is.null(map()) | is.null(phen()) | is.null(master())){
            return()
          }
          else{
            return(parent_SNPs_culled(phen(), master(), map()))
          }
        })
        input_summary <- eventReactive(input$load, {
          if(is.null(map()) | is.null(phen()) | is.null(master())){
            return()
          }
          
          temp_summary <- input_SNP_summary(phen(), master(), map())
          
          if(input$remove_homozygous_markers == FALSE &
             input$by_locus == FALSE){
            input_SNP_summary(phen(), master(), map())
          }
          else if(input$remove_homozygous_markers == TRUE &
                  input$by_locus == FALSE){
            temp_parent_summary <- parent_SNP_summary(phen(), master(), map())
            p_homozygous_vector <- to_extract_parent_homozygous(temp_parent_summary)
            temp_summary <- subtraction_input_summary(temp_summary, p_homozygous_vector)
          }
          else if(input$remove_homozygous_markers == FALSE &
                  input$by_locus == TRUE){
            temp_summary <- cull_by_locus(temp_summary)
          }
          else{
            temp_parent_summary <- parent_SNP_summary(phen(), master(), map())
            p_homozygous_vector <- to_extract_parent_homozygous(temp_parent_summary)
            temp_summary <- subtraction_input_summary(temp_summary, p_homozygous_vector)
            temp_summary <- cull_by_locus(temp_summary)
          }
          
          if(input$resolution > 0){
            temp_summary <- summary_resolution_adjust(input$resolution, map(), temp_summary)
          }
          
          return(temp_summary)
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
            temp_out <- update_out_by_summary(phen(), master(), input_summary())
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

  observeEvent(input$export,   {
    if(!is.null(input$folder)){
      fileinfo <- parseSavePath(volumes, input$folder)
      path <- as.character(fileinfo$datapath)
      setwd(path)
      if(!is.null(group_count()) & !is.null(map_file()) & !is.null(errors())
         & !is.null(dat_file())){
        write.table(dat_file(), 
                    paste(input$sessionID, "_dat", ".csv", sep = ''),
                    row.names = FALSE, 
                    col.names = FALSE, 
                    sep=',', na="")
        write.table(group_count(), 
                    paste(input$sessionID, "_marker_count", ".csv", sep = ''),
                    row.names = FALSE, 
                    col.names = FALSE, 
                    sep=',', na="")
        write.table(errors(), 
                    paste(input$sessionID, "_errors", ".csv", sep = ''),
                    row.names = FALSE, 
                    col.names = TRUE, 
                    sep=',', na="")
        write.table(map_file(), 
                    paste(input$sessionID, "_map", ".csv", sep = ''),
                    row.names = FALSE, 
                    col.names = TRUE, 
                    sep=',', na="")
      }
    }
  })
  
}

shinyApp(ui = ui, server = server)
