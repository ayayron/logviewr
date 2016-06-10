#' Log Viewer Addin
#' @export
logViewerAddin <- function() {
  ui <- miniPage(
    gadgetTitleBar("Log Viewer"),
    miniTabstripPanel(
    miniTabPanel("Create", icon = icon("plus-square"),
      miniContentPanel(
        h2("Create log4r Object in RStudio Session"),
        hr(),
        fillRow(textInput("logger_name", "Logger Object Name", value = "logger"),
                selectInput("create_logger_level", "Set Logger Level", 
                    choices = list("DEBUG", "INFO", "WARN", "ERROR", "FATAL")),
                height = "75px"),
        fillRow(textInput("logger_location", "Logfile location", value = "./logfile.log"),
                textInput("line_location", "Enter the line number to start the log", 
                          value = "1"), height = "75px"),
        fillRow(checkboxInput("print_to_log", "Convert Print Statements to Logs?"),
                actionButton("create_logger_btn", "Create Logger", class = "btn-primary"),
                height = "75px"),
        uiOutput("warning_output"),
        verbatimTextOutput("code_output")
      )
    ),
      miniTabPanel("View", icon = icon("search"),
                   miniButtonBlock(actionButton("refresh_btn", "Refresh", 
                                                class = "btn-primary", icon = icon("refresh"))),
                   miniContentPanel(
                     h2("View log4r Logs Currently Available in R Session"),
                     hr(),
                     
                     fillRow(
                       selectInput("log_list", "List of Loggers", choices = list()),
                       selectInput("level_filter", "At Least Level:", 
                                   choices = list("DEBUG", "INFO", "WARN", "ERROR", "FATAL"),
                                   selected = "DEBUG"), height = "75px"),
                     fillRow(textInput("log_search", "Search for Log Text"), height = "50px"),
                     fillRow(checkboxInput("ignore_case", "Ignore Case?", value = TRUE), height = "75px"),
                     fillRow(verbatimTextOutput("log_text"))
                   )
      )
  ))
  
  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.
    
    get_env = eventReactive(input$refresh_btn,{
      ls(".GlobalEnv")
    })

    create_logger = reactive({
      assign_logfile = paste0("log4r::logfile(", input$logger_name,
                              ") = file.path('", input$logger_location,"')")
      assign_level = paste0("log4r::level(",input$logger_name, ") = '",
                            input$create_logger_level,"'")

      c("# creating log file", "library(log4r)",
              paste(input$logger_name, "<- log4r::create.logger()"),
              assign_logfile,assign_level, "")
    })

    observe({
      # find the current objects in the R environment that are loggers
      obj_class = sapply(get_env(), function(x){class(get(x))})
      updateSelectInput(session, "log_list", 
                        choices = names(as.list(obj_class[obj_class == "logger"])))
    })

    filtered_log = reactive({
      req(input$log_list)

      log_levels = c("DEBUG", "INFO", "WARN", "ERROR", "FATAL")
      if (input$level_filter == "INFO") {
        log_levels = log_levels[2:5]
      } else if (input$level_filter == "WARN") {
        log_levels = log_levels[3:5]
      } else if (input$level_filter == "ERROR") {
        log_levels = log_levels[4:5]
      } else if (input$level_filter == "FATAL") {
        log_levels = log_levels[5]
      }
      log_levels = paste(log_levels, collapse = "|")

      log_file = get(input$log_list)$logfile
      results = tryCatch({
        log_text = readChar(log_file, file.info(log_file)$size)
        
        log_df = data.frame(lines = strsplit(log_text, "\n")[[1]],
                            stringsAsFactors = FALSE)
        
        # filter by the log level
        if (length(log_df[grep(log_levels, log_df$lines),]) == 0) {
          return("No logs with these filters")
        }
        sub_log = data.frame(lines = log_df[grep(log_levels, log_df$lines),],
                             stringsAsFactors = FALSE)
        
        # filter by the text
        if (length(sub_log[grep(input$log_search, sub_log$lines, ignore.case = input$ignore_case),]) == 0) {
          return("No logs with these filters")
        }
        
        paste(sub_log[grep(input$log_search, sub_log$lines, 
                           ignore.case = input$ignore_case),],collapse = "\n")
      }, warning = function(w) {
        return(w$message)
      }, error = function(e) {
        return(e$message)
      }, finally = {})
    
      return(results)
    })
    
    output$log_text = renderText({
      filtered_log()
    })

    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      stopApp()
    })
    
    output$warning_output = renderUI({
      doc = rstudioapi::getActiveDocumentContext()
      if (doc$id == "#console") {
        return(tagList(icon("exclamation-triangle"), 
                       span("Warning: Your active document is the console.\
                            To use a file, close this Addin, click in the window\
                            of your document and open the Addin again.")))
      }
      
    })
    create_code = function() {
      # get the active document text and render it 
      # with the logger and the updated print2log changes
      doc = rstudioapi::getActiveDocumentContext()
      create_log_lines = paste(create_logger(), collapse = "\n")
      
      if (input$line_location == "1" || is.na(input$line_location)) {
        code_lines = c(create_log_lines, doc$contents)
      } else if (as.integer(input$line_location) > length(doc$contents)) {
        code_lines = c(doc$contents, create_log_lines)
      } else if (as.integer(input$line_location) > 1) {
        code_lines = c(doc$contents[1:as.integer(input$line_location)], "",
                       create_log_lines,
                       doc$contents[as.integer(input$line_location):length(doc$contents)])
      }
      
      if (input$print_to_log) {
        code_lines = sub("print\\(",
                         paste0("log4r::",tolower(input$create_logger_level),"(", input$logger_name, ", "),
                         code_lines)
      }
      
      return(paste(code_lines,collapse = "\n"))
    }
    
    output$code_output = renderText({
      create_code()
    })
    
    observeEvent(input$create_logger_btn, {
      rstudioapi::setDocumentContents(create_code())
      stopApp()
    })
    
  }

  viewer <- dialogViewer("LogViewr", width = 800, height = 800)
  runGadget(ui, server, viewer = viewer)
}