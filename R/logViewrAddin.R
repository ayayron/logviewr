#' Log Viewer Addin
#' @export
logViewerAddin <- function() {
  library(shiny)
  library(miniUI)
  
  ui <- miniPage(
    gadgetTitleBar("Log Viewer"),
    miniTabstripPanel(
      miniTabPanel("View", icon = icon("search"),
        miniButtonBlock(actionButton("refresh_btn", "Refresh", icon = icon("refresh"))),
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
  ),
    miniTabPanel("Create", icon = icon("plus-square"),
      miniContentPanel(
        h2("Create log4r Object in RStudio Session"),
        hr(),
        textInput("logger_name", "Logger Object Name", value = "logger"),
        selectInput("create_logger_level", "Set Logger Level", 
                    choices = list("DEBUG", "INFO", "WARN", "ERROR", "FATAL")),
        textInput("logger_location", "Logfile location", value = "./logfile.log"),
        actionButton("create_logger_btn", "Create Logger")
      )
    )
  ))
  
  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.
    
    get_env = eventReactive(input$refresh_btn,{
      ls(".GlobalEnv")
    })

    observeEvent(input$create_logger_btn, {
      library(log4r)
      assign(input$logger_name, create.logger(), envir = as.environment(".GlobalEnv"))
      
      assign_logfile = paste0("logfile(", input$logger_name ,") = file.path('", input$logger_location,"')")
      assign_level = paste0("level(",input$logger_name, ") = '", input$create_logger_level,"'")

      eval(parse(text = assign_logfile ), envir = as.environment(".GlobalEnv"))
      eval(parse(text = assign_level), envir = as.environment(".GlobalEnv"))
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
      }, warning = function (w) {
        return(w$message)
      }, error = function (e) {
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
  }

  viewer <- dialogViewer("LogViewr")
  runGadget(ui, server, viewer = viewer)
}