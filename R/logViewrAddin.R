#' Log Viewer Addin
#' @export
logViewerAddin <- function() {
  library(shiny)
  library(miniUI)
  env = ls(".GlobalEnv")
  ui <- miniPage(
    gadgetTitleBar("Log Viewer"),
    miniContentPanel(
      h2("View log4r Logs Currently Available in R Session"),
      hr(),
      fillRow(
      selectInput("log_list", "List of Loggers", choices = list()),
      selectInput("level_filter", "At Least Level:", 
                  choices = list("DEBUG", "INFO", "WARN", "ERROR", "FATAL"),
                  selected = "FATAL"), height = "75px"),
      fillRow(textInput("log_search", "Search for Log Text"), height = "50px"),
      fillRow(checkboxInput("ignore_case", "Ignore Case?", value = TRUE), height = "75px"),
      fillRow(verbatimTextOutput("log_text"))
    )
  )
  
  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.
    observe({
      # find the current objects in the R environment that are loggers
      obj_class = sapply(env, function(x){class(get(x))})
      updateSelectInput(session, "log_list", 
                        choices = as.list(obj_class[obj_class == "logger"]))
    })

    filtered_log = reactive({
      req(input$log_list)
      log_file = get(input$log_list)$logfile
      log_text = readChar(log_file, file.info(log_file)$size)
      log_df = data.frame(lines = strsplit(log_text, "\n")[[1]],
                          stringsAsFactors = FALSE)

      # filter by the log level
      if (length(log_df[grep(input$level_filter, log_df$lines),]) == 0) {
        return("No logs with these filters")
      }
      sub_log = data.frame(lines = log_df[grep(input$level_filter, log_df$lines),],
                           stringsAsFactors = FALSE)
      
      # filter by the text
      if (length(sub_log[grep(input$log_search, sub_log$lines, ignore.case = input$ignore_case),]) == 0) {
        return("No logs with these filters")
      }

      paste(sub_log[grep(input$log_search, sub_log$lines, 
                         ignore.case = input$ignore_case),],collapse = "\n")
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