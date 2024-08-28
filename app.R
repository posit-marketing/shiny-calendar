# Setup -------------------------------------------------------------------

library(shiny)
library(plotly)
# pak::pak("rstudio/gridlayout")
library(gridlayout)
library(bslib)
library(toastui)
library(shinyfilter)
library(pins)
library(bslib)
library(tibble)
library(htmltools)
library(showtext)

board <-
  board_connect(server = Sys.getenv("CONNECT_SERVER"),
                key = Sys.getenv("CONNECT_API_KEY"))

name = "isabella.velasquez/shiny-calendar-pin"

sysfonts::font_add_google("Open Sans", "open-sans")
showtext::showtext_auto()

# UI ----------------------------------------------------------------------

foot <-
  tags$div(
    style = "background-color: #FFFFFF; padding: 0px; text-align: center; bottom: 0; width: 100%;",
    HTML(
      "Powered by <a href='https://posit.co'><img src='https://www.rstudio.com/assets/img/posit-logo-fullcolor-TM.svg' alt='Posit Logo' style='width:55px;'></a>"
    )
  )

ui <- page_sidebar(
  theme = bs_theme(
    bootswatch = "cosmo",
    base_font = font_google("Open Sans"),
    bg = "white",
    fg = "#17212B",
    secondary = "#447099",
    heading_font = font_google("Open Sans", wght = "300")
  ),
  
  sidebar = sidebar(title = "Filters", uiOutput("filterUI")),
  
  grid_card_text(
    area = "header",
    content = "Shiny Content Calendar",
    alignment = "start",
    is_title = TRUE,
    icon = "https://docs.posit.co/images/posit-ball.png"
  ),
  
  card(
    card_header("Calendar"),
    calendarOutput(
      outputId = "calendar",
      width = "100%",
      height = "100%"
    )
  ),
  
  card_footer(foot)
  
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  reactive_pin <- pins::pin_reactive_read(name = name, board = board)
  
  r <- reactiveValues(all_data = NULL, filtered_data = NULL)
  
  observe({
    req(reactive_pin())
    all_results <- reactive_pin() |>
      as.data.frame() |>
      filter(!is.na(title) & !is.na(duedate)) |>
      rowid_to_column("id")
    
    r$all_data <- all_results
    r$filtered_data <- all_results
  })
  
  output$filterUI <- renderUI({
    req(r$all_data)
    
    tagList(
      selectizeInput(
        inputId = "categoryCheckbox",
        label = "Category",
        choices = sort(unique(r$all_data$Category)),
        multiple = TRUE,
        options = list(onChange = event("ev_click"))
      ),
      
      selectizeInput(
        inputId = "priorityCheckbox",
        label = "Priority",
        choices = sort(unique(r$all_data$Priority)),
        multiple = TRUE,
        options = list(onChange = event("ev_click"))
      ),
      
      selectizeInput(
        inputId = "statusCheckbox",
        label = "Status",
        choices = sort(unique(r$all_data$Status)),
        multiple = TRUE,
        options = list(onChange = event("ev_click"))
      )
    )
  })
  
  observe({
    req(r$all_data)
    define_filters(
      input,
      "calendar",
      c(
        categoryCheckbox = "Category",
        priorityCheckbox = "Priority",
        statusCheckbox = "Status"
      ),
      r$all_data
    )
  })
  
  observeEvent(input$ev_click, {
    r$filtered_data <- update_filters(input, session, "calendar")
    update_tooltips("calendar", session)
  })
  
  output$calendar <- renderCalendar({
    req(r$filtered_data)
    calendar(
      defaultDate = "2024-09-02",
      r$filtered_data,
      isReadOnly = TRUE,
      navigation = TRUE,
      useDetailPopup = FALSE,
      useCreationPopup = FALSE,
      navOpts = navigation_options(color = "#447099", bg = "white")
    ) |>
      cal_theme(month = list(
        moreView = list(
          border = "1px solid #D3D3D3",
          backgroundColor = "white",
          width = 350,
          height = 200
        )
      )) |>
      cal_month_options(workweek = TRUE) |>
      cal_events(
        clickSchedule = JS(
          "function(obj) {",
          "Shiny.setInputValue('calendar_id_click', {id: obj.event.id, x: obj.nativeEvent.clientX, y: obj.nativeEvent.clientY});",
          "}"
        )
      )
  })
  
  observeEvent(input$calendar_id_click, {
    removeUI(selector = "#custom_popup")
    id <- as.numeric(input$calendar_id_click$id)
    sched <- r$all_data[r$all_data$id == id, ]
    
    insertUI(
      selector = "body",
      ui = absolutePanel(
        id = "custom_popup",
        top = input$calendar_id_click$y,
        left = input$calendar_id_click$x,
        draggable = FALSE,
        width = "400px",
        tags$div(
          style = "font-size: 14px; width: 250px; position: relative; z-index: 9999; background: #FFF; padding: 10px; border: 1px solid #D3D3D3; border-radius: 1px;",
          actionLink(
            inputId = "close_calendar_panel",
            label = NULL,
            icon = icon("close"),
            style = "position: absolute; top: 5px; right: 5px;"
          ),
          tags$div(
            style = "text-align: left;",
            tags$b(sched$title),
            tags$p("Due Date: ", sched$duedate),
            tags$br(),
            tags$a("More info", target = "_blank", href = sched$url)
          )
        )
      )
    )
  })
  
  observeEvent(input$close_calendar_panel, {
    removeUI(selector = "#custom_popup")
  })
}

shinyApp(ui, server)