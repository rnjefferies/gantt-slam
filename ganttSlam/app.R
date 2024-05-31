library(shiny)
library(tidyverse)
library(DT)
library(glue)
library(RColorBrewer)
library(colourpicker)
library(shinyjqui)
library(ggtext)
library(forcats)
library(stringr)
library(ggplot2)
library(lubridate)

# Source the Gantt chart module using a relative path
source("ganttChartModule.R")


ui <- fluidPage(
    tags$head(
        includeCSS("www/main.css"),
    ),
    sidebarLayout(
        sidebarPanel(
            class = "sidebar",
            tags$div(
                  # Add logo here
                
                style = "text-align: center;",  # Center-align the content
                tags$h2("|-GANTT SLAM-|"),
                tags$img(src = "logo.png", class = "responsive-logo", width = "270px", alt = "App Logo", style = "display: block; margin-left: auto; margin-right: auto;"),                
                tags$hr()
            ),
            textInput(inputId = "chartTitle", label = "Chart Title:", placeholder = "e.g., PhD Gantt Chart"),
            textInput(inputId = "mainTaskName", label = "Main Task:", placeholder = "e.g., Holiday Time"),
            colourInput(inputId = "mainTaskColor", label = "Main Task Color:", value = colors[1]),
            textInput(inputId = "subTaskName", label = "Sub-Task:", placeholder = "e.g., Skateboarding"),
            dateInput(inputId = "inStartDate", value = Sys.Date(), label = "Start Date:"), 
            dateInput(inputId = "inEndDate", value = Sys.Date() + 10, label = "End Date:"),
            actionButton(inputId = "btnAddMainTask", label = "Add Main Task"),
            actionButton(inputId = "btnAddSubTask", label = "Add Sub-Task"),
            fileInput(inputId = "fileInput", label = "Load Tasks (for big Gantts!)", accept = ".csv"),
            downloadButton(outputId = "downloadData", label = "Save Tasks"),
            actionButton(inputId = "btnOpenChart", label = "Open Gantt Chart")
        ),
        mainPanel(
            tags$div(
                class = "card",
                tags$h3("Task Table View"),
                tags$hr(),
                DTOutput(outputId = "tableTasks")
            ),
            tags$div(
                class = "card",
                tags$h3("Task Chart Preview"),
                tags$hr(),
                plotOutput(outputId = "plotTasks")
            )
        )
    )
)

server <- function(input, output, session) {
    initial_data <- tibble(
        ID = integer(),
        Task = character(),
        SubTask = character(),
        StartDate = as.Date(character()),
        EndDate = as.Date(character()),
        Type = character(),
        Color = character(),
        Remove = character(),
        TaskLabel = character(),
        TaskFace = character()
    )
    
    df <- reactiveValues(data = initial_data)
    
    update_task_factors <- function(df) {
        df %>%
            mutate(
                TaskLabel = factor(TaskLabel, levels = unique(TaskLabel)),
                TaskFace = factor(TaskFace, levels = c("plain", "bold"))
            )
    }
    
    add_task <- function(task_name, sub_task_name, start_date, end_date, type, color) {
        new_id <- ifelse(nrow(df$data) == 0, 1, max(df$data$ID, na.rm = TRUE) + 1)
        new_row <- tibble(
            ID = new_id,
            Task = task_name,
            SubTask = sub_task_name,
            StartDate = as.Date(start_date),
            EndDate = as.Date(end_date),
            Type = type,
            Color = color,
            Remove = glue('<button id="custom_btn" onclick="Shiny.onInputChange(\'button_id\', \'{new_id}\')">Remove</button>'),
            TaskLabel = ifelse(type == "Main", paste0("<b>", task_name, "</b>"), paste0("&nbsp;&nbsp;&nbsp;", sub_task_name)),
            TaskFace = ifelse(type == "Main", "bold", "plain")
        )
        df$data <- bind_rows(df$data, new_row) %>% arrange(ID)
        
        # Ensure TaskLabel and TaskFace are consistently factored and ordered
        df$data <- update_task_factors(df$data)
        
        # Debug: Print the data frame
        print("Data frame after adding a task:")
        print(df$data)
    }
    
    observeEvent(input$btnAddMainTask, {
        task_name <- input$mainTaskName
        task_start_date <- input$inStartDate
        task_end_date <- input$inEndDate
        task_color <- input$mainTaskColor
        
        if (!is.null(task_name) && !is.null(task_start_date) && !is.null(task_end_date)) {
            add_task(task_name, NA, task_start_date, task_end_date, "Main", task_color)
        }
    })
    
    observeEvent(input$btnAddSubTask, {
        sub_task_name <- input$subTaskName
        task_start_date <- input$inStartDate
        task_end_date <- input$inEndDate
        main_task_name <- input$mainTaskName
        
        if (!is.null(sub_task_name) && !is.null(task_start_date) && !is.null(task_end_date) && !is.null(main_task_name)) {
            parent_color <- df$data %>%
                filter(Task == main_task_name & Type == "Main") %>%
                pull(Color)
            if (length(parent_color) > 0) {
                add_task(main_task_name, sub_task_name, task_start_date, task_end_date, "Sub", parent_color)
            }
        }
    })
    
    observeEvent(input$button_id, {
        df$data <- df$data %>%
            filter(ID != as.integer(input$button_id)) %>%
            arrange(ID)
        
        # Ensure TaskLabel and TaskFace are consistently factored and ordered
        df$data <- update_task_factors(df$data)
        
        # Debug: Print the data frame after removing a task
        print("Data frame after removing a task:")
        print(df$data)
    })
    
    observeEvent(input$fileInput, {
        req(input$fileInput)
        df$data <- read_csv(input$fileInput$datapath)
        df$data <- df$data %>%
            mutate(
                StartDate = as.Date(StartDate),
                EndDate = as.Date(EndDate),
                TaskLabel = ifelse(Type == "Main", paste0("<b>", Task, "</b>"), paste0("&nbsp;&nbsp;&nbsp;", SubTask)),
                TaskFace = ifelse(Type == "Main", "bold", "plain")
            ) %>%
            arrange(ID)
        
        # Ensure TaskLabel and TaskFace are consistently factored and ordered
        df$data <- update_task_factors(df$data)
        
        # Debug: Print the data frame after loading from file
        print("Data frame after loading from file:")
        print(df$data)
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("tasks-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write_csv(df$data %>% select(-Remove), file)
        }
    )
    
    output$tableTasks <- renderDT({
        datatable(df$data, escape = FALSE, options = list(
            columnDefs = list(
                list(targets = 7, render = JS(
                    "function(data, type, row, meta) {",
                    "return '<div style=\"background-color:' + data + ';width:20px;height:20px;border-radius:50%;\"></div>';",
                    "}"
                ))
            )
        ))
    })
    
    output$plotTasks <- renderPlot({
        if (nrow(df$data) > 0) {
            # Ensure TaskLabel and TaskFace are consistently factored and ordered
            df$data <- update_task_factors(df$data)
            
            # Debug: Print the data frame
            print("Data frame before plotting:")
            print(df$data)
            
            # Define the breaks and labels for the x-axis
            date_breaks <- function(x) {
                seq(from = floor_date(min(df$data$StartDate, na.rm = TRUE), "month"), 
                    to = ceiling_date(max(df$data$EndDate, na.rm = TRUE), "month"), 
                    by = "month")
            }
            
            date_labels <- function(x) {
                format(x, "%b %Y")  # Format the date labels as "Jan 2024", "Feb 2024", etc.
            }
            
            ggplot(df$data, aes(x = StartDate, xend = EndDate, y = fct_rev(TaskLabel), yend = fct_rev(TaskLabel), color = Color)) +
                geom_segment(linewidth = 10) +
                scale_color_identity() +
                scale_x_date(breaks = date_breaks(), labels = date_labels, expand = c(0, 0), position = "top") +  # Add position = "top"
                labs(
                    title = input$chartTitle,
                    x = NULL,
                    y = NULL,
                ) +
                theme_minimal(base_size = 15) +
                theme(
                    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
                    axis.title.x.top = element_text(size = 16, face = "bold"), 
                    axis.text.x.top = element_text(size = 12, angle = 0, hjust = 0.5), 
                    axis.ticks.length.x.top = unit(0.3, "cm"), 
                    axis.ticks.x.top = element_line(color = "black"),  
                    axis.text.y = element_markdown(size = 12),
                    axis.ticks.y = element_blank(),
                    panel.grid.major.x = element_line(color = "gray80"),
                    panel.grid.minor.x = element_line(color = "gray90"),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    plot.margin = margin(20, 20, 20, 20),
                    legend.position = "none",
                    panel.background = element_rect(fill = "white", colour = "grey80"),
                    plot.background = element_rect(fill = "white", colour = NA)
                )
        }
    })
    
    observeEvent(input$btnOpenChart, {
        showModal(modalDialog(
            title = "Gantt Chart - Set the desired dates and dimensions then download!",
            ganttChartUI("ganttChartModule"),
            easyClose = TRUE,
            size = "l",
            footer = NULL
        ))
        jqui_draggable("#shiny-modal .modal-dialog")  # Make the modal draggable
        jqui_resizable("#shiny-modal .modal-content")  # Make the modal resizable
    })
    
    ganttChartServer("ganttChartModule", reactive(df$data))
}

shinyApp(ui = ui, server = server)

