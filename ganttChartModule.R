library(shiny)
library(ggplot2)
library(forcats)
library(stringr)
library(ggtext)
library(lubridate)
library(dplyr)

ganttChartUI <- function(id) {
    ns <- NS(id)
    tagList(
        fluidPage(
            div(class = "form-well",
                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        textInput(ns("chartTitle"), "Chart Title:", placeholder = "e.g., Project Gantt Chart"),
                        dateRangeInput(ns("dateRange"), "Date Range:", start = Sys.Date(), end = Sys.Date() + 365),
                        selectInput(ns("timeFormat"), "Time Format:", choices = c("Year-Month" = "yearmonth", "Year" = "year", "Month" = "month")),
                        numericInput(ns("chartHeight"), "Chart Height (px):", value = 600, min = 100),
                        numericInput(ns("chartWidth"), "Chart Width (px):", value = 800, min = 100),
                        actionButton(ns("btnUpdateChart"), "Update Chart"),
                        downloadButton(ns("downloadPlot"), "Download Chart")
                    ),
                    mainPanel(
                        width = 9,
                        div(style = "overflow-x: auto; overflow-y: auto; max-width: 100%; max-height: 80vh;",
                            plotOutput(ns("ganttPlot"), height = "600px", width = "100%")
                        )
                    )
                )
            )
        )
    )
}

ganttChartServer <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        filteredData <- reactive({
            req(input$dateRange)
            data() %>%
                filter(StartDate >= input$dateRange[1] & EndDate <= input$dateRange[2])
        })
        
        date_breaks <- reactive({
            seq(from = floor_date(min(filteredData()$StartDate, na.rm = TRUE), "month"), 
                to = ceiling_date(max(filteredData()$EndDate, na.rm = TRUE), "month"), 
                by = "month")
        })
        
        date_labels <- reactive({
            function(x) {
                if (input$timeFormat == "yearmonth") {
                    format(x, "%b %Y")
                } else if (input$timeFormat == "year") {
                    format(x, "%Y")
                } else {
                    format(x, "%b")
                }
            }
        })
        
        output$ganttPlot <- renderPlot({
            req(input$chartHeight, input$chartWidth)
            df <- filteredData()
            if (nrow(df) == 0) {
                return(NULL)
            }
            
            df <- df %>%
                mutate(TaskLabel = as.character(TaskLabel)) %>%
                mutate(TaskLabel = if_else(Type == "Main", paste0("<b>", TaskLabel, "</b>"), TaskLabel)) %>%
                mutate(TaskLabel = factor(TaskLabel, levels = unique(TaskLabel)))
            
            ggplot(df, aes(x = StartDate, xend = EndDate, y = fct_rev(TaskLabel), yend = fct_rev(TaskLabel), color = Color)) +
                geom_segment(linewidth = 8, alpha = 0.9) +
                scale_color_identity() +
                scale_x_date(breaks = date_breaks(), labels = date_labels(), expand = c(0, 0), position = "top") +
                labs(
                    title = input$chartTitle,
                    x = NULL,
                    y = NULL
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
        }, height = reactive(input$chartHeight), width = reactive(input$chartWidth))
        
        output$downloadPlot <- downloadHandler(
            filename = function() {
                paste("gantt_chart_", Sys.Date(), ".png", sep = "")
            },
            content = function(file) {
                df <- filteredData()
                print("Saving plot to file:")
                print(file)
                p <- ggplot(df, aes(x = StartDate, xend = EndDate, y = fct_rev(TaskLabel), yend = fct_rev(TaskLabel), color = Color)) +
                    geom_segment(linewidth = 8, alpha = 0.9) +
                    scale_color_identity() +
                    scale_x_date(breaks = date_breaks(), labels = date_labels(), expand = c(0, 0), position = "top") +
                    labs(
                        title = input$chartTitle,
                        x = NULL,
                        y = NULL
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
                
                ggsave(file, plot = p, device = "png", width = input$chartWidth / 96, height = input$chartHeight / 96, dpi = 96)
            },
            contentType = "image/png"
        )
    })
}




shinyApp(ui, server)
