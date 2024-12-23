library(shiny)
library(shinyjs)
library(DT)  # For interactive tables

ui <- fluidPage(
    useShinyjs(),
    titlePanel("Breeding Cubicle Manager"),
    
    sidebarLayout(
        sidebarPanel(
            # Upload crossing plan
            fileInput("crossing_plan", "Upload Crossing Plan (CSV)",
                     accept = c("text/csv", ".csv")),
            
            # Create cubicle button
            actionButton("create_cubicle", "Create New Cubicle"),
            
            # Other existing buttons
            actionButton("print_layout", "Print Layout"),
            downloadButton("save_data", "Save Layout"),
            fileInput("load_data", "Load Layout"),
            
            hr(),
            
            # Modified instructions
            helpText("1. Upload your crossing plan"),
            helpText("2. Select crosses with the same male"),
            helpText("3. Click 'Create New Cubicle' to group them"),
            helpText("4. The ratio will turn red if it exceeds 3:1"),
            
            dateInput("planting_date", "Planting Date:", value = Sys.Date()),
            dateInput("expected_flowering", "Expected Flowering:", value = Sys.Date() + 60)
        ),
        
        mainPanel(
            # Crossing plan table
            h4("Available Crosses"),
            DTOutput("crossing_table"),
            
            hr(),
            
            # Container for dynamically created cubicles
            uiOutput("cubicles_container"),
            
            fluidRow(
                column(12,
                    h3("Summary Statistics"),
                    verbatimTextOutput("statistics")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    # Store crossing plan data
    crossing_plan <- reactiveVal(NULL)
    
    # Read crossing plan
    observeEvent(input$crossing_plan, {
        req(input$crossing_plan)
        plan_data <- read.csv(input$crossing_plan$datapath)
        crossing_plan(plan_data)
    })
    
    # Display crossing plan table with selection
    output$crossing_table <- renderDT({
        req(crossing_plan())
        datatable(crossing_plan(),
                  selection = 'multiple',
                  options = list(pageLength = 5))
    })
    
    # Modified cubicle creation to use selected crosses
    observeEvent(input$create_cubicle, {
        req(crossing_plan())
        selected_rows <- input$crossing_table_rows_selected
        
        if (length(selected_rows) == 0) {
            showNotification("Please select crosses first", type = "error")
            return()
        }
        
        selected_crosses <- crossing_plan()[selected_rows, ]
        unique_males <- unique(selected_crosses$male)
        
        if (length(unique_males) > 1) {
            showNotification("All crosses in a cubicle must share the same male", type = "error")
            return()
        }
        
        # Create new cubicle with selected crosses
        current_cubicles <- cubicles()
        new_cubicle_id <- length(current_cubicles) + 1
        
        new_cubicle <- list(
            id = new_cubicle_id,
            male = unique_males[1],
            crosses = selected_crosses,
            notes = ""
        )
        
        current_cubicles[[new_cubicle_id]] <- new_cubicle
        cubicles(current_cubicles)
    })
    
    # Store cubicle data
    cubicles <- reactiveVal(list())
    
    # Render cubicles
    output$cubicles_container <- renderUI({
        current_cubicles <- cubicles()
        
        if (length(current_cubicles) == 0) {
            return(h4("No cubicles created yet"))
        }
        
        cubicle_elements <- lapply(current_cubicles, function(cubicle) {
            cubicle_id <- cubicle$id
            
            div(
                class = "well",
                style = "margin: 10px 0;",
                
                # Cubicle header
                h4(paste("Cubicle", cubicle_id)),
                
                # Male cultivar display
                p(strong("Male: "), cubicle$male),
                
                # Ratio display
                uiOutput(paste0("ratio_", cubicle_id)),
                
                textAreaInput(paste0("notes_", cubicle_id), 
                             "Notes:", 
                             value = cubicle$notes),
                
                actionButton(paste0("delete_", cubicle_id), "Delete Cubicle", 
                             class = "btn-danger"),
                
                div(
                    class = "date-info",
                    p(strong("Planting Date: "), format(input$planting_date, "%B %d, %Y")),
                    p(strong("Expected Flowering: "), format(input$expected_flowering, "%B %d, %Y"))
                )
            )
        })
        
        do.call(tagList, cubicle_elements)
    })
    
    # Handle ratio displays for each cubicle
    observe({
        current_cubicles <- cubicles()
        
        for (cubicle in current_cubicles) {
            local({
                cubicle_id <- cubicle$id
                
                output[[paste0("ratio_", cubicle_id)]] <- renderUI({
                    female_count <- input[[paste0("female_count_", cubicle_id)]]
                    ratio <- female_count / 1  # 1 male
                    
                    ratio_color <- if (ratio > 3) "red" else "black"
                    
                    p(
                        style = paste0("color: ", ratio_color, ";"),
                        sprintf("Female to Male Ratio: %.1f:1", ratio)
                    )
                })
            })
        }
    })
    
    # Handle print button
    observeEvent(input$print_layout, {
        # Create a formatted layout for printing
        layout_html <- div(
            h2("Breeding Cubicle Layout"),
            p("Generated on: ", format(Sys.time(), "%B %d, %Y")),
            hr(),
            lapply(cubicles(), function(cubicle) {
                div(
                    style = "border: 1px solid black; padding: 10px; margin: 10px 0;",
                    h4(paste("Cubicle", cubicle$id)),
                    p(strong("Male: "), cubicle$male),
                    p(strong("Females: "), cubicle$female_count),
                    p(strong("Notes: "), input[[paste0("notes_", cubicle$id)]])
                )
            })
        )
        
        # Open in new window for printing
        showModal(modalDialog(
            layout_html,
            footer = tagList(
                actionButton("print_now", "Print"),
                modalButton("Close")
            )
        ))
    })
    
    observeEvent(input$print_now, {
        runjs("window.print();")
    })
    
    observe({
        current_cubicles <- cubicles()
        lapply(current_cubicles, function(cubicle) {
            observeEvent(input[[paste0("delete_", cubicle$id)]], {
                current_cubicles <- cubicles()
                current_cubicles[[cubicle$id]] <- NULL
                cubicles(current_cubicles)
            })
        })
    })
    
    output$save_data <- downloadHandler(
        filename = function() {
            paste0("cubicle_layout_", Sys.Date(), ".rds")
        },
        content = function(file) {
            saveRDS(cubicles(), file)
        }
    )
    
    observeEvent(input$load_data, {
        req(input$load_data)
        loaded_cubicles <- readRDS(input$load_data$datapath)
        cubicles(loaded_cubicles)
    })
    
    output$statistics <- renderText({
        current_cubicles <- cubicles()
        if (length(current_cubicles) == 0) return("No data available")
        
        total_males <- length(current_cubicles)
        total_females <- sum(sapply(current_cubicles, function(x) {
            count <- input[[paste0("female_count_", x$id)]]
            if (is.null(count)) return(0)
            as.numeric(count)
        }))
        
        paste0(
            "Total Cubicles: ", length(current_cubicles), "\n",
            "Total Males: ", total_males, "\n",
            "Total Females: ", total_females, "\n",
            "Overall Ratio: ", round(total_females/total_males, 2), ":1"
        )
    })
    
    observe({
        current_cubicles <- cubicles()
        if (length(current_cubicles) > 0) {
            total_females <- sum(sapply(current_cubicles, function(x) {
                count <- input[[paste0("female_count_", x$id)]]
                if (is.null(count)) return(0)
                as.numeric(count)
            }))
            
            if (total_females > 100) {
                showNotification(
                    "Warning: Total female count exceeds 100", 
                    type = "warning",
                    duration = NULL
                )
            }
        }
    })
}

shinyApp(ui = ui, server = server)
