#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(magrittr)
library(tidyverse)

fields <- c("gene","category","comment","pmid")

listcategory <- read_rds("data/listofcategories.Rds")
init_table <- read_rds("data/annotation_initialTable.Rds")
responses <- read_csv("data/annotations_tableRBP.csv")
listgenes <- anti_join(init_table, responses, by=c("gene", "category")) %$% unique(gene)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("simplex"),
                navbarPage("Genes annotations",
                           tabPanel("Summary"),
                           tabPanel("Input",

                                    fluidRow(
                                      column(1,h4("Input"), "-----"),
                             column(2, selectizeInput("gene","Gene: ",
                                                      choices = listgenes,
                                                      options = list(create = TRUE)),
                                    uiOutput("category")),
                             column(5, textAreaInput("comment", "comments: ", value = "", width = '100%', rows = 5, resize = "both")),
                             column(2,textInput("pmid","reference (PMID): "),
                                    actionButton("save","Add"))
                           ),
                           mainPanel(
                             fluidRow(
                               column(12, align="center",
                                      DT::dataTableOutput("responses")
                               )
                             ),
                             downloadButton("downloadData", "Download")
                           ))
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$category <- renderUI({
     selectizeInput('category', 'Category: ', choices = setdiff(filter(init_table, gene==input$gene) %$% category, filter(responses, gene==input$gene) %$% category),
                    options = list(create = TRUE), multiple=F
     )
   })


#create a data frame called responses
saveData <- function(data) {

 data <- as.data.frame(t(data)) %>%
   mutate(date = Sys.time())

if (exists("responses")) {
 responses <<- rbind(responses, data)
 } else {
responses <<- data
}
}

loadData <- function() {
 if (exists("responses")) {
    responses
  }
}
   # Whenever a field is filled, aggregate all form data
   #formData is a reactive function
   formData <- reactive({
   data <- sapply(fields, function(x) input[[x]])
   data
   })
   # When the Save button is clicked, save the form data
   observeEvent(input$save, {
     saveData(formData())
   })
   # Show the previous responses
   # (update with current response when save is clicked)
   output$responses <- DT::renderDataTable({
     input$save
     loadData()
   })

#DOWNLOAD DATA INPUT
   output$downloadData <- downloadHandler(
     filename = function() {
       paste("test.csv", sep = "")
     },
     content = function(file) {
       write_csv(loadData(), file)
     }
   )
}

# Run the application
shinyApp(ui = ui, server = server)

