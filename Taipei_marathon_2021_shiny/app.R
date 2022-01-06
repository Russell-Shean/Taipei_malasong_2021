#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

#load("./data/taipei_combined_results.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Taipei Marathon Results"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("bibnmbr",
                        "Enter a Bib Number:",
                        value = 22579),
            textOutput("runner_info"),
            textOutput("runner_info2"),
            textOutput("runner_info3"),       #no reason I can't do this in one line of code :,/ ???
            textOutput("runner_info4"),
            textOutput("runner_info5"),
            textOutput("runner_info6"),
            textOutput("runner_info7"),
            textOutput("runner_info8")
        ),


        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("histPlot")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$histPlot <- renderPlot({

        ggplot(taipei_combined_results[taipei_combined_results$course_length=="Marathon",])+
            geom_density(aes(x=min_per_mile_net), color="red", fill="red", alpha=0.3)+
            geom_density(data=taipei_combined_results[taipei_combined_results$course_length=="Half Marathon",], aes(x=min_per_mile_race), color="blue", fill="blue", alpha=0.3)+
            geom_vline(xintercept = as.numeric(taipei_combined_results[taipei_combined_results$Bib_number==as.character(input$bibnmbr),"min_per_mile_net"]), color="red")+
            geom_vline(xintercept = as.numeric(taipei_combined_results[taipei_combined_results$Bib_number==as.character(input$bibnmbr),"min_per_mile_race"]), color="blue")
        
        
        
        
        
    })
    
    # for examples of how you SHOULDNT format your code, please refer to the following:
    #IneedtolearnHTML
    #AndorfigureouthowtodothisinShiny
    
    output$runner_info <- renderText({paste("Name: ",taipei_combined_results[taipei_combined_results$Bib_number==as.character(input$bibnmbr),"name"])})
    output$runner_info2 <- renderText({paste("Net time: ",taipei_combined_results[taipei_combined_results$Bib_number==as.character(input$bibnmbr),"net_time"])})
    output$runner_info3 <- renderText({paste("race time: ",taipei_combined_results[taipei_combined_results$Bib_number==as.character(input$bibnmbr),"race_time"])})
    output$runner_info4 <- renderText({paste0("net pace: ",taipei_combined_results[taipei_combined_results$Bib_number==as.character(input$bibnmbr),"min_per_mile_net_m"]," min ",taipei_combined_results[taipei_combined_results$Bib_number==as.character(input$bibnmbr),"min_per_mile_net_s"]," sec /mile")})
    output$runner_info5 <- renderText({paste0("race pace: ",taipei_combined_results[taipei_combined_results$Bib_number==as.character(input$bibnmbr),"min_per_mile_race_m"]," min ",taipei_combined_results[taipei_combined_results$Bib_number==as.character(input$bibnmbr),"min_per_mile_race_s"]," sec /mile")})
    output$runner_info6 <- renderText({paste("Race type: ",taipei_combined_results[taipei_combined_results$Bib_number==as.character(input$bibnmbr),"course_length"])})
    output$runner_info7 <- renderText({paste("Race Rank： ",taipei_combined_results[taipei_combined_results$Bib_number==as.character(input$bibnmbr),"race_rank"])})
    output$runner_info8 <- renderText({paste("Net Rank： ",taipei_combined_results[taipei_combined_results$Bib_number==as.character(input$bibnmbr),"net_rank"])})
                                           
    

}

# Run the application 
shinyApp(ui = ui, server = server)
