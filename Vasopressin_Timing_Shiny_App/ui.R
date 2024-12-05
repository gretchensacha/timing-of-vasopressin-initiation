#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for the Shiny app - User interface, what the end user sees
ui <- navbarPage("Vasopressin Data Visualizations",
                 
                 # Top bar with a dropdown menu
                 tabPanel("Main View", 
                          fluidPage(
                            # Dropdown menu at the top of the page
                            fluidRow(
                              column(12, 
                                     selectInput("var_choice", "Select Variable of Interest:",
                                                 choices = list(
                                                   "Norepinephrine-equivalent dose" = "NEQ",
                                                   "Lactate concentration" = "Lactate",
                                                   "Shock Duration" = "ShockDuration"
                                                 ),
                                                 selected = "NEQ")
                              )
                            ),
                            
                            # Main panel for displaying the plots
                            fluidRow(
                              column(6, plotlyOutput("figure1")),
                              column(6, plotlyOutput("figure2"))
                            ),
                            fluidRow(
                              column(6, plotlyOutput("figure3a")),
                              column(6, plotlyOutput("figure3b"))
                            )
                          )
                 )
)
