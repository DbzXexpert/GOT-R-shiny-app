#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#


library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)


#set imgs
top_left <- "https://images.unsplash.com/photo-1495834041987-92052c2f2865?ixlib=rb-0.3.5&ixid=eyJhcHBfaWQiOjEyMDd9&s=3d771d2cc226047515072dba7a5f03bc&auto=format&fit=crop&w=1050&q=80"
top_right <- "https://images.unsplash.com/photo-1494088391210-792bbadb00f4?ixlib=rb-0.3.5&ixid=eyJhcHBfaWQiOjEyMDd9&s=a421613e91c8475243ad4630171f4374&auto=format&fit=crop&w=1050&q=80"
bottom_left <- "https://images.unsplash.com/photo-1526411061437-7a7d51ec44c8?ixlib=rb-0.3.5&ixid=eyJhcHBfaWQiOjEyMDd9&s=e507916666b43919185fb16cf4e71813&auto=format&fit=crop&w=1050&q=80"

bottom_right <- "https://images.unsplash.com/photo-1525869916826-972885c91c1e?ixlib=rb-0.3.5&ixid=eyJhcHBfaWQiOjEyMDd9&s=f7cce16b11befb3dc6ed56074727b7b6&auto=format&fit=crop&w=1050&q=80"



###got data
characters = read_csv("data/characters.csv")
episodes = read_csv("data/episodes.csv")
scenes = read_csv("data/scenes.csv")
appearances = read_csv("data/appearances.csv")

###houses
main_char= c("Jon Snow", "Tyrion Lannister","Daenerys Targaryen","Sansa Stark","Cersei Lannister","Arya Stark")
stark_char = c("Eddard Stark", "Jon Snow", "Arya Stark", "Bran Stark", "Sansa Stark", "Benjen Stark")
lannister_char = c("Cersei Lannister", "Tyrion Lannister", "Jaime Lannister", "Tywin Lannister")
targaryen_char = c("Daenerys Targaryen", "Viserys Targaryen", "Drogon")
greyjoy_char = c("Aeron Greyjoy", "Balon Greyjoy", "Euron Greyjoy", "Theon Greyjoy", "Yara Greyjoy")

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    
    ##-- Logo ----
    list(tags$head(HTML('<link rel="icon", 
                        href="data/got_logo.png",
                        type="image/png" />')), 
                  tags$link(href="/app.css", rel="stylesheet", type="text/css")),
    
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title="", windowTitle = "GAME OF THRONES"
        )
    ),
    setBackgroundColor(
      color = "#1e1e1e",
      gradient = c("linear", "radial"),
      direction = c("bottom", "top", "right", "left"),
      shinydashboard = FALSE
    ),
    
    setBackgroundImage(
      src = "http://assets.viewers-guide.hbo.com/xlarge5c9921f0eefa4@2x.jpg"
    ),
    ##-- Header ----
    navbarPage(title = div(img(src="http://viewers-guide.hbo.com/images/original531de918067e7.png",
                               height = "32px", width="100%"), style = "padding-right: 100px;padding-top: 4px;"),
               tags$head(
                 tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:4px; 
                            padding-bottom:0;
                            height: 50px;
                 }
                            .navbar {min-height:25px;}'))
               ),
               id = "navbar",
               selected = "Home",
               fluid = T,
               inverse = TRUE,
               ##-- tabs ----
               ## home
               tabPanel("Home", 
                        fluidRow(style = "height :100px"),
                        #img(src = "http://assets.viewers-guide.hbo.com/xlarge5c9921f0eefa4@2x.jpg", height="50%",width="100%"),
                        fluidRow(
                          column(width = 6, offset = 3, 
                                 wellPanel(style="background-color: white; border-radius: 10px; box-shadow: 5px 5px 10px;",
                                           tags$h1("This is a shiny app for visualizing game of thrones data", align = "center"),
                                           tags$br(),
                                           tags$p("The app was created by :"),
                                           tags$ul(
                                          tags$li("Yassir")),
                                           tags$br(),
                                           tags$p("The tabular data used was collected by Jeffrey Lancaster and can be found in ",
                                                  tags$a("this project", href = "https://github.com/jeffreylancaster/game-of-thrones"),
                                                  ".The spatial data was created by ESRI and is available ",
                                                  tags$a("here", href = "https://www.arcgis.com/home/item.html?id=43d03779288048bfb5d3c46e4bc4ccb0?"),
                                                  "."
                                                  )
                                           )
                                 )
                        )),
               
              
               ##Appearance
               tabPanel("Appearance",
                        wellPanel(width = 12,
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("name", "name:" ,c(characters$name), selected = "Jon Snow"),
                              selectInput("numsaison","Season:" ,c("All seasons", episodes$seasonNum)),),
                            
                            mainPanel(
                              plotOutput("appearancePlot"),
                            ))),
                        tags$br(), 
                        mainPanel( width = 12, wellPanel (align = "center",
                                                          tags$h4("Cumulative appearance time per character and season"), 
                                                          plotOutput("appearencePSPlot", height = 400, width = "100%"))),
               ),
               ##Death
               tabPanel("Death", mainPanel(width = 12, wellPanel(align = "center",
                                                                 tags$h4("Evolution of the number of deaths over time"),
                                                                 plotOutput("deathPlot")))),
               ## map
               tabPanel("Map", 
                        sidebarLayout(
                          sidebarPanel(
                            width = 4,
                            style = "heiht: 800px",
                            titlePanel("Desired Program Characteristics"),
                            radioButtons("houses", 
                                         "Select a house:",
                                         c("Main characters" = "main_char",
                                           "Targaryen of King's Landing" = "targaryen_char", 
                                           "Stark of Winterfell" = "stark_char", 
                                           "Lannister of Casterly Rock" = "lannister_char",
                                           "Greyjoy of Pyke" = "greyjoy_char")),
                            
                            # Only show this panel if main char is selected
                            conditionalPanel( condition = "input.houses == 'main_char'",
                                              checkboxGroupInput(inputId = "members_main",
                                                                 label = "Select the members:",
                                                                 choices = main_char)),
                            # Only show this panel if main char is selected
                            conditionalPanel( condition = "input.houses == 'targaryen_char'",
                                              checkboxGroupInput(inputId = "members_targ",
                                                                 label = "Select members of the house:",
                                                                 choices = targaryen_char)
                                              ),
                            # Only show this panel if main char is selected
                            conditionalPanel( condition = "input.houses == 'stark_char'",
                                              checkboxGroupInput(inputId = "members_stark",
                                                                 label = "Select members of the house:",
                                                                 choices = stark_char)
                            ),
                            # Only show this panel if main char is selected
                            conditionalPanel( condition = "input.houses == 'lannister_char'",
                                              checkboxGroupInput(inputId = "members_lann",
                                                                 label = "Select members of the house:",
                                                                 choices = lannister_char)
                            ),
                            # Only show this panel if main char is selected
                            conditionalPanel( condition = "input.houses == 'greyjoy_char'",
                                              checkboxGroupInput(inputId = "members_grey",
                                                                 label = "Select members of the house:",
                                                                 choices = greyjoy_char)
                            )
                            
                          ),
                          mainPanel(wellPanel("", style = "background: white", plotOutput("mapGOT", width = "100%",height = "800px")))
                        ))
    )
    )
  )
