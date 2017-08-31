library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(dbplyr)
library(RMySQL)
library(leaflet)
library(DT)

dashboardPage(skin="red",
              # Header elements for the visualization
              dashboardHeader(title = "Malaria Analysis Project", disable = FALSE,
                              # Dropdown menu for notifications
                              
                              dropdownMenu(type = "notifications", badgeStatus = "info",
                                           notificationItem(icon = icon("users"), status = "info",href = "blog",
                                                            "some new posts unread today."
                                           ),
                                           notificationItem(icon = icon("warning"), status = "danger",
                                                            "Ugandans!!! Malaria is for real"
                                           ),
                                           notificationItem(icon = icon("shopping-cart", lib = "glyphicon"),
                                                            status = "success", "Thank you for your contribution"
                                           ),
                                           notificationItem(icon = icon("user", lib = "glyphicon"),
                                                            status = "danger", "Dont hesitate to give us your feedback."
                                           )
                              )
              ),
              
              # Sidebar elements for the search visualizations
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Dashboard", tabName = "dashboard"),
                  menuItem("Districts", tabName = "districts"),
                  menuItem(text = "Graphs",
                           menuSubItem(text = "Age groups", tabName = "age"),
                           menuSubItem(text = "population density", tabName = "population"),
                           menuSubItem(text = "Gender", tabName = "gender")
                           
                  ),
                  menuItem("Statistics", tabName = "statistics"),
                  menuItem("Feedback", tabName = "blog"),
                  menuItem("Upload", tabName = "upload")
                  
                ) # /sidebarMenu
              ), # /dashboardSidebar
              
              #Body elements for the search visualizations.
              dashboardBody(
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                ),
                tabItems(
                  tabItem(tabName = "dashboard",
                          fluidRow(
                            box( width = "12%",
                                 tabsetPanel(type = "tab",
                                             tabPanel("About MAP", h2("The Malaria Analysis Project Web Application"),
                                                      ("The MAP application is an application used for malaria analysis in Uganda's central region, that comprises of districts 
                                                       like Kampala, Mubende, Lwengo among others. The analysis is based on sex, age groups and population size."), 
                                                      
                                                      ("Malaria is a life-threatening blood disease caused by parasites transmitted to humans through the bite of an anopheles'
                                                       mosquito.  
                                                       Malaria continues to be a leading cause of morbidity and mortality in many tropical regions of the world, despite global efforts
                                                       to eradicate the disease. While the disease is easily preventable, curable and treatable, it remains a big health threat to many
                                                       communities all over the world, most especially in Sub-Saharan Africa. Although there have been advances in terms of new drugs and
                                                       vaccines, eradication is still a way off and many health strategies now focus on malaria prevention and control.
                                                       
                                                       The Uganda Ministry of Health (MoH) has comprehensively reviewed the malaria program for 10 years and in its Malaria Program
                                                       Review Report (MPR) one of the gaps identified in the MPR is the abstract presentation
                                                       of analytical data that is in a generalized format, making the results of decision making towards malaria prevention policies to 
                                                       be inclined to Uganda as a whole country and not specialized to highly affected regions.
                                                       
                                                       The central region presents a typical case study of a community grappling with the challenge of high morbidity due to malaria.  
                                                       People infected from the various districts of the region confirmed the high prevalence of malaria in the region. Unfortunately,
                                                       the information available to the stake holders is not strongly structured to support strategic decision making in policies 
                                                       concerning malaria prevention and control within the region.
                                                       We therefore present to you the MAP software which provides a better information source.
                                                       "),
                                                      uiOutput("image1")
                                                      ),
                                             
                                             tabPanel("Single Search",
                                                      uiOutput("single_search"),
                                                      #htmlOutput('searchtype'),
                                                      
                                                      plotOutput("sea"),
                                                      downloadButton('downloadPlot18', 'Download Plot as an image'),
                                                      downloadButton('downloadPlot19', 'Download Plot as a pdf'),
                                                      
                                                      plotOutput("searchplot2"),
                                                      plotOutput("searchplot3"),
                                                      
                                                      downloadButton('downloadPlot20', 'Download Plot as an image'),
                                                      downloadButton('downloadPlot21', 'Download Plot as a pdf')
                                                      
                                                      
                                             ),
                                             tabPanel("Double Search",
                                                      h2("Comparision of two districts"),
                                                      uiOutput("double_search"),
                                                      
                                                      uiOutput('dataTable'),
                                                      downloadButton('downloadPlot22', 'Download Plot as an image'),
                                                      downloadButton('downloadPlot23', 'Download Plot as a pdf')
                                                      
                                                      
                                             ),
                                             tabPanel("Map of Uganda", h2("Central region Uganda"),
                                                      column(width = 9,
                                                             box(width = NULL, solidHeader = TRUE,
                                                                 leafletOutput("CentralMap", height=500)
                                                             )
                                                             
                                                      )
                                             ),
                                             tabPanel("Help", h2("Help and FAQS platform"),
                                                      box(background = "olive", title = "Search platform",
                                                          textOutput("help1")
                                                          
                                                      ),
                                                      box(background = "maroon", title = "Feedback platform",
                                                          textOutput("help3")
                                                          
                                                      ),
                                                      box(background = "teal", title = "Upload platform",
                                                          textOutput("help2")
                                                          
                                                      )
                                             )
                                             
                                                      )
                                             ))
                          ),
                  tabItem(tabName = "statistics",
                          fluidRow(
                            box(background = "black", width = "12%",
                                h2("Generic graphs for malaria prevelance in the central region"),
                                
                                plotOutput("main1"),
                                
                                downloadButton('downloadPlot', 'Download Plot as an image'),
                                downloadButton('downloadPlot1', 'Download Plot as a pdf'),
                                
                                plotOutput("main3"),
                                
                                downloadButton('downloadPlot4', 'Download Plot as an image'),
                                downloadButton('downloadPlot5', 'Download Plot as a pdf'),
                                
                               
                                plotOutput("line2"),
                                downloadButton('downloadPlot12', 'Download Plot as an image'),
                                downloadButton('downloadPlot13', 'Download Plot as a pdf')
                                
                            )
                          )
                          
                  ),
                  tabItem(tabName = "districts",
                          fluidRow(
                            box(background = "black", width = "12%",
                                h2("Currently uploaded districts"),
                                tableOutput("districts")
                                
                            )
                          )
                          
                  ),
                  tabItem(tabName = "age",
                          fluidRow(
                            box(background = "black", width = "12%",
                                h2("Age Group Based Analysis"),
                                plotOutput("pyramidplot"),
                                downloadButton('downloadPlot24', 'Download Plot as an image'),
                                downloadButton('downloadPlot25', 'Download Plot as a pdf'),
                                
                                plotOutput("line1"),
                                downloadButton('downloadPlot10', 'Download Plot as an image'),
                                downloadButton('downloadPlot11', 'Download Plot as a pdf')
                                
                                
                                
                            )
                          )
                          
                  ),
                  tabItem(tabName = "gender",
                          fluidRow(
                            box(background = "black", width = "12%",
                                h2("Gender Based Analysis"),
                                box(
                                  title= "Total number of male malaria patients", background = "red", solidHeader = TRUE,
                                  width = 5,
                                  plotOutput("box1"),
                                  downloadButton('downloadPlot6', 'Download Plot as an image'),
                                  downloadButton('downloadPlot7', 'Download Plot as a pdf')
                                  
                                ),
                                
                                box(title= "Total number of female malaria patients", background = "yellow", solidHeader = TRUE,
                                    width = 5,
                                    plotOutput("box2"),
                                    downloadButton('downloadPlot8', 'Download Plot as an image'),
                                    downloadButton('downloadPlot9', 'Download Plot as a pdf')
                                    
                                ),
                                plotOutput("line3"),
                                downloadButton('downloadPlot14', 'Download Plot as an image'),
                                downloadButton('downloadPlot15', 'Download Plot as a pdf')
                                
                            )
                          ) 
                  ),
                  tabItem(tabName = "population",
                          fluidRow(
                            box(background = "black", width = "12%",
                                h2("Population Based Analysis"),
                                box(
                                  title= "Malaria prevalence over land area", background = "red", solidHeader = TRUE,
                                  width = 5,
                                  plotOutput("scatter1"),
                                  downloadButton('downloadPlot26', 'Download Plot as an image'),
                                  downloadButton('downloadPlot27', 'Download Plot as a pdf')
                                  
                                ),
                                
                                box(title= "Malaria prevalence over population density", background = "yellow", solidHeader = TRUE,
                                    width = 5,
                                    plotOutput("scatter2"),
                                    downloadButton('downloadPlot28', 'Download Plot as an image'),
                                    downloadButton('downloadPlot29', 'Download Plot as a pdf')
                                    
                                ),
                                
                                plotOutput("line4"),
                                downloadButton('downloadPlot16', 'Download Plot as an image'),
                                downloadButton('downloadPlot17', 'Download Plot as a pdf')
                                
                                
                                
                            )
                          ) 
                  ),
                  tabItem(tabName = "blog",
                          fluidRow(
                            
                            h2("Feedback platform"),
                            
                            uiOutput("feedback"),
                            uiOutput("feeds"),
                            uiOutput("blogs")
                            
                          )
                          
                  ),
                  tabItem(tabName = "upload", 
                          fluidRow(
                            
                            box(width = "12%",
                                
                                uiOutput("uiLogin"),          
                                textOutput("pass"),
                                tags$head(tags$style("#pass{color: red;")),   
                                uiOutput("userPanel")
                                
                                
                                
                            )
                          )
                  )
                  
                  
                  )
                ) # /dashboardBody
              
              #dashboardPage(header, sidebar, body, skin = "red")
              )
