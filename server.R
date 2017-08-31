library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(dbplyr)
library(RMySQL)
library(reshape2)
library(maps)
library(mapdata)
library(ggmap)
library(RColorBrewer)
library(plotrix)
library(tidyr)
library(leaflet)
library(rgdal)
library(DT)
#library(gridExtra)
#library(XML)
boundaries<-readOGR(dsn="Data/statistical-gis-boundary-files/boundary2", layer="District_Boundaries_2014")

# Cut out unnecessary columns
boundaries@data<-boundaries@data[,c(2,4)]

# transform to WGS884 reference system 
boundaries<-spTransform(boundaries, CRS("+init=epsg:4326"))
boundaries <- subset(boundaries, REGION == "CENTRAL REGION")

boundaries@data$DNAME2014 <- as.character(boundaries@data$DNAME2014)
boundaries@data$REGION <- as.character(boundaries@data$REGION)
# Find the edges of our map
bnds<-bbox(boundaries)
bnds


# Get the income data 
districtPoln<-read.csv("Data/mapdata.csv", header = TRUE, stringsAsFactors = FALSE)
districtPoln

function(input, output, session) {
  
  ##################################
  #    DATABASE CONNECTION         #
  ##################################
  
  mydb = dbConnect(MySQL(), user = 'root', password = '', host = 'localhost')
  
  #selecting the database for use
  dbSendQuery(mydb, "USE malaria_analysis")
  
  #reconnecting to the database
  malariadb = dbConnect(MySQL(), user = 'root', password = '', host = 'localhost', dbname = "malaria_analysis")
  
  ##############################################################MAPS SECTION############################################################################################### 
  library(dplyr)
  library(tidyr)
  library(leaflet)
  library(rgdal)
  library(DT)
  getDataSet<-reactive({
    
    # Get a subset of the income data which is contingent on the input variables
    #dataSet<-income_long[income_long$Year==input$dataYear & income_long$Measure==input$meas,]
    
    # Copy our GIS data
    joinedDataset<-boundaries
    
    # Join the two datasets together
    joinedDataset@data <- suppressWarnings(left_join(joinedDataset@data, districtPoln, by=c("DNAME2014"="districts")))
    
    # If input specifies, don't include data for City of London
    #if(input$city==FALSE){
    #joinedDataset@data[joinedDataset@data$NAME=="City of London",]$Income=NA
    #}
    
    joinedDataset
  })
  
  # Due to use of leafletProxy below, this should only be called once
  output$CentralMap<-renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      
      # Centre the map in the middle of our co-ordinates
      setView(mean(bnds[1,]),
              mean(bnds[2,]),
              zoom=8 # set to 10 as 9 is a bit too zoomed out
      )       
    
  })
  
  
  
  observe({
    theData<-getDataSet() 
    
    # colour palette mapped to data
    pal <- colorQuantile("Reds", theData$tpoln, n = 10)
    
    # set text for the clickable popup labels
    borough_popup <- paste0("<strong>District: </strong>", 
                            theData$DNAME2014, 
                            "<br><strong>", 
                            "population: </strong>", 
                            formatC(theData$tpoln, format="d", big.mark=',')
    )
    
    # If the data changes, the polygons are cleared and redrawn, however, the map (above) is not redrawn
    leafletProxy("CentralMap", data = theData) %>%
      clearShapes() %>%
      addPolygons(data = theData,
                  fillColor = pal(theData$tpoln), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 2,
                  popup = borough_popup)  
    
  })
  
  
  ##################################
  #    LOGIN SECTION               #
  ##################################
  USER <- reactiveValues(Logged = FALSE , session = session$user) 
  
  PASSWORD <- data.frame(
    Brukernavn = c("government"), 
    Passord = c("admin")
  )
  output$uiLogin <- renderUI({
    if (USER$Logged == FALSE) {
      wellPanel(
        textInput("userName", "User Name:"),
        passwordInput("passwd", "Pass word:"),
        br(),
        actionButton("Login", "Log in")
      )
    }
  })
  
  output$pass <- renderText({  
    if (USER$Logged == FALSE) {
      USER$pass
    }  
  })
  
  # Login info during session ----
  output$userPanel <- renderUI({
    if (USER$Logged == TRUE) {
      fluidRow(
        column(2,
               "User: ", USER$name
        ),
        column(1, actionLink("logout", "Logout"))
      )
      box(background = "black", width = "12%",
          tabsetPanel(type = "tab",
                      
                      tabPanel("Upload",h2("Upload files that contain malaria prevalence data"),
                               #upload data for a given district
                               fileInput("districtfile", "upload a .csv file containing age groups in a given district", accept = ".csv"),
                               #upload data for divisions
                               fileInput("divisionfile", "upload a .csv file containg sub divisions and population density data", accept = ".csv"),
                               helpText("The file should be .csv and maximum size is 10MB"),
                               tags$hr(),
                               checkboxInput('header', 'Header', TRUE),
                               radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),',')),
                      
                      tabPanel("Currently inserted",h2("This is the district data you have currently inserted"),
                               tableOutput("content1")
                               
                      ),
                      tabPanel("Currently inserted",h2("This is the sub county data you have currently inserted"),
                               tableOutput("content2")
                      )
          )
      )
    }  
  })
  
  # control login
  observeEvent(input$Login , {
    Username <- isolate(input$userName)
    Password <- isolate(input$passwd)
    Id.username <- which(PASSWORD$Brukernavn == Username)
    Id.password <- which(PASSWORD$Passord    == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        USER$Logged <- TRUE
        USER$name <- Username      
      } 
    } else {
      USER$pass <- "User name or password failed!"
    }
  })
  
  # control logout
  observeEvent(input$logout , {
    USER$Logged <- FALSE
    USER$pass <- ""
  })
  
  
  
  ##############################################################DISTRICTS SECTION############################################################################################### 
  
  #-------currently uploaded handle--------
  output$contents <- renderTable({
    inFile <- input$districtfile
    inFiles <- input$divisionfile
    if(is.null(inFile))
      return(
        dataz <- read.csv(inFiles$datapath,header = input$header, sep = input$sep)
      )
    ourDataz <- read.csv(inFile$datapath,header = input$header, sep = input$sep)
    
  })
  
  #-----read the uploaded districts------
  data <- reactive({
    if (USER$Logged == TRUE) {
      inFile <- input$districtfile
      if(is.null(inFile)){return()}
      else{
        #read.table(file = inFile$datapath, sep = input$sep)
        ourData <- read.csv(inFile$datapath,header = input$header, sep = input$sep)  
        
      }
    }
  })
  
  #---------insert the districts into the database---------
  output$content1 <- renderTable({
    if (USER$Logged == TRUE) {
      theData <- input$districtfile
      if(is.null(data())){return()}
      else{
        
        dis <- data.frame(data())
        #insert data into the database
        dbWriteTable(malariadb, "district", dis, append = TRUE, row.names = FALSE, overwrite = FALSE)
        
        
      }
      result1 <- read.csv(theData$datapath, header = input$header, sep = input$sep)
    }  
  })
  
  #---------------retrieve all distinct districts-----------------------
  output$districts <- renderTable({
    main_fetch3 <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
    main_total3 = aggregate(main_fetch3$Total, by=list(Category=main_fetch3$District), FUN = sum)
    main_total3
  })
  
  ##################################
  #        MAJOR STATISTICS        #
  ##################################
  
  #------------------density plot-------------------------
  output$main1 <- renderPlot({
    main_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
    main_total = aggregate(main_fetch$Total, by=list(Category=main_fetch$District), FUN = sum)
    dense_plot <- main_total$x
    plot(dense_plot, main = "Density plot of malaria prevalence in the central region", xlab = "Districts", ylab = "Density")
    polygon(dense_plot, col = "red", border = "blue")
    text(main_total$x, labels = main_total$Category, cex = 0.7)
    
    
  })
  ##################################
  #    DOWNLOADS(IMAGE OR PDF)     #
  ##################################
  
  #download the graph as image
  output$downloadplot <- downloadHandler(
    filename = function() {
      paste('density', 'png', sep = ".")
    },
    content = function(file) {
      png(file)
      
      #plot the graph
      main_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
      main_total = aggregate(main_fetch$Total, by=list(Category=main_fetch$District), FUN = sum)
      dense_plot <- main_total$x
      plot(dense_plot, main = "Density plot of malaria prevalence in the central region", xlab = "Districts", ylab = "Density")
      polygon(dense_plot, col = "red", border = "blue")
      text(main_total$x, labels = main_total$Category, cex = 0.7)
      
      dev.off()
      
    }
  )
  #download the graph as pdf
  output$downloadplot1 <- downloadHandler(
    filename = function() {
      paste('density', 'pdf', sep = ".")
    },
    content = function(file) {
      pdf(file)
      
      #plot the graph
      main_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
      main_total = aggregate(main_fetch$Total, by=list(Category=main_fetch$District), FUN = sum)
      dense_plot <- main_total$x
      plot(dense_plot, main = "Density plot of malaria prevalence in the central region", xlab = "Districts", ylab = "Density")
      polygon(dense_plot, col = "red", border = "blue")
      text(main_total$x, labels = main_total$Category, cex = 0.7)
      
      dev.off()
      
    }
  )
 
  #--------------bar plot-----------------
  output$main3 <- renderPlot({
    main_fetch3 <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
    main_total3 = aggregate(main_fetch3$Total, by=list(Category=main_fetch3$District), FUN = sum)
    main_total3
    #bar<-aggregate(main_total3~Category, main_total3, mean)
    barplot(main_total3[,2], names.arg = main_total3$Category,las=2,ylab = "Total",main = "Malaria Analysis graph",
            col=brewer.pal(5,"Dark2")
    )
    
  })
  
  ##################################
  #    DOWNLOADS(IMAGE OR PDF)     #
  ##################################
  
  #download the graph as image
  output$downloadplot4 <- downloadHandler(
    filename = function() {
      paste('bar1', 'png', sep = ".")
    },
    content = function(file) {
      png(file)
      
      #plot the graph
      main_fetch3 <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
      main_total3 = aggregate(main_fetch3$Total, by=list(Category=main_fetch3$District), FUN = sum)
      main_total3
      #bar<-aggregate(main_total3~Category, main_total3, mean)
      barplot(main_total3[,2], names.arg = main_total3$Category,las=2,ylab = "Total",main = "Malaria Analysis graph",
              col=brewer.pal(5,"Dark2")
      )
      dev.off()
      
    }
  )
  #download the graph as pdf
  output$downloadplot5 <- downloadHandler(
    filename = function() {
      paste('bar1', 'pdf', sep = ".")
    },
    content = function(file) {
      pdf(file)
      
      #plot the graph
      main_fetch3 <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
      main_total3 = aggregate(main_fetch3$Total, by=list(Category=main_fetch3$District), FUN = sum)
      main_total3
      #bar<-aggregate(main_total3~Category, main_total3, mean)
      barplot(main_total3[,2], names.arg = main_total3$Category,las=2,ylab = "Total",main = "Malaria Analysis graph",
              col=brewer.pal(5,"Dark2")
      )
      dev.off()
      
    }
  )
  
  ##################################
  #        BOX PLOts               #
  ##################################
  output$box1 <- renderPlot({
    #box_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
    #distinctAge = box_plot_fetch %>% distinct(Age_group)
    box_plot_fetch1 <- read.csv("C:/Users/swabra/Documents/ByAge.csv", header = TRUE)
    
    plot(box_plot_fetch1$Age_group, box_plot_fetch1$Male, xlab = "Age group",main = "Number of Males infected in a given age group", ylab = "Males")
    
  })
  ##################################
  #    DOWNLOADS(IMAGE OR PDF)     #
  ##################################
  
  #download the graph as image
  output$downloadplot6 <- downloadHandler(
    filename = function() {
      paste('box1', 'png', sep = ".")
    },
    content = function(file) {
      png(file)
      
      #plot the graph
      box_plot_fetch1 <- read.csv("C:/Users/swabra/Documents/ByAge.csv", header = TRUE)
      
      plot(box_plot_fetch1$Age_group, box_plot_fetch1$Male, xlab = "Age group",main = "Number of Males infected in a given age group", ylab = "Males")
      
      dev.off()
      
    }
  )
  #download the graph as pdf
  output$downloadplot7 <- downloadHandler(
    filename = function() {
      paste('box1', 'pdf', sep = ".")
    },
    content = function(file) {
      pdf(file)
      
      #plot the graph
      box_plot_fetch1 <- read.csv("C:/Users/swabra/Documents/ByAge.csv", header = TRUE)
      
      plot(box_plot_fetch1$Age_group, box_plot_fetch1$Male, xlab = "Age group",main = "Number of Males infected in a given age group", ylab = "Males")
      
      dev.off()
      
    }
  )
  
  output$box2 <- renderPlot({
    #box_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
    #distinctAge = box_plot_fetch %>% distinct(Age_group)
    box_plot_fetch2 <- read.csv("C:/Users/swabra/Documents/ByAge.csv", header = TRUE)
    
    plot(box_plot_fetch2$Age_group, box_plot_fetch2$Female, xlab = "Age group",main = "Number of Females infected in a given age group", ylab = "Females")
    
  })
  
  ##################################
  #    DOWNLOADS(IMAGE OR PDF)     #
  ##################################
  
  #download the graph as image
  output$downloadplot8 <- downloadHandler(
    filename = function() {
      paste('box2', 'png', sep = ".")
    },
    content = function(file) {
      png(file)
      
      #plot the graph
      box_plot_fetch2 <- read.csv("C:/Users/swabra/Documents/ByAge.csv", header = TRUE)
      
      plot(box_plot_fetch2$Age_group, box_plot_fetch2$Female, xlab = "Age group",main = "Number of Females infected in a given age group", ylab = "Females")
      
      dev.off()
      
    }
  )
  #download the graph as pdf
  output$downloadplot9 <- downloadHandler(
    filename = function() {
      paste('box2', 'pdf', sep = ".")
    },
    content = function(file) {
      pdf(file)
      
      #plot the graph
      box_plot_fetch2 <- read.csv("C:/Users/swabra/Documents/ByAge.csv", header = TRUE)
      
      plot(box_plot_fetch2$Age_group, box_plot_fetch2$Female, xlab = "Age group",main = "Number of Females infected in a given age group", ylab = "Females")
      
      dev.off()
      
    }
  )
  
  ##################################
  #        LINE GRAPHS             #
  ##################################
  output$line1 <- renderPlot({
    line1_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
    #calculate total by age group
    total_age_group = aggregate(line1_plot_fetch$Total, by=list(Category=line1_plot_fetch$Age_group), FUN = sum)
    affected_number = sum(total_age_group$x)
    percentages = total_age_group$x/affected_number * 100
    total_age_group = cbind(total_age_group, percentages)
    total_age_group
    #plot showing percentage per age group
    position=rep(3, length(total_age_group$Category))
    plot(total_age_group$percentages, type="o", col="blue", xlab = "Age Group", ylab = "Total Percentage", main = "A line graph showing the percentage of infected people in a given age group")
    text(total_age_group$percentages, labels = total_age_group$Category, cex = 0.7, pos = position)
    
  })
  
  ##################################
  #    DOWNLOADS(IMAGE OR PDF)     #
  ##################################
  
  #download the graph as image
  output$downloadplot10 <- downloadHandler(
    filename = function() {
      paste('line1', 'png', sep = ".")
    },
    content = function(file) {
      png(file)
      
      #plot the graph
      line1_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
      #calculate total by age group
      total_age_group = aggregate(line1_plot_fetch$Total, by=list(Category=line1_plot_fetch$Age_group), FUN = sum)
      affected_number = sum(total_age_group$x)
      percentages = total_age_group$x/affected_number * 100
      total_age_group = cbind(total_age_group, percentages)
      total_age_group
      #plot showing percentage per age group
      position=rep(3, length(total_age_group$Category))
      plot(total_age_group$percentages, type="o", col="blue", xlab = "Age Group", ylab = "Total Percentage", main = "A line graph shpwing the percentage of infected people in a given age group")
      text(total_age_group$percentages, labels = total_age_group$Category, cex = 0.7, pos = position)
      
      dev.off()
      
    }
  )
  #download the graph as pdf
  output$downloadplot11 <- downloadHandler(
    filename = function() {
      paste('line1', 'pdf', sep = ".")
    },
    content = function(file) {
      pdf(file)
      
      #plot the graph
      line1_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
      #calculate total by age group
      total_age_group = aggregate(line1_plot_fetch$Total, by=list(Category=line1_plot_fetch$Age_group), FUN = sum)
      affected_number = sum(total_age_group$x)
      percentages = total_age_group$x/affected_number * 100
      total_age_group = cbind(total_age_group, percentages)
      total_age_group
      #plot showing percentage per age group
      position=rep(3, length(total_age_group$Category))
      plot(total_age_group$percentages, type="o", col="blue", xlab = "Age Group", ylab = "Total Percentage", main = "A line graph shpwing the percentage of infected people in a given age group")
      text(total_age_group$percentages, labels = total_age_group$Category, cex = 0.7, pos = position)
      
      dev.off()
      
    }
  )
  
  output$line2 <- renderPlot({
    line2_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
    #box_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
    
    sick_males=aggregate(line2_plot_fetch$Male, by=list(Category=line2_plot_fetch$District), FUN=sum)
    sick_females=aggregate(line2_plot_fetch$Female, by = list(Category=line2_plot_fetch$District), FUN = sum)
    plot(sick_females$x, type="o", col="red", xlab = "Number of Districts", ylab = "Number of Malaria patients", main="A line grapgh showing the variation of gender of malaria patients in districts")
    lines(sick_males$x, type = "o", col="blue")
    text(sick_females$x,   labels=sick_females$Category, cex=0.7)
    
  })
  
  ##################################
  #    DOWNLOADS(IMAGE OR PDF)     #
  ##################################
  
  #download the graph as image
  output$downloadplot12 <- downloadHandler(
    filename = function() {
      paste('line2', 'png', sep = ".")
    },
    content = function(file) {
      png(file)
      
      #plot the graph
      line2_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
      #box_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
      
      sick_males=aggregate(line2_plot_fetch$Male, by=list(Category=line2_plot_fetch$District), FUN=sum)
      sick_females=aggregate(line2_plot_fetch$Female, by = list(Category=line2_plot_fetch$District), FUN = sum)
      plot(sick_females$x, type="o", col="red", xlab = "Number of Districts", ylab = "Number of Malaria patients", main="A line grapgh showing the variation of gender of malaria patients in districts")
      lines(sick_males$x, type = "o", col="blue")
      text(sick_females$x,   labels=sick_females$Category, cex=0.7)
      
      dev.off()
      
    }
  )
  #download the graph as pdf
  output$downloadplot13 <- downloadHandler(
    filename = function() {
      paste('line2', 'pdf', sep = ".")
    },
    content = function(file) {
      pdf(file)
      
      #plot the graph
      line2_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
      #box_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
      
      sick_males=aggregate(line2_plot_fetch$Male, by=list(Category=line2_plot_fetch$District), FUN=sum)
      sick_females=aggregate(line2_plot_fetch$Female, by = list(Category=line2_plot_fetch$District), FUN = sum)
      plot(sick_females$x, type="o", col="red", xlab = "Number of Districts", ylab = "Number of Malaria patients", main="A line grapgh showing the variation of gender of malaria patients in districts")
      lines(sick_males$x, type = "o", col="blue")
      text(sick_females$x,   labels=sick_females$Category, cex=0.7)
      
      dev.off()
      
    }
  )
  
  #Affected percentage per gender
  output$line3 <- renderPlot({
    line3_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM division")))
    line3_plot_fetchs <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
    
    the_total = aggregate(line3_plot_fetch$Total, by=list(Category=line3_plot_fetch$District), FUN = sum)
    infectedMales=aggregate(line3_plot_fetchs$Male, by=list(Category=line3_plot_fetchs$District), FUN=sum)
    
    males_percentage = infectedMales$x/the_total$x*100
    total_males = cbind(infectedMales, males_percentage)
    
    plot(total_males$males_percentage, type="o", col="red",  xlab = "Districts", ylab = "Infected males (%)", main="Percentages of malaria prevalence amongst males in the districts")
    text(total_males$males_percentage, labels = total_males$Category, cex = 0.7)
    
  })
  
  ##################################
  #    DOWNLOADS(IMAGE OR PDF)     #
  ##################################
  
  #download the graph as image
  output$downloadplot14 <- downloadHandler(
    filename = function() {
      paste('line3', 'png', sep = ".")
    },
    content = function(file) {
      png(file)
      
      #plot the graph
      line3_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM division")))
      line3_plot_fetchs <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
      
      the_total = aggregate(line3_plot_fetch$Total, by=list(Category=line3_plot_fetch$District), FUN = sum)
      infectedMales=aggregate(line3_plot_fetchs$Male, by=list(Category=line3_plot_fetchs$District), FUN=sum)
      
      males_percentage = infectedMales$x/the_total$x*100
      total_males = cbind(infectedMales, males_percentage)
      
      plot(total_males$males_percentage, type="o", col="red",  xlab = "Districts", ylab = "Infected males (%)", main="Percentages of malaria prevalence amongst males in the districts")
      text(total_males$males_percentage, labels = total_males$Category, cex = 0.7)
      
      dev.off()
      
    }
  )
  #download the graph as pdf
  output$downloadplot15 <- downloadHandler(
    filename = function() {
      paste('line3', 'pdf', sep = ".")
    },
    content = function(file) {
      pdf(file)
      
      #plot the graph
      line3_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM division")))
      line3_plot_fetchs <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
      
      the_total = aggregate(line3_plot_fetch$Total, by=list(Category=line3_plot_fetch$District), FUN = sum)
      infectedMales=aggregate(line3_plot_fetchs$Male, by=list(Category=line3_plot_fetchs$District), FUN=sum)
      
      males_percentage = infectedMales$x/the_total$x*100
      total_males = cbind(infectedMales, males_percentage)
      
      plot(total_males$males_percentage, type="o", col="red",  xlab = "Districts", ylab = "Infected males (%)", main="Percentages of malaria prevalence amongst males in the districts")
      text(total_males$males_percentage, labels = total_males$Category, cex = 0.7)
      
      dev.off()
      
    }
  )
  output$line4 <- renderPlot({
    line4_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM division")))
    line4_plot_fetchs <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
    
    summation = aggregate(line4_plot_fetch$Total, by=list(Category=line4_plot_fetch$District), FUN = sum)
    infectedFemales=aggregate(line4_plot_fetchs$Female, by = list(Category=line4_plot_fetchs$District), FUN = sum)
    
    females_percentage = infectedFemales$x/summation$x*100
    total_females = cbind(infectedFemales, females_percentage)
    
    plot(total_females$females_percentage, type="o", col="red",  xlab = "Districts", ylab = "Infected females (%)", main="Percentages of malaria prevalence amongst females in the districts")
    text(total_females$females_percentage, labels = total_females$Category, cex = 0.7)
    
  })
  
  ##################################
  #    DOWNLOADS(IMAGE OR PDF)     #
  ##################################
  
  #download the graph as image
  output$downloadplot16 <- downloadHandler(
    filename = function() {
      paste('line4', 'png', sep = ".")
    },
    content = function(file) {
      png(file)
      
      #plot the graph
      line4_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM division")))
      line4_plot_fetchs <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
      
      summation = aggregate(line4_plot_fetch$Total, by=list(Category=line4_plot_fetch$District), FUN = sum)
      infectedFemales=aggregate(line4_plot_fetchs$Female, by = list(Category=line4_plot_fetchs$District), FUN = sum)
      
      females_percentage = infectedFemales$x/summation$x*100
      total_females = cbind(infectedFemales, females_percentage)
      
      plot(total_females$females_percentage, type="o", col="red",  xlab = "Districts", ylab = "Infected females (%)", main="Percentages of malaria prevalence amongst females in the districts")
      text(total_females$females_percentage, labels = total_females$Category, cex = 0.7)
      
      dev.off()
      
    }
  )
  #download the graph as pdf
  output$downloadplot17 <- downloadHandler(
    filename = function() {
      paste('line4', 'pdf', sep = ".")
    },
    content = function(file) {
      pdf(file)
      
      #plot the graph
      line4_plot_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM division")))
      line4_plot_fetchs <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
      
      summation = aggregate(line4_plot_fetch$Total, by=list(Category=line4_plot_fetch$District), FUN = sum)
      infectedFemales=aggregate(line4_plot_fetchs$Female, by = list(Category=line4_plot_fetchs$District), FUN = sum)
      
      females_percentage = infectedFemales$x/summation$x*100
      total_females = cbind(infectedFemales, females_percentage)
      
      plot(total_females$females_percentage, type="o", col="red",  xlab = "Districts", ylab = "Infected females (%)", main="Percentages of malaria prevalence amongst females in the districts")
      text(total_females$females_percentage, labels = total_females$Category, cex = 0.7)
      
      dev.off()
      
    }
  )
  ##################################
  #   SINGLE SEARCH               #
  ##################################
  output$single_search <- renderUI({    
    search_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
    
    list(
      selectizeInput(
        'searchDistrict', 'Select the district to search', choices = as.character(unique(search_fetch$District)), multiple = TRUE
      ),
      
      actionButton('singleSearch', 'Search')
    )
    
  })
  
  searching1 <- reactive({
    sweat <- "Buikwe"
    try2 = fetch(dbSendQuery(malariadb, paste("SELECT Age_group, Male, Female FROM district WHERE District = '",input$searchDistrict,"'",sep="")))
    
    
  })
  
  
  output$sea <- renderPlot({
    
    
      
    input$singleSearch
    isolate({
      dataploting <- melt(searching1(), id.vars ="Age_group")
      ggplot(dataploting, aes(Age_group,value))+
        geom_bar(aes(fill = variable),
                 width = 0.4, position = position_dodge(width = 0.5),stat = "identity")+
        theme(legend.position = "top",legend.title = element_blank()
              ,axis.title.x = element_blank(),axis.title.y = element_blank())
      
    })
    
  })
  
  ##################################
  #    DOWNLOADS(IMAGE OR PDF)     #
  ##################################
  
  #download the graph as image
  output$downloadplot18 <- downloadHandler(
    filename = function() {
      paste('search1', 'png', sep = ".")
    },
    content = function(file) {
      png(file)
      
      #plot the graph
      input$singleSearch
      isolate({
        dataplot <- melt(searching1(), id.vars ="Age_group")
        ggplot(dataplot, aes(Age_group,value))+
          geom_bar(aes(fill = variable),
                   width = 0.4, position = position_dodge(width = 0.5),stat = "identity")+
          theme(legend.position = "top",legend.title = element_blank()
                ,axis.title.x = element_blank(),axis.title.y = element_blank()
          )
      })
      dev.off()
      
    }
  )
  #download the graph as pdf
  output$downloadplot1 <- downloadHandler(
    filename = function() {
      paste('search1', 'pdf', sep = ".")
    },
    content = function(file) {
      pdf(file)
      
      #plot the graph
      input$singleSearch
      isolate({
        dataplot <- melt(searching1(), id.vars ="Age_group")
        ggplot(dataplot, aes(Age_group,value))+
          geom_bar(aes(fill = variable),
                   width = 0.4, position = position_dodge(width = 0.5),stat = "identity")+
          theme(legend.position = "top",legend.title = element_blank()
                ,axis.title.x = element_blank(),axis.title.y = element_blank()
          )
      })
      dev.off()
      
    }
  )
  searching2 <- reactive({
    divis <- input$searchDistrict
    div_result = fetch(dbSendQuery(malariadb, paste("SELECT Sub_county, Male ,Female FROM division WHERE District = '",divis,"'",sep="")))
    
  })
  
  output$searchplot2 <- renderPlot({
    input$singleSearch
    isolate({
      dataploting <- melt(searching2(), id.vars ="Sub_county")
      ggplot(dataploting, aes(Sub_county,value))+
        geom_bar(aes(fill = variable),
                 width = 0.4, position = position_dodge(width = 0.5),stat = "identity")+
        theme(legend.position = "top",legend.title = element_blank()
              ,axis.title.x = element_blank(),axis.title.y = element_blank())
      
    })
  })
  searching4 <- reactive({
    divis <- input$searchDistrict
    div_result = fetch(dbSendQuery(malariadb, paste("SELECT Sub_county,Total ,Land_area, Population_density FROM division WHERE District = '",divis,"'",sep="")))
    
  })
  
  output$searchplot3 <- renderPlot({
    input$singleSearch
    ggplot(searching4(),aes(x=Land_area,y=Total))+
      geom_point(aes(col=Sub_county,size=Population_density))+
      geom_smooth(method = "lm",se=F)+xlab("Land area")+ylab("total population")
  })
  
  output$searchplotry <- renderTable({
    searching()
    helping <- data.frame(searching())
    
  })
  
  ##################################
  #    DOWNLOADS(IMAGE OR PDF)     #
  ##################################
  
  #download the graph as image
  output$downloadplot20 <- downloadHandler(
    filename = function() {
      paste('search2', 'png', sep = ".")
    },
    content = function(file) {
      png(file)
      
      #plot the graph
      input$singleSearch
      isolate({
        dataploting <- melt(searching2(), id.vars ="Sub_county")
        ggplot(dataploting, aes(Sub_county,value))+
          geom_bar(aes(fill = variable),
                   width = 0.4, position = position_dodge(width = 0.5),stat = "identity")+
          theme(legend.position = "top",legend.title = element_blank()
                ,axis.title.x = element_blank(),axis.title.y = element_blank())
        
      })
      dev.off()
      
    }
  )
  #download the graph as pdf
  output$downloadplot21 <- downloadHandler(
    filename = function() {
      paste('search2', 'pdf', sep = ".")
    },
    content = function(file) {
      pdf(file)
      
      #plot the graph
      input$singleSearch
      isolate({
        dataploting <- melt(searching2(), id.vars ="Sub_county")
        ggplot(dataploting, aes(Sub_county,value))+
          geom_bar(aes(fill = variable),
                   width = 0.4, position = position_dodge(width = 0.5),stat = "identity")+
          theme(legend.position = "top",legend.title = element_blank()
                ,axis.title.x = element_blank(),axis.title.y = element_blank())
        
      })
      dev.off()
      
    }
  )
  
  ##################################
  #   DOUBLE SEARCH               #
  ##################################
  output$double_search <- renderUI({    
    search_fetch <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
    
    list(
      selectizeInput(
        'selectDistrict1', 'Select the first district', choices = as.character(unique(search_fetch$District)), multiple = TRUE
      ),
      selectizeInput(
        'selectDistrict2', 'Select the second district', choices = as.character(unique(search_fetch$District)), multiple = TRUE
      ),
      actionButton('comparison', 'Compare')
    )
    
  })     
  getDat <- eventReactive(input$comparison,{
    withProgress(
      message = 'Comparison in progress',
      detail = 'get comparison visualizations', value=0 , {
        
        setDistrict1 <- isolate(input$selectDistrict1)
        setDistrict2 <- isolate(input$selectDistrict2)
        
        incProgress(0.5)
        
        if (!is.null(setDistrict1)) {
          Dat1 <-  fetch(dbSendQuery(malariadb, paste("SELECT Age_group, Total FROM district WHERE District = '",setDistrict1,"'",sep="")))
          
        } else {
          Dat1 <- NULL
        }
        if (!is.null(setDistrict2)) {
          Dat2 <-  fetch(dbSendQuery(malariadb, paste("SELECT Age_group, Total FROM district WHERE District = '",setDistrict2,"'",sep="")))
          
        } else {
          Dat2 <- NULL
        }
        
        setProgress(1)
        
        Dat <- Dat1
        Dat["Total2"] <- NA # That creates the new column named "Total2" filled with "NA"
        Dat$Total2 <- Dat2$Total
        
        
      })
    
    return(Dat)
  })  
  output$dataTable <- renderUI({    
    plotOutput("table")
  })
  output$table <- renderPlot({
    getDat()
    dataplot <- melt(getDat(), id.vars ="Age_group")
    ggplot(dataplot, aes(Age_group,value))+
      ggtitle(paste("A comparison graph based on age groups between",input$selectDistrict1,"and",input$selectDistrict2))+
      geom_bar(aes(fill = variable),
               width = 0.4, position = position_dodge(width = 0.5),stat = "identity")+
      theme(legend.position = "top",legend.title = element_blank()
            ,axis.title.x = element_blank(),axis.title.y = element_blank()
      )
  })
  
  
  ##################################
  #    DOWNLOADS(IMAGE OR PDF)     #
  ##################################
  
  #download the graph as image
  output$downloadplot22 <- downloadHandler(
    filename = function() {
      paste('dsearch', 'png', sep = ".")
    },
    content = function(file) {
      png(file)
      
      #plot the graph
      getDat()
      dataplot <- melt(getDat(), id.vars ="Age_group")
      ggplot(dataplot, aes(Age_group,value))+
        geom_bar(aes(fill = variable),
                 width = 0.4, position = position_dodge(width = 0.5),stat = "identity")+
        theme(legend.position = "top",legend.title = element_blank()
              ,axis.title.x = element_blank(),axis.title.y = element_blank()
        )
      dev.off()
      
    }
  )
  #download the graph as pdf
  output$downloadplot23 <- downloadHandler(
    filename = function() {
      paste('dsearch', 'pdf', sep = ".")
    },
    content = function(file) {
      pdf(file)
      
      #plot the graph
      getDat()
      dataplot <- melt(getDat(), id.vars ="Age_group")
      ggplot(dataplot, aes(Age_group,value))+
        geom_bar(aes(fill = variable),
                 width = 0.4, position = position_dodge(width = 0.5),stat = "identity")+
        theme(legend.position = "top",legend.title = element_blank()
              ,axis.title.x = element_blank(),axis.title.y = element_blank()
        )
      dev.off()
      
    }
  )
  
  ##################################
  #    POPULATION PYRAMID          #
  ##################################
  
  output$pyramidplot <- renderPlot({
    
    fetched_data <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
    fetched_dataz <- aggregate(cbind(fetched_data$Female, fetched_data$Male)~fetched_data$Age_group, fetched_data, sum)
    names(fetched_dataz) <- c("Age", "Male", "Female")
    fetched_dataz$Age <- factor(fetched_dataz$Age, levels = fetched_dataz$Age, labels = fetched_dataz$Age)
    fetched_dataz$Male <- -1*fetched_dataz$Male
    fetched_dataz$Female <- fetched_dataz$Female
    meltedPyramid <- melt(fetched_dataz, value.name = 'Population', variable.name = 'Gender', id.vars = 'Age')
    
    ggplot(meltedPyramid, aes(x=Age, y=Population, fill=Gender))+
      geom_bar(subset = (Gender="Female"), stat="identity")+
      geom_bar(subset = (Gender="Male"), stat="identity")+
      scale_y_continuous(breaks = seq(-2000000, 2000000,500000),
                         labels=paste0(as.numeric(c(seq(2000000, 0, -500000),seq(500000,2000000,500000)))))+
      
      labs(title = "A pyramid of population showing the total number of affected people per age group within the population", y = "Total Population", x = "Age group")+
      theme(plot.title = element_text(size = 50, colour = "#668cff"))+
      coord_flip()+
      scale_fill_brewer(palette = "Set1")+
      theme_bw()
  })
  
  ##################################
  #    DOWNLOADS(IMAGE OR PDF)     #
  ##################################
  
  #download the graph as image
  output$downloadplot24 <- downloadHandler(
    filename = function() {
      paste('pyramid', 'png', sep = ".")
    },
    content = function(file) {
      png(file)
      
      #plot the graph
      fetched_data <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
      fetched_dataz <- aggregate(cbind(fetched_data$Female, fetched_data$Male)~fetched_data$Age_group, fetched_data, sum)
      names(fetched_dataz) <- c("Age", "Male", "Female")
      fetched_dataz$Age <- factor(fetched_dataz$Age, levels = fetched_dataz$Age, labels = fetched_dataz$Age)
      fetched_dataz$Male <- -1*fetched_dataz$Male
      fetched_dataz$Female <- fetched_dataz$Female
      meltedPyramid <- melt(fetched_dataz, value.name = 'Population', variable.name = 'Gender', id.vars = 'Age')
      
      ggplot(meltedPyramid, aes(x=Age, y=Population, fill=Gender))+
        geom_bar(subset = (Gender="Female"), stat="identity")+
        geom_bar(subset = (Gender="Male"), stat="identity")+
        scale_y_continuous(breaks = seq(-2000000, 2000000,500000),
                           labels=paste0(as.numeric(c(seq(2000000, 0, -500000),seq(500000,2000000,500000)))))+
        
        labs(title = "A pyramid of population showing the total number of affected people per age group within the population", y = "Total Population", x = "Age group")+
        theme(plot.title = element_text(size = 50, colour = "#668cff"))+
        coord_flip()+
        scale_fill_brewer(palette = "Set1")+
        theme_bw()
      dev.off()
      
    }
  )
  #download the graph as pdf
  output$downloadplot25 <- downloadHandler(
    filename = function() {
      paste('pyramid', 'pdf', sep = ".")
    },
    content = function(file) {
      pdf(file)
      
      #plot the graph
      fetched_data <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM district")))
      fetched_dataz <- aggregate(cbind(fetched_data$Female, fetched_data$Male)~fetched_data$Age_group, fetched_data, sum)
      names(fetched_dataz) <- c("Age", "Male", "Female")
      fetched_dataz$Age <- factor(fetched_dataz$Age, levels = fetched_dataz$Age, labels = fetched_dataz$Age)
      fetched_dataz$Male <- -1*fetched_dataz$Male
      fetched_dataz$Female <- fetched_dataz$Female
      meltedPyramid <- melt(fetched_dataz, value.name = 'Population', variable.name = 'Gender', id.vars = 'Age')
      
      ggplot(meltedPyramid, aes(x=Age, y=Population, fill=Gender))+
        geom_bar(subset = (Gender="Female"), stat="identity")+
        geom_bar(subset = (Gender="Male"), stat="identity")+
        scale_y_continuous(breaks = seq(-2000000, 2000000,500000),
                           labels=paste0(as.numeric(c(seq(2000000, 0, -500000),seq(500000,2000000,500000)))))+
        
        labs(title = "A pyramid of population showing the total number of affected people per age group within the population", y = "Total Population", x = "Age group")+
        theme(plot.title = element_text(size = 50, colour = "#668cff"))+
        coord_flip()+
        scale_fill_brewer(palette = "Set1")+
        theme_bw()
      dev.off()
      
    }
  )
  ##############################################################DIVISION SECTION############################################################################################### 
  #-----read the uploaded divisions------
  divisiondata <- reactive({
    if (USER$Logged == TRUE) {
      inFiler <- input$divisionfile
      inFiler
      if(is.null(inFiler)){return()}
      else{
        #read.table(file = inFile$datapath, sep = input$sep)
        ourDataz <- read.csv(inFiler$datapath,header = input$header, sep = input$sep)  
      }
    }
  })
  
  ##################################
  #    BLOG SECTION                #
  ##################################
  output$feedback <- renderUI({
    box(background = "fuchsia",width = "12%", title = "Please avail your feedback about the malaria subject",
        list(
          textInput(
            'username', 'Please insert your name.'
          ),
          textInput(
            'category', 'Please ithe type of user that you are i.e local country-man, government official, health personnel, NGO official, or unspecified .'
          ),
          textInput(
            'message', 'Please write your message.'
          ),
          actionButton('post', 'Post Feedback'),
          actionButton('see', 'View Recent Feedback')
        )
    )
  })
  #------------insert the feedback------
  insertFeed <- eventReactive(input$post,{
    withProgress(
      message = 'Insersion in progress',
      detail = 'inserting blog into the database', value=0 , {
        
        setUser <- isolate(input$username)
        setCategory <- isolate(input$category)
        setMessage <- isolate(input$message)
        
        incProgress(0.5)
        
        if (!is.null(setMessage)) {
          if(!is.null(setCategory)){
            poster <- paste("INSERT INTO feedback set user='",setUser,"',category='",setCategory,"',message='",setMessage,"'")
            dbSendQuery(malariadb, poster)
            
          }
        } else {
          NULL
        }
        
        setProgress(1)
        
      })
    
  }) 
  output$feeds <- renderUI({
    textOutput("deal")
    
  })
  output$deal <- renderText({
    insertFeed()
    
  })
  #----------retrieve the feedback-------------
  bringFeed <- eventReactive(input$see,{
    withProgress(
      message = 'Retrieve in progress',
      detail = 'Retrieving blog into the database', value=0 , {
        
        
        incProgress(0.5)
        
        fetched_blogs <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM feedback")))
        
        setProgress(1)
        
      })
    return(fetched_blogs)
  })
  output$blogs <- renderUI({
    
    dataTableOutput('results')
    
  })
  output$results <- renderDataTable(
    bringFeed(),
    options = list(
      pageLength = 100,
      lengthMenu = c(50,100,200,500)
    )
  )
  
  #---------insert the divisions into the database---------
  output$content2 <- renderTable({
    if (USER$Logged == TRUE) {
      theData1 <- input$divisionfile
      
      if(is.null(divisiondata())){return()}
      else{
        
        diss <- data.frame(divisiondata())
        diss
        #insert data into the database
        dbWriteTable(malariadb, "division", diss, append = TRUE, row.names = FALSE, overwrite = FALSE)
        
        
      }
      result2 <- read.csv(theData1$datapath, header = input$header, sep = input$sep)
    }
  })
  
  ##################################
  #    SCATTER PLOTS               #
  ##################################
  
  output$scatter1 <- renderPlot({
    scatter_data1 <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM division")))
    plot(scatter_data1$Land_Area, scatter_data1$Total, xlab = "Land Area (Sq.Km)", ylab = "Number of infected people")
  })
  
  ##################################
  #    DOWNLOADS(IMAGE OR PDF)     #
  ##################################
  
  #download the graph as image
  output$downloadplot26 <- downloadHandler(
    filename = function() {
      paste('scater1', 'png', sep = ".")
    },
    content = function(file) {
      png(file)
      
      #plot the graph
      scatter_data1 <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM division")))
      plot(scatter_data1$Land_Area, scatter_data1$Total, xlab = "Land Area (Sq.Km)", ylab = "Number of infected people")
      
      dev.off()
      
    }
  )
  #download the graph as pdf
  output$downloadplot27 <- downloadHandler(
    filename = function() {
      paste('scater1', 'pdf', sep = ".")
    },
    content = function(file) {
      pdf(file)
      
      #plot the graph
      scatter_data1 <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM division")))
      plot(scatter_data1$Land_Area, scatter_data1$Total, xlab = "Land Area (Sq.Km)", ylab = "Number of infected people")
      
      dev.off()
      
    }
  )
  output$scatter2 <- renderPlot({
    scatter_data2 <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM division")))
    plot(scatter_data2$Population_Density, scatter_data2$Total, xlab = "Population density", ylab = "Number of infected people")
    
  }) 
  
  ##################################
  #    DOWNLOADS(IMAGE OR PDF)     #
  ##################################
  
  #download the graph as image
  output$downloadplot28 <- downloadHandler(
    filename = function() {
      paste('scater2', 'png', sep = ".")
    },
    content = function(file) {
      png(file)
      
      #plot the graph
      scatter_data2 <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM division")))
      plot(scatter_data2$Population_Density, scatter_data2$Total, xlab = "Population density", ylab = "Number of infected people")
      
      dev.off()
      
    }
  )
  #download the graph as pdf
  output$downloadplot29 <- downloadHandler(
    filename = function() {
      paste('scater2', 'pdf', sep = ".")
    },
    content = function(file) {
      pdf(file)
      
      #plot the graph
      scatter_data2 <- fetch(dbSendQuery(malariadb, paste("SELECT * FROM division")))
      plot(scatter_data2$Population_Density, scatter_data2$Total, xlab = "Population density", ylab = "Number of infected people")
      
      dev.off()
      
    }
  )
  #--------------images--------------
  output$image1 <- renderUI({
    #tags$img(src='ug.jpg', width=200, height = 300)
    #tags$img(src = 'minister.jpg', width=300, height=400)
  })
  #--------------help--------------------------
  output$help1 <- renderText("Search is a functionality of the system that enables you to sort results of a specific district.
                             Single search platform enables you search for a single district and results(two graphs) belonging
                             specifically to this district are returned. Please just type one of the correct letters that constitute 
                             the district name and the rest will be auto suggested for you to choose from.
                             If you get an warning, please retry.....
                             Double search is a platform that enables you to search for two specific districts and results of their comparision 
                             returned. The same criteria applies as for the single search")
  output$help2 <- renderText("The upload platform is restricted to be used only by government officials and other authorised persons.
                             For this reason a login platform is availed to autheticate these users.
                             Users are to feed in given credentials and will only proceed if the entered credentials are true......
                             After a successful login user will be directed to the upload page and they are supposed to upload only files of
                             type .CSV otherwise the operation will not be successful. 
                             They can upload two types of files i.e district files(these contain information about districts specifying
                             age group statistics in an order of(District, Age_group, Male, Female, Total).
                             Failure to follow this format will lead to unsuccessful uplaod.....
                             The second file type is the sub county file that contains information about distict subcouunties statistics in an order of
                             (District, Sub_county, Male, Female, Land_area, Sex_ratio, Population_density).
                             Failure to follow this format will also lead to unsuccessful uplaod.....
                             Successful upload will result into update of the data storage medium(database) thus new analysis models are gotten.")
  output$help3 <- renderText("The feedback platform is one that enables you to give us feedback on the malaria subject. That is to say you are free to give 
                             us your thoughts on how best you think we can eradicate malaria, ask questions about malaria, give advice to any inquiry made on the 
                             platform and also advertise anything about the malaria subject.
                             You can do this by just filling in your name, category of user you are and your message.
                             You can also view other recents posts by clicking the RECENT POSTS button.")
}

