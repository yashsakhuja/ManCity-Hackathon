library(shiny)  
library(shinydashboard)  
library(highcharter)  
library(tidyverse)  
library(shinythemes)  
library(dashboardthemes)  
library(readr)  
library(plotly)  
library(ggpubr)

# reading in the data files using read.csv
full_city <- read.csv("full_city.csv")
full_city <- full_city %>%  mutate(title=case_when(title == "Manchester City WFC [ 2 - 1 ] Brighton & Hove Albion WFC" ~ "Manchester City WFC [ 2 - 1 ] Brighton Hove Albion WFC",
                                                   TRUE ~ title))


games_1 <- read.csv("games_1.csv")
fullxpts <- read.csv("fullxpts.csv")
fullxpts <- fullxpts %>%  mutate(matchid=case_when(matchid == "Manchester City WFC [ 2 - 1 ] Brighton & Hove Albion WFC" ~ "Manchester City WFC [ 2 - 1 ] Brighton Hove Albion WFC",
                                                   TRUE ~ matchid))
predictedscores <- read.csv("predictedscores.csv")
predictedscores <- predictedscores %>%  mutate(matchid=case_when(matchid == "Manchester City WFC [ 2 - 1 ] Brighton & Hove Albion WFC" ~ "Manchester City WFC [ 2 - 1 ] Brighton Hove Albion WFC",
                                                                 TRUE ~ matchid))

utility <- read.csv("utility.csv")

# defining a function that takes in homeXg and awayXg as arguments and returns a matrix of score probabilities
ScoreGrid<-function(homeXg,awayXg){
  
  A <- as.numeric()  # creating an empty numeric vector
  B <- as.numeric()  # creating another empty numeric vector
  
  for(i in 0:9) {  # loop through 0 to 9
    A[(i+1)] <- dpois(i,homeXg)  # assign the Poisson probability of i goals given homeXg to the A vector
    B[(i+1)] <- dpois(i,awayXg)  # assign the Poisson probability of i goals given awayXg to the B vector
  }
  
  A[11] <- 1 - sum(A[1:10])  # assign the probability of 10 or more goals to the last element of the A vector
  B[11] <- 1 - sum(B[1:10])  # assign the probability of 10 or more goals to the last element of the B vector
  
  name <- c("0","1","2","3","4","5","6","7","8","9","10+")  # create a vector of strings with score values
  zero <- mat.or.vec(11,1)  # create an empty matrix with 11 rows and 1 column
  
  C <- data.frame(row.names=name, "0"=zero, "1"=zero, "2"=zero, "3"=zero, "4"=zero,
                  "5"=zero, "6"=zero, "7"=zero,"8"=zero,"9"=zero,"10+"=zero)  # create a data frame with the score values as row names and zeros for the score probabilities
  
  for(j in 1:11) {  # loop through the rows
    for(k in 1:11) {  # loop through the columns
      C[j,k] <- A[k]*B[j]  # assign the probability of the score in row j and column k to the C data frame
    }
  }
  
  
  colnames(C) <- name
  
  return(round(C*100,2)/100)
}

# A function that creates a heatmap of the score probabilities for a soccer match based on the home and away teams and their respective expected goals (xG) values.
ScoreHeatMap <- function(home, away, homeXg, awayXg, datasource) {
  
  # Swap the home and away team names for easier reading on the heatmap
  adjustedHome <- away
  adjustedAway <- home
  
  # Call the ScoreGrid function to create a data frame with score probabilities
  df <- ScoreGrid(homeXg, awayXg)
  
  # Reshape the data frame into a "tidy" format for plotting
  df %>% 
    as_tibble(rownames = all_of(away)) %>% # Convert to tibble and set row names to the away team
    pivot_longer(cols = -all_of(away),  # Pivot columns to long format, keeping the away team as a column
                 names_to = home, 
                 values_to = "Probability") %>%
    mutate_at(vars(all_of(away), home),  # Reorder the levels of the factor variables for better presentation on the heatmap
              ~forcats::fct_relevel(.x, "10+", after = 10)) %>% 
    ggplot() +  # Start a ggplot object
    geom_tile(aes_string(x=all_of(away), y=home, fill = "Probability")) +  # Add tiles to the plot based on the probability of each score
    scale_fill_gradient2(mid= "white", high = "#6CABDD")+  # Set the color scale for the tiles
    theme(plot.margin = unit(c(1,1,1,1),"cm"),  # Set margins and fonts for the plot
          plot.title = element_text(size=20,hjust = 0.5,face="bold",vjust =4),
          plot.caption = element_text(hjust=1.1,size=10,face = "italic"), 
          plot.subtitle = element_text(size=12,hjust = 0.5,vjust=4),
          axis.title.x=element_text(size=14,vjust=-0.5,face="bold"),
          axis.title.y=element_text(size=14, vjust =0.5,face="bold"),
          axis.text = element_text(size=16),
          legend.text=element_text(size=14),
          legend.title = element_text(size=14))+
    labs(x=adjustedAway,y=adjustedHome) %>%  # Add axis labels to the plot
    print()  # Print the plot object
  
}

# Define UI for the dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Manchester City WFC Web App"),  # create the header for the dashboard page
  dashboardSidebar(width=150,  # create the sidebar on the left with a width of 150px
                   sidebarMenu(
                     menuItem("Tactics", tabName = "dashboard", icon = icon("dashboard")),
                     menuItem("Passing", tabName = "pass",icon = icon("file")),
                     menuItem("Defensive Rank", tabName = "defense",icon = icon("file"))
                     # create a second menu item "Passing" in the sidebar
                   )
  ),
  dashboardBody(#shinyDashboardThemes("grey_dark"),# create the main body of the dashboard page
    tabItems(
      tabItem(tabName = "dashboard",  # create a tab called "dashboard"
              selectInput("match_name",  # create a dropdown input for selecting a match, and store the selected value in "match_name"
                          label=h5("Select Match"),  # label for the dropdown input
                          choices = c(unique(full_city$title)),""),  # choices for the dropdown input
              selected=c(unique(full_city$title))[1],  # default selected value for the dropdown input
              
              fluidRow(uiOutput("text")),  # create a fluid row with dynamic UI output
              #fluidRow(uiOutput("text2")),
              tags$style(".small-box.bg-blue { background-color: #6CABDD !important; color: white !important; }"),
              tags$style(".small-box.bg-red { background-color: #994242 !important; color: white !important; }"),
              #tags$style(".small-box .bg-red { background-color: #994242 !important; color: white !important; }"),# add custom CSS style for small boxes
              fluidRow(  # create another fluid row with value boxes
                valueBoxOutput("name1",width=2), 
                valueBoxOutput("name5",width=2),
                valueBoxOutput("name3"),
                valueBoxOutput("name2",width=2),
                valueBoxOutput("name4",width=2)),
              
              
              
              
              fluidRow(
                box(
                  title = "Expected Win Probability",
                  width = 4,
                  height = 300,
                  
                  #plotOutput("my_chart1", height = "250px"))
                  highchartOutput("my_chart3", height = "260px"))
                ,
                box(
                  title = "Expected Scores",
                  width=8,
                  height=500,
                  fluidRow(
                    column(
                      width =12,
                      height = 600,
                      plotOutput("my_chart1", height = "400px"))),
                )
                ,
                box(
                  title="Expected Points vs Actual Points over 5 games",
                  width=4,
                  height=350,
                  fluidRow(
                    column(
                      width = 12,
                      height = 300,
                      highchartOutput("my_chart4", height = "300px"))))
                
                
              )
              
      ),
      
      tabItem(tabName = "pass",
              h1("Passing Clusters and Shots Created"),
              fluidRow(uiOutput("text4")),
              fluidRow(
                box(height=750,width=12,
                    
                    column(height=10,width=12, align = "center",
                           plotlyOutput("arrow_plot",height=350,width=800)),
                    fluidRow(
                      # box(height=400,width=12,
                      column(height=10,width=12, align = "center",
                             plotOutput("pass_plot",height=350,width=1200)
                      ))))),
      
      tabItem(tabName = "defense", # create a tab called "dashboard"
              selectInput("team_name",  # create a dropdown input for selecting a match, and store the selected value in "match_name"
                          label=h5("Select Team"),  # label for the dropdown input
                          choices = c(unique(utility$Team)),""),  # choices for the dropdown input
              selected=c(unique(utility$Team))[1],
              h1("Defensive Performance Ranked"),
              fluidRow(uiOutput("text6")),
              plotOutput("my_chart9",height=600,width=1400)))
    
    
    
    ,
    
    
    tags$head(
      tags$style(
        HTML(' /* logo */
        .skin-blue .main-header .logo {
            background-color: #6CABDD;
        }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
            background-color: #6CABDD;
        }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
            background-color: #6CABDD;
        }        

        /* main sidebar */
        .skin-blue .main-sidebar {
            background-color: "black";
        }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
            background-color: #800000;
        }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a {
            background-color: "red";
            color: "white":;
        }

        /* other links in the sidebarmenu when hovered */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
            background-color: #994242;
        }
        
        /* toggle button when hovered  */                    
        .skin-blue .main-header .navbar .sidebar-toggle:hover {
            background-color: "red";
        }
        
        .box{
            border-top: 4px solid #994242;
        }
        
        div.col-sm-4 ~ div.col-sm-4 .box  {
            height: 300px;
            bottom: 180px;
        }
                              
        
        
        
        #text h3 {
            font-size: 34px;
            color: "black";
            text-align: center;
            margin-bottom: 60px;
            font-family: system-ui;
        }

        #text2 h3 {
            font-size: 34px;
            color: "black";
            text-align: center;
            margin-bottom: 60px;
        }
        
        #text4 h4 {                
        
            margin-right: 150px;
            text-align: center;
            margin-left: 150px;

        }
        
         #text6 h4 {                
        
            margin-right: 150px;
                        text-align: center;

            margin-left: 150px;

        }
        
        
      #my_chart9 > img {
          margin-left: 200px;
          margin-top: 10px;
      }
      
        .h1, h1 {
    font-size: 36px;
    text-align: center;
}
        
        .small-box {
        text-align: center;
        }
       
       #name1 .small-box {
    left: 110px;
    margin-right: 80px;
     border-top-left-radius: 20px;
    border-bottom-left-radius: 20px;
    box-shadow: 1px 1px 0px #999,
                2px 2px 0px #999,
                3px 3px 0px #999,
                4px 4px 0px #999,
                5px 5px 0px #999,
                6px 6px 0px #999;}
    
    
    #name2 .small-box {
    margin-right: 74px;
    border-top-left-radius: 20px;
    border-bottom-left-radius: 20px;
    box-shadow: 1px 1px 0px #999,
                2px 2px 0px #999,
                3px 3px 0px #999,
                4px 4px 0px #999,
                5px 5px 0px #999,
                6px 6px 0px #999;
    }
    
    #name3 .small-box {
    margin-right: 80px;
    box-shadow: 1px 1px 0px #999,
     2px 2px 0px #999,
            3px 3px 0px #999,
     4px 4px 0px #999,
     5px 5px 0px #999,
            6px 6px 0px #999;
    border-top-right-radius: 20px;
    border-bottom-right-radius: 20px;
    border-top-left-radius: 20px;
    border-bottom-left-radius: 20px;
    
    
    }
    
    
    #name5 .small-box {
    margin-right: 80px;
    box-shadow: 1px 1px 0px #999,
     2px 2px 0px #999,
            3px 3px 0px #999,
     4px 4px 0px #999,
     5px 5px 0px #999,
            6px 6px 0px #999;
    border-top-right-radius: 20px;
    border-bottom-right-radius: 20px;

    }
    
    #name4 .small-box {
    
    margin-right: 8-;
    margin-right: 80px;
    margin-left: 5px;
    right: 110px;
    box-shadow: 1px 1px 0px #999,
     2px 2px 0px #999,
            3px 3px 0px #999,
     4px 4px 0px #999,
     5px 5px 0px #999,
            6px 6px 0px #999;
    border-top-right-radius: 20px;
    border-bottom-right-radius: 20px;

    }
        
    ')
      )
    )))



# Define server logic for the dashboard
server <- function(input, output) {
  
  
  output$my_chart3 <- renderHighchart({  # creates an output object named "my_chart3" and renders a Highchart plot inside it
    hchart(  # creates a Highchart plot
      fullxpts %>% filter(matchid==input$match_name) %>% head(3), "pie", hcaes(name = KPI, y = Value),  # specifies the data to use and the chart type ("pie")
      name = "KPI",  # sets the name of the chart series
      innerSize = "40%",  # sets the size of the inner circle of the pie chart as a percentage
      dataLabels = list(format = "{point.KPI}<br>({point.Value}%)")  # sets the format of the data labels that appear on the chart
    )
  })
  
  
  output$my_chart1 <- renderPlot({
    
    
    ScoreHeatMap(full_city %>% filter(title==input$match_name) %>% filter(team.name=="Manchester City WFC") %>% summarize(unique(gsub("\\s+", "", team.name))) %>% pull,
                 full_city %>% filter(title==input$match_name) %>% filter(team.name!="Manchester City WFC") %>% summarize(unique(gsub("\\s+", "", team.name))) %>% pull, 
                 full_city %>% filter(title==input$match_name & type.name=="Shot") %>% filter(team.name!="Manchester City WFC") %>%  dplyr::summarise(n=sum(shot.statsbomb_xg)) %>% pull(n),
                 full_city %>% filter(title==input$match_name & type.name=="Shot") %>% filter(team.name=="Manchester City WFC") %>%  dplyr::summarise(n=sum(shot.statsbomb_xg)) %>% pull(n),"StatsBomb")
  })
  
  output$my_chart4 <- renderHighchart({
    hc <- highchart() %>%
      hc_chart(type = "line") %>%
      #hc_subtitle(text = "Expected Points vs Actual Points over 5 games") %>%
      hc_xAxis(categories = games_1$game_number) %>%
      hc_yAxis(title = list(text = "Points")) %>%
      hc_add_series(
        name = "Actual Points",
        data = games_1$actual_points
      ) %>%
      hc_add_series(
        name = "Expected Points",
        data = games_1$xpts
      ) 
  })
  
  output$text <- renderUI({
    tags$h3(input$match_name)
  })
  output$text4 <- renderUI({
    tags$h4(" To get started with this visualization, click on any of the black point (average starting location for that particular cluster of passes), 
            and you should then see the individual passes that make up that cluster as well as the shots that were created as result of those passes.
            (If you happen to be zoomed in, double-click anywhere in the blank space.)")
  })
  
  output$text6 <- renderUI({
    tags$h4("The Evidential Reasoning Model is based on the Dempster-Shafer theory for modelling uncertainities and possibilities. 
    Using this model we have made an attempt to use a Multi Criteria Decision Analysis model for scoring and 
            prescribing best defensive measures based on defensive attributes (metrics) based on our domain knowledge. Here are the scores and rankings for 
            players based on their defensive performance for both Manchester City WFC and Arsenal WFC")
  })
  
  
  output$name1 <- renderValueBox({
    valueBox(round(full_city %>% filter(title==input$match_name & type.name=="Shot") %>% filter(team.name=="Manchester City WFC") %>%  dplyr::summarise(n=sum(shot.statsbomb_xg)) %>% pull(n),2), "Manchester City xG",color="blue")
  })
  
  output$name5 <- renderValueBox({
    valueBox(round(full_city %>% filter(title==input$match_name & type.name=="Shot") %>% filter(team.name!="Manchester City WFC") %>%  dplyr::summarise(n=sum(shot.statsbomb_xg)) %>% pull(n),2), "Oppenent xG",color="red")
  })
  
  output$name2 <- renderValueBox({
    #valueBox(games_1 %>% filter(matches==input$match_name) %>% pull(xpts), "xPts", color = "blue")
    valueBox(fullxpts %>% filter(matchid==input$match_name) %>% select(Value) %>% slice(4),"Manchester City xPts", color = "blue")
  })
  output$name4 <- renderValueBox({
    #valueBox(games_1 %>% filter(matches==input$match_name) %>% pull(xpts), "xPts", color = "blue")
    valueBox(fullxpts %>% filter(matchid==input$match_name) %>% select(Value) %>% slice(5)," Opponent xPts", color = "red")
  })
  output$name3 <- renderValueBox({
    valueBox(
      predictedscores %>% filter(matchid==input$match_name) %>% pull(scores), "Predicted Score", color = "black")
  })
  
  passes <- full_city %>% filter(type.name=="Pass" & team.name=="Manchester City WFC" &
                                   play_pattern.name=="Regular Play" & is.na(pass.outcome.name) &
                                   title=="Manchester City WFC [ 2 - 1 ] Liverpool WFC")
  
  passes$location.x <- as.numeric(gsub("^\\[(.*),.*\\]$", "\\1", passes$location))
  passes$location.y <- as.numeric(gsub("^\\[.*,\\s*(.*)\\]$", "\\1",  passes$location))
  
  passes$end_x <- as.numeric(gsub("^\\[(.*),.*\\]$", "\\1", passes$pass.end_location))
  passes$end_y <- as.numeric(gsub("^\\[.*,\\s*(.*)\\]$", "\\1",  passes$pass.end_location))
  
  passes <- passes %>% filter(location.x>=60 & end_x>=60)
  
  pass_clusters <- kmeans(passes[,c("location.x", "location.y", "end_x", "end_y")], centers =6)
  
  cluster_means <- passes %>% group_by(cluster = pass_clusters$cluster) %>% summarise(mean_x = mean(location.x), mean_y = mean(location.y), mean_dx = mean(end_x), mean_dy = mean(end_y))
  
  # Create arrow plot of passes
  arrow_plot <- ggplot(cluster_means, aes(x = mean_x, y = mean_y)) + 
    # half-way line
    annotate("rect", xmin = 60, xmax = 120, ymin = 0, ymax = 80,
             fill = NA, colour = "#1E1E1EFF", size = 0.5) +
    annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62,
             fill = NA, colour = "#1E1E1EFF", size = 0.5) +
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "#1E1E1EFF", size = 0.5) +
    annotate("rect", xmin = 120, xmax = 120.5, ymin =36, ymax = 44,
             fill = NA, colour = "#1E1E1EFF", size = 0.5) +
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80,
             colour = "#1E1E1EFF", size = 0.5) +
    annotate("point", x = 108 , y = 40, colour = "#1E1E1EFF", size = 1.05) +
    annotate("point", x = 60 , y = 40, colour = "#1E1E1EFF", size = 1.05) +
    annotate("path", x=60+10*cos(seq(0,2*pi,length.out=2000)), size = 0.5,
             y=40+10*sin(seq(0,2*pi,length.out=2000)), col="#1E1E1EFF") +
    annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)),
             size = 0.5, y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)),
             col="#1E1E1EFF")+coord_cartesian(xlim = c(60, 120))+
    geom_point(size=4) + 
    geom_segment(aes(x = mean_x, y = mean_y, 
                     xend =  mean_dx, 
                     yend =  mean_dy,
                     color = as.factor(cluster)),size=1.5,arrow=arrow(length = unit(0.08, "inches"),type = "open")) + 
    scale_color_discrete(name = "Pass Cluster") +theme_void()+theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      strip.text = element_blank(),
      strip.background = element_blank(),
      axis.line = element_line()
    ) 
  
  output$arrow_plot <- renderPlotly({
    ggplotly(arrow_plot)
  })
  
  # Render pass plot on click
  output$pass_plot <- renderPlot({
    click_data <- event_data("plotly_click")
    if (is.null(click_data)) {
      return()
    }
    cluster_num <- pass_clusters$cluster[which.max(pass_clusters$cluster == click_data$pointNumber)]
    pass_data <- passes %>% filter(pass_clusters$cluster == cluster_num)
    shot_data <- full_city %>% filter(title=="Manchester City WFC [ 2 - 1 ] Liverpool WFC") %>% filter(possession %in% unique(pass_data$possession))
    shot_data <- shot_data %>% filter(type.name=="Shot")
    shot_data$location.x <- as.numeric(gsub("\\[([^,]+).*", "\\1",  shot_data$location))
    shot_data$location.y <-as.numeric(gsub("\\[[^,]+,\\s*([^,]+).*", "\\1",   shot_data$location))
    shotmapxgcolors <- c("#192780", "#2a5d9f", "#40a7d0", "#87cdcf", "#e7f8e6", "#f4ef95", "#FDE960", "#FCDC5F", "#F5B94D", "#F0983E", "#ED8A37", "#E66424", "#D54F1B", "#DC2608", "#BF0000", "#7F0000", "#5F0000")
    
    
    p <-ggplot(pass_data) +   # half-way line
      # half-way line
      annotate("rect", xmin = 60, xmax = 120, ymin = 0, ymax = 80,
               fill = NA, colour = "#1E1E1EFF", size = 0.5) +
      annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62,
               fill = NA, colour = "#1E1E1EFF", size = 0.5) +
      annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "#1E1E1EFF", size = 0.5) +
      annotate("rect", xmin = 120, xmax = 120.5, ymin =36, ymax = 44,
               fill = NA, colour = "#1E1E1EFF", size = 0.5) +
      annotate("segment", x = 120, xend = 120, y = 0, yend = 80,
               colour = "#1E1E1EFF", size = 0.5) +
      annotate("point", x = 108 , y = 40, colour = "#1E1E1EFF", size = 1.05) +
      annotate("point", x = 60 , y = 40, colour = "#1E1E1EFF", size = 1.05) +
      annotate("path", x=60+10*cos(seq(0,2*pi,length.out=2000)), size = 0.5,
               y=40+10*sin(seq(0,2*pi,length.out=2000)), col="#1E1E1EFF") +
      annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)),
               size = 0.5, y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)),
               col="#1E1E1EFF")+coord_cartesian(xlim = c(60, 120))+
      geom_segment(aes(x = location.x, y = location.y, 
                       xend =  end_x, 
                       yend =  end_y),size=1.5,arrow=arrow(length = unit(0.08, "inches"),type = "open"))+theme_void()+theme(
                         axis.title = element_blank(),
                         axis.text = element_blank(),
                         strip.text = element_blank(),
                         strip.background = element_blank(),
                         axis.line = element_line()
                       ) 
    
    p2 <- ggplot(shot_data) +   # half-way line
      # half-way line
      annotate("rect", xmin = 60, xmax = 120, ymin = 0, ymax = 80,
               fill = NA, colour = "#1E1E1EFF", size = 0.5) +
      annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62,
               fill = NA, colour = "#1E1E1EFF", size = 0.5) +
      annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "#1E1E1EFF", size = 0.5) +
      annotate("rect", xmin = 120, xmax = 120.5, ymin =36, ymax = 44,
               fill = NA, colour = "#1E1E1EFF", size = 0.5) +
      annotate("segment", x = 120, xend = 120, y = 0, yend = 80,
               colour = "#1E1E1EFF", size = 0.5) +
      annotate("point", x = 108 , y = 40, colour = "#1E1E1EFF", size = 1.05) +
      annotate("point", x = 60 , y = 40, colour = "#1E1E1EFF", size = 1.05) +
      annotate("path", x=60+10*cos(seq(0,2*pi,length.out=2000)), size = 0.5,
               y=40+10*sin(seq(0,2*pi,length.out=2000)), col="#1E1E1EFF") +
      annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)),
               size = 0.5, y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)),
               col="#1E1E1EFF")+coord_cartesian(xlim = c(60, 120))+
      geom_point(data = shot_data, aes(x = location.x, y = location.y, color = shot.statsbomb_xg, shape = shot.body_part.name),size = 8)+
      scale_color_gradientn(colours = shotmapxgcolors, limit = c(0,0.8))+theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(),axis.line = element_line()
      ) +theme_void()
    ggarrange(p, p2, ncol = 2)
  })
  
  
  output$my_chart9 <- renderPlot({
    ggplot(utility %>% filter(Team==input$team_name) %>% arrange(desc(Utility)), aes(x = Player, y = Utility, fill = color)) +
      geom_bar(stat = "identity",show.legend = FALSE) +
      scale_fill_manual(values = utility %>% filter(Team==input$team_name) %>% pull(color) %>% unique()) +
      theme(panel.grid = element_blank()) +
      ggtitle(paste("Top 3 Defensive Scores (Utilities) by Player for",input$team_name))+theme_classic()+
      theme(axis.text.x = element_text(size=16,angle = 25,vjust = 0.4),
            plot.title = element_text(size=23),
            axis.text.y = element_text(size=16))
    
  })
  
  
}

# Run the dashboard application
shinyApp(ui, server)
