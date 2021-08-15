library(shiny)
library(shinyWidgets)
library(shinythemes)
library(bslib)
library(thematic)
library(shinycssloaders)
library(rsconnect)
library(plyr)
library(mlr)
library(lubridate)
library(plotly)
library(DT)
library(raster)
library(sf)
library(tmap)
library(mapview)
library(rgdal)
library(patchwork)
library(tidyverse)


# --------Import data
# 
# car_ass <- read_csv("Deliverables/ShinyApp/data/car-assignments.csv")
# #gps <- read_csv("Deliverables/ShinyApp/data/gps.csv")
# #saveRDS(gps, file = "Deliverables/ShinyApp/data/gps_rds")
# cc <- read_csv("Deliverables/ShinyApp/data/cc_data.csv",
#            locale = locale(encoding = "windows-1252"))
# loyalty <- read_csv("Deliverables/ShinyApp/data/loyalty_data.csv",
#                    locale = locale(encoding = "windows-1252"))
# gps <- readRDS("Deliverables/ShinyApp/data/gps_rds") %>%
#   filter(id < 100)
# map_hourcolor <- read_csv("Deliverables/ShinyApp/data/map_hourcolor.csv")
# cb_cc_loy <- read_csv("Deliverables/ShinyApp/data/combine_cc_loyalty.csv", locale = locale(encoding = "windows-1252"))

## For Shiny
car_ass <- read_csv("data/car-assignments.csv")
gps <- readRDS("data/gps_rds") %>%
    filter(as.character(id) %in% c("1","3","4","5","6","7","8","9","11","13","14","16",
                     "18","21","22","24","25","33"))
cc <- read_csv("data/cc_data.csv",
               locale = locale(encoding = "windows-1252"))
loyalty <- read_csv("data/loyalty_data.csv",
                    locale = locale(encoding = "windows-1252"))
map_hourcolor <- read_csv("data/map_hourcolor.csv")
cb_cc_loy <- read_csv("data/combine_cc_loyalty.csv",
                      locale = locale(encoding = "windows-1252"))


# --------Edit columns
## convert to standard datetime
gps$Timestamp = mdy_hms(gps$Timestamp)
cc$timestamp = mdy_hm(cc$timestamp)
loyalty$timestamp = mdy(loyalty$timestamp)
cb_cc_loy$timestamp = dmy_hm(cb_cc_loy$timestamp)
cb_cc_loy$date_new = dmy(cb_cc_loy$date_new)

## Revise data type
car_ass$CarID = as.character(car_ass$CarID)
gps$id = as.character(gps$id)
cc$last4ccnum = as.character(cc$last4ccnum)

## Split datetime
cc$day = day(cc$timestamp)
cc$weekday = wday(cc$timestamp, label = T, abbr = T)
cc$hour = hour(cc$timestamp)
loyalty$day = day(loyalty$timestamp)
loyalty$weekday = wday(loyalty$timestamp, label = T, abbr = T)
gps$day = as.factor(day(gps$Timestamp))
gps$weekday = wday(gps$Timestamp, label = T, abbr = T)
gps$hour = as.factor(hour(gps$Timestamp))


# --------Combine cc and loyalty data
cards <- cc %>%
    dplyr::select(-hour) %>%
    dplyr::rename(cardnum = last4ccnum) %>%
    dplyr::mutate(card = "Credit Card") %>%
    rbind(loyalty %>%
              dplyr::rename(cardnum = loyaltynum) %>%
              mutate(card = "Loyalty Card")) 


# ---------For GPS tracking
## Background Map
bgmap <- raster("data/MC2-tourist.jpg")
#Abila_st <- st_read(dsn = "data/Geospatial", layer = "Abila")
xmin(bgmap) = 24.82401
xmax(bgmap) = 24.90997
ymin(bgmap) = 36.04502
ymax(bgmap) = 36.09492

## Convert GPS spatial data into a Simple Feature (SF) data frame
gps_sf <- st_as_sf(gps, 
                   coords = c("long", "lat"),
                   crs = 4326)

## Group by id and day
gps_path <- gps_sf %>%
    # filter(id %in% c("1","3","4","5","6","7","8","9","11","13","14","16",
    #                  "18","21","22","24","25","33")) %>%
    dplyr::group_by(id, day) %>%
    dplyr::summarize(m = mean(Timestamp), 
                     do_union=FALSE) %>%
    st_cast("LINESTRING")

np = npts(gps_path, by_feature = T)
gps_path2 <- cbind(gps_path, np) %>%
    dplyr::filter(np > 1) # exclude orphan coordinate records

## Car ID
# car_id <- as.character(sort(unique(c(as.integer(gps$id)))))
car_id <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15",
            "16","17","18","19","20","21","22","23","24","25","26","27","28",
            "29","30","31","32","33","34","35")

# --------Join owner matching result between car assignment and cards number
# owner_match_ori <- read_csv("Deliverables/ShinyApp/data/car-card-match.csv") %>%
#     dplyr::rename("Credit card num" = "CC", "Loyalty card num" = "Loyalty")

owner_match_ori <- read_csv("data/car-card-match.csv") %>%
    dplyr::rename("Credit card num" = "CC", "Loyalty card num" = "Loyalty")

owner_match_ori$`Credit card num` = as.character(owner_match_ori$`Credit card num`)
owner_match_ori$CarID = as.factor(owner_match_ori$CarID)

owner_match <- left_join(car_ass, owner_match_ori, by = "CarID", 
                         na_matches = "never") %>%
    dplyr::rename("Current Employment Type" = "CurrentEmploymentType",
                  "Current Employment Title" = "CurrentEmploymentTitle",
                  "Last Name" = "LastName", "First Name" = "FirstName")
owner_match <- owner_match[c(3,6,7,1,2,4,5)] %>%
    filter(is.na(CarID) == F)
owner_match$CarID = factor(owner_match$CarID, levels = c(car_id))

matchres <- merge(owner_match_ori, cc, 
                  by.x = "Credit card num", by.y = "last4ccnum", 
                  all = T)
matchres <- merge(matchres, loyalty,
                  by.x = "Loyalty card num", by.y = "loyaltynum",
                  all = T)
names(matchres)[4] <- "timestamp_cc"
names(matchres)[5] <- "location_cc"
names(matchres)[6] <- "price_cc"
names(matchres)[10] <- "timestamp_loy"
names(matchres)[11] <- "location_loy"
names(matchres)[12] <- "price_loy"


# --------Join transaction data of credit card and loyalty card
cb_cc_loy$last4ccnum = as.character(cb_cc_loy$last4ccnum)
cb_cc_loy <- cb_cc_loy %>%
    select(-c(1,6))

cbccloy <- left_join(cb_cc_loy, owner_match_ori, 
                     by= c("last4ccnum" = "Credit card num",
                           "loyaltynum" = "Loyalty card num"), 
                     na_matches = "never") %>%
    rename("Datatime (credit card)" = "timestamp", "Location" = "location",
           "Price" = "price", "Credit card num" = "last4ccnum",
           "Loyalty card num" = "loyaltynum", "Date (loyalty card)" = "date_new")

#cbccloy$`Date (loyalty)` = dmy(cbccloy$`Date (loyalty)`)
cbccloy$`Weekday (loyalty card)` = wday(cbccloy$`Date (loyalty card)`, label = T, abbr = T)
cbccloy$`Weekday (credit card)` = wday(cbccloy$`Datatime (credit card)`, label = T, abbr = T)

cbccloy <- cbccloy[c(2,3,4,1,9,5,6,8,7)]


# --------All global parameters
## Card Number
cardnum <- sort(unique(c(cards$cardnum)))
cc_num <- sort(unique(c(cc$last4ccnum)))
loy_num <- sort(unique(c(loyalty$loyaltynum)))

## Weekday
wd <- c("Mon", "Tue", "Wed", "Thu", "Fri")
wnd <- c("Sat", "Sun")
wwd <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

## date
day1 <- unique(c(cards$day))

## hour
hour1 <- unique(c(cc$hour))



# --------Self define function
hmcolor <- function(cardtype){
    if(cardtype == "Loyalty Card" ){
        return("Greens")
    }
    else{
        return("Blues")
    }
}


# ------------------------------------------------------------------------------
# --------Define UI for application
ui <- fluidPage(#shinythemes::themeSelector(),
    theme = bslib::bs_theme(bootswatch = "cosmo"),
    #theme = shinytheme("simplex"),
    
    # ------Navigation Bar
    navbarPage("EATTGE", 
               fluid = T, windowTitle="VAST Challenge 2021 Mini Challenge 2", 
               selected="instr",
               
               # ------Panel Overview
               tabPanel("Instruction", value = "instr", fluid = T, icon = icon("book-open"),
                        titlePanel(tags$b("An Application for Exploring Abnormal Trajectories and Transactions of GAStech Employees")),
                        tags$hr(),
                        fluidRow(column(width = 6,
                                        # hr(),
                                        h4(tags$b("About")),
                                        p("The IEEE Visual Analytics Science and Technology (VAST) 
                                          Challenge is an annual contest of which the goal is to 
                                          advance the field of visual analytics through the competition. 
                                          2021 Mini Vast Challenge 2 presented the scenario: some 
                                          Kronos-based employees of GAStech oil and gas company 
                                          went missing. Car tracking data for the two weeks leading 
                                          up to the disappearance, as well as credit card transactions 
                                          and loyalty card usage data related to missing employees are provided."),
                                        p("For more information on 2021 Mini Vast Challenge 2, please visit",
                                            tags$a(href= "https://vast-challenge.github.io/2021/MC2.html","the official website"),
                                          "."),
                                        p("This Shiny App is designed as a visualization tool for
                                          GAStech employees's GPS track and transaction data, so that 
                                          the users not only could get an overview of data distribution, 
                                          but also do exploration and analysis to derive deeper insights on data.")),
                                 column(width = 6,
                                        # hr(),
                                        h4(" "),
                                        br(),
                                        br(),
                                        tags$img(src="vastlogo.jpg", width = 300))
                                 ),
                        fluidRow(column(width = 6,
                                        verticalLayout(
                                            h4(tags$b("Application feature")),
                                            p("As shown in the figure on the right, the application consists 4 panels:"),
                                            p("The Exploratory Data Analysis panel enables users to perform analysis on 
                                            transaction data of credit and loyalty card, observe the popular spots and 
                                            detect abnormal activities."),
                                            p("The GPS panel provide the trajactory maps and heatmap to visualize the GPS track data of selected cars and date."),
                                            p("The Owner Matching panel shows the correspondence between owners of cars 
                                            and credit/loyalty cards, as well as charts for exploration."),
                                            p("The Data panel provides a data table for details of individual transaction record, 
                                            in which the credit card and loyalty card records are join by matching card number, location, price and date.")
                                        ),
                                        verticalLayout(
                                            h4(tags$b("User Guide")),
                                            p("To optimise your user experience, please refer to our",
                                              tags$a(href="https://isss608vaag2group3.netlify.app/posts/2021-08-04-user-guide/",
                                                     tags$i("User Guide")),
                                              ".")
                                        )
                                        ),
                                 column(width = 6,
                                        h4(" "),
                                        tags$img(src="overview.png", width = 600)
                                        )
                                 )
                        # ,
                        # fluidRow(column(width = 6,
                        #                 h4(tags$b("User Guide")),
                        #                 p("To optimise your user experience, please refer to our",
                        #                   tags$a(href="https://isss608vaag2group3.netlify.app/posts/2021-08-04-user-guide/",
                        #                          tags$i("User Guide")),
                        #                   ".")
                        #                 )
                        #          )
                    
               ),
               
               
               # ------Panel EDA
               tabPanel("EDA", value = "eda", fluid = T, icon = icon("chart-bar"),
                        titlePanel(tags$b("Exploratory Data Analysis for transaction data of credit and loyalty cards")),
                        
                        sidebarLayout(position = "left", fluid = T, 
                                      sidebarPanel(width = 3, fluid = T,
                                                   
                                                   # ----Heatmap option in tab1
                                                   radioGroupButtons("cardtypet1",
                                                                     "Select card type",
                                                                     choices = c("Credit Card",
                                                                                 "Loyalty Card"),
                                                                     selected = "Credit Card",
                                                                     status = "outline-dark"),
                                                   
                                                   # ----Boxplot option in tab1
                                                   pickerInput("ccnumt1",
                                                               "Select Credit Card number",
                                                               choices = cc_num,
                                                               multiple = T,
                                                               selected = cc_num,
                                                               options = list(`actions-box` = TRUE,
                                                                              `live-search` = TRUE,
                                                                              style = "btn-secondary",
                                                                              size = 10)),
                                                   
                                                   pickerInput("loynumt1",
                                                               "Select Loyalty Card number",
                                                               choices = loy_num,
                                                               multiple = T,
                                                               selected = loy_num,
                                                               options = list(`actions-box` = TRUE,
                                                                              `live-search` = TRUE,
                                                                              style = "btn-secondary",
                                                                              size = 10)),
                                                   
                                                   awesomeCheckboxGroup("wdayt1", "Select Weekday",
                                                                        choices = c("Weekday", "Weekend"),
                                                                        selected = c("Weekday", "Weekend"),
                                                                        status = "secondary",
                                                                        inline = T),
                                                   
                                                   # awesomeCheckboxGroup("wdayt1", "Select Weekday",
                                                   #                      choices = c("Mon", "Tue", "Wed", "Thu", "Fri","Sat", "Sun"),
                                                   #                      #choiceNames = list("Weekday", "Weekend"),
                                                   #                      #choiceValues = list(c("Mon", "Tue", "Wed", "Thu", "Fri"), c("Sat", "Sun")),
                                                   #                      selected = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                                                   #                      status = "secondary",
                                                   #                      inline = T),
                                                   
                                                   #pickerInput("wdayt1", "Select weekday",
                                                   #            choices = list(weekday = wd, weekend = wnd),
                                                   #            multiple = T,
                                                   #            selected = wwd,
                                                   #            options = list(`actions-box` = TRUE)),
                                                   
                                                   pickerInput("dayt1",
                                                               "Select the date of Jan",
                                                               choices = day1,
                                                               multiple = T,
                                                               selected = day1,
                                                               options = list(`actions-box` = TRUE,
                                                                              style = "btn-secondary",
                                                                              size = 14))
                                      ),
                                      
                                      
                                      mainPanel(width = 9, fluid = T,
                                                # ----Heatmap plotting in tab1
                                                h4("Heatmap of Transaction Frequency"),
                                                
                                                fluidRow(
                                                    column(width = 6,
                                                           verticalLayout(htmlOutput("heat_bd"),
                                                                          plotlyOutput("heatt1"))
                                                    ),
                                                    column(width = 6,
                                                           verticalLayout(htmlOutput("heat_bh"),
                                                                          plotlyOutput("heatt1_hour"))
                                                    )
                                                ),
                                                
                                                
                                                # ----Boxplot plotting in tab1
                                                fluidRow(column(width = 12,
                                                                hr(),
                                                                h4("Box Plot of Transaction Price by Location"),
                                                                plotlyOutput("boxt1"))
                                                )
                                      )
                        )
                        
               ),
               
               # ------Panel GPS
               tabPanel("GPS", value = "gps", fluid = T, icon = icon("compass"),
                        titlePanel(tags$b("Analysis on movement path of cars assigned by GASTech")),
                        
                        sidebarLayout(position = "left", fluid = T,
                                      sidebarPanel(width = 3, fluid = T,
                                                   conditionalPanel('input.GPSset === "GPS Movement Path"',
                                                                    pickerInput("cidt2", "Select a CarID",
                                                                                # choices = car_id,
                                                                                choices = c("1","3","4","5","6","7","8","9","11","13","14","16",
                                                                                            "18","21","22","24","25","33"),
                                                                                selected = "1",
                                                                                options = list(`live-search` = TRUE,
                                                                                               size = 10)),
                                                                    pickerInput("dayt2", "Select the date of Jan",
                                                                                choices = day1,
                                                                                multiple = T,
                                                                                selected = day1,
                                                                                options = list(`actions-box` = TRUE,
                                                                                               size = 14))
                                                                    
                                                   )
                                                   ,
                                                   conditionalPanel('input.GPSset === "GPS Data Overview"',
                                                                    awesomeCheckboxGroup("wdayt2", "Select Weekday",
                                                                                         choices = c("Weekday", "Weekend"),
                                                                                         selected = "Weekend",
                                                                                         status = "secondary",
                                                                                         inline = T)
                                                   )
                                      ),
                                      mainPanel(width = 9, fluid = T,
                                                tabsetPanel(id='GPSset',type = "tabs",
                                                            
                                                            tabPanel(title = "GPS Movement Path",
                                                                     # ----GPS movement path plot on map
                                                                     
                                                                     fluidRow(column(width = 6,
                                                                                     h4("The GPS track of selected Car ID in specific date(s)"),
                                                                                     h6(tags$i("Distinguish colors by date")),
                                                                                     tmapOutput("gps1",
                                                                                                width = "100%",
                                                                                                height = "650px"),
                                                                                     hr(),
                                                                                     p(tags$i("Note: As the Memory limitation of free Shiny App server, after deploying, 
                                                                                              the Shiny APP can't afford displaying GPS movement lines for all cars. 
                                                                                              Thus we filtered GPS tracking data and retained several typical car IDs data 
                                                                                              to generate the key movement lines for further exploration and analysis."))
                                                                                     ),
                                                                     column(width = 6,
                                                                            h4("The GPS track of selected Car ID in specific date(s)"),
                                                                            h6(tags$i("Distinguish colors by timeslot")),
                                                                            tmapOutput("gps2",
                                                                                       width = "100%",
                                                                                       height = "650px"
                                                                                       ),
                                                                            dataTableOutput("map_color", width = "100%", height = "300px")
                                                                            )
                                                                     )
                                                            )
                                                            ,
                                                            
                                                            # ----GPS data heatmap plot
                                                            
                                                            tabPanel(title = "GPS Data Overview", 
                                                                     h4("Hourly heatmap of different car tracking data, ID by hour"),
                                                                     fluidRow(column(width = 12,
                                                                                     plotlyOutput("heatt2",
                                                                                                  width = "800px",
                                                                                                  height = "600px"
                                                                                     ),
                                                                                     hr(),
                                                                                     p(tags$i("Note: As the Memory limitation of free Shiny App server, after deploying, 
                                                                                              the Shiny APP can't afford displaying GPS movement lines for all cars. 
                                                                                              Thus we filtered GPS tracking data and retained several typical car IDs data 
                                                                                              to generate the key movement lines for further exploration and analysis."))
                                                                     )
                                                                     )
                                                                     
                                                            )
                                                )
                                      )
                        )
               ),
               
               # ------Panel Matching
               tabPanel("Owner Matching", value = "match", fluid =T, icon = icon("phabricator"),
                        titlePanel(tags$b("Correspondence between car owner and credit card/loyalty card holder")),
                        
                        fluidRow(column(width = 6,
                                        p(tags$i("*Tip: click the row in the data table to see specific heatmap and line chart for selected employee")),
                                        DT::dataTableOutput("matcht3" #, height = "650px"
                                        )                # left side
                        ),
                        column(width = 6,
                               plotlyOutput("heatt3", height = "350px"),              # right side
                               hr(),
                               radioGroupButtons("cardtypet3",
                                                 "Select card type",
                                                 choices = c("Credit Card",
                                                             "Loyalty Card"),
                                                 selected = "Credit Card",
                                                 status = "outline-dark"),
                               plotlyOutput("linet3", height = "300px" )
                        )
                        )
               ),
               
               # ------Panel Transaction Data
               tabPanel("Data", value = "datatable", fluid = T, icon = icon("database"),
                        titlePanel(tags$b("Transaction data table of credit card and loyalty card")),
                        
                        fluidRow(column(width = 12,
                                        DT::dataTableOutput("cbccloy_t4", 
                                                            width = "100%", 
                                                            height = "700px"
                                                            )
                                        ))
                   
               )
               
               
    )
    
)


# ------------------------------------------------------------------------------
# --------Define server logic
server <- function(input, output, session) {
    
    
    # ------Heatmaps in tab 1
    output$heatt1 <- renderPlotly({
        thematic::thematic_shiny()
        
        d1 <- filter(cards,
                     card %in% input$cardtypet1,
                     cardnum %in% input$ccnumt1 | cardnum %in% input$loynumt1,
                     day %in% input$dayt1)
        
        if(length(input$wdayt1) == 1) {
            
            if (c(input$wdayt1) == c("Weekend")){
                d1 <- d1 %>%
                    dplyr::filter(weekday %in% wnd)
            }
            else if (c(input$wdayt1) == c("Weekday")) {
                d1 <- d1 %>% 
                    dplyr::filter(weekday %in% wd)
            } }
        
        else if(length(input$wdayt1) == 2){
            d1
        }

        plot_ly(data = d1, x = ~as.factor(day), y = ~location,
                hovertemplate = paste(
                    " %{yaxis.title.text}: %{y}<br>",
                    "%{xaxis.title.text}: %{x}<br>",
                    "Transaction Count: %{z}",
                    "<extra></extra>")) %>%
            add_histogram2d(colors = hmcolor(input$cardtypet1)) %>%
            layout(xaxis = list(title = "Date of Jan", tickmode = "linear"),
                   yaxis = list(title = "Location", tickmode = "linear"))
    })
    
    output$heatt1_hour <- renderPlotly({
        thematic::thematic_shiny()
        
        d2 <- filter(cc,
                     last4ccnum %in% input$ccnumt1,
                     day %in% input$dayt1)
        
        if(length(input$wdayt1) == 1) {
            
            if (c(input$wdayt1) == c("Weekend")){
                d2 <- d2 %>%
                    dplyr::filter(weekday %in% wnd)
            }
            else if (c(input$wdayt1) == c("Weekday")) {
                d2 <- d2 %>% 
                    dplyr::filter(weekday %in% wd)
            } }
        
        else if(length(input$wdayt1) == 2){
            d2
        }
        
        plot_ly(data = d2, x = ~as.factor(hour), y = ~location,
                hovertemplate = paste(
                    " %{yaxis.title.text}: %{y}<br>",
                    "%{xaxis.title.text}: %{x}<br>",
                    "Transaction Count: %{z}",
                    "<extra></extra>")) %>%
            add_histogram2d(colors = "Blues") %>%
            layout(xaxis = list(title = "Hour of Day", tickmode = "linear"),
                   yaxis = list(title = "Location", tickmode = "linear"))
    })
    
    output$heat_bd <- renderText({
        paste("<font color=\"#808080\">","Location by date for", 
              "<i>","<u>", input$cardtypet1, "</u>","</i>","</font>")
    })
    
    output$heat_bh <- renderText({
        paste("<font color=\"#808080\">","Location by hour for", 
              "<i>","<u>", "Credit Card", "</u>","</i>","</font>")
    })
    
    # ------Boxplot in tab 1
    output$boxt1 <- renderPlotly({
        thematic::thematic_shiny()
        
        d <- dplyr::filter(cards,
                           cardnum %in% input$ccnumt1 | cardnum %in% input$loynumt1,
                           day %in% input$dayt1)
        
        if(length(input$wdayt1) == 1) {
            
            if (c(input$wdayt1) == c("Weekend")){
                d <- d %>%
                    dplyr::filter(weekday %in% wnd)
            }
            else if (c(input$wdayt1) == c("Weekday")) {
                d <- d %>% 
                    dplyr::filter(weekday %in% wd)
            } }
        
        else if(length(input$wdayt1) == 2){
            d
        }
        
        plot_ly(data = d,x = ~location, y= ~price, 
                color = ~card, colors = "Paired",
                type = 'box', boxmean = T) %>%
            layout(#title = "Box Plot of Transaction Price by Location",
                xaxis = list(title = "Location"),
                yaxis = list(title = "Price"),
                boxmode = "group")
    })
    
    
    # ------GPS by day in tab 2
    
    
    output$gps1 <- renderTmap({
        thematic::thematic_shiny()
        
        gps_path_selected <- dplyr::filter(gps_path2,
                                           id == input$cidt2,
                                           day %in% input$dayt2)
        
        tmap_mode("view")
        tm_shape(bgmap) +
            tm_rgb(r=1, g=2, b=3,
                   alpha = NA, saturation = 1,
                   interpolate = T,
                   max.value = 255) +
            tm_shape(gps_path_selected) +
            tm_lines(col = "day", palette = "Dark2")
    })
    
    # ------GPS by hour in tab 2
    
    output$gps2 <- renderTmap({
        thematic::thematic_shiny()
        
        gps_id <- gps_sf %>%
            filter(id == input$cidt2 & day == input$dayt2)
        
        gps_default <- gps_sf %>%
            filter(id == 1, day == 6, hour == 23) %>%
            select(geometry)
        
        gps_earlymorning <- gps_id %>% 
            filter(hour(Timestamp) < 6) %>% 
            select(geometry)
        
        if(empty(gps_earlymorning)){
            gps_earlymorning <- gps_default
        }
        
        gps_morning <- gps_id %>% 
            filter(hour(Timestamp) >= 6 & hour(Timestamp) <= 8) %>% 
            select(geometry)
        
        if(empty(gps_morning)){
            gps_morning <- gps_default
        }
        
        gps_mornwork <- gps_id %>% 
            filter(hour(Timestamp) > 8 & hour(Timestamp) < 10) %>% 
            select(geometry)
        
        if(empty(gps_mornwork)){
            gps_mornwork <- gps_default
        }
        
        gps_noon <- gps_id %>% 
            filter(hour(Timestamp) >= 10 & hour(Timestamp) <= 14) %>% 
            select(geometry)
        
        if(empty(gps_noon)){
            gps_noon <- gps_default
        }
        
        gps_afternoon <- gps_id %>% 
            filter(hour(Timestamp) > 14 & hour(Timestamp) <= 16) %>% 
            select(geometry)
        
        if(empty(gps_afternoon)){
            gps_afternoon <- gps_default
        }
        
        gps_backhome <- gps_id %>% 
            filter(hour(Timestamp) == 17) %>% 
            select(geometry)
        
        if(empty(gps_backhome)){
            gps_backhome <- gps_default
        }
        
        gps_evening <- gps_id %>% 
            filter(hour(Timestamp) > 17 & hour(Timestamp) <= 21) %>% 
            select(geometry)
        
        if(empty(gps_evening)){
            gps_evening <- gps_default
        }
        
        gps_midnight <- gps_id %>% 
            filter(hour(Timestamp) > 21) %>% 
            select(geometry)
        
        if(empty(gps_midnight)){
            gps_midnight <- gps_default
        }
        
        siz = 0.03
        sie = 0.08
        tmap_mode("view")
        tm_shape(bgmap) +
            tm_rgb(bgmap, r = 1, g = 2, b = 3,
                   alpha = NA,
                   saturation = 1,
                   interpolate = TRUE,
                   max.value = 255) +
            tm_shape(gps_earlymorning) +
            tm_dots(col = "red", size = sie, alpha = 0.8) +
            tm_shape(gps_morning) +
            tm_dots(col = "gray", size = siz, alpha = 0.3) +
            tm_shape(gps_mornwork) +
            tm_dots(col = "orange", size = sie, alpha = 0.8) +
            tm_shape(gps_noon) +
            tm_dots(col = "blue", size = siz, alpha = 0.3) +
            tm_shape(gps_afternoon) +
            tm_dots(col = "yellow", size = sie, alpha = 0.8) +
            tm_shape(gps_backhome) +
            tm_dots(col = "green", size = siz, alpha = 0.3) +
            tm_shape(gps_evening) +
            tm_dots(col = "purple", size = siz, alpha = 0.3) +
            tm_shape(gps_midnight) +
            tm_dots(col = "black", size = sie, alpha = 0.3)
        
    })
    
    
    output$map_color <- renderDataTable({
        thematic::thematic_shiny()
        DT::datatable(map_hourcolor,
                      filter = c("none"), selection = c("none"), class = "hover",
                      caption = "*Mapping of colors in each time slot of the day ",
                      options = list(dom = "t")
                      )
        
    })
    
    # ------Heatmap in tab2
    output$heatt2 <- renderPlotly({
        
        # gps_sf <- filter(gps_sf, id %in% c("1","3","4","5","6","7","8","9","11","13","14","16",
        #                                    "18","21","22","24","25","33")
        #                      )
        
        if(length(input$wdayt2) == 1) {
            
            if (c(input$wdayt2) == c("Weekend")){
                gps_sf_w <- gps_sf %>%
                    dplyr::filter(weekday %in% wnd)
                }
            else if (c(input$wdayt2) == c("Weekday")) {
                gps_sf_w <- gps_sf %>% 
                    dplyr::filter(weekday %in% wd)
                } }
        
        else if(length(input$wdayt2) == 2){
            gps_sf_w <- gps_sf
            }
        
        #if (input$gpshour_daynum != "ALL"){
        #    gps_sf_w <- gps_sf_w %>% 
        #        filter(gps_sf_w$md == input$gpshour_daynum)
        #}
        
        hm_gps <- plot_ly(gps_sf_w, x = ~hour, y = ~factor(id, 
                                                          levels = c("1","3","4","5","6","7","8","9","11","13","14","16",
                                                                     "18","21","22","24","25","33")), #Car ID order
                          hovertemplate = paste(
                              " %{yaxis.title.text}: %{y}<br>",
                              "%{xaxis.title.text}: %{x}<br>",
                              "Record Count: %{z}",
                              "<extra></extra>")) %>% 
            add_histogram2d(colors = "Blues") %>%
            layout(
                xaxis = list(title = "Hour", tickmode = "linear"),
                yaxis = list(title="Car ID", tickmode = "linear"))
        hm_gps
    }) 
    
    
    # ------Matching result
    output$matcht3 <- renderDataTable({
        thematic::thematic_shiny()
        DT::datatable(owner_match, filter = c("top"),
                      class = "hover", rownames = F,
                      extensions= "Scroller",
                      options = list(dom = "t", scrollX = TRUE,
                          scroller=TRUE, scrollY=570, deferRender=TRUE,
                          search = list(regex = TRUE)
                      )
        )
    }, server = FALSE)
    
    output$heatt3 <- renderPlotly({
        thematic::thematic_shiny()
        
        s <- input$matcht3_rows_selected
        if(is.null(s)) return(NULL)
        
        s_carid <- filter(owner_match, row_number() %in% s)$CarID
        
        # if(length(s_carid) == 0) {s_carid <- owner_match$CarID}
        
        gps_sf_temp <- gps_sf %>% 
            filter(id %in% s_carid)
        
        if(empty(gps_sf_temp)) return(NULL)
        
        else{
        plot_ly(gps_sf_temp, x = ~day, y = ~hour,
                hovertemplate = paste(
                    " %{yaxis.title.text}: %{y}<br>",
                    "%{xaxis.title.text}: %{x}<br>",
                    "Record Count: %{z}",
                    "<extra></extra>")) %>%
            add_histogram2d(colors = "Blues") %>%
            layout(title = "Heatmap of GPS tracking data of the selected car(s), hour by date",
                   xaxis = list(title = "Date", tickmode = "linear"),
                   yaxis = list(title="Hour", tickmode = "linear")
            )}
        
    })
    
    output$linet3 <- renderPlotly({
        thematic::thematic_shiny()
        
        s <- input$matcht3_rows_selected
        if(is.null(s)) return(NULL)
        
        s_carid <- filter(owner_match, row_number() %in% s)$CarID
        
        # if(length(s_carid) == 0) {s_carid <- owner_match$CarID}
        
        match_selected <- matchres %>% 
            filter(CarID %in% s_carid)
        
        if (input$cardtypet3 == "Credit Card"){
            linec <- plot_ly(data = match_selected, x = ~timestamp_cc, y = ~price_cc, color = ~location_cc,
                             colors = "Accent", type = 'scatter', mode = 'lines+markers') %>%
                layout(title = "Credit card transactions of selected employee(s), price by date",
                       xaxis = list(title = "Timestamp"),
                       yaxis = list(title = "Price"))
            
            #linec <- ggplot(data = match_selected, aes(x = timestamp_cc, y = price_cc, color = location_cc)) +
            #    geom_line() +
            #    geom_point() +
            #    labs(x = "Timestamp", y = "Price", color = "Location") +
            #    ggtitle("Credit card transactions of the selected cars")
        } else {
            linec <- plot_ly(data = match_selected, x = ~timestamp_loy, y = ~price_loy, color = ~location_loy,
                             colors = "Accent", type = 'scatter', mode = 'lines+markers') %>%
                layout(title = "Loyalty card transactions of selected employee(s), price by date",
                       xaxis = list(title = "Timestamp"),
                       yaxis = list(title = "Price"))
            
            #linec <- ggplot(data = match_selected, aes(x = timestamp_loy, y = price_loy, color = location_loy)) +
            #    geom_line() +
            #    geom_point() +
            #    labs(x = "Timestamp", y = "Price", color = "Location") +
            #    ggtitle("Loyalty card transactions of the selected cars") +
            #    theme(plot.title = element_text(hjust = 0.5)) +
            #    theme_light()
        }
        
        linec
    })
    
    
    # ------Combined CC and Loy cards transaction result
    output$cbccloy_t4 <- renderDataTable({
        thematic::thematic_shiny()
        DT::datatable(cbccloy, filter = c("top"),
                      class = "hover", #rownames = F,
                      #extensions= "Scroller",
                      options = list(dom = "rtip", scrollX = TRUE,
                                     autoWidth = TRUE, pageLength = 12,
                                     columnDefs = list(list(className = "dt-right", targets = "_all")),
                                     search = list(regex = TRUE)
                                     #scroller=TRUE, scrollY=570, deferRender=TRUE
                                     )
        )
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
