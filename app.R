## Import necessary libraries


library(shiny)
library(shinydashboard)
library(plotly)
library(shinythemes)
library(data.table)
library(shinyWidgets)
library(tmap)
library(readxl)
library(sf)
library(dplyr)
library(shinycssloaders)
library(tidyverse)
library(lubridate)

######################## Functions ##################
source("functions/districtMapSL.R") ## TODO :: Add functions here -Done 
source("functions/clusterPieChart.R")
source("functions/cumulative_plot.R")
source("functions/districtLinePlot.R")

############################################# Import the datasets and rerrange #################################################
cummulativeData<-read.csv("Data/Cummulative Data/SL_cum_counts.csv")
cummulativeData$Date<-as.Date(cummulativeData$Date,format="%m/%d/%y")


clusterData<-read.csv("Data/Cluster_classified_data/cluster_data.csv")
clusterData$Date<-as.Date(clusterData$Date,format="%m/%d/%y")
colnames(clusterData)<-c("Date","infectedCount","districtSum","importedCase","otherCluster")

#districtData_map<-read_excel("Data/District_confirmed_data/District_data_Nov14_Apr21_transpose.xlsx",
#                            col_names = FALSE)



date_seq<-seq.Date(from = as.Date("2020-11-13"), to=as.Date("2021-04-21"), by="day")


#colnames(districtData_map)<-c(date_seq)


#colnames(districtData_map)[1]<-"DISTRICT"

##############################################################################
covid19DistrictCases <- read_csv(file = "Data/District_confirmed_data/District_data_Nov14_Apr21_transpose.csv")

colnames(covid19DistrictCases)
colnames(covid19DistrictCases)[19]<-"Monaragala"
covid19DistrictCases %>% 
    gather("district", "count", -Date) -> covid19DistrictCasesLong

mapSL <- st_read("Data/Shape_files/lka_admbnda_adm2_slsd_20200305.shp") 

mapSL <- mapSL %>% 
    select(ADM2_EN) %>% 
    filter(ADM2_EN != "[unknown]")



#mapSL[which(mapSL$ADM2_EN=="Monaragala",arr.ind=TRUE),"ADM2_EN"]<-"Moneragala"

mapSLCovid19 <-   left_join(mapSL, covid19DistrictCasesLong, by = c("ADM2_EN" = "district"))

mapSLCovid19 %>% mutate(Date = mdy(Date)) -> mapSLCovid19

#SLShapeDataframe<-st_read("Data/Shape_files/lka_admbnda_adm2_slsd_20200305.shp")%>% dplyr::select(ADM2_EN,geometry)
## Remove unknown district

# SLCoordFrame<-SLShapeDataframe %>% filter(!(ADM2_EN=="[unknown]")) 
# 
# colnames(SLCoordFrame)[1]<-"DISTRICT"
# ## Join with distribution data
# #covidDistrictsSL<-left_join(SLCoordFrame,districtData_map,by=c("DISTRICT"))
# 
colnames(mapSLCovid19)<-c("District","Date","Count","geometry")
covidDistrictsSL<-mapSLCovid19



districtLineData<-read_excel("Data/District_confirmed_data/District_data_Nov14_Apr21_transpose.xlsx",
                             col_names = TRUE)
districtLineData$Date<-as.Date(districtLineData$Date,format="%y-%m-%d")


############################################### Data preprocessing ######################################
clusterData_min_date = as.Date(min(clusterData$Date),"%y-%m-%d")
clusterData_max_date = as.Date(max(clusterData$Date),"%y-%m-%d")

############################################## CSS themes ###############################################
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)


####################################### SHINY APP FUNCTIONS ###############################################


# Define UI for application 
ui <-tagList(
    bootstrapPage(

   # tags$head(includeHTML("gtag.html")),
    navbarPage(theme = shinytheme("cerulean"), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">COVID-19 SL tracker</a>'), id="nav",
               windowTitle = "COVID-19 SL tracker",
               
               
     
            
          
                ###################################Page 1##############################
                tabPanel( "Home",
                       
                        
                        h1("Introduction to the app.Highlight the visualization of clusters and districs"),
                        h2("Objectives"),
                        h2("Refenrences used"),
                        h2("Extensions in progress")
                        
                ),
            
                ####################################Page2 #############################
                tabPanel("Cummulative Incidences",
                        h2("Cummulative count content"),
                        
                        # List of tiles
                         fluidRow(
                            
                             valueBoxOutput("infected",width=5),
                             valueBoxOutput("recovered", width=3),
                             valueBoxOutput("deaths",width=3)
                         ),
                         
                         h1("Distribution of cummulative counts"),
                         br(),
                        plotlyOutput("graph"),
                        br(),
                        br(),
                        br(),
                        plotlyOutput("deathCurve")
                         
                ),
                
                ###################################### Page3   #####################
                tabPanel("Cluster Distribution",
                        h2("content here"),
                        ####################################################
                        
                         sidebarLayout(
                             sidebarPanel(
                                 sliderInput(inputId = "plotDate", "Looping Animation:",
                                             min = as.Date(min(clusterData$Date),"%Y-%m-%d"),
                                             max = as.Date(max(clusterData$Date),"%Y-%m-%d"), ### TODO:: Find the start date and end date from the dataset - Done
                                             value=as.Date("2020-11-14"),
                                             step = 1,
                                             animate =
                                                 animationOptions(interval = 500, loop = TRUE))
                                
                               
                             ),
                             
                             mainPanel(
                                 tabsetPanel(
                                     tabPanel("Cumulative",h2("Distribution of clusters until seleted day:" ),br(),br(),plotOutput("cummulativeGraph")),
                                     
                                     tabPanel("Last Day",h2("Distribution of cluster in selected day:"),br(),br(),br(),br(),plotlyOutput("lastDayGraph"))
                                 
                                 )
                             )
                          )

                        
                ),
                
                ##################################### Page 4 #############################
                
                tabPanel("District Distribution",
                         h2("description of  district distribution"),
                         
                         
                       
                         sidebarLayout(
                             sidebarPanel(
                                 
                                 sliderInput(inputId = "page4plotDate", "Date",
                                             min = as.Date("2020-11-14","%Y-%m-%d"),
                                             max = as.Date("2021-04-21","%Y-%m-%d"), ### TODO:: Find the start date and end date from the dataset 
                                             value=as.Date("2020-11-14"),
                                             step = 1,                                
                                             animate =                                ## Delay of tmap. 

                                                 animationOptions(interval = 400, loop = TRUE)),
                                 

                                 verbatimTextOutput(outputId = "Dates"),
                
                                 selectInput(inputId = "districtSelect",
                                             label = h5("Select mapping district"),
                                              choices = unique(mapSLCovid19$District)
                                                 
                                                 
                                 )

                                
                                 

                                 # verbatimTextOutput(outputId = "Dates"),
                                 # 
                                 # selectInput(inputId = "districtSelect",
                                 #             label = h5("Select mapping district"),
                                 #              choices = unique(covidDistrictsSL$DISTRICT)
                                 #                 
                                 #                 
                                 # )


                                 

                                 
                             ),
                            
                            mainPanel(
                                
                                
                                  fluidRow(  
                                   
                                    splitLayout(
                                        cellWidths = c("50%","50%"),
                                        tmapOutput("district_map",height=500),
                                         plotlyOutput("districtLineGraph",height = 500)
                                    )
                                        
                                        
                                    
                                          
                                   )
                                  
                                # tabsetPanel(
                                #   #  tabPanel("Sri lanka Map ",h2("Visualize"),column(width=10, withSpinner(tmapOutput("district_map", width = "100%", height =500), type = 2))),
                                #     tabPanel("Sri lanka Map ",h2("Visualize"),column(width=10, tmapOutput("district_map", width = "100%", height =500))),
                                #      tabPanel("District Graph",plotlyOutput("districtLineGraph"))           
                                # )    
                                #   
                             )
                   
                   
                        )
               
               
                ),
               
               
               
               #################################page5###########################################################3
               
               tabPanel("About Us",
                        h2("Authors"),
                        h2("Data Sources"),
                        h2("All suggestions and comments and welcome: Email us")
               )
            
    ),
    tags$footer("myfooter",align="center",style="
                bottom:0;
                z-index:1000;
                background-color:black")
    )
        
)



# Define server logic required 
server <- function(input, output) {
    
   

    
    
    ## VAlue for valuebox-1 page2
    output$infected<-renderValueBox(
        {
            valueBox(value = cummulativeData[nrow(cummulativeData),2],
            subtitle="infected cummulative",
            color="yellow")
            
        }
    )
    
    ## Value for valuebox-2, page2
    output$recovered<-renderValueBox(
        {
            valueBox(value=cummulativeData[nrow(cummulativeData),3],
            subtitle="Recovered Cummmulative")
        }
    )
    
    ## Valuefor valuebox-3
    output$deaths<-renderValueBox(
       {
           valueBox(value = cummulativeData[nrow(cummulativeData),4],
           subtitle="Deaths Cummulative")
       }
    )
    
    ## Plotly graph page2
    output$graph <- renderPlotly({
        cummulativeDistribution <- plot_ly(cummulativeData, x = ~Date, y = ~Infected,name= "Total infected count", type = 'scatter', mode = 'lines')
        cummulativeDistribution<-cummulativeDistribution %>%add_trace(y = ~Recovered, name="Total Recovered Count",type = 'scatter', mode = 'lines')
        
        cummulativeDistribution <- cummulativeDistribution %>% layout(title = "Distribution of Infected and Recovered Cases",
                                                              xaxis = list(title = "Month"),
                                                              yaxis = list (title = "Count of individuals"))
        cummulativeDistribution <- cummulativeDistribution %>%layout(autosize=FALSE,width=1000,height=500)  
    })
    
    output$deathCurve<-renderPlotly({
        
        deathCurve<-plot_ly(cummulativeData,x=~Date,y=~Deaths,name="Total reported ",mode="lines")
        deathCurve<-deathCurve %>% layout(title="total number od deaths",
                                          xaxis=list(title="Month"),
                                         yaxis=list(title="Cout"))
        
        deathCurve<- deathCurve %>%layout(autosize=TRUE)  
        
    })
    
    
    ### Graphs for page 3

    
    ##################################Line graph of cummulative tab
    output$cummulativeGraph <- renderPlot({
        cumulative_plot(clusterData, as.Date(input$plotDate,"%y-%m-%d"))
    })

    output$result <- renderPrint({
        paste0("Date selected: ", as.Date(input$plotDate,"%y-%m-%d")) 
    })

    ################################Pie chart for  selected last day
    
    output$lastDayGraph<-renderPlotly({
        clusterPieChart(clusterData,as.Date(input$plotDate,"%y-%m-%d"))
        
    })
   
    
    
    #########################Graph for page 4 ##########
    output$Dates <- renderPrint({
        paste0("Date selected: ", input$page4plotDate) 
    })
    
    
    output$district_map<-renderTmap({

       


        districtMapSL(covidDistrictsSL,input$page4plotDate)
       
    }) 
    
    # TODO ::: FIX this 
    output$districtLineGraph<-renderPlotly(
        districtLinePlot(districtLineData,input$districtSelect,input$page4plotDate)
    )

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

