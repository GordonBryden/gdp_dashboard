library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(sparkline)
library(htmlwidgets)
library(DT)
library(RColorBrewer)
library(scales)
library(treemap)
library(lubridate)
library(shinycssloaders)

#Loading data

source("xml_collect.R")

# handle parsing errors
if(next_release_date == "NA NA NA") {
  if(month(df2$date %>% first())+3 > 12){
    new_month <- month.name[month(df2$date %>% first()) - 9]
    new_year <- year(df2$date %>% first()) + 1
  } else {
    new_month <- month.name[month(df2$date %>% first())+3]
    new_year <- year(df2$date %>% first())
  }
  
  next_release_date <- paste(new_month, new_year, "(Estimated)")
}


#collect gdp data
source("sparql_collect.R")

#generate graph label
four_year_ago <- paste0(
  year(yq(current_quarter)) - 4,
  substr(current_quarter, start=5, stop=8)
)

#fix ordering issue
gdp_raw <- gdp_raw %>%
  arrange(refPeriod)

gdp_quarterly <- gdp_raw %>%
  filter(measureType == "q-on-q",
         industrySectorsic07 == "Total Gross Value Added (GVA) (Section A-T)") %>%
  select(value) %>%
  last() %>%
  last()

gdp_annual <- gdp_raw %>%
  filter(measureType == "q-on-q year ago",
         industrySectorsic07 == "Total Gross Value Added (GVA) (Section A-T)") %>%
  select(value) %>%
  last() %>%
  last()

#create weight file

level1 <- c(
  "AGRICULTURE FORESTRY & FISHING",
  "CONSTRUCTION",
  "PRODUCTION",
  "PRODUCTION",
  "PRODUCTION",
  "PRODUCTION",
  "SERVICES",
  "SERVICES",
  "SERVICES",
  "SERVICES"
)
level2 <- c(
  "AGRICULTURE FORESTRY & FISHING",
  "CONSTRUCTION",
  "ELECTRICITY & GAS",
  "MANUFACTURING",
  "MINING & QUARRYING",
  "WATER & WASTE MANAGEMENT",
  "BUSINESS SERVICES & FINANCE",
  "DISTRIBUTION HOTELS & CATERING",
  "GOVERNMENT & OTHER SERVICES",
  "TRANSPORT STORAGE & COMMUNICATION"
)
weight <- c(
  11.8755,
  63.4894,
  25.0706,
  112.8241,
  28.5742,
  12.6651,
  288.7431,
  131.0373,
  249.653,
  207.1049
)

sector_weights <- data_frame(
  level1,
  level2,
  weight
)

industry_sectors = c("Total Gross Value Added (GVA) (Section A-T)", 
                     "Agriculture, Forestry and Fishing (Section A)",
                     "Mining & Quarrying (Section B)", "Manufacturing (Section C)",
                     "Electricity, Gas, Steam and Air (Section D)",
                     "Water Supply and Sewerage (Section E)",
                     "Construction (Section F)", 
                     "Transport, Storage and Communication (Section H,J)", 
                     "Distribution, Hotels and Restaurants (Section G,I)", 
                     "Business Services and Finance (Section K-N)",
                     "Government and Other Services (Section O-T)")

data_gdp = gdp_raw %>%
  filter(industrySectorsic07 %in% industry_sectors,
         measureType == "Index") %>%
  select(refPeriod, industrySectorsic07, value)

data_gdp2 <- gdp_raw %>%
  filter(industrySectorsic07 %in% industry_sectors,
         measureType == "Index",
         !industrySectorsic07 == "Total Gross Value Added (GVA) (Section A-T)") %>%
  select(refPeriod, industrySectorsic07, value) %>%
  filter(refPeriod >= "2012-Q1") %>%
  group_by(industrySectorsic07) %>%
  slice((n()-16):n()) %>%
  summarise(Growth = percent((last(value) / nth(value, n()-1) -1),
                             accuracy = 0.1),
            Sparkline = spk_chr(value, type= 'line'))

rm(gdp_raw)

########################################################################################################################################

# Define UI for application

body <- dashboardBody(
  # initialize shinyjs
  #shinyjs::useShinyjs(),
  # add custom JS code
  #shinyjs::extendShinyjs(text = "shinyjs.hidehead = none"),
  
  tags$head(tags$style(HTML('
                            /* logo */
                            .skin-blue .main-header .logo {
                            background-color: #f4b943;
                            }
                            
                            /* logo when hovered */
                            .skin-blue .main-header .logo:hover {
                            background-color: #f4b943;
                            }
                            
                            /* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #f4b943;
                            }
                            .box-header h3 {
                            font-weight: bold;
                            }
                            /* main sidebar */
                            .skin-blue .main-sidebar {
                            background-color: #f4b943;
                            }
                            
                            /* active selected tab in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: #ff0000;
                            }
                            
                            /* other links in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                            background-color: #00ff00;
                            color: #000000;
                            }
                            
                            /* other links in the sidebarmenu when hovered */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                            background-color: #ff69b4;
                            }
                            /* toggle button when hovered  */
                            .info-box {
                            display: block;
                            min-height: 90px;
                            background: #eee;
                            width: 100%;
                            box-shadow: 0 0px 0px rgba(0, 0, 0, 0.1);
                            border-radius: 2px;
                            margin-bottom: 15px;
                            }
                            
                            
                            /* body */
                            .content-wrapper, .right-side {
                            background-color: #ffffff;
                            }
                            
                            ')))
  ,
  
  
  fluidPage(
    
    titlePanel("Scotland's GDP: at a glance"),
    
    
    mainPanel(
      
      #The dash board is split into a series of columns and rows
      
      #Top row, split into 3 columns for publication dates
      fluidRow(
        fluidRow(
          fluidRow(
            infoBox(
              "Current quarter",
              width = 4,
              paste(current_quarter),
              icon = icon("ok circle", lib = "glyphicon")
            ),
            infoBox(
              "Release date",
              width = 4,
              paste(release_date),
              icon = icon("calendar", lib = "glyphicon"),
              tags$head(tags$style(HTML('.infobox{-webkit-box-shadow: none; 
                                        -moz-box-shadow: none;
                                        box-shadow: none;}')
              )
              )
              ),
            infoBox(
              "Next quarter release",
              width = 4,
              paste(next_release_date),
              icon = icon("calendar", lib = "glyphicon")
            )
              ),
          fluidRow(
            infoBox(
              "Quarterly growth rate",
              width = 6,
              paste(gdp_quarterly),
              color = if(gdp_quarterly > 0) {"green"
              } else {
                "red"
              },
              icon = if(gdp_quarterly > 0) {
                icon("chevron-up", lib="glyphicon")
              } else {
                icon("chevron-down", lib="glyphicon")
              }
            ),
            infoBox(
              "Annual growth rate",
              width = 6,
              color = if(gdp_annual > 0) {"green"
              } else {
                "red"
              },
              paste(gdp_annual),
              icon = if(gdp_quarterly > 0) {
                icon("chevron-up", lib="glyphicon")
              } else {
                icon("chevron-down", lib="glyphicon")
              }
            )
          )
          ,
          fluidRow(
            box(solidHeader = TRUE,
                width = 6,
                title = "Scottish GDP per quarter",
                plotOutput("gdp_plot")%>% withSpinner()
            ),
            box(
              solidHeader = TRUE,
              width = 6,
              title = "Growth per quarter",
              plotOutput("growth_plot")%>% withSpinner(),
              tags$head(tags$style(HTML('.box{-webkit-box-shadow: none;
                                        -moz-box-shadow: none;
                                        box-shadow: none;}')
              )
              )
              )
              )
            ),
        fluidRow(
          box(solidHeader = TRUE,
              title = "Growth of economy sectors",
              getDependency('sparkline'),
              dataTableOutput('myTable')%>% withSpinner()
          ),
          box(
            solidHeader = TRUE,
            title = "Relative size of each sector",
            plotOutput("treemap") %>% withSpinner()
          )
        )
          )
    )
  )
  )


########################################################################################################################################

server <- function(input, output) {
  
  output$gdp_plot <- renderPlot({
    
    data_gdp %>%
      filter(industrySectorsic07 == "Total Gross Value Added (GVA) (Section A-T)") %>%
      slice((n()-16):n()) %>%
      ggplot(aes(x = refPeriod, y = value, group = 1)) +
      geom_line( size = 2, color = "lightblue") +
      theme_classic() +
      scale_x_discrete(name = "Date", breaks=c(four_year_ago,current_quarter)) +
      scale_y_continuous(name="Index (GDP in 2015 = 100)") +
      theme(
        text = element_text(size=20),
        axis.text.x = element_text(angle=0,hjust = 1, vjust = -2) 
                #axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
        #axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank()
      )
  }, bg = "transparent")
  
  output$growth_plot <- renderPlot({
    
    data_gdp %>%
      filter(industrySectorsic07 == "Total Gross Value Added (GVA) (Section A-T)") %>%
      mutate(growth = (value/lag(value) - 1)) %>%
      slice((n()-16):n()) %>%
      ggplot(aes(x = refPeriod, y = growth)) +
      geom_col( size = 1, color = "lightblue", fill = "lightblue") +
      theme_classic() +
      scale_x_discrete(name = "Date", breaks=c(four_year_ago,current_quarter))+
      scale_y_continuous(name="Quarterly growth rate", labels = percent_format(accuracy = 0.1)) +
      theme(
        text = element_text(size=20),
        axis.text.x = element_text(angle=0,hjust = 1, vjust = -2) 
        #axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
        #axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank()
      )
  }, bg = "transparent")
  
  #n <- "0.8%"
  #m <- "1.6%"
  
  # output$quarterly_gdp_growth <- renderInfoBox({
  #   infoBox(
  #     "Quarterly growth", paste0(n), icon = icon("list"),
  #     color = "purple"
  #   )
  # })
  output$quarterly_gdp_growth <- renderText({ paste(n) })
  output$annual_gdp_growth <- renderText({ paste(m) })
  
  #SECTION SPARKLINE PLOTS
  
  
  staticRender_cb <- JS('function(){debugger;HTMLWidgets.staticRender();}')
  
  output$myTable <- renderDataTable(data_gdp2 %>%
                                    rename("Industry sector" = "industrySectorsic07"),
                                    escape = FALSE,
                                    rownames= FALSE,
                                    options = list(ordering = F,
                                                   drawCallback = staticRender_cb,
                                                   dom  = 't',
                                                   pageLength = 11
                                    )
  )
  
  output$latest_quarter <- renderInfoBox({
    infoBox(
      "latest_quarter", paste0(current_quarter), icon = icon("list"),
      color = "purple"
    )
  })
  
  
  #create treemap
  output$treemap <- renderPlot({
    sector_weights %>%
      treemap(
        title = "",
        index = "level2",
        vSize = "weight",
        type = "index",
        
        palette = c("#f7fbff",
"#deebf7",
"#c6dbef",
"#9ecae1",
"#6baed6",
"#4292c6",
"#2171b5",
"#08519c",
"#08306b",
"#000000")
        
        #palette = c("#8dd3c7",
        #            "#ffffb3",
        #            "#bebada",
        #            "#fb8072",
        #            "#80b1d3",
        #            "#fdb462",
        #            "#b3de69",
        #            "#fccde5",
        #            "#d9d9d9",
        #            "#bc80bd")
      )
  })
}

ui <- dashboardPage(
  dashboardHeader(disable= TRUE),
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu()),
  body
)



shinyApp(ui = ui, server = server)
