exampleFile <- read.csv("all4data.csv")
clusterRange = ncol(exampleFile)-1

dates <- data.frame()
for(i in 1:nrow(exampleFile[1])) {
  dates[i,1] <- mdy(exampleFile[i,1])
}
exampleFile[1] <- dates

ui <- fluidPage(
  navbarPage(
    title = "CCC",
    tabPanel(title = "Comparison of Countries",
              sidebarLayout(
                sidebarPanel(
                  h2("Country Comparison"),
                  tags$p(""),
                  tags$details(
                    tags$summary("Read Explaination",style="cursor: pointer; color: #0000FF;"),
                    "This section helps you visualize and compare two different countries in terms of a time-event. 
                    You can either upload your own data or you can test out the features of this application with 
                    the sample data. Check out 'About' section for more detailed information such as accepted data 
                    format."
                  ) ,
                  hr(),
                  fileInput("file1", "Choose CSV File", accept = c(".csv")),
                  dateRangeInput("dateRange", "Date range:",
                                 start  = exampleFile[1,1],
                                 end    = exampleFile[nrow(exampleFile),1],
                                 min    = exampleFile[1,1],
                                 max    = exampleFile[nrow(exampleFile),1]),
                  selectInput("state1", "Country 1", choices = c("",colnames(exampleFile[2:length(exampleFile)]))),
                  selectInput("state2", "Country 2", choices = c("",colnames(exampleFile[2:length(exampleFile)]))),
                  selectInput("plotType", "Plot Type", choices = c("","Box Plot", "Line Chart", "Density Plot")),
                  radioButtons("outlier", "Discard Extreme Outliers", choices = c("No", "Yes")),
                ),
                mainPanel(
                  h3(textOutput("title")),
                  plotOutput("plot")
                )
              )),
    
    
    tabPanel(title = "Clustering of Countries",
      sidebarPanel(
        h2("Country Clustering"),
        tags$p(""),
        tags$details(
          tags$summary("Read Explaination",style="cursor: pointer; color: #0000FF;"),
          "This section helps you visualize the clustering of all different Country in terms of a time-event. You 
          can either upload your own data or you can test out the features of this application with the sample 
          data. Check out 'About' section for more detailed information such as accepted data format."
        ) ,
        hr(),
        fileInput("file2", "Choose CSV File", accept = c(".csv")),
        dateRangeInput("dateRange2", "Date range:",
                       start  = exampleFile[1,1],
                       end    = exampleFile[nrow(exampleFile),1],
                       min    = exampleFile[1,1],
                       max    = exampleFile[nrow(exampleFile),1]),
        selectInput("clusterCount", "Cluster Count:", choices = c(1:clusterRange)),
        radioButtons("outlier2", "Discard Extreme Outliers", choices = c("No", "Yes")),
        width = 4
      ),
      mainPanel(
        h3(textOutput("title2")),
        leafletOutput("leaflet"),
        width = 4
      ),

      sidebarPanel(
        h3("Clusters"),
        tableOutput("clusterInfo"),
        width = 4
      )

    ),

    tabPanel(
      title = "About",
      fluidRow(
        column(width = 2),
        column(
          h1("Comparison and Clustering of Countries",
            style = "text-align:center;color:black;background-color:#ebebeb;padding:15px;border-radius:10px;
            box-shadow: 0px 0px 20px 3px #dbdbdb inset;"
          ),
          div(
            p("This application is created with the intention of creating a tool
                                that allows you to visualize the comparison and clustering of
                                different countries in terms of a time-event.  ",
              style = "font-size:16px;text-align:justify;color:black;"
            ),
            br(),
            p(strong("Walkthrough:"), style = "font-size:20px;text-align:justify;color:black;"),
            p(strong("Comparison:"), style = "font-size:18px;text-align:justify;color:black;"),
            p("First page allows you to visualize the comparison of two different
                                countries in terms of a time-event with the help of a variaty of charts.",
              br(),
              "If you are going to use the sample data, firstly, you should select
                                the date range you want to visualize. Then, you should select the
                                countries you want to compare and the type of chart you want to use. 
                                Finally, you should select whether you want to discard the extreme
                                outliers or not.",
              style = "font-size:16px;text-align:justify;color:black;"
            ),
            p(strong("Clustering:"), style = "font-size:18px;text-align:justify;color:black;"),
            p("Second page allows you to visualize the clustering of all
                                different countries in terms of a time-event on a world map.",
              br(),
              "If you are going to use the sample data, all you need to do is to
                                select the date range you want to visualize the clusters and select 
                                whether you want to discard the extreme outliers or not. 
                                Finally, you should choose the number of clusters you want.",
              style = "font-size:16px;text-align:justify;color:black;"
            ),
            br(),
            p("You can either upload your own data or you can test out the
                                features of this application with the sample data.",
              strong("Check out the section below for details of restrictions on data format."),
              style = "font-size:16px;text-align:justify;color:black;"
            ),
            style = "background-color:#ebebeb;padding:15px;border-radius:10px;box-shadow: 0px 0px 20px 3px #dbdbdb inset;"
          ),
          br(),
          div(
            p(strong("Uploading data:"), style = "font-size:20px;text-align:justify;color:black;"),
            p("If you are going to use your own data, you should upload your data
                                  in a CSV format. The first column of your data should be the date
                                  column and the rest of the columns should be the countries you want to
                                  compare. The date column should be in the following format: 'mm_dd_yyyy'.
                                  Also your country names should be in 'ISO3' format.",
              style = "font-size:16px;text-align:justify;color:black;"
            ),
            style = "background-color:#ebebeb;padding:15px;border-radius:10px;box-shadow: 0px 0px 20px 3px #dbdbdb inset;"
          ),
          br(),
          br(),
          width = 8
        ),
        column(width = 2)
      ),
    ),
    inverse = T
  )
)
