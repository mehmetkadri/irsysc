exampleFile <- read.csv("all4data.csv")
clustered_states <- data.frame()

server <- function(input, output, session) {

  observe({
    fileUploaded <- input$file1

    if (!is.null(input$file1)) {
      inputFile <- read.csv(input$file1$datapath)

      dates <- data.frame()

      for (i in 1:nrow(inputFile[1])) {
        dates[i, 1] <- mdy(inputFile[i, 1])
      }

      inputFile[1] <- dates

      states <- inputFile[2:length(inputFile)]

      updateSelectInput(session, "state1",
        choices = c("", colnames(states))
      )

      updateSelectInput(session, "state2",
        choices = c("", colnames(states))
      )

      updateDateRangeInput(session, "dateRange",
        start  = inputFile[1, 1],
        end    = inputFile[nrow(inputFile), 1],
        min    = inputFile[1, 1],
        max    = inputFile[nrow(inputFile), 1]
      )
    }
  })

  observe({
    fileUploaded <- input$file2

    if (!is.null(input$file2)) {
      inputFile <- read.csv(input$file2$datapath)

      dates <- data.frame()

      for (i in 1:nrow(inputFile[1])) {
        dates[i, 1] <- mdy(inputFile[i, 1])
      }

      inputFile[1] <- dates

      states <- inputFile[2:length(inputFile)]

      updateDateRangeInput(session, "dateRange2",
        start  = inputFile[1, 1],
        end    = inputFile[nrow(inputFile), 1],
        min    = inputFile[1, 1],
        max    = inputFile[nrow(inputFile), 1]
      )
    }
  })

  cleanData <- function(dates){

    startDate <- dates[1]
    endDate <- dates[2]
    exampleFile$Date <- as.Date(exampleFile$Date, format = "%m_%d_%Y")
    tempData <- subset(exampleFile, exampleFile$Date >= as.Date(startDate, format = "%m_%d_%Y") & exampleFile$Date <= as.Date(endDate, format = "%m_%d_%Y"))

    return(tempData)
  }

  ceilNegtoZero <- function(data) {
    data[data < 0] <- 0
    return(data)
  }

  extremeOuttoMedian <- function(data) {
    for (i in 2:length(data)) {
      # get the median of the column
      median <- median(data[, i], na.rm = TRUE)
      # get the IQR of the column
      IQR <- IQR(data[, i], na.rm = TRUE)
      # get Q1 and Q3
      Q1 <- quantile(data[, i], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[, i], 0.75, na.rm = TRUE)
      # get the upper and lower bounds
      upper <- Q3 + 1.5 * IQR
      lower <- Q1 - 1.5 * IQR
      # fill outliers with median
      data[data[, i] > upper, i] <- median
      data[data[, i] < lower, i] <- median
    }
    return(data)
  }

  lineChart <- function(data) {
    ggplot(data) +
      geom_line(aes_q(x = as.name(colnames(data)[1]), y = as.name(input$state1), color = "red")) +
      geom_line(aes_q(x = as.name(colnames(data)[1]), y = as.name(input$state2), color = "blue")) +
      scale_colour_discrete(name = "Countries", labels = c(input$state1, input$state2)) +
      scale_x_continuous(name = "") +
      scale_y_continuous(name = "‰ (per mille)")
  }

  boxPlot <- function(data) {
    temp1 <- cbind(data["Date"], data[input$state1])
    names(temp1)[2] <- "Data"
    temp1$State <- input$state1
    temp2 <- cbind(data["Date"], data[input$state2])
    names(temp2)[2] <- "Data"
    temp2$State <- input$state2
    boxData <- rbind(temp1, temp2)
    colnames(boxData)[3] <- c("Country")
    ggplot(boxData) +
      geom_boxplot(aes(x = Country, y = Data, color = Country)) +
      scale_y_continuous(name = "‰ (per mille)")
  }

  densityPlot <- function(data) {
    temp1 <- cbind(data["Date"], data[input$state1])
    names(temp1)[2] <- "Data"
    temp1$State <- input$state1
    temp2 <- cbind(data["Date"], data[input$state2])
    names(temp2)[2] <- "Data"
    temp2$State <- input$state2
    densData <- rbind(temp1, temp2)
    colnames(densData)[3] <- c("Country")
    ggplot(densData, aes(x = Data, color = Country)) +
      geom_density() +
      scale_x_continuous(name = "‰ (per mille)")
  }

  cleanInputData <- function(uploadedData,inputDates) {
    inputFile <- read.csv(uploadedData$datapath)
    dates <- data.frame()
    for (i in 1:nrow(inputFile[1])) {
      dates[i, 1] <- mdy(inputFile[i, 1])
    }
    inputFile[1] <- dates
    states <- inputFile[2:length(inputFile)]
    startDate <- inputDates[1]
    endDate <- inputDates[2]
    inputFile$Date <- as.Date(inputFile$Date, format = "%m_%d_%Y")
    tempData <- subset(inputFile, inputFile$Date >= as.Date(startDate, format = "%m_%d_%Y") & inputFile$Date <= as.Date(endDate, format = "%m_%d_%Y"))
    return(tempData)
  }

  output$plot <- renderPlot({
    if ((input$state1 != "" & input$state2 != "")) {
      if (is.null(input$file1)) {
        
        tempData <- ceilNegtoZero(cleanData(input$dateRange))
    
        if (input$outlier == "Yes") {
          tempData <- extremeOuttoMedian(tempData)
        }

        if (input$plotType == "Line Chart") {
          lineChart(tempData)
        } else if (input$plotType == "Box Plot") {
          boxPlot(tempData)
        } else if (input$plotType == "Density Plot") {
          densityPlot(tempData)
        }
      } else {
        
        tempData <- ceilNegtoZero(cleanInputData(input$file1,input$dateRange))

        if (input$outlier == "Yes") {
          tempData <- extremeOuttoMedian(tempData)
        }

        if (input$plotType == "Line Chart") {
          lineChart(tempData)
        } else if (input$plotType == "Box Plot") {
          boxPlot(tempData)
        } else if (input$plotType == "Density Plot") {
          densityPlot(tempData)
        }
      }
    }
  })

  output$title <- renderText({
    if (input$state1 != "" & input$state2 != "") {
      paste0(gsub(".", " ", input$state1, fixed = TRUE), " vs ", gsub(".", " ", input$state2, fixed = TRUE))
    }
  })

  output$title2 <- renderText({
    if (input$clusterCount != 1) {
      paste0("World Map with ", input$clusterCount, " Clusters")
    }
  })

  output$leaflet <- renderLeaflet({

    # download the world map data
    if (TRUE) {
      download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="./world_shape_file.zip")
      unzip("./world_shape_file.zip")
      world_spdf <- readOGR( 
        dsn= getwd() , 
        layer="TM_WORLD_BORDERS_SIMPL-0.3",
        verbose=FALSE
      )
      file.remove(c("world_shape_file.zip","Readme.txt","TM_WORLD_BORDERS_SIMPL-0.3.dbf","TM_WORLD_BORDERS_SIMPL-0.3.shp","TM_WORLD_BORDERS_SIMPL-0.3.prj","TM_WORLD_BORDERS_SIMPL-0.3.shx"))
    }

    if (input$clusterCount > 1) {
      
      if (is.null(input$file2)) {
        exampleData <- ceilNegtoZero(cleanData(input$dateRange2))

        if (input$outlier2 == "Yes") {
          exampleData <- extremeOuttoMedian(exampleData)
        }
        
        # cluster the states
        if (TRUE) {
          
          splitX <- split(t(exampleData[2:length(exampleData)]), rownames(t(exampleData[2:length(exampleData)])))
          states = colnames(exampleData[2:length(exampleData)])
          names(splitX) <- states
          mvc <- tsclust(splitX, k = as.numeric(input$clusterCount), distance = "euclidean", seed = 390,
                  args = tsclust_args(dist = list(sigma = 100)))

          clustered_states <- data.frame(Country = states, Cluster= mvc@cluster)
          world_spdf@data$cluster <- NA
          for (i in 1:as.numeric(input$clusterCount)) {
            world_spdf@data[world_spdf@data$ISO3 %in% clustered_states[clustered_states$Cluster == i, "Country"], "cluster"] <- i
          }
          world_spdf@data[["cluster"]][is.na(world_spdf@data[["cluster"]])] <- "not-listed"
        }

        # plot the map
        if (TRUE) {
          mypalette <- colorFactor('Dark2', world_spdf@data$cluster)      

          mytext <- paste(
            "Country: ", world_spdf@data$NAME,"<br/>",
            "Cluster: ", world_spdf@data$cluster,"<br/>",
            sep="") %>% lapply(htmltools::HTML)
          leaflet(world_spdf,height=100,options = leafletOptions(minZoom = 1,maxBoundsViscosity = 1.0))%>% addTiles() %>% setView( lat=10, lng=0 , zoom=2) %>%
          setView(0, 0, zoom = 1)%>%
          setMaxBounds(-180,-90,180,90) %>% 
            addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor = ~mypalette(cluster), color="white",
              label = mytext,
              labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px", 
                direction = "auto"
              ))%>%
            addLegend( pal=mypalette, values=~cluster, opacity=0.9, title = "Clusters", position = "bottomleft" )
        }

      } else {
        inputData <- ceilNegtoZero(cleanInputData(input$file2,input$dateRange2))

        if (input$outlier2 == "Yes") {
          inputData <- extremeOuttoMedian(inputData)
        }

        if (TRUE) {
          
          splitX <- split(t(inputData[2:length(inputData)]), rownames(t(inputData[2:length(inputData)])))
          states = colnames(inputData[2:length(inputData)])
          names(splitX) <- states
          mvc <- tsclust(splitX, k = as.numeric(input$clusterCount), distance = "euclidean", seed = 390,
                  args = tsclust_args(dist = list(sigma = 100)))

          clustered_states <- data.frame(State = states, Cluster= mvc@cluster)
          world_spdf@data$cluster <- NA
          for (i in 1:as.numeric(input$clusterCount)) {
            world_spdf@data[world_spdf@data$ISO3 %in% clustered_states[clustered_states$Cluster == i, "State"], "cluster"] <- i
          }
          world_spdf@data[["cluster"]][is.na(world_spdf@data[["cluster"]])] <- "not-listed"
        }

        if (TRUE) {
          mypalette <- colorFactor('Dark2', world_spdf@data$cluster)      

          mytext <- paste(
            "Country: ", world_spdf@data$NAME,"<br/>",
            "Cluster: ", world_spdf@data$cluster,"<br/>",
            sep="") %>% lapply(htmltools::HTML)
          leaflet(world_spdf)%>% addTiles() %>% setView( lat=10, lng=0 , zoom=2) %>%
            addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor = ~mypalette(cluster), color="white",
              label = mytext,
              labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px", 
                direction = "auto"
              ))%>%
            addLegend( pal=mypalette, values=~cluster, opacity=0.9, title = "Clusters", position = "bottomleft" )
        }
      }
    }else {   
      mytext <- paste(
        "Country: ", world_spdf@data$NAME,"<br/>",
        sep="") %>% lapply(htmltools::HTML)
      leaflet(world_spdf,height=100,options = leafletOptions(minZoom = 1,maxBoundsViscosity = 1.0))%>% 
      addTiles() %>% 
      setView(0, 0, zoom = 1)%>%
      setMaxBounds(-180,-90,180,90) %>% 
        addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color="white",
          label = mytext,
          labelOptions = labelOptions( 
            style = list("font-weight" = "normal", padding = "3px 8px"), 
            textsize = "13px", 
            direction = "auto"
          ))
    }
  })

  output$clusterInfo <- renderTable({

    if (input$clusterCount > 1) {
      
      if (is.null(input$file2)) {
        exampleData <- ceilNegtoZero(cleanData(input$dateRange2))

        if (input$outlier2 == "Yes") {
          exampleData <- extremeOuttoMedian(exampleData)
        }
        
        # cluster the states
        if (TRUE) {
          splitX <- split(t(exampleData[2:length(exampleData)]), rownames(t(exampleData[2:length(exampleData)])))
          states = colnames(exampleData[2:length(exampleData)])
          names(splitX) <- states
          mvc <- tsclust(splitX, k = as.numeric(input$clusterCount), distance = "euclidean", seed = 390,
                  args = tsclust_args(dist = list(sigma = 100)))

          clustered_states <- data.frame(State = states, Cluster= mvc@cluster)
        }
      } else {
        inputData <- ceilNegtoZero(cleanInputData(input$file2,input$dateRange2))

        if (input$outlier2 == "Yes") {
          inputData <- extremeOuttoMedian(inputData)
        }

        if (TRUE) {
          splitX <- split(t(inputData[2:length(inputData)]), rownames(t(inputData[2:length(inputData)])))
          states = colnames(inputData[2:length(inputData)])
          names(splitX) <- states
          mvc <- tsclust(splitX, k = as.numeric(input$clusterCount), distance = "euclidean", seed = 390,
                  args = tsclust_args(dist = list(sigma = 100)))

          clustered_states <- data.frame(State = states, Cluster= mvc@cluster)
        }
      }
      rows = c()
      for (i in 1:as.numeric(input$clusterCount)) {
        rows<-append(rows,length(clustered_states[clustered_states$Cluster == i, "State"]))
      }

      rowCount = max(rows)

      # create outputDataFrame as a dataframe that has a column for each cluster
      outputDataFrame <- data.frame(matrix(ncol = as.numeric(input$clusterCount), nrow = rowCount))

      # for each cluster, add the states in that cluster to the outputDataFrame
      clusterStates <- c()
      for (i in 1:as.numeric(input$clusterCount)) {
        for (j in 1:length(clustered_states[clustered_states$Cluster == i, "State"])) {
          outputDataFrame[j,i] <- clustered_states[clustered_states$Cluster == i, "State"][j]
        }
      }

      # rename the columns as "Cluster 1", "Cluster 2", etc.
      names(outputDataFrame) <- paste("Cluster", 1:as.numeric(input$clusterCount), sep = " ")

      # replace the NA values with ""
      outputDataFrame[is.na(outputDataFrame)] <- ""
      outputDataFrame
    }
  })
}
