library(shiny)

shinyServer(function(input, output) {
  
  dataInput <- reactive({
    infile <- input$file
    if (is.null(infile)) {
      return(NULL)
    }
    df<-read.csv(infile$datapath, header = FALSE, sep = ",")
  })
  
  ##################################################### DYNAMIC GUIwith SLIDERS #########################################################
  output$ui_All <- renderUI({
    df <- dataInput()
    if(!is.null(df)) {
        maxY <- max(df[,1], df[,2], df[,3])
        minY <- min(df[,1], df[,2], df[,3])
        size <- length(df[,1])
        fluidRow(
          column(3, sliderInput("rangeT", "Range for the time axis:", min = 1, max = size, value = c(1, size))  ),
          column(3, sliderInput("rangeY", label = "Range for the measurements:", min = -4000, max = 4000, value = c(minY, maxY))  )
        )
    }
  })
  
  output$ui_X <- renderUI({
    df <- dataInput()
    if(!is.null(df)) {
      maxY <- max(df[,1])
      minY <- min(df[,1])
      size <- length(df[,1])
      fluidRow(
        column(3, sliderInput("rangeTX", "Range for the time axis:", min = 1, max = size, value = c(1, size))  ),
        column(3, sliderInput("rangeYX", label = "Range for the measurements:", min = -4000, max = 4000, value = c(minY, maxY))  )
      )
    }
  })
  
  output$ui_Y <- renderUI({
    df <- dataInput()
    if(!is.null(df)) {
      maxY <- max(df[,2])
      minY <- min(df[,2])
      size <- length(df[,1])
      fluidRow(
        column(3, sliderInput("rangeTY", "Range for the time axis:", min = 1, max = size, value = c(1, size))  ),
        column(3, sliderInput("rangeYY", label = "Range for the measurements:", min = -4000, max = 4000, value = c(minY, maxY))  )
      )
    }
  })

  output$ui_Z <- renderUI({
    df <- dataInput()
    if(!is.null(df)) {
      maxY <- max(df[,3])
      minY <- min(df[,3])
      size <- length(df[,1])
      fluidRow(
        column(3, sliderInput("rangeTZ", "Range for the time axis:", min = 1, max = size, value = c(1, size))  ),
        column(3, sliderInput("rangeYZ", label = "Range for the measurements:", min = -4000, max = 4000, value = c(minY, maxY))  )
      )
    }
  })
  output$ui_Mod <- renderUI({
    df <- dataInput()
    if(!is.null(df)) {
      temp<-sqrt(df[,1]^2 + df[,2]^2 + df[,3]^2)
      maxY <- max(temp)
      minY <- min(temp)
      size <- length(df[,1])
      fluidRow(
        column(3, sliderInput("rangeTM", "Range for the time axis:", min = 1, max = size, value = c(1, size))  ),
        column(3, sliderInput("rangeYM", label = "Range for the measurements:", min = -4000, max = 4000, value = c(minY, maxY))  )
      )
    }
  })
  
    ####################################################### PLOTS ################################################################ 
  output$distPlot <- renderPlot({
    df <- dataInput()
    xarray<-seq(1, length(df[,1]))
    if(!is.null(df) && !is.null(input$rangeT)) {
      times0<-as.POSIXct(df[input$rangeT[1],4], origin="1970-01-01")
      times1<-as.POSIXct(df[input$rangeT[2],4], origin="1970-01-01")
      mn <-paste("Data for: ", times0, " --- ", times1, sep="");
      plot(xarray[input$rangeT[1]:input$rangeT[2]], df[input$rangeT[1]:input$rangeT[2],1], ylab="Acceleration (x, y, z)", xlab="time", col="blue", ylim=input$rangeY, main=mn);  
      points(xarray[input$rangeT[1]:input$rangeT[2]],df[input$rangeT[1]:input$rangeT[2],2], ylab="Acceleration (y-axis)", xlab="time", col="red", ylim=input$rangeY);  
      points(xarray[input$rangeT[1]:input$rangeT[2]],df[input$rangeT[1]:input$rangeT[2],3], ylab="Acceleration (z-axis)", xlab="time", col="green", ylim=input$rangeY);  
      grid(NA,NULL)
      legend('topright', c("x-axis", "y-axis", "z-axis"), col=c("blue", "red", "green"), lwd=c(2, 2, 2))
    }
  })

  output$plotX <- renderPlot({
    df <- dataInput()
    xarray<-seq(1, length(df[,1]))
    if(!is.null(df) && !is.null(input$rangeTX) && !is.null(input$rangeYX)) {
      times0<-as.POSIXct(df[input$rangeT[1],4], origin="1970-01-01")
      times1<-as.POSIXct(df[input$rangeT[2],4], origin="1970-01-01")
      mn <-paste("Data for: ", times0, " --- ", times1, sep="");
      plot( xarray[input$rangeTX[1]:input$rangeTX[2]],df[input$rangeTX[1]:input$rangeTX[2],1], ylab="Acceleration (x-axis)", xlab="time", col="blue", ylim=input$rangeYX, main=mn);  
      lines(xarray[input$rangeTX[1]:input$rangeTX[2]],df[input$rangeTX[1]:input$rangeTX[2],1], ylab="Acceleration (x-axis)", xlab="time", col="blue", ylim=input$rangeYX);  
      grid(NA,NULL)
      legend('topright', c("x-axis", "y-axis", "z-axis"), col=c("blue", "red", "green"), lwd=c(2, 2, 2))
    }
  })

  output$plotY <- renderPlot({
    df <- dataInput()
    xarray<-seq(1, length(df[,1]))
    if(!is.null(df) && !is.null(input$rangeTY) && !is.null(input$rangeYY)) {
      times0<-as.POSIXct(df[input$rangeT[1],4], origin="1970-01-01")
      times1<-as.POSIXct(df[input$rangeT[2],4], origin="1970-01-01")
      mn <-paste("Data for: ", times0, " --- ", times1, sep="");  
      plot( xarray[input$rangeTY[1]:input$rangeTY[2]],df[input$rangeTY[1]:input$rangeTY[2],2], ylab="Acceleration (y-axis)", xlab="time", col="red", ylim=input$rangeYY, main=mn);  
      lines(xarray[input$rangeTY[1]:input$rangeTY[2]],df[input$rangeTY[1]:input$rangeTY[2],2], ylab="Acceleration (y-axis)", xlab="time", col="red", ylim=input$rangeYY);  
      grid(NA,NULL)
      legend('topright', c("x-axis", "y-axis", "z-axis"), col=c("blue", "red", "green"), lwd=c(2, 2, 2))
    }
  })

  output$plotZ <- renderPlot({
    df <- dataInput()
    xarray<-seq(1, length(df[,1]))
    if(!is.null(df) && !is.null(input$rangeTZ) && !is.null(input$rangeYZ)) {
      times0<-as.POSIXct(df[input$rangeT[1],4], origin="1970-01-01")
      times1<-as.POSIXct(df[input$rangeT[2],4], origin="1970-01-01")
      mn <-paste("Data for: ", times0, " --- ", times1, sep="");  
      plot( xarray[input$rangeTZ[1]:input$rangeTZ[2]],df[input$rangeTZ[1]:input$rangeTZ[2],3], ylab="Acceleration (z-axis)", xlab="time", col="green", ylim=input$rangeYZ, main=mn);  
      lines(xarray[input$rangeTZ[1]:input$rangeTZ[2]],df[input$rangeTZ[1]:input$rangeTZ[2],3], ylab="Acceleration (z-axis)", xlab="time", col="green", ylim=input$rangeYZ);  
      grid(NA,NULL)
      legend('topright', c("x-axis", "y-axis", "z-axis"), col=c("blue", "red", "green"), lwd=c(2, 2, 2))
    }
  })
  
  output$plotMod <- renderPlot({
    df <- dataInput()
    xarray<-seq(1, length(df[,1]))
    if(!is.null(df) && !is.null(input$rangeTM) && !is.null(input$rangeYM)) {
      times0<-as.POSIXct(df[input$rangeT[1],4], origin="1970-01-01")
      times1<-as.POSIXct(df[input$rangeT[2],4], origin="1970-01-01")
      mn <-paste("Data for: ", times0, " --- ", times1, sep="");
      moddf <-sqrt(df[input$rangeTM[1]:input$rangeTM[2],3]^2 + df[input$rangeTM[1]:input$rangeTM[2],1] ^2 + df[input$rangeTM[1]:input$rangeTM[2],2] ^2)
      plot( xarray[input$rangeTM[1]:input$rangeTM[2]],moddf, ylab="Modulus", xlab="time", col="purple", ylim=input$rangeYM, main=mn);  
      lines(xarray[input$rangeTM[1]:input$rangeTM[2]],moddf, ylab="Modulus", xlab="time", col="purple", ylim=input$rangeYM);  
      grid(NA,NULL)
    }
  })
  
})