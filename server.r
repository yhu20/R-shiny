library(shiny)

shinyServer(function(input, output, session) {

  data <- reactive({
    inFile <- input$file1 
    if (is.null(inFile)){return(NULL)} 
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  })
  

  # Updata value user could select
  observe({
    updateSelectInput(
      session,
      "var1",
      choices=names(data()))

  })
  # Updata value user could select
  observe({
    updateSelectInput(
      session,
      "var2",
      choices=names(data()))
    
  })
  
  # Output a t distribution plot
  output$tplot <- renderPlot({
    # Display the Student's t distributions with various
    # degrees of freedom and compare to the normal distribution
    
    x <- seq(input$range[1], input$range[2], length=100)
    hx <- dnorm(x)  
    labels <- c('t distribution', 'normal distribution')
    
    plot(x, hx, type="l", lty=2, xlab="x value", col = "black",
         ylab="Density", main="t Distributions")
    

    lines(x, dt(x,input$df), lwd=2, col="red")

    
    legend("topright", inset=.05, title="Distributions",
           labels, lwd=2, lty=c(1, 2), col=c("red", "black"))
  })
  
  # Output a data table for the upload tab page
  output$contents <- renderTable({
    inFile <- input$file1 
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
 
  })
  
  # Output a histogram for the variables user chose
  output$graph <- renderPlot({
    var1 <- data()[,input$var1]
    var2 <- data()[,input$var2]
    if (is.null(var1)){return(NULL)}
    if (is.null(var2)){return(NULL)}
    graph2 <- ifelse(input$sample == 'oneSamp', FALSE, TRUE)
    p1 <- hist(var1, breaks = input$bins)
    p2 <- hist(var2, breaks = input$bins)
    plot(p1, col=rgb(0,0,1,1/4))
    if(input$sample == 'twoSamp')
    plot(p2, col=rgb(1,0,0,1/4),add = graph2)
  })
  
  # Output of discriptive summary of this variable
  output$disc <-  renderPrint({
    Data <- data()
    if (is.null(Data)){return(NULL)}
    summary(Data)
  })
  
  # Output of the data structure
  output$str <- renderPrint({
    Data <- data()
    if (is.null(Data)){return(NULL)}
    str(Data)
  })
  
  # Create a one sample and two sample t-test reactive function
  ttestout <- reactive({
      var1 <- data()[,input$var1]
      conf <- input$conf
      if (is.null(var1)){return(NULL)}
      t1 <- t.test(var1, alternative = input$tail, mu = input$test, conf.level = conf)
      var2 <- data()[,input$var2]
      if (is.null(var2)){return(NULL)}
      ve <- ifelse(input$varequal == 'y', TRUE, FALSE)
      t2 <- t.test(var1, var2, alternative = input$tail, var.equal = ve, conf.level = conf)
      if(input$sample == "oneSamp") {return(t1)}
      if(input$sample == "twoSamp") {return(t2)}
    
  })
  
  # Output of one sample t value of t-test
  output$tvalue <- renderPrint({
    vals <- ttestout()
    if (is.null(vals)){return(NULL)}
    vals$statistic
  })
  
  # Output of p value
  output$pvalue <- renderPrint({
    vals <- ttestout()
    if (is.null(vals)){return(NULL)}
    vals$p.value 
  })
  
  # Output of key statistical parametric
  output$parametric <- renderTable({
    var1 <- data()[,input$var1]
    if (is.null(var)){return(NULL)}
    var2 <- data()[,input$var2]
    if (is.null(var)){return(NULL)}
    mean1 <- mean(var1)
    mean2 <- mean(var2)
    standard_deviation1 <- sd(var1)
    standard_deviation2 <- sd(var2)
    standard_error1 <- sd(var1)/sqrt(length(var1))
    standard_error2 <- sd(var2)/sqrt(length(var2))
    parametric1 <- data.frame(mean = mean1, 
                             standard_deviation=standard_deviation1, 
                             standard_error=standard_error1)
    rownames(parametric1) <- input$var1
    parametric2 <- data.frame(mean = mean2, 
                              standard_deviation=standard_deviation2, 
                              standard_error=standard_error2)
    rownames(parametric2) <- input$var2
    if(input$sample == "oneSamp") {return(parametric1)}
    if(input$sample == "twoSamp") {return(rbind(parametric1,parametric2))}
    })
    
  
  
})
