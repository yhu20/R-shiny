library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("united"),
  titlePanel("Hypothesis Test (T-test)"),
  br(),
  br(),
  br(),

  sidebarPanel(
    # SlidebarPanel for t-test
    conditionalPanel(condition = "$('li.active a').first().html()==='About T-test'",
                     h2("About One Sample T-test:"),
                     p("A t-test is any statistical hypothesis test in which the test statistic follows a Student's t distribution if the null hypothesis is supported."),
                     p("it is most commonly applied when the test statistic would follow a normal distribution if the value of a scaling term in the test statistic were known."),
                     p("When the scaling term is unknown and is replaced by an estimate based on the data, the test statistic (under certain conditions) follows a Student's t distribution."),
                     br(),
                     tags$a(href = "http://en.wikipedia.org/wiki/Student's_t-test", "More Detail About t-test."),
                     br(),
                     br(),
                     sliderInput('range',
                                 'The range you feel interest:',
                                  min = -50,
                                  max = 50,
                                  value = c(-10,10)),
                     sliderInput('df',
                                 'Degree of Freedom:',
                                  min = 1,
                                  max = 50,
                                  value = 1)
                     ),
    
    # SlidebarPanel for file upload tab
    conditionalPanel(condition = "$('li.active a').first().html()==='Data View'",
                     fileInput('file1', 'Choose CSV File',
                               accept=c('text/csv', 
                                        'text/comma-separated-values,text/plain', 
                                        '.csv')),
                     tags$hr(),
                     checkboxInput('header', 'Header', TRUE),
                     radioButtons('sep', 'Separator',
                                  c(Comma=',',
                                    Semicolon=';',
                                    Tab='\t'),
                                  ','),
                     radioButtons('quote', 'Quote',
                                  c(None='',
                                    'Double Quote'='"',
                                    'Single Quote'="'"),
                                  '"')
                     ),

    # SliderbarPanel for t-test tab
    conditionalPanel(condition = "$('li.active a').first().html()==='T-test'",
                     sliderInput("bins",
                                 "Numer of bins:",
                                 min = 1,
                                 max = 50,
                                 value = 2
                     ),
                     radioButtons("sample",
                                 "Please choose one sample t test or two sample t test:",
                                 choices = c("One sample" = "oneSamp", 
                                             "Two sample" = "twoSamp")),
                     selectInput("var1", 
                                 label = "Please Select a Numerical Variable",
                                 ""
                     ),
                     conditionalPanel(condition = "input.sample == 'twoSamp'",
                                      selectInput("var2", 
                                                  label = "Please Select a Numerical Variable",
                                                  ""
                                      ),
                                      radioButtons("varequal",
                                                   "Are the two samples have equal variance:",
                                                   choices = c("Yes" = "y",
                                                               "No" = "n"))
                                      ),
                     selectInput("tail",
                                 label = "Please Select a relationship you want to test:",
                                 choices = c("Equal" = "two.sided", 
                                             "Less" = "less",
                                             "Greater" = "greater")),
                     conditionalPanel(condition = "input.sample == 'oneSamp'",
                                      numericInput("test",
                                                   "Mean value You Want to Test",
                                                   value = 0
          
                                      )
                                      ),
                     numericInput("conf",
                                 label = "Please Select a confidence level:",
                                 value = 0.95,
                                 min = 0.8,
                                 max = 0.99),
                     helpText("Note: Please assign a number between 0 and 1 in the numeric Input")

                     )
    
    ),
  mainPanel(
    tabsetPanel(
      tabPanel('About T-test',
               plotOutput('tplot')),
      tabPanel('Data View', 
               fluidRow(column(10, offset = 1,
                               h2("Data Summary"),
                               verbatimTextOutput('disc'))),
               
               fluidRow(column(10, offset = 1,
                               h2("Data Structure"),
                               verbatimTextOutput('str'))),
               fluidRow(column(10, offset = 1,
                               h2("Data Table"),
                               tableOutput('contents')))      
               ),           
      tabPanel('T-test',
               fluidRow(column(10, offset = 1,
                               plotOutput('graph'))),
               fluidRow(column(8, offset = 1,
                               h2("Key summary statistics"),
                               p("The observed sample statistics were:"),
                               tableOutput('parametric'),
                               h2("Hypothesis of the t-test"),
                               p("We are testing the null hypothesis that the mean of population equals to the value you set"),
                               p("The observed t test statistic :"),
                               textOutput('tvalue'),
                               p("A low P value suggests that your sample provides enough evidence that you can reject the null hypothesis for the entire population."),
                               textOutput('pvalue')))
               )
      

      )
    )
  ))
