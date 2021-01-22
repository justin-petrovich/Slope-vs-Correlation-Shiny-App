library(shiny)
library(ggplot2)
library(wooldridge)
library(scales)



ui <- navbarPage("Correlation vs. Slope",
                 tabPanel("Overview","This app provides deeper insight into the similarities and differences between correlation and the slope of a regression line. The 'Overview' and 'Similarities and Differences' pages provide a conceptual explanation of correlation, slope, and their relationship. 
                          The 'Plotted Data' and 'Simulated Data' pages provide an interactive interface in which the user can explore correlation, slope, and their connection under various conditions, using real and simulated data, respectively.",
                          tags$br(tags$br(tags$b("Correlation Coefficient:"), "A statistic that provides a numerical measure of the strength and direction of a linear relationship between two variables." )),
                          withMathJax(),
                          tags$div(HTML("<script type='text/x-mathjax-config'>
                                        MathJax.Hub.Config({
                                        tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                                        });
                                        </script>
                                        ")),
                          
                          "In this app, we are refering to the Pearson correlation coefficeint. When working with sample data, the sample correlation measures the linear relationship formed between two variables, say x and y. 
                          The statistic is a unitless measurement derived by dividing the covariance by the product of standard deviations, as shown below.",
                          "In the equation, $r$ represents the sample correlation of the two variables, $x$ and $y$, $Cov$ represents their covariance, and $S_x$ and $S_y$ are their respective standard deviations.",
                          tags$br(h3("$$r=\\frac{Cov(x,y)}{S_xS_y}$$")),
                          "One useful characteristic of a correlation coefficient is that its value is restricted between -1 and 1. While there are no strict rules for interpreting the size of the correlation, any variables 
                          with a correlation greater than 0.6 can loosely be consideried ", 
                          tags$b("strongly correlated"), 
                          ". Similarly, variables with a correlation coefficient between 0.2 and 0.6 are often considered",
                          tags$b("moderately correlated."), 
                          "Finally, variables with a correlation coeffecient under 0.2 are considered", 
                          tags$b("weakly correlated."),
                          "Note that these are only rough guidelines, though, and that labeling a correlation as strong or weak will typically depend on the context of the data.",
                          
                          fluidRow(
                              column(plotOutput("StrongCorrelation"),height=3,width = 4),
                              column(plotOutput("WeakCorrelation"), height=3, width = 4),
                              column(plotOutput("NoCorrelation"),height=3,width = 4)
                          ),
                      
                          tags$br("The above scatter plots show representations of", tags$b("strong, moderate,"), "and", tags$b("weak Correlation"), 
                          "from left to right. Notice the close cluster of points in the strongly correlated representation. Notice the slight positive linear trend in the moderately correlated example. 
                          Finally, notice that there is basically no discernible linear trend in the weakly correlated example. For reference, a negative correlation of each strength would look identical, with the exception 
                          that the linear trend would be downward-sloping instead of upward-sloping."),
                          
                          tags$br(tags$b("Slope Coefficient:"), "The amount of change in the dependent variable, $y$, that can be expected, given a one-unit change in the independent variable, $x$."),
                          
                          tags$br("Unlike correlation, the slope coefficient does have units, which depend on the units of the two variables, $x$ and $y$. The slope coefficient can also be viewed as the 'best' fit line to the pairs $(x,y)$. 
                                  Below is the equation for the slope of a best fit line"),
                          
                          tags$br(h3("$$\\beta_1=\\frac{Cov(x,y)}{Var(x)}$$")),
                          "where $Var(x)$ represents the sample variance of $x$."
                          
                          
                        
                          
                 ),
                 tabPanel("Similarities and Differences",
                          
                          h3("Similarities"),
                          tags$br("1.) The signs of the correlation and slope are the same. That is, if two variables are negatively correlated, then the slope between the  two is negative, and vice versa."),
                          
                          tags$br("2.) Both correlation and slope only capture linear relationships. That is, any nonlinear relationship is not accurately represented by either the correlation coefficeint or the slope parameter."),
                          
                          tags$br("3.) The correlation coefficient (r) and the slope coefficient are mathematically related. That is, each term can be expressed in terms of the other. A sketch of the derivation follows."),
                          
                          tags$br("Recall from the overview page:"),
                          tags$br(" $$\\beta_1=\\frac{Cov(x,y)}{Var(x)}$$"),
                          "$$r=\\frac{Cov(x,y)}{S_xS_y}$$",
                          
                          "The first equation can be rewritten as follows:",
                          "$$\\beta_1*Var(x)=Cov(x,y)$$",
                          "Substitute this expression for the covariance on the right-hand side of the equation for $r$, which gives",
                          "$$r=\\frac{\\beta_1*var(x)}{S_xS_y}$$",
                          "Notice that $Var(x)$ is simply $S_x^2$. Therefore, the expression reduces to",
                          "$$r=\\frac{\\beta_1*S_x}{S_y}$$",
                          "To express the relationship in terms of the slope coefficient, simply multiply each side by the reciprocal of the ratio of standard deviations. 
                          Notice that the correlation coefficient and slope parameter are linearly related, so that as one increases, so will the other.",
                          
                          h3("Differences"),
                          tags$br("1.) The correlation coefficient of two variables must be between -1 and 1. The slope coefficient has a domain of all real numbers.
                          This is a consequence of the fact that the correlation coefficient is unitless, while the slope coefficient depends on the units of $x$ and $y$."),
                          
                          tags$br("2.) The purpose of the correlation coefficient is to determine how two variables move linearly together. 
                          As such, there is no distinction between independent and dependent variables; that is, the variables can be reversed and the correlation will be identical. 
                          The slope of the best fit line attempts to determine how the dependent variable, $y$, changes BECAUSE OF the independent variable, $x$. 
                          Though the variables can be mathematically reversed, the slope parameter will change due to the reversal."),
                          
                          tags$br("3.) The correlation coefficient states the strength of the linear relationship. 
                          That is, a correlation coefficient close to 1 has a strong linear relationship and a coefficient close to 0 has no linear relationship. 
                          The slope parameter does not address the strength of a linear relationship. Rather it states the rate of change.
                          Importantly, this means that a large slope coefficient does not necessarily imply a strong correlation, and vice versa.
                          This can be seen through the mathematical relationship between $r$ and $\\beta_1$ shown above, and through the 'Simulated Data' tab of this app.")
                          
                          
                          ),
                 tabPanel("Plotted Data",
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("selector", "Select an Input",
                                          c("Assess",
                                            "Number of Bedrooms",
                                            "Lot Size",
                                            "Square Footage",
                                            "Log-lot Size",
                                            "Log-square Footage",
                                            "Price")),
                              h4("Instructions"),
                              ("Select an above input to compare to the actual price value. Price is always on the Y axis and the selected input is on the x axis. 
                              The correlation coefficient between the two variables is displayed below the graphic along with the slope of the best fit line."),
                              tags$br("Notice that for each input the correlation and slope are different. Also take note of how outlier values affect both the slope and correlation for the pair of variables. 
                                      Finally, take note of what occurs when price is plotted against itself."),
                               
                              h4("About the Data"),
                              ("The data plotted to the right are from the 'hprice1' dataset in the Wooldridge R package. The dataset consists of house prices and associated variables from listed houses in the Boston area in 1990.
                              The variable 'Assess' is the valued assessment of the house. 'Number of Bedrooms' is the number of bedrooms contained in the house. 
                              'Lot Size' is the size of the entire property. 'Square Footage' is the square footage of the house. 
                              'Log-Square Footage' and 'Log-Lot Size' are simply a logarithmic transformation of the respective variables.")
                              
                            ),
                            mainPanel(
                              "The slope of the best fit line is derived via the ordinary least squares regression method. In this case, only one variable is used. The equation for the regression follows.",
                              "$$Price= \\beta_0+\\beta_1*x_i+u_i$$",
                              "Where, Price is the dependent variable, $\\beta_0$ is the intercept, $\\beta_1$ is the slope parameter, $x_i$ is the chosen independent variable, and $u_i$ is the error term",
                              plotOutput("scatterPlot"),
                            h4("Correlation Coefficient"),                             
                               textOutput("Correlation"),
                            h4("Slope Coefficient"),  
                              textOutput("slope")
                            )
                          )),
          
                ###Simulated Data UI Section!!! Probably going to have to tweak###        
                 tabPanel("Simulated Data",
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              
                              numericInput("Beta1", "Choose Slope Value",
                                          value=1,min=-20, max=20),
                              
                              numericInput("Sdx", "Choose Standard Deviation of X",
                                          value=1, min=0, max=1000),
                              numericInput("Sdu", "Choose Standard Deviation of U",
                                          value = 1, min = 0, max = 1000),
                              
                              plotOutput("SimulatedData1"),
                              plotOutput("SimulatedData2")
                            
                            ),
                            mainPanel(
                              "This page provides simulated data and an interface to manipulate parameters. Enter values for Slope, Standard Deviation of $x$, and Standard Deviation of $u$ to see the effect on correlation. 
                              See below for the conncetion between the standard deviation of $u$ and the standard deviation of $y$.",
              
                              plotOutput("scatterPlot2"),
                              
                              h4("Correlation Coefficient"),
                              textOutput("Correlation2"),
                              h4("Slope Coefficient"),
                              textOutput("slope2"),
                              
                              h4("Relationship between $S_u$ and $S_y$"),
                              "Remember that the slope and correlation are related as follows:",
                              "$$r=\\frac{\\beta_1\\times S_x}{S_y}.$$",
                              "However, the standard deviation of $y$ can actually be rewritten:",
                              "$$S_y = \\sqrt{Var(y)} = \\sqrt{\\beta_1^2 S_x^2 + S_u^2}.$$",
                              "Substituting this expression for $S_y$, the correlation can be rewritten as",
                              "$$r = \\frac{\\beta_1\\times S_x}{\\sqrt{\\beta_1^2 S_x^2 + S_u^2}}.$$",
                              "This illustrates the relationship between the variation of the error term and the correlation coefficient.",
                              "From this expression we can see that the denominator is always greater than or equal to the nummerator, thereby explaining why the magnitude of the correlation is bounded by 1.",
                              "To explore these relationships, try entering different values of $\\beta_1$, $S_x$, and $S_u$ to see how these change the slope and correlation.",
                              h4("Important Takeaways"),
                              tags$br("1.) With a positive slope and all other parameters fixed, how does increasing the standard deviation of the x term affect the correlation coefficient? What about if the slope is negative?"),
                              tags$br("2.) With a positive slope and all other parameters fixed, how does increasing the standard deviation of the y term affect the correlation coefficient? What about if the slope is negative?"),
                              tags$br("3.) With a positive slope and all other parameters fixed, how does increasing the standard deviation of the u term affect the correlation Coefficient? What about if the slope is negative?"),
                              tags$br("4.) If the correlation coefficient is fixed, how does increasing the standard deviation of x affect the slope parameter?"),
                              tags$br("5.) If the correlation coefficeint is fixed, how does increasing the standard deviation of y affect the slope parameter?")
                            )
                          )
                          
                          
                          )
                ###End of Simulated Data UI Section###
)

server <- function(input, output, session) {
  set.seed(1)
  f<-(1:100)
  g<-f+rnorm(100, mean = 0, sd=10)
  output$StrongCorrelation<-renderPlot(plot(f,g, xlab = "X-axis", ylab = "Y-axis"))
  set.seed(2)
  U<-(1:100)
  W<-U+rnorm(100,mean = 0,sd=25)
  output$WeakCorrelation<-renderPlot(plot(U,W, xlab = "X-axis", ylab = "Y-axis"))
  
  set.seed(3)
  Q<-(1:100)
  A<-Q+rnorm(100,mean = 0,sd=500)
  output$NoCorrelation<-renderPlot(plot(Q,A, xlab = "X-axis", ylab = "Y-axis"))
  
  
  
  
  dataz<-reactive({switch(input$selector,
                          "Assess"= hprice1$assess,
                          "Number of Bedrooms"= hprice1$bdrms,
                          "Lot Size"= hprice1$lotsize ,
                          "Square Footage"= hprice1$sqrft,
                          "Log-lot Size"= hprice1$llotsize,
                          "Log-square Footage"= hprice1$lsqrft,
                          "Price"=hprice1$price)})
  
  fit <- reactive(lm(price~dataz(), data = hprice1))
  
  output$scatterPlot<- renderPlot({
    ggplot(hprice1,aes(x=dataz(), y=price)) + 
      geom_point(col="red") +
      geom_abline(intercept = coef(fit())[1],
                  slope = coef(fit())[2],
                  col="blue") +
      labs(x = input$selector)
  })
  
  output$Correlation<- renderPrint({cor(dataz(),hprice1$price)})
   
  slope <- reactive(coef(fit())[2])
  output$slope<- renderPrint(unname(slope()))

 ####start of simulated Data Server Section!!### 
  
  beta0 <- 15
  datax <- reactive(rnorm(1000,mean = 0,sd = input$Sdx))
  error <- reactive(rnorm(1000,mean = 0,sd = input$Sdu))
  
  y <- reactive({
    beta1 <- input$Beta1
    datay <- beta0 + beta1*datax() + error()
    datay
  })
  
  
  output$scatterPlot2 <- renderPlot({
    df<-data.frame(cbind(y(),datax()))

    ggplot(df,aes(x=datax(),y=y())) +
      geom_point(col="red") +
      geom_abline(intercept = beta0, slope = input$Beta1, col="blue") +
      labs(x="x-axis", y="y-axis")
  })
  output$Correlation2 <- renderPrint({cor(datax(),y())})
  output$slope2 <- renderPrint({input$Beta1})
  output$SimulatedData1 <- renderPlot(hist(datax(),xlab = "x",main = "Histogram of x"))
  output$SimulatedData2 <- renderPlot(hist(y(),xlab = "y",main = "Histogram of y"))
  
####End of Simulated Data Server Section####  
  
  
  
  
  }

shinyApp(ui, server)