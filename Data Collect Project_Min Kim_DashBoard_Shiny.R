#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# setwd("C:/Users/Lenovo/OneDrive/Documents/Degree/SKKU/Economic Data Analysis/Homework")
library(markdown)
library(shinydashboard)
library(flexdashboard)
library(dashboardthemes)
library(shiny)
library(shinyjs)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)
library(ggplot2)
library(xlsx)
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(stringr)
library(quantmod)
library(stargazer)
library(frenchdata)
library(rvest)
library(readxl)
library(writexl)
library(stringr)
library(magrittr)
library(lubridate)
library(RColorBrewer)
library(ggrepel)
library(data.table)
library(esquisse)
library(ggthemes)
library(highcharter)
library(ggeffects)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(plotly)
library(extrafont)
library(htmlwidgets)

linebreaks <- function(n){HTML(strrep(br(), n))}
Factor.Variables = c("MKT","SMB","HML","RMW","CMA","WML","EP","CFP","DIV","ACC")
Year.Variables = c("1964","2000","2010","2020","YTD")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # class="text-center",
  # tabItems(
  #   tabItem(
  #     "Tools", 
  #     tags$iframe(
  #       src = "https://minkim.shinyapps.io/EDAproject/", 
  #       id = 'myIframe',
  #       scrolling = 'no'
  #     ),
  #     tags$script(HTML("iFrameResize({ log: true }, '#myIframe')"))
  #   )
  # ),
  
  navbarPage("Fama-French Dashboard",
             theme = bslib::bs_theme(bootswatch = "yeti"),
             fluid = TRUE,
             tabPanel("Factor Returns",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "factor", 
                                        label = "Select Factor:", 
                                        choices = Factor.Variables,
                                        selected = "HML"),
                            selectInput(inputId = "indexed", 
                                        label = "Select Indexed Year:", 
                                        choices = Year.Variables,
                                        selected = "2000"),
                            
                            linebreaks(2),
                            
                            helpText("Note: The portfolios constructed each month include NYSE, AMEX, and NASDAQ stocks with prior return data."),
                            
                            linebreaks(1),
                            
                            helpText("References: 
                              Fama, Eugene F, and Kenneth R French. 1992. “The Cross-Section of Expected Stock Returns.” The Journal of Finance 47 (2): 427?65.", linebreaks(1),"???. Fama, Eugene F, and Kenneth R French. 1993. “Common Risk Factors in the Returns on Stocks and Bonds.” Journal of Financial Economics 33 (1): 3?56."),
                          ),
                          
                          mainPanel(
                            
                            fluidRow(
                              plotlyOutput("decile")
                            ),
                            
                            linebreaks(1),
                            
                            fluidRow(
                              plotlyOutput("longshort")
                            ),
                            
                            linebreaks(1),
                            
                            helpText(strong("Description:"), 
                                     linebreaks(2), 
                                     strong(" SMB (Small Minus Big)")," is the average return on the nine small stock portfolios minus the average return on the nine big stock portfolios.", linebreaks(2), strong(" HML (High Minus Low")," is the average return on the two value portfolios minus the average return on the two growth portfolios.", linebreaks(2), strong(" RMW (Robust Minus Weak)")," is the average return on the two robust operating profitability portfolios minus the average return on the two weak operating profitability portfolios.", linebreaks(2), strong(" CMA (Conservative Minus Aggressive)")," is the average return on the two conservative investment portfolios minus the average return on the two aggressive investment portfolios.", linebreaks(2), strong(" Rm-Rf, the excess return on the market"), ", value-weight return of all CRSP firms incorporated in the US and listed on the NYSE, AMEX, or NASDAQ that have a CRSP share code of 10 or 11 at the beginning of month t, good shares and price data at the beginning of t, and good return data for t minus the one-month Treasury bill rate (from Ibbotson Associates).",linebreaks(2),strong("Other factors denote:"), "earnings-price (EP); cash earnings to price (CEP); dividend yield (DP); accruals (ACC); and momentum(WML).")
                          ),
                          fluid = TRUE
                        ),
                      )
             ),
             
             tabPanel("Stock Return Decomposition",
                      fluidPage(
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            
                            p("Type stock code and select factors"),
                            
                            textInput("codeInput","Stock Code:",
                                      value = "AAPL"),
                            helpText("Enter a stock code here (e.g. NVDA)", linebreaks(1),"Note: US stocks only"),
                            
                            linebreaks(1),
                            
                            checkboxGroupInput("factorInput", "Factor(s):",
                                               choices = c("MKTminusRF","SMB","HML","RMW","CMA"),
                                               selected = c("MKTminusRF","SMB","HML")),                         
                            linebreaks(2),
                            
                          ),
                          
                          mainPanel(
                            
                            fluidRow(
                              tableOutput("regression_stats")
                            ),
                            
                            linebreaks(1),
                            
                            fluidRow(
                              helpText(strong("Description:"), "The Fama/French 5 factors (2x3) are constructed using the 6 value-weight portfolios formed on size and book-to-market, the 6 value-weight portfolios formed on size and operating profitability, and the 6 value-weight portfolios formed on size and investment.",strong(" SMB (Small Minus Big)")," is the average return on the nine small stock portfolios minus the average return on the nine big stock portfolios.", strong(" HML (High Minus Low")," is the average return on the two value portfolios minus the average return on the two growth portfolios.", strong(" RMW (Robust Minus Weak)")," is the average return on the two robust operating profitability portfolios minus the average return on the two weak operating profitability portfolios.", strong(" CMA (Conservative Minus Aggressive)")," is the average return on the two conservative investment portfolios minus the average return on the two aggressive investment portfolios.", strong(" Rm-Rf, the excess return on the market"), ", value-weight return of all CRSP firms incorporated in the US and listed on the NYSE, AMEX, or NASDAQ that have a CRSP share code of 10 or 11 at the beginning of month t, good shares and price data at the beginning of t, and good return data for t minus the one-month Treasury bill rate (from Ibbotson Associates).")
                            )
                          ),
                          fluid = TRUE
                        ),
                        
                      )
             ),
             
             tabPanel("Residuals",
                      fluidPage(
                        fluidRow(
                          plotOutput("residualplots")
                        ),
                      ))
  ))




server <- function(input, output){
  
  sheet_name <- reactive({
    
    sheet.name <- str_c("FF_",input$factor,"_1_MO_",input$indexed)
    
  })
  
  output$decile <- renderPlotly({
    
    data1 <- read_xlsx("FF_Factor_Returns.xlsx", sheet = sheet_name()) %>% as.data.frame()
    
    ggplotly(
      ggplot(data1, aes(x = date, y = round(value,digits=1), group = key)) +
        geom_line(aes(color = key), size = 0.5) +
        theme_bw() +
        xlab('') + ylab('Cumulative Logarithmic Return (Deciles)') + labs(fill = "")+
        theme(legend.title = element_blank(), 
              legend.direction='vertical',
              legend.text = element_text(size = 8),
              text=element_text(family = "Arial")) +
        scale_color_brewer(palette='Set3') +
        guides(color = guide_legend(reverse = TRUE))
    )
  })
  
  output$longshort <- renderPlotly({
    
    data2 <- read_xlsx("FF_Factor_Returns.xlsx", sheet = sheet_name()) %>%
      as.data.frame() %>% 
      pivot_wider(id_cols = 'date', names_from = 'key', values_from = 'value')
    data2$longminusshort <- 
      if(str_contains(sheet_name(), c("SMB","CMA","ACC"), logic = "or") == TRUE){
        data2[,2] - data2[,11]
      }else{
        data2[,11] - data2[,2]
      }
    as.data.frame(data2)
    data2$longminusshort <- unlist(data2$longminusshort)
    
    ggplotly(
      ggplot(data2, aes(x = date, y = round(longminusshort,digits = 1))) +
        geom_line(size = 0.5) +
        theme_bw() +
        xlab('') + ylab('Cumulative Logarithmic Return (Long-Short)') + labs(fill = "")+
        theme(text=element_text(family = "Arial"))
    )
  })
  
  FF5 <- download_french_data("Fama/French 5 Factors (2x3) [Daily]") 
  FF_5Factors <- FF5$subsets$data[[1]] %>%  na.omit() 
  FF_5Factors$date <- as.Date(strptime(FF_5Factors$date, format = "%Y%m%d"))
  colnames(FF_5Factors)[1] <-  "Date"
  colnames(FF_5Factors)[2] <-  "MKTminusRF"
  
  get.stock.data <- function(Stock_Code){
    
    Stock_Price_Data <- getSymbols(Stock_Code, from = "2010-01-01", to = Sys.Date()-1, auto.assign = F)
    
    Stock_Price_Close <- Stock_Price_Data[,4]
    
    Convert_to_Return <- dailyReturn(Stock_Price_Close, type="log") 
    
    Stock_Price_Return <- fortify.zoo(Convert_to_Return) 
    
    colnames(Stock_Price_Return) = c("Date", "Return")
    
    return(Stock_Price_Return)
    
  }
  
  FF_Dataset <- reactive({
    FF_5Factors
  })
  
  Regression_Data <- reactive({
    
    assign("Stock_Price_Return", get.stock.data(Stock_Code = input$codeInput))
    Regression_Data <- merge(Stock_Price_Return, FF_Dataset(), by = "Date")
    Regression_Data$ExcessReturn <- Regression_Data$Return - Regression_Data$RF
    Regression_Data
  })
  
  ExcessReturn <- reactive({
    Regression_Data() %>% select(ExcessReturn) %>% unlist()
    
  })
  
  regFormula <- reactive({
    as.formula(paste("ExcessReturn()"," ~ ",paste(input$factorInput, collapse = "+")))
    
  })
  
  model <- reactive({
    lm(regFormula(), data = Regression_Data())
  })
  
  output$regression_stats <- renderTable({
    
    req(model())
    stargazer(model(), summary = T, title = "Regression Statistics", type = "text")
  })
  
  output$residualplots <- renderPlot({
    
    par(mfrow = c(2,2))
    plot(model())
  })
  
  
}


shinyApp(ui = ui, server = server)