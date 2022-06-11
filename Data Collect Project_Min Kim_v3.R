### Rules ###
# 1 <- value weight; 2 <-  equal weight
# MO <- monthly; YR <- yearly 

setwd("C:/Users/Lenovo/OneDrive/Documents/Degree/SKKU/Economic Data Analysis/Homework")

library(dplyr)
library(readr)
library(rvest)
library(readxl)
library(xlsx)
library(writexl)
library(stringr)
library(magrittr)
library(ggplot2)
library(plotly)
library(lubridate)
library(frenchdata)
library(zoo)
library(tidyr)
library(RColorBrewer)
library(ggrepel)
library(data.table)
library(esquisse)
library(ggthemes)
library(highcharter)
library(quantmod)
library(stargazer)
library(ggeffects)
library(lubridate)
library(rJava)

### [Data Collect] ###

FF_List <- read_xlsx("FF List.xlsx")

get.ff.data.1 <- function(Factor){
  
  Name1 <- as.character(FF_List[which(FF_List$Code == Factor),"Name"])
  assign("Data1", download_french_data(Name1))
  
  return(Data1$subsets$data[[1]])
  
}

### [Data Trim] ###

# mutate(key = factor(key, levels = colnames(ff_mom$subsets$data[[1]])[-1])) 

Factor_Single <- list("MKT","SMB","HML","RMW","CMA","WML","EP","CFP","DIV","ACC")

# Since 1964: 

for (i in Factor_Single){
    
  out <- get.ff.data.1(i)%>% 
    mutate(date = sub("(\\d{4})", "\\1-", date)) %>% 
    mutate(date = as.Date(as.yearmon(date), frac = 1)) %>%
    filter(date > "1963-12-31") %>% 
    mutate(across(-date, ~. / 100)) %>%
    mutate(across(-date, ~log(1+.))) %>%
    mutate(across(-date, ~cumsum(.)))
  
  out1 <- out[,(ncol(out)-9):ncol(out)] 
  out2 <- cbind(out[,1],out1) %>% 
    pivot_longer(names_to = 'key', values_to = 'value', -date)
  
  FF.Data.Name <- str_c("FF_",i,"_1_MO_1964")

  assign(FF.Data.Name, out2)
    
  Sys.sleep(3)
  
}

# Since 2000: 

for (i in Factor_Single){
  
  out <- get.ff.data.1(i)%>% 
    mutate(date = sub("(\\d{4})", "\\1-", date)) %>% 
    mutate(date = as.Date(as.yearmon(date), frac = 1)) %>%
    filter(date > "1999-12-31") %>% 
    mutate(across(-date, ~. / 100)) %>%
    mutate(across(-date, ~log(1+.))) %>%
    mutate(across(-date, ~cumsum(.)))
  
  out1 <- out[,(ncol(out)-9):ncol(out)] 
  out2 <- cbind(out[,1],out1) %>% 
    pivot_longer(names_to = 'key', values_to = 'value', -date)
  
  FF.Data.Name <- str_c("FF_",i,"_1_MO_2000")
  
  assign(FF.Data.Name, out2)
  
  Sys.sleep(3)
  
}

# Since 2010: 

for (i in Factor_Single){
  
  out <- get.ff.data.1(i)%>% 
    mutate(date = sub("(\\d{4})", "\\1-", date)) %>% 
    mutate(date = as.Date(as.yearmon(date), frac = 1)) %>%
    filter(date > "2009-12-31") %>% 
    mutate(across(-date, ~. / 100)) %>%
    mutate(across(-date, ~log(1+.))) %>%
    mutate(across(-date, ~cumsum(.)))
  
  out1 <- out[,(ncol(out)-9):ncol(out)] 
  out2 <- cbind(out[,1],out1) %>% 
    pivot_longer(names_to = 'key', values_to = 'value', -date)
  
  FF.Data.Name <- str_c("FF_",i,"_1_MO_2010")
  
  assign(FF.Data.Name, out2)
  
  Sys.sleep(3)
  
}

# Since 2020: 

for (i in Factor_Single){
  
  out <- get.ff.data.1(i)%>% 
    mutate(date = sub("(\\d{4})", "\\1-", date)) %>% 
    mutate(date = as.Date(as.yearmon(date), frac = 1)) %>%
    filter(date > "2019-12-31") %>% 
    mutate(across(-date, ~. / 100)) %>%
    mutate(across(-date, ~log(1+.))) %>%
    mutate(across(-date, ~cumsum(.)))
  
  out1 <- out[,(ncol(out)-9):ncol(out)] 
  out2 <- cbind(out[,1],out1) %>% 
    pivot_longer(names_to = 'key', values_to = 'value', -date)
  
  FF.Data.Name <- str_c("FF_",i,"_1_MO_2020")
  
  assign(FF.Data.Name, out2)
  
  Sys.sleep(3)
  
}

# YTD: 

for (i in Factor_Single){
  
  out <- get.ff.data.1(i)%>% 
    mutate(date = sub("(\\d{4})", "\\1-", date)) %>% 
    mutate(date = as.Date(as.yearmon(date), frac = 1)) %>%
    filter(date > as.Date(str_c(year(Sys.Date())-1,"-12-31"))) %>% 
    mutate(across(-date, ~. / 100)) %>%
    mutate(across(-date, ~log(1+.))) %>%
    mutate(across(-date, ~cumsum(.)))
  
  out1 <- out[,(ncol(out)-9):ncol(out)] 
  out2 <- cbind(out[,1],out1) %>% 
    pivot_longer(names_to = 'key', values_to = 'value', -date)
  
  FF.Data.Name <- str_c("FF_",i,"_1_MO_YTD")
  
  assign(FF.Data.Name, out2)
  
  Sys.sleep(3)
  
}

### [Save as Excel] ### 

options(java.parameters = "-Xmx8g")  ## memory set to 8 GB

Factor_Date <- list("1964","2000","2010","2020","YTD")

for (i in Factor_Single){
  
  if(i=="MKT")
  
  for (d in Factor_Date){
    
    name <- str_c("FF_",i,"_1_MO_",d)
    out3 <- get(name)
    
    if(d=="1964")
    write.xlsx(out3, file = "FF_Factor_Returns.xlsx",
               sheetName = name)
    else
      write.xlsx(out3, file = "FF_Factor_Returns.xlsx",
                 sheetName = name, append = TRUE)
    
    Sys.sleep(1)
    print(paste(d," Complete!"))
    gc()
    
  }
  
  else
    for (d in Factor_Date){
      
      name <- str_c("FF_",i,"_1_MO_",d)
      out3 <- get(name)
      write.xlsx(out3, file = "FF_Factor_Returns.xlsx",
                   sheetName = name, append = TRUE)
      
      Sys.sleep(1)
      print(paste(d," Complete!"))
      gc()
    }
  }


### [Stock Data] ###

get.stock.data <- function(Stock_Code){

  Stock_Price_Data <- getSymbols(Stock_Code, from = "2019-01-01", to = Sys.Date()-1, auto.assign = F)
  
  Stock_Price_Close <- Stock_Price_Data[,4]
  
  Convert_to_Return <- dailyReturn(Stock_Price_Close, type="log") 
  
  Stock_Price_Return <- fortify.zoo(Convert_to_Return) 
  
  colnames(Stock_Price_Return) = c("Date", "Return")
  
  return(Stock_Price_Return)
  
}

assign("Stock_Price_Return", get.stock.data(Stock_Code = "NVDA")) # stock code here.
View(Stock_Price_Return)

### [F.F. 5 Factors] ###

FF5 <- download_french_data("Fama/French 5 Factors (2x3) [Daily]") 
FF_5Factors <- FF5$subsets$data[[1]] %>%  na.omit() 
FF_5Factors$date <- as.Date(strptime(FF_5Factors$date, format = "%Y%m%d"))
colnames(FF_5Factors)[1] <-  "Date"
colnames(FF_5Factors)[2] <-  "Mkt.RF"

### [Factor Attribution Analysis] ###

Attr_Data <- merge(Stock_Price_Return, FF_5Factors, by = "Date")
Attr_Data$ExcessReturn <- Attr_Data$Return - Attr_Data$RF
Attr_Reg <- lm(ExcessReturn ~ Mkt.RF + SMB + HML + RMW + CMA, data = Attr_Data)

### [Regression Plots] ### 

par(mfrow = c(2,2))
plot(Attr_Reg)

### [Regression Table] ### 
install.packages("ztable")
library(ztable)
options(ztable.type="html")
z <- ztable(Attr_Reg)
print(z, type  = 'viewer')

# Find Factor List here: get_french_data_list()
# Find Data Details here: browse_details_page()
# References
# Fama, Eugene F, and Kenneth R French. 1992. “The Cross-Section of Expected Stock Returns.” The Journal of Finance 47 (2): 427–65.
# Fama, Eugene F, and Kenneth R French. 1993. “Common Risk Factors in the Returns on Stocks and Bonds.” Journal of Financial Economics 33 (1): 3–56.