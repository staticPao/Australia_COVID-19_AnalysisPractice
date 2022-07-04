
#Zihao Huang

library(rvest)
library(stringr)

#Define a function for collecting case data from a web page. 
#The input to the function is the table type and state abbreviation,
#Specifically,the types of tables include£ºdaily-cases / daily-vaccinations / daily-hospitalised / daily-active-cases
getWebData <- function(Table,State){
  
  #load web page
  #combining links based on the regularity of links between different tables.
  Url <- paste("https://covidlive.com.au/report/",Table,"/",State,sep = "")
  html <- read_html(Url)
  
  #Gather date information and process the format
  date <- html %>% html_nodes("td.COL1.DATE") %>% html_text() %>% gsub(pattern = " ",replacement = "-") 
  date <- str_replace_all(date,c("Jan" = "1","Feb" = "2","Mar" = "3","Apr" = "4","May" = "5",
                        "Jun" = "6","Jul" = "7","Aug" = "8","Sep" = "9","Oct" = "10","Nov" = "11","Dec" = "12"))
  date <- as.Date(date,format = "%d-%m-%y")
  
  #for daily cases
  if (Table == "daily-cases"){ 
    new <- html %>% html_nodes("td.COL2.NEW") %>% html_text() %>% gsub(pattern = ",", replacement = "") %>% as.numeric()
    
    #Merge the above data and create a dataframe
    cases_table <- data.frame(date, new)
    colnames(cases_table) <- c("DATE","NEW")
    cases_table$STATE = State
                                  
    return(cases_table)
  }
  
  #for daily vaccinations
  if (Table == "daily-vaccinations"){
      doses <- html %>% html_nodes("td.COL2.DOSES") %>% html_text() %>% gsub(pattern = ",", replacement = "") %>% as.numeric()
      net <- html %>% html_nodes("td.COL4.NET") %>% html_text() %>% gsub(pattern = "-", replacement = "") %>% 
        gsub(pattern = ",", replacement = "") %>% as.numeric()
      
      #Merge the above data and create a dataframe
      vaccinations_table <- data.frame(date, doses, net )
      colnames(vaccinations_table) <- c("DATE","DOSES","NET")
      vaccinations_table$STATE = State
      
      return(vaccinations_table)
  }
  
  #for daily hospitalised
  if (Table == "daily-hospitalised"){
      hosp <- html %>% html_nodes("td.COL2.HOSP") %>% html_text() %>% gsub(pattern = ",", replacement = "") %>% as.numeric()
      icu <- html %>% html_nodes("td.COL3.ICU") %>% html_text() %>% as.numeric()
      
      #Merge the above data and create a dataframe
      hospital_table <- data.frame(date, hosp, icu)
      colnames(hospital_table) <- c("DATE","HOSP","ICU")
      hospital_table$STATE = State
      
      return(hospital_table)
  }
  
  #for daily active cases
  if (Table == "daily-active-cases"){
      active <- html %>% html_nodes("td.COL2.ACTIVE") %>% html_text() %>% gsub(pattern = ",", replacement = "") %>% as.numeric()
      net <- html %>% html_nodes("td.COL4.NET") %>% html_text() %>% gsub(pattern = "-", replacement = "") %>% 
        gsub(pattern = ",", replacement = "") %>% as.numeric()
      
      #Merge the above data and create a dataframe
      active_table <- data.frame(date, active, net)
      colnames(active_table) <- c("DATE","ACTIVE","NET")
      active_table$STATE = State
      
      return(active_table)
    
  }
}

#Collect and organize the required data based on the above custom function
all_cases <- rbind(getWebData("daily-cases","vic"),getWebData("daily-cases","nsw"),getWebData("daily-cases","wa"))  
all_vaccinations <- rbind(getWebData("daily-vaccinations","vic"),getWebData("daily-vaccinations","nsw"),getWebData("daily-vaccinations","wa"))  
all_hospitalised <- rbind(getWebData("daily-hospitalised","vic"),getWebData("daily-hospitalised","nsw"),getWebData("daily-hospitalised","wa"))
all_active <- rbind(getWebData("daily-active-cases","vic"),getWebData("daily-active-cases","nsw"),getWebData("daily-active-cases","wa"))

#Save data to local (saved in working directory)
save(all_cases, all_vaccinations, all_hospitalised, all_active, file = "data.RData")
#print(getwd())
