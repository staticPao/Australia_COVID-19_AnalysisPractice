#COMP 5070
#Zihao Huang

library(shiny)
library(shinydashboard)
library(ggplot2)

#Make sure analysis.R has been run before running this program
ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 in Australia"),
  dashboardSidebar(
    #The left panel contains the labels of all tables and the components that control the table, 
    #specifically, the date range or the number of weeks of the table can be changed.
    sidebarMenu(
      #Date control component
      dateRangeInput("Date1","Date range (new & cumulative)",
                     start = "2020-01-26",
                     end = "2022-06-27"),
      menuItem("Development - new", tabName = "Plot"),
      menuItem("Development - cumulative", tabName = "Plot1"),
      menuItem("Pre 1000 - new", tabName = "Plot2"),
      menuItem("Pre 1000 - cumulative", tabName = "Plot3"),
      #Week number control component
      sliderInput("week1", "Number of Week(case&vac)",
                  min = 0, max = 71, value = 1, animate = TRUE),
      menuItem("Weekly case&vac -VIC", tabName = "Plot4"),
      menuItem("Weekly case&vac -NSW", tabName = "Plot5"),
      menuItem("Weekly case&vac -WA", tabName = "Plot6"),
      dateRangeInput("Date2","Date range (proportion)",
                     start = "2020-04-01",
                     end = "2022-06-27"),
      menuItem("Proportion hosp/active -VIC", tabName = "Plot7"),
      menuItem("Proportion hosp/active -NSW", tabName = "Plot8"),
      menuItem("Proportion hosp/active -WA", tabName = "Plot9"),
      sliderInput("week2", "Number of Week(proportion)",
                  min = 0, max = 120, value = 1, animate = TRUE),
      menuItem("Proportion hosp/active -VIC(7days)", tabName = "Plot7w"),
      menuItem("Proportion hosp/active -NSW(7days)", tabName = "Plot8w"),
      menuItem("Proportion hosp/active -WA(7days)", tabName = "Plot9w")
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Plot", plotOutput("Plot")),
      tabItem(tabName = "Plot1",plotOutput("Plot1")),
      tabItem(tabName = "Plot2",plotOutput("Plot2")),
      tabItem(tabName = "Plot3",plotOutput("Plot3")),
      tabItem(tabName = "Plot4",plotOutput("Plot4")),
      tabItem(tabName = "Plot5",plotOutput("Plot5")),
      tabItem(tabName = "Plot6",plotOutput("Plot6")),
      tabItem(tabName = "Plot7",plotOutput("Plot7")),
      tabItem(tabName = "Plot7w",plotOutput("Plot7w")),
      tabItem(tabName = "Plot8",plotOutput("Plot8")),
      tabItem(tabName = "Plot8w",plotOutput("Plot8w")),
      tabItem(tabName = "Plot9",plotOutput("Plot9")),
      tabItem(tabName = "Plot9w",plotOutput("Plot9w"))
      )
  )
)

server <- function(input, output) {
  #Reflect the output according to the plot section in analysis.R 
  #which contains all tables
  output$Plot <- renderPlot({
    all_cases_plot <- subset(all_cases,DATE >input$Date1[1])
    all_cases_plot <- subset(all_cases_plot,DATE < input$Date1[2])
    ggplot(all_cases_plot, aes(x = DATE, y = NEW, group = STATE, col = STATE)) + 
      ggtitle("Development of cumulative cases") +
      geom_line(size = 0.8,na.rm = TRUE) + theme_bw() + labs(x = "Date",y = "Cumulative cases")
  })
  
  output$Plot1 <- renderPlot({
    all_cases_plot1 <- subset(all_cases,DATE >input$Date1[1])
    all_cases_plot1 <- subset(all_cases_plot1,DATE < input$Date1[2])
    ggplot(all_cases_plot1 , aes(x = DATE, y = CASES, group = STATE, col = STATE)) + 
      ggtitle("Development of daily new cases ") +
      geom_line(size = 0.8,na.rm = TRUE) + theme_bw() + labs(x = "Date",y = "New cases")
  })
  
  output$Plot2 <- renderPlot({
    cases_pre_plot2 <- subset(cases_pre,DATE >input$Date1[1])
    cases_pre_plot2 <- subset(cases_pre_plot2,DATE < input$Date1[2])
    ggplot(cases_pre_plot2 , aes(x = DATE, y = NEW_PRE_1000, group = STATE, col = STATE)) + 
      ggtitle("Daily new cases per 1,000 people") +
      geom_line(size = 0.8,na.rm = TRUE) + theme_bw() + labs(x = "Date",y = "Daily new cases(pre 1,000)")
  })
  
  output$Plot3 <- renderPlot({
    cases_pre_plot3 <- subset(cases_pre,DATE >input$Date1[1])
    cases_pre_plot3 <- subset(cases_pre_plot3,DATE < input$Date1[2])
    ggplot(cases_pre_plot3 , aes(x = DATE, y = CASES_PRE_1000, group = STATE, col = STATE)) + 
      ggtitle("Cumulative cases per 1,000 people") +
      geom_line(size = 0.8,na.rm = TRUE) + theme_bw() + labs(x = "Date",y = "Cumulative cases(pre 1,000)")
  })
  
  output$Plot4 <- renderPlot({
    vic_weekly_vc_plot4 <- vic_weekly_vc[0:input$week1,]
    ggplot(vic_weekly_vc_plot4) + 
      geom_line(aes(x=WEEK_NUM, y=SUM_DOSE_NET), color = "blue",size = 0.8,na.rm = TRUE) +
      geom_line(aes(x=WEEK_NUM, y=SUM_NEW), color = "orange",size = 0.8,na.rm = TRUE) +
      ggtitle("Weekly case distribution and vaccinations in VIC") +
      labs(x = "Week (week number) ",y = "Cases") + scale_x_continuous(breaks=seq(0,nrow(vic_weekly_vc),10))
  })
  
  output$Plot5 <- renderPlot({
    nsw_weekly_vc_plot5 <- nsw_weekly_vc[0:input$week1,]
    ggplot(nsw_weekly_vc_plot5) + 
      geom_line(aes(x=WEEK_NUM, y=SUM_DOSE_NET), color = "blue",size = 0.8,na.rm = TRUE) +
      geom_line(aes(x=WEEK_NUM, y=SUM_NEW), color = "orange",size = 0.8,na.rm = TRUE) +
      ggtitle("Weekly case distribution and vaccinations in NSW") +
      labs(x = "Week (week number) ",y = "Cases") + scale_x_continuous(breaks=seq(0,nrow(nsw_weekly_vc),10))
  })
  
  output$Plot6 <- renderPlot({
    wa_weekly_vc_plot6 <- wa_weekly_vc[0:input$week1,]
    ggplot(wa_weekly_vc_plot6) + 
      geom_line(aes(x=WEEK_NUM, y=SUM_DOSE_NET), color = "blue",size = 0.8,na.rm = TRUE) +
      geom_line(aes(x=WEEK_NUM, y=SUM_NEW), color = "orange",size = 0.8,na.rm = TRUE) +
      ggtitle("Weekly case distribution and vaccinations in WA") +
      labs(x = "Week (week number) ",y = "Cases") + scale_x_continuous(breaks=seq(0,nrow(wa_weekly_vc),10))
  })
  
  output$Plot7 <- renderPlot({
    vic_ha_plot7 <- subset(vic_ha,DATE >input$Date2[1])
    vic_ha_plot7 <- subset(vic_ha_plot7,DATE < input$Date2[2])
    ggplot(vic_ha_plot7) + 
      geom_line(aes(x=DATE, y=PRO), color = "blue",size = 0.8,na.rm = TRUE) + 
      ggtitle("Proportion of hospitalized cases and daily active cases in VIC") +
      labs(x = "Date ",y = "Proportion( % )")
  })
  
  output$Plot7w <- renderPlot({
    vic_weekly_ha_plot7w <- vic_weekly_ha[0:input$week2,]
    ggplot(vic_weekly_ha_plot7w) + 
      geom_line(aes(x=WEEK_NUM, y=MEAN_PRO), color = "red",size = 0.8,na.rm = TRUE) + 
      ggtitle("Weekly proportion of hospitalized cases and daily active cases in VIC") +
      labs(x = "Week ",y = "Proportion( % )")
  })
  
  output$Plot8 <- renderPlot({
    nsw_ha_plot8 <- subset(nsw_ha,DATE >input$Date2[1])
    nsw_ha_plot8 <- subset(nsw_ha_plot8,DATE < input$Date2[2])
    ggplot(nsw_ha_plot8) + 
      geom_line(aes(x=DATE, y=PRO), color = "blue",size = 0.8,na.rm = TRUE) + 
      ggtitle("Proportion of hospitalized cases and daily active cases in NSW") +
      labs(x = "Date ",y = "Proportion( % )")
  })
  
  output$Plot8w <- renderPlot({
    nsw_weekly_ha_plot8w <- nsw_weekly_ha[0:input$week2,]
    ggplot(nsw_weekly_ha_plot8w) + 
      geom_line(aes(x=WEEK_NUM, y=MEAN_PRO), color = "red",size = 0.8,na.rm = TRUE) + 
      ggtitle("Weekly proportion of hospitalized cases and daily active cases in NSW") +
      labs(x = "Week ",y = "Proportion( % )")
  })
  
  output$Plot9 <- renderPlot({
    wa_ha_plot9 <- subset(wa_ha,DATE >input$Date2[1])
    wa_ha_plot9 <- subset(wa_ha_plot9,DATE < input$Date2[2])
    ggplot(wa_ha_plot9) + 
      geom_line(aes(x=DATE, y=PRO), color = "blue",size = 0.8,na.rm = TRUE) + 
      ggtitle("Proportion of hospitalized cases and daily active cases in WA") +
      labs(x = "Date ",y = "Proportion( % )")
  })
  
  output$Plot9w <- renderPlot({
    wa_weekly_ha_plot9w <- wa_weekly_ha[0:input$week2,]
    ggplot(wa_weekly_ha_plot9w) + 
      geom_line(aes(x=WEEK_NUM, y=MEAN_PRO), color = "red",size = 0.8,na.rm = TRUE) + 
      ggtitle("Weekly proportion of hospitalized cases and daily active cases in WA") +
      labs(x = "Week ",y = "Proportion( % )")
  })
  
}

shinyApp(ui, server)