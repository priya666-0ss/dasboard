#install.packages("rlang")
library(shiny)
library(plyr)
library(lubridate)
library(quantmod)
library(curl)
library(ggplot2)
library(plotly)

rm(list = ls())

# some useful functions
ret.f <- function(x) c(NA,x[2:length(x)]/x[1:(length(x)-1)] - 1)

lag.f <- function(x) c(NA,x[1:(length(x)-1)])

# get the prices function
get.prices <- function(v,p,t1,t2,price) {
  
  ds.list <- lapply(v, function(tic) get(getSymbols(tic, from=t1)) )
  names(ds.list) <- v
  
  # aggregate data respectively
  if(p == "w") {
    ds.list <- lapply(ds.list,function(x) apply.weekly(x, function(y) y[nrow(y),])   )
  }
  
  if(p == "m") {
    ds.list <- lapply(ds.list,function(x) apply.monthly(x, function(y) y[nrow(y),])   )
  }
  
  
  # do some adjustment to the data list before merging all together
  ds.list <- lapply(ds.list, function(x) data.frame(x[,grep(price,names(x))]) )
  ds.list <- lapply(ds.list, function(x) data.frame(Date = rownames(x), Price = x[,1]) )
  ds.list <- lapply(v, function(v.i) { names(ds.list[[v.i]])[2] <- v.i ; return(ds.list[[v.i]])}  )
  ds <- Reduce(function(...) merge(...,by = "Date", all = T), ds.list)
  ds$Date <- as.Date(ds$Date)
  
  if( !is.null(t2) ) {
    ds <- ds[ds$Date <= t2 ,]
  }
  
  return(ds)
}


#### ui ############
ui <- fluidPage(
  titlePanel("Visualizing Data From Yahoo Finance" ),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dates", label = "Date range", start = "2020-01-01"),

      selectInput("frequency", "choose frequency:",choices = c("daily","weekly","monthly") ),
      selectInput("price", "choose data type:",choices = c("Adjusted","Open","High","Low","Close","Volume")),
      textInput("v", label = "choose tickers separated by spaces", value = c("SPY XLF XLU")  ),
      helpText("Note: Ticker symbols are used to identify a specific security,",
               "such as a stock or bond. ", 
               " For example, the ticker symbol for Apple Inc. is AAPL",
               "You can choose any ticker of your choice.")

    ),
    
    mainPanel(
      # verbatimTextOutput("output_dates"),
      plotlyOutput('plot1'),
      h4("Past 6 trading (from current day) days data"),
      tableOutput("table.ret"),
    )
  )
)



##### server #########
server <- function(input,output) {
  
  
  f1 <- function(v,p,y0,m0,d0,y1,m1,d1,price) {
    
    v <- as.character(v)
    v <- unlist(strsplit(v, " "))
    p <- substr(p,1,1)
    
    if(nchar(d0) < 2) {
      d0 <- paste(0,d0,sep = "")
    }
    
    if(nchar(m0) < 2) {
      m0 <- paste(0,m0,sep = "")
    }
    
    # month day year
    t1 <- paste(c(y0,m0,d0),collapse = "-")
    # today's date
    t2 <- paste(c(y1,m1,d1),collapse = "-")
    
    P <- get.prices(v,p,t1,t2,price)
    
    
    P$Date <- as.character(P$Date)
    return(P)
  }
  
  f2 <- function(v,p,y0,m0,d0,price) {
    
    v <- as.character(v)
    v <- unlist(strsplit(v, " "))
    p <- substr(p,1,1)
    
    if(nchar(d0) < 2) {
      d0 <- paste(0,d0,sep = "")
    }
    
    if(nchar(m0) < 2) {
      m0 <- paste(0,m0,sep = "")
    }
    
    # month day year
    t1 <- paste(c(y0,m0,d0),collapse = "-")
    # today's date
    t2 <- as.character(today())
    
    P <- get.prices(v,p,t1,t2,price)
    
    
    P$Date <- as.character(P$Date)
    return(P)
  }
  
  
  DataInput <- reactive({ 
    
    start_date <- ymd(input$dates[1])
    end_date <- ymd(input$dates[2])
    start_year <- year(start_date)
    end_year <- year(end_date)
    start_month <- month(start_date)
    end_month <- month(end_date)
    start_day <- day(start_date)
    end_day <- day(end_date)
    
    return(f1(input$v,input$frequency,start_year,start_month,start_day,end_year,end_month,end_day,input$price))
    
  })
  
  DataInput2 <- reactive ({
    
    start_date <- ymd(input$dates[1])

    start_year <- year(start_date)

    start_month <- month(start_date)
    start_day <- day(start_date)
    
    return(f2(input$v,input$frequency,start_year,start_month,start_day,input$price))
    
  })
  
  output$table.ret <- renderTable( {
    tail(DataInput2())
  } )
  
  
  
  output$plot1 <- renderPlotly( {
    ds <- na.omit(DataInput())
    ds.list <- lapply(2:ncol(ds),function(i) data.frame(Date = ds[,1], Price = ds[,i], Tic = as.factor(names(ds)[i])  ) )
    ds.list <- lapply(ds.list, function(ds.i) { ds.i$Price <- ds.i$Price/ds.i$Price[1]; return(ds.i) }  )
    ds <- Reduce(rbind,ds.list)
    my.plot <- ggplot(ds, aes(x = Date, y = Price, group = Tic , colour = Tic) ) + geom_line(alpha = 0.5, size=0.7) + theme(legend.text=element_text(size=16)  )  + scale_x_discrete(breaks = ds$Date[(1:7)*floor(nrow(ds)/7)])
    my.plot <- ggplotly(my.plot)
    return(my.plot)
  } )
  
  
}

shinyApp(ui = ui, server = server)