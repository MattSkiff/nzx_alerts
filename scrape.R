library(rvest) # scraping
library(magrittr) # pipes
library(sendmailR) # send mail via smtp
library(shiny) # app
library(sqldf) # sql for df
library(pander) # markdown email
library(lubridate) # time manip
library(anytime) # time manip2
panderOptions('table.split.table', Inf)

url <- "https://www.nzx.com/markets/NZSX/announcements"
iterations <- 0


simple <- read_html(url)
simple_table <- simple %>% html_table() %>% as.data.frame()
cat("retrieving initial announcements table...")

ui <- fluidPage()

server <- function(input, output) {
  
  # Anything that calls autoInvalidate will automatically invalidate
  # every 60 seconds.
  autoInvalidate <- reactiveTimer(180000)
  
  observe({
    # Invalidate and re-execute this reactive expression every time the
    # timer fires.
    
    autoInvalidate()
    
    iterations <- iterations + 1 # check iteration number
    cat("Iteration: ",iterations,"\n")
    
    # TODO: add uptime
    
    # trading day params
    date <- date(now())
    start <- "20:30:00 UTC"
    end <- "03:30:00 UTC"
    
    # day ahead in UTC - hence -1
    start_time <- as_datetime(ymd_hms(paste(date-1,start,sep = " "),tz = "UTC"))
    end_time <- as_datetime(ymd_hms(paste(date,end,sep = " "),tz = "UTC")) 
    
    trading_day <- interval(start_time,end_time)
    utc_time <- format(parse_date_time(now(tzone = "Pacific/Auckland"),"ymd HMS"),format = "%H:%M:%S")
    cat("UTC time is",utc_time,"\n")
    
    trading_status <- now(tzone = "utc") %within% trading_day
    cat("checking NZX trading open...\n")
    
    if (trading_status) {
      cat("NZX open...\n")
      cat("polling NZX for new announcements...\n")
      
      nzx_latest <- read_html(url)
      nzx_latest <- nzx_latest %>% html_table() %>% as.data.frame()
      
      any_change <- !identical(nzx_latest,simple_table)
      
      # has nzx table changed?
      if (any_change) {
        cat("new announcements detected...\n")

        
        # has nzx table got new 'P' announcement
        nzxlatestNotInsimple_table <- sqldf('SELECT * FROM nzx_latest EXCEPT SELECT * FROM simple_table')
      
        # any new price sensitive announcements?
        price_sensitive <- any(grepl("P",unique(nzxlatestNotInsimple_table$Flag),fixed = TRUE))
        
        if(price_sensitive) {
          
          warning("new announcements is price sensitive....\n")
          latest_announcement_time <- max(nzxlatestNotInsimple_table$Date)
          
          simple_table <- nzx_latest
          cat("overwriting benchmark announcements table....\n")
          
          addresses <- c("<skiffcoffee@gmail.com>","<rcs20@students.waikato.ac.nz>")
          
          for (i in 1:length(addresses)) {
          
            Server<-list(smtpServer= "127.0.0.1")
            from <- sprintf("<matthewnzxalert@localfglinkalert.com>","Matthew NZX alert system") # the sender's name is an optional value
            to <- sprintf(addresses[i])
            
            subject <- paste("Matthew S NZX alert",latest_announcement_time)
            body <- paste("New price sensitive announcement detected. Details follow:",pander_return(nzxlatestNotInsimple_table),collapse ="\n")
            
            sendmail(from,to,subject,body,control=list(smtpServer= "127.0.0.1"))
            cat("message sent!\n")
          
          }
        } else {
          cat("new announcement is not price sensitive...\n")
        }
      } else {
        cat("no new announcements detected...\n")
      }
    } else {
      cat("NZX is closed...")
    }
  })
}

shinyApp(ui, server)