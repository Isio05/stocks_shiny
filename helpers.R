source("shared_variables.R")

chart_unempl <- function(min_date, max_date, transformation){
  r <- GET(paste("https://www.quandl.com/api/v3/datasets/FRED/NROUST.json?api_key=", API_KEY, 
                 "&transform=", transformation, 
                 sep = ""))
  r_content <- content(r)
  # print(r_content$dataset$data[1][[1]][[1]])
  
  if(transformation == " ") transformation == ""
  
  unem_dates <- unlist(lapply(r_content$dataset$data, "[", c(1)))
  unem_vals <- unlist(lapply(r_content$dataset$data, "[", c(2)))
  unem_df = data.frame(unem_dates, unem_vals)
  colnames(unem_df) <- c("date", "vals")
  unem_df$date <- factor(unem_df$date)
  
  dates_to_keep <- seq(as.Date(min_date), as.Date(max_date), by="months")
  unem_df <- subset(unem_df, date %in% as.character(dates_to_keep))
  
  #grid.newpage()
  ggplot(data=unem_df) +
    geom_line(aes(x=date, y=vals, group=1)) +
    scale_x_discrete(breaks=as.character(unem_df$date[as.integer(seq(1,length(unem_df$date), length.out = 8))]),
                     name = "Date") +
    ylab("Rate") +
    theme(axis.text.x = element_text(angle = 45)) +
    theme_classic()
}

if (!exists(".inflation")) {
  .inflation <- getSymbols('CPIAUCNS', src = 'FRED', 
                           auto.assign = FALSE)
} 

adjust <- function(data) {
  
  latestcpi <- last(.inflation)[[1]]
  inf.latest <- time(last(.inflation))
  months <- split(data)               
  
  adjust_month <- function(month) {               
    date <- substr(min(time(month[1]), inf.latest), 1, 7)
    coredata(month) * latestcpi / .inflation[date][[1]]
  }
  
  adjs <- lapply(months, adjust_month)
  adj <- do.call("rbind", adjs)
  axts <- xts(adj, order.by = time(data))
  axts[ , 5] <- Vo(data)
  axts
}