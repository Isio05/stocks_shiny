source("shared_variables.R")

macro_chart <- function(indicator, min_date, max_date, transformation){
  if(transformation == " ") transformation == ""
  
  if(indicator == "unem_st"){
    r <- GET(paste("https://www.quandl.com/api/v3/datasets/FRED/NROUST.json?api_key=", API_KEY, 
                   "&transform=", transformation, 
                   sep = ""))    
  }else if(indicator == "npgdp"){
    r <- GET(paste("https://www.quandl.com/api/v3/datasets/FRED/NGDPPOT.json?api_key=", API_KEY, 
                   "&transform=", transformation, 
                   sep = ""))   
  }else if(indicator == "rpgdp"){
    r <- GET(paste("https://www.quandl.com/api/v3/datasets/FRED/GDPPOT.json?api_key=", API_KEY, 
                   "&transform=", transformation, 
                   sep = ""))   
  }else if(indicator == "unem_lt"){
    r <- GET(paste("https://www.quandl.com/api/v3/datasets/FRED/NROU.json?api_key=", API_KEY, 
                   "&transform=", transformation, 
                   sep = "")) 
  }
  
  r_content <- content(r)
  # print(r_content$dataset$data[1][[1]][[1]])
  
  unem_dates <- unlist(lapply(r_content$dataset$data, "[", c(1)))
  unem_vals <- unlist(lapply(r_content$dataset$data, "[", c(2)))
  unem_df <- data.frame(unem_dates, unem_vals)
  colnames(unem_df) <- c("date", "vals")
  
  min_date_from_df <- which.min(abs(as.Date(unem_df$date) - rep(as.Date(min_date),length(unem_df$date)))) - 1
  # unem_df <- unem_df[min_date_from_df:length(unem_df$date),,drop=F]

  unem_df$date <- factor(unem_df$date)
  
  dates_to_keep <- seq(as.Date(unem_df$date[min_date_from_df]), as.Date(max_date), by="months")
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

macro_stats <- function(indicator, min_date, max_date, transformation){
  if(transformation == " ") transformation == ""
  
  if(indicator == "unem_st"){
    r <- GET(paste("https://www.quandl.com/api/v3/datasets/FRED/NROUST.json?api_key=", API_KEY, 
                   "&transform=", transformation, 
                   sep = ""))    
  }else if(indicator == "npgdp"){
    r <- GET(paste("https://www.quandl.com/api/v3/datasets/FRED/NGDPPOT.json?api_key=", API_KEY, 
                   "&transform=", transformation, 
                   sep = ""))   
  }else if(indicator == "rpgdp"){
    r <- GET(paste("https://www.quandl.com/api/v3/datasets/FRED/GDPPOT.json?api_key=", API_KEY, 
                   "&transform=", transformation, 
                   sep = ""))   
  }else if(indicator == "unem_lt"){
    r <- GET(paste("https://www.quandl.com/api/v3/datasets/FRED/NROU.json?api_key=", API_KEY, 
                   "&transform=", transformation, 
                   sep = "")) 
  }
  
  r_content <- content(r)
  # print(r_content$dataset$data[1][[1]][[1]])
  
  unem_dates <- unlist(lapply(r_content$dataset$data, "[", c(1)))
  unem_vals <- unlist(lapply(r_content$dataset$data, "[", c(2)))
  unem_df = data.frame(unem_dates, unem_vals)
  colnames(unem_df) <- c("date", "vals")
  
  min_date_from_df <- which.min(abs(as.Date(unem_df$date) - rep(as.Date(min_date),length(unem_df$date)))) - 1
  # unem_df <- unem_df[min_date_from_df:length(unem_df$date),,drop=F]
  
  unem_df$date <- factor(unem_df$date)
  
  dates_to_keep <- seq(as.Date(unem_df$date[min_date_from_df]), as.Date(max_date), by="months")
  unem_df <- subset(unem_df, date %in% as.character(dates_to_keep))
  colnames(unem_df) <- c("Dates","Values")
  # unem_df <- unem_df[,"vals"]
  
  return(unem_df)
}

housing_chart <- function(indicator, state, chart_type, min_date, max_date, transformation){
  
  r <- GET(paste("https://www.quandl.com/api/v3/datasets/ZILLOW/S",
                 state,
                 "_",
                 indicator,
                 ".json?api_key=",
                 API_KEY,
                 "&transform=",
                 transformation,
                 sep = ""))    

  
  r_content <- content(r)
  # print(r_content$dataset$data[1][[1]][[1]])
  
  unem_dates <- unlist(lapply(r_content$dataset$data, "[", c(1)))
  unem_vals <- unlist(lapply(r_content$dataset$data, "[", c(2)))
  unem_df = data.frame(unem_dates, unem_vals)
  colnames(unem_df) <- c("date", "vals")
  unem_df$date <- factor(unem_df$date)
  
  min_date_from_df <- which.min(abs(as.Date(unem_df$date) - rep(as.Date(min_date),length(unem_df$date))))
  max_date_from_df <- which.min(abs(as.Date(unem_df$date) - rep(as.Date(max_date),length(unem_df$date))))
  # unem_df <- unem_df[min_date_from_df:length(unem_df$date),,drop=F]
  unem_df$date <- factor(unem_df$date)
  unem_df <- unem_df[max_date_from_df:min_date_from_df,,drop=F]
  
  if(chart_type == "Line"){
  ggplot(data=unem_df) +
    geom_line(aes(x=date, y=vals, group=1)) +
    scale_x_discrete(breaks=as.character(unem_df$date[as.integer(seq(1,length(unem_df$date), length.out = 8))]),
                     name = "Date") +
    ylab("Rate") +
    theme(axis.text.x = element_text(angle = 45, size=12)) +
    theme_classic(base_size = 14, base_family = "sans")
  }else if(chart_type == "Histogram"){
    ggplot(data=unem_df) +
      geom_histogram(aes(x=vals), bins = 10, fill = "white", color = "grey", size = 1.5) + 
      xlab("Values") +
      ylab("Count") +
      theme_classic(base_size = 14, base_family = "sans")
  }else if(chart_type == "Dots"){
    unem_df$date = as.character(unem_df$date)
    grid.newpage()
    pushViewport(viewport())
    pushViewport(plotViewport(margins = rep(4,4)))
    pushViewport(dataViewport(as.Date(unem_df$date), unem_df$vals))
    grid.rect()
    grid.points(as.Date(unem_df$date), unem_df$vals, pch=21, gp=gpar(fill="grey80"))
    grid.xaxis(name="xaxis", label = as.character(unem_df$date)[as.integer(seq(1,length(unem_df$date), length.out = 6))],
               at = as.Date(unem_df$date)[as.integer(seq(1,length(unem_df$date), length.out = 6))])
    grid.edit(gPath = gPath("xaxis", "labels"), rot = 0)
    grid.yaxis(name="yaxis")
    grid.text("Dates", y=unit(-3, "lines"))
    grid.text("Value", x=unit(-3, "lines"), rot = 90)
  }else if(chart_type == "Candles"){
    unem_df["years"] <- format(as.Date(unem_df$date, format="%Y-%m-%d"),"%Y")
    ggplot(data=unem_df) +
      geom_boxplot(aes(x=years, y=vals)) +
      xlab("Values") +
      ylab("Count") +
      theme_classic(base_size = 14, base_family = "sans")
  }else if(chart_type == "Year groups"){
    unem_df["years"] <- format(as.Date(unem_df$date, format="%Y-%m-%d"),"%Y")
    unem_df["lps"] <- seq(1,length(unem_df$years))
    unem_df["months"] <- format(as.Date(unem_df$date, format="%Y-%m-%d"),"%m")
    xyplot(vals~months|years, data = unem_df, xlab="Period", ylab="Value", xlim=c(1,12),
           panel = function(x,y){
             panel.lmline(x,y)
             panel.xyplot(x,y)
           })
  }
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