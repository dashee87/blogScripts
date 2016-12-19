# loading the packages we'll need
library(dplyr) # data manipulation/filtering
library(ggplot2) # vanilla graphs
library(plotly) # interactive graphs
library(lubridate) # time manipulation functions
library(zoo) # some more time manipulation functions
library(forecast) # time series forecasting
library(RCurl) # import Dow Jones data
library(tseries) # time series statistical analysis

options(stringsAsFactors = FALSE)

# accidents file
tot_accs = rbind(read.csv("/Accidents0514.csv") %>%
                   rename_("Accident_Index" ="ï..Accident_Index"),
                 read.csv("Accidents_2015.csv")) %>%
  mutate(Date=as.POSIXct(Date, format="%d/%m/%Y"))

# casualties file
tot_cas= rbind(read.csv("/Casualties0514.csv") %>%
                 rename_("Accident_Index" ="ï..Accident_Index"),
               read.csv("/Casualties_2015.csv") %>%
                 select(-Casualty_IMD_Decile))

# vehicles file
tot_veh= rbind(read.csv("/Vehicles0514.csv") %>%
                 rename_("Accident_Index" ="ï..Accident_Index"),
               read.csv("/Vehicles_2015.csv") %>%
                 select(-Vehicle_IMD_Decile))

# looking at the structure of the datasets
explore_data=as.data.frame(t(sapply(list(tot_accs,tot_cas,tot_veh),function(x){
  c(length(unique(x$Accident_Index)),
    length(x),
    nrow(x))
})))

colnames(explore_data)=c("# Accidents", "# Columns","# Rows")
rownames(explore_data)=c("Accidents File","casualties File","Vehicles File")
explore_data

# Drivers in road accidents split by sex
tot_veh %>% group_by(Sex_of_Driver) %>% summarize(num_accs=n()) %>% 
  mutate(Sex_of_Driver=c("Data Missing","Male","Female","Unknown")) %>% 
  mutate(prop=paste(round(100*num_accs/sum(num_accs),2),"%"))

# number of road accidents by day of the week
tot_accs %>% group_by(Day_of_Week) %>% summarize(num_accs=n()) %>% 
  mutate(Day_of_Week=c("Sunday","Monday","Tuesday","Wednesday",
                       "Thursday","Friday","Saturday")) %>%
  mutate(prop=paste(round(100*num_accs/sum(num_accs),2),"%"))

# number of accidents by hour for each day of the week
daily_data <- mutate(tot_accs,day=weekdays(Date),hour=substring(Time,1,2)) %>% 
  arrange(Day_of_Week) %>%
  group_by(day, hour) %>% summarize(num_accs=n()) %>% 
  mutate(prop=round(100*num_accs/sum(num_accs), 1)) %>%
  filter(hour!="") 

########## plot number of accidents by hour for each day of the week ##########
###############################################################################

format_axis <- function(plottitle,size=18,colour="black",
                        font = "Arial, sans-serif"){
  list(
    title = plottitle,
    titlefont = list(
      family = font,
      size = size,
      color = colour))}
plot_ly(daily_data %>% 
          group_by(day, hour) %>% summarize(tot=sum(num_accs)) %>% 
          mutate(prop=round(100*tot/sum(tot), 1)),
        x=~hour,y=~prop, color =~day, type = "scatter", mode = "lines") %>%
  add_trace(data=daily_data %>% group_by(hour) %>% 
              summarize(tot=sum(num_accs)) %>% 
              mutate(prop=round(100*tot/sum(tot), 1)),
            x=~hour,y=~prop,name="All Days",
            line=list(width=3)) %>% 
  layout(xaxis=format_axis("Hour"),yaxis=format_axis("Proportion of Accidents (%)"), 
         title="Proportion of Road Accidents By Hour (2005-2015)") %>%
  layout(legend = list(x = 0.02, y = 0.99,font=list(size=14)))

###############################################################################

# just reformatting the days by the yearmonth (e.g. June 2008)
yearlymon_data <- tot_accs %>% group_by(as.yearmon(Date, format="%d/%m/%Y")) %>% summarize(num_accs=n())
colnames(yearlymon_data)[1]="YearMonth"

########## plot number of accidents by month ##########
###############################################################################

line_list <- list()
for(i in 1:length(unique(year(tot_accs$Date)))){
  line_list[[i]]=list(type      = "line",
                      line      = list(color = "black", dash="dashdot"),
                      opacity   = 0.3,
                      x0        = unique(year(tot_accs$Date))[i],
                      x1        = unique(year(tot_accs$Date))[i],
                      xref      = "x",
                      y0        = min(yearlymon_data$num_accs),
                      y1        = max(yearlymon_data$num_accs),
                      yref      = "y")
}

plot_ly(yearlymon_data,x = ~YearMonth, y = ~num_accs, type = "scatter", 
        mode = "lines", text=sapply(yearlymon_data$YearMonth,toString),
        hoverinfo="text+y") %>% layout(shapes=line_list) %>%  
  layout(xaxis=format_axis("Year"),yaxis=format_axis("Number of  Accidents"),
         title="Number of Road Accidents By Month (2005-2015)")

###############################################################################

# the stl function only takes additive model
# since we want a multiplicative model, we need to first take the log
decomp_accs_ts <- stl(ts(log(yearlymon_data$num_accs),frequency = 12,start=2005),s.window = "periodic")
decomp_accs_ts$time.series <- exp(decomp_accs_ts$time.series)


###### plot multiplicative decomposition of monthly accident time series ######
###############################################################################

plot_ly_decomp <- function(time_series){
  ts_df <- as.data.frame(time_series$time.series) %>% 
    mutate(date = as.yearmon(time(time_series$time.series))) %>% 
    tidyr::gather(variable, value, -date) %>% transform(id = as.integer(factor(variable)))
  seas_plotly <- plot_ly(filter(ts_df, variable == "seasonal"),
                         x = ~date, y = ~value, colors = "Dark2", name="seasonal", 
                         type="scatter", mode="lines",
                         text=unique(sapply(ts_df$date,toString)),hoverinfo="text + y") %>%
    layout(xaxis=list(title="", showticklabels = FALSE))
  remain_plotly <- plot_ly(filter(ts_df, variable == "remainder"),
                           x = ~date, y = ~value, colors = "Dark2", name="noise", 
                           type="scatter", mode = "lines",
                           text=unique(sapply(ts_df$date,toString)),hoverinfo="text + y") %>%
    add_trace(x = ~date, y= 1,mode = "lines",showlegend=FALSE,line = list(
      color = "gray",
      dash = "dashed"                                
    )) %>%
    layout(xaxis=list(title=""))
  trend_plotly <- plot_ly(filter(ts_df, variable == "trend"),
                          x = ~date, y = ~value, colors = "Dark2", name="trend", 
                          type="scatter", mode="lines",
                          text=unique(sapply(ts_df$date,toString)),hoverinfo="text + y") %>%
    layout(xaxis=list(title="", showticklabels = FALSE))
  data_plotly <- plot_ly(group_by(ts_df,date) %>% summarize(value = prod(value)),
                         x = ~date, y = ~value, colors = "Dark2", name="data", 
                         type="scatter", mode="lines",
                         text=unique(sapply(ts_df$date,toString)),hoverinfo="text + y") %>%
    layout(title = "UK Road Accidents Multiplicative Model",
           xaxis=list(title="", showticklabels = FALSE))
  subplot(list(data_plotly, seas_plotly, trend_plotly, remain_plotly),nrows=4) 
}
plot_ly_decomp(decomp_accs_ts) %>% 
  layout(legend = list(font=list(size=14)))

###############################################################################

# importing Dow Jones data for 2015 from yahoo finance
dow_jones <- read.csv(text=getURL(
  "http://chart.finance.yahoo.com/table.csv?s=^DJI&a=0&b=1&c=2015&d=11&e=31&f=2015&g=d&ignore=.csv"), 
  stringsAsFactors = FALSE) %>%
  mutate(Date=as.Date(Date)) %>% arrange(Date)

#########################  stationary processes plot  #########################
###############################################################################

subplot(list(
  plot_ly(dow_jones, x=~Date,y=~Close,type="scatter",
          mode="lines", text="Dow Jones",hoverinfo="text+y+x")  %>%
    layout(annotations = list(
      x = as.Date("2016-06-01"), 
      y = 1.0, 
      font = list(size = 14), 
      showarrow = FALSE, 
      text = '<b>Dow Jones Closing Price 2015 (Non-Stationary Process)</b>', 
      xanchor = "center", 
      xref = "paper", 
      yanchor = "bottom", 
      yref = "paper"
    ),showlegend=FALSE,
    xaxis=list(title="", showticklabels = FALSE)),
  plot_ly(data.frame(Date=as.Date("2015-01-01")+0:364,Price = rnorm(365,50,10)),
          x=~Date,y=~Price,type="scatter",mode="lines", 
          text="Random", hoverinfo="text+y+x") %>%
    layout(annotations = list(
      x = as.Date("2016-06-01"), 
      y = 1.0, 
      font = list(size = 14), 
      showarrow = FALSE, 
      text = '<b>Random Numbers (Stationary Process)</b>', 
      xanchor = "center", 
      xref = "paper", 
      yanchor = "bottom", 
      yref = "paper"
    ),
    showlegend=FALSE,
    xaxis=format_axis("Date"))),nrows = 2, margin = 0.03)

###############################################################################

### Autoregressive Models
set.seed(100)
#AR(0)
ar0 = 0
for(i in 2:365){
  ar0[i] = rnorm(1)}

#AR(1)
ar1 = 0
for(i in 2:365){
  ar1[i] = ar1[i-1]*0.8 + rnorm(1)}

#AR(2)
ar2 = 0
ar2[2] = 0
for(i in 3:365){
  ar2[i] = ar2[i-1]*0.5 + ar2[i-2]*0.3 + rnorm(1)}


#########################  autoregressive model plots #########################
###############################################################################
subplot(list(
  plot_ly(x=as.Date("2015-01-01")+0:364,y=ar0,type="scatter",
          mode="lines", text="AR(0)",hoverinfo="text+y")  %>%
    layout(annotations = list(
      x = as.Date("2016-06-01"), 
      y = 1.0, 
      font = list(size = 14), 
      showarrow = FALSE, 
      text = '<b>AR(0): y[t] = &#949;[t], &#949;[t] ~ N(0,1)', 
      xanchor = "center", 
      xref = "paper", 
      yanchor = "bottom", 
      yref = "paper"
    ),showlegend=FALSE,
    xaxis=list(title="", showticklabels = FALSE)),
  plot_ly(x=as.Date("2015-01-01")+0:364,y=ar1,type="scatter",mode="lines", text="AR(1)",
          hoverinfo="text+y") %>%
    layout(annotations = list(
      x = as.Date("2016-06-01"), 
      y = 1.0, 
      font = list(size = 14), 
      showarrow = FALSE, 
      text = '<b>AR(1): y[t] = 0.8*y[t-1] + &#949;[t], &#949;[t] ~ N(0,1)', 
      xanchor = "center", 
      xref = "paper", 
      yanchor = "bottom", 
      yref = "paper"
    ),
    showlegend=FALSE,
    xaxis=list(title="", showticklabels = FALSE)),
  plot_ly(x=as.Date("2015-01-01")+0:364,y=ar2,type="scatter",mode="lines", text="AR(2)",
          hoverinfo="text+y") %>%
    layout(annotations = list(
      x = as.Date("2016-06-01"), 
      y = 1.0, 
      font = list(size = 14), 
      showarrow = FALSE, 
      text = '<b>AR(2): y[t] = 0.5*y[t-1] + 0.3*y[t-2] + &#949;[t], &#949;[t] ~ N(0,1)',  
      xanchor = "center", 
      xref = "paper", 
      yanchor = "bottom", 
      yref = "paper"
    ),
    showlegend=FALSE,
    xaxis=format_axis("Date"))),nrows = 3, margin = 0.03)

###############################################################################

##### Moving Average Model ######
set.seed(101)
#MA(0)
ma0 = 0
for(i in 2:365){
  ma0[i] = rnorm(1)}

#MA(1)
ma1 = 0
for(i in 2:365){
  ma1[i] = rnorm(1)*0.5 + rnorm(1)}

#MA(2)
ma2 = 0
ma2[2] = 0
for(i in 3:365){
  ma2[i] = rnorm(1)*0.5 - rnorm(1)*0.3 + rnorm(1)}

#########################  moving average model plots #########################
###############################################################################

subplot(list(
  plot_ly(x=as.Date("2015-01-01")+0:364,y=ma0,type="scatter",mode="lines", 
          text="MA(0)",hoverinfo="text+y")  %>%
    layout(annotations = list(
      x = as.Date("2016-06-01"), 
      y = 1.0, 
      font = list(size = 14), 
      showarrow = FALSE, 
      text = '<b>MA(0): y[t] = &#949;[t], &#949;[t] ~ N(0,1)', 
      xanchor = "center", 
      xref = "paper", 
      yanchor = "bottom", 
      yref = "paper"
    ),showlegend=FALSE,
    xaxis=list(title="", showticklabels = FALSE)),
  plot_ly(x=as.Date("2015-01-01")+0:364,y=ma1,type="scatter",mode="lines", text="MA(1)",
          hoverinfo="text+y") %>%
    layout(annotations = list(
      x = as.Date("2016-06-01"), 
      y = 1.0, 
      font = list(size = 14), 
      showarrow = FALSE, 
      text = 
        '<b>MA(1): y[t] = &#949;[t] + 0.5*&#949;[t-1], &#949;[t] ~ N(0,1) &#8704; t', 
      xanchor = "center", 
      xref = "paper", 
      yanchor = "bottom", 
      yref = "paper"
    ),
    showlegend=FALSE,
    xaxis=list(title="", showticklabels = FALSE)),
  plot_ly(x=as.Date("2015-01-01")+0:364,y=ma2,type="scatter",mode="lines", text="MA(2)",
          hoverinfo="text+y") %>%
    layout(annotations = list(
      x = as.Date("2016-06-01"), 
      y = 1.0, 
      font = list(size = 14), 
      showarrow = FALSE, 
      text = 
        '<b>MA(2): y[t] = &#949;[t] + 0.5*&#949;[t-1] - 0.3*&#949;[t-2], &#949;[t] ~ N(0,1) &#8704; t',
      xanchor = "center", 
      xref = "paper", 
      yanchor = "bottom", 
      yref = "paper"
    ),
    showlegend=FALSE,
    xaxis=format_axis("Date"))),nrows = 3, margin = 0.03)

###############################################################################

##############################  difference plot  ##############################
###############################################################################

subplot(list(
  plot_ly(data.frame(Date=dow_jones$Date[-1],Diff=diff(dow_jones$Close)),
          x=~Date,y=~Diff,type="scatter",mode="lines", line=list(color="blue")) %>%
    layout(title="<b>Dow Jones Inter-Day Difference 2015</b>"),
  plot_ly(x = ~diff(dow_jones$Close), type = "histogram",
          nbinsx=20, marker=list(color="blue"))), nrows =2, margin = 0.03) %>% 
  layout(showlegend=FALSE)

###############################################################################

# augmented Dickey Fuller Test
# not stationary
adf.test(dow_jones$Close)
# stationary
adf.test(diff(dow_jones$Close))

# fitting ARIMA model to road accident data
acc_arima.fit <- auto.arima(ts(yearlymon_data$num_accs,frequency = 12,start=2005),
                            allowdrift = TRUE, approximation=FALSE)
acc_arima.fit


acc_forecast <- forecast(acc_arima.fit, h=12)

##############################  plot ARIMA model ##############################
###############################################################################

plot_ly(yearlymon_data, x=~YearMonth,y=~num_accs,type="scatter", mode="lines",name="Observed",
        text=~sapply(YearMonth,toString),hoverinfo="text+y+name") %>% 
  add_trace(x=c(max(yearlymon_data$YearMonth)+seq(1/12,1,1/12), 
                max(yearlymon_data$YearMonth)+seq(1,1/12,-1/12)),
            y=c(acc_forecast$lower[,2],rev(acc_forecast$upper[,2])),name="95% Confidence",
            fill="toself", hoveron = "points",
            text=c(sapply(max(yearlymon_data$YearMonth)+seq(1/12,1,1/12),toString),
                   sapply(max(yearlymon_data$YearMonth)+seq(1,1/12,-1/12),toString)), 
            hoverinfo="text+y+name", hoveron = "points") %>%
  add_trace(x=c(max(yearlymon_data$YearMonth)+seq(1/12,1,1/12), 
                max(yearlymon_data$YearMonth)+seq(1,1/12,-1/12)),
            y=c(acc_forecast$lower[,1],rev(acc_forecast$upper[,1])),name="80% Confidence",
            fill="toself", hoveron = "points",
            text=c(sapply(max(yearlymon_data$YearMonth)+seq(1/12,1,1/12),toString),
                   sapply(max(yearlymon_data$YearMonth)+seq(1,1/12,-1/12),toString)), 
            hoverinfo="text+y+name", hoveron = "points")  %>%
  add_trace(x=c(max(yearlymon_data$YearMonth)+seq(1/12,1,1/12)),
            y=as.vector(acc_forecast$mean),name="Mean Prediction",
            text=sapply(max(yearlymon_data$YearMonth)+seq(1/12,1,1/12),toString), 
            hoverinfo="text+y+name", hoveron = "points") %>%
  add_trace(x=~YearMonth,
            y=as.vector(acc_forecast$fitted),name="Model",
            text=~sapply(YearMonth,toString),hoverinfo="text+y+name",
            line = list(color = "#A9A9A9", dash = "dashed")) %>%
  layout(xaxis=format_axis("Year"),yaxis=format_axis("Number of Accidents"), 
         title="Number of Road Accidents By Month (2005-2016)") %>%
  layout(legend = list(x = 0.8, y = 0.99, font=list(size=14)))

###############################################################################