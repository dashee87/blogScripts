library(rvest) # for scraping
library(dplyr) # data frame manipulation
library(plotly) # plotting
library(jsonlite) # importing json files

# function to scrape the UK Singles Chart
get_UK_Chart <- function(week){
  url = paste0("http://www.officialcharts.com/charts/singles-chart/",
               format(as.Date(week), format="%Y%m%d"),"/7501/")
  scraped_page <- read_html(url)
  artist_info <- scraped_page %>% html_nodes("#main .artist a")
  data.frame(
    chart_week = week,
    chart_pos = scraped_page %>% html_nodes(".position") %>%
      html_text() %>% as.integer(),
    artist = artist_info %>% html_text(),
    artist_number = artist_info %>% html_attr("href") %>% 
      gsub("/artist/","",.) %>% gsub("/.*","",.) %>% as.integer(),
    track = scraped_page %>% html_nodes(".track .title a") %>% 
      html_text(),
    label = scraped_page %>% html_nodes(".label-cat .label") %>% 
      html_text(),
    last_week = scraped_page %>% html_nodes(".last-week") %>% 
      html_text() %>% gsub("[^0-9]", "", .) %>% as.integer(),
    peak_pos = scraped_page %>% html_nodes("td:nth-child(4)") %>% 
      html_text() %>% as.integer(),
    weeks_on_chart = scraped_page %>% html_nodes("td:nth-child(5)") %>% 
      html_text())
  }

# litte bit of formatting to scrape each UK singles chart all the back to 1952
today_date <- Sys.Date()
first_uk_chart_date <- as.Date("1952-11-14")
date_diffs_uk <- as.numeric(today_date - first_uk_chart_date)
recent_chart_date_uk <- Sys.Date() - date_diffs_uk%%7


uk_chart <- lapply(seq(0,date_diffs_uk,7), function(x){
  print(recent_chart_date_uk - x)
  output <- NULL
  attempt <- 0
  # scrapes can fail occasionally (e.g. conenction timeout) so we'll allow it to fail 5 times in a row
  while( is.null(output) && attempt <= 5 ) {
    attempt <- attempt + 1
    if(attempt>=2){
      print(paste("Scraping Attempt #",attempt))
      Sys.sleep(1)
    }
    try(
      output <- get_UK_Chart(recent_chart_date_uk - x)
    )
  }
  # not necessary but sometimes it's good to pause between scrapes
  Sys.sleep(0.5)
  return(output)
}) %>% dplyr::bind_rows()

# function to scrape the Billboard Top 100
get_US_Chart <- function(week){
  url = paste0("https://www.billboard.com/charts/hot-100/",
               format(as.Date(week), format="%Y-%m-%d"))
  scraped_page <- read_html(url)
  data.frame(
    chart_week = scraped_page %>% html_nodes(".article-heading+ .article-date") %>% 
      html_text() %>% trimws() %>% gsub(" -.*","",.),
    chart_pos = scraped_page %>% html_nodes(".chart-row__current-week") %>%
      html_text() %>% as.integer(),
    artist = scraped_page %>% html_nodes(".chart-row__artist") %>% html_text() %>%
      gsub("\n", "", .),
    track = scraped_page %>% html_nodes(".chart-row__song") %>% 
      html_text(),
    last_week = scraped_page %>% html_nodes(".chart-row__rank .chart-row__last-week") %>% 
      html_text() %>% gsub("[^0-9]", "", .) %>% as.integer(),
    peak_pos = scraped_page %>% html_nodes(".chart-row__top-spot .chart-row__value") %>% 
      html_text() %>% as.integer(),
    weeks_on_chart = scraped_page %>% html_nodes(".chart-row__weeks-on-chart .chart-row__value") %>% 
      html_text())
}

# litte bit of formatting to scrape Billboard Top 100 all the back to 1958
first_us_chart_date <- as.Date("1958-08-04")
date_diffs_us <- as.numeric(today_date - first_us_chart_date)

us_chart <- lapply(seq(0,date_diffs_us,7), function(x){
  print(today_date - x)
  output <- NULL
  attempt <- 0
  while( is.null(output) && attempt <= 5 ) {
    attempt <- attempt + 1
    if(attempt>=2){
      print(paste("Scraping Attempt #",attempt))
      Sys.sleep(5)
    }
    try(
      output <- get_US_Chart(today_date - x)
    )
  }
  Sys.sleep(0.5)
  return(output)
}) %>% dplyr::bind_rows()

# in case that loop missed out on the first Billboard top 100
if(min(us_chart$chart_week)!=first_us_chart_date){
  us_chart <- bind_rows(us_chart,
                       get_US_Chart(first_us_chart_date))
}

# load in JSON file
uk_chart <- fromJSON("C:/Users/david.sheehan/Desktop/2017/November/Collabs/uk_chart_fixed.json") %>%
  mutate(chart_week=as.Date(chart_week, format="%d %B %Y"))

# some different types of collaborations
bind_rows(
  list(
    filter(uk_chart, grepl(" FEAT\\. ", artist)) %>% filter(row_number()==1),
    filter(uk_chart, grepl(" FEAT ", artist)) %>% filter(row_number()==1),
    filter(uk_chart, grepl(" FEATURING ", artist)) %>% filter(row_number()==1),
    filter(uk_chart, grepl("/", artist)) %>% filter(row_number()==1),
    filter(uk_chart, grepl(" & ", artist)) %>% filter(row_number()==1),
    filter(uk_chart, grepl(" AND ", artist)) %>% filter(row_number()==1),
    filter(uk_chart, grepl(" WITH ", artist)) %>% filter(row_number()==1),
    filter(uk_chart, grepl(" VS ", artist)) %>% filter(row_number()==1),
    filter(uk_chart, grepl(" VS. ", artist)) %>% filter(row_number()==1)))

# why domain knowledge is useful
bind_rows(list(
    filter(uk_chart, grepl("AC/DC", artist)) %>% filter(row_number()==1),
    filter(uk_chart, grepl("BOB MARLEY & ", artist)) %>% filter(row_number()==1),
    filter(uk_chart, grepl("BOB MARLEY AND ", artist)) %>% filter(row_number()==1)))

filter(uk_chart, grepl("DEREK AND THE DOMINOES", artist)) %>% filter(weeks_on_chart==1)

# append column denoting whether that artist only ever had one charted song
uk_chart <- inner_join(uk_chart,
           uk_chart %>% group_by(artist) %>% 
             summarize(one_hit=n_distinct(track)==1), by=c("artist"))
head(uk_chart)

# repeat the process for Billboard Top 100
us_chart <- jsonlite::fromJSON("C:/Users/david.sheehan/Desktop/2017/November/Collabs/us_chart.json")  %>%
  dplyr::mutate(chart_week=as.Date(chart_week), artist= toupper(artist))
us_chart <- inner_join(us_chart,
                       us_chart %>% group_by(artist) %>% 
                         summarize(one_hit=n_distinct(track)==1), by=c("artist"))


# add flag columns if the artist contained various collaborations titles
# note that you could reduce the code here by combining the two dataframes (bind_rows) and
# performing this action just once
uk_chart <- mutate(uk_chart, FT=grepl(" FT ", artist),
                   FEAT=grepl(" FEAT\\. | FEAT ", artist),
                   FEATURING=grepl(" FEATURING ", artist),
                   AND=grepl(" AND ", artist) & 
                     !(grepl(" AND THE ", artist) & !grepl(" THE WEEKND", artist)) & one_hit,
                   AMPERSAND=grepl(" & ", artist) & 
                     !(grepl(" & THE ", artist) & !grepl(" THE WEEKND", artist)) & one_hit,
                   SLASH=grepl("/", artist) & one_hit,
                   WITH=grepl(" WITH ", artist) & 
                     !grepl(" WITH THE ", artist) & one_hit,
                   VS=grepl(" VS | VS\\. ", artist) & one_hit,
                   X=grepl(" X ", artist) & one_hit &
                     !grepl("RICHARD X|LIBERTY X|TWISTED X|MALCOLM X|X MEN", artist))

us_chart <- mutate(us_chart, FT=grepl(" FT ", artist),
                   FEAT=grepl(" FEAT\\. | FEAT ", artist),
                   FEATURING=grepl(" FEATURING ", artist),
                   AND=grepl(" AND ", artist) & 
                     !(grepl(" AND THE ", artist) & !grepl(" THE WEEKND", artist)) & one_hit,
                   AMPERSAND=grepl(" & ", artist) & 
                     !(grepl(" & THE ", artist) & !grepl(" THE WEEKND", artist)) & one_hit,
                   SLASH=grepl("/", artist) & one_hit,
                   WITH=grepl(" WITH ", artist) & 
                     !grepl(" WITH THE ", artist) & one_hit,
                   VS=grepl(" VS | VS\\. ", artist) & one_hit,
                   X=grepl(" X ", artist) & one_hit &
                     !grepl("RICHARD X|LIBERTY X|TWISTED X|MALCOLM X|X MEN", artist))


# plot the proportion of different collaboration types on the chart each week
uk_collab_plot <- uk_chart %>% group_by(chart_week) %>% 
  summarize_at(c("FT", "FEAT", "FEATURING", "AND", "AMPERSAND", "SLASH", "WITH", "VS", "X"), mean) %>%
  tidyr::gather(collab_type, track_prop, -chart_week) %>%
  plot_ly(x=~chart_week, y=~100*track_prop, color=~collab_type, type="scatter",
                  mode="lines", legendgroup=~collab_type) %>%
  plotly::layout(yaxis=list(range = c(0, 43)), xaxis=list(title="", tickfont=list(size=14)),
                 legend = list(x = 0.07, y = 1,font=list(size=13), tracegroupgap=0))

us_collab_plot <- us_chart %>% group_by(chart_week) %>% 
  summarize_at(c("FT", "FEAT", "FEATURING", "AND", "AMPERSAND", "SLASH", "WITH", "VS", "X"), mean) %>%
  tidyr::gather(collab_type, track_prop, -chart_week) %>%
  plot_ly(x=~chart_week, y=~100*track_prop, color=~collab_type, type="scatter", legendgroup=~collab_type,
                  mode="lines", showlegend = F) %>%
  plotly::layout(yaxis=list(range = c(0, 43)), xaxis=list(title="", tickfont=list(size=14)))

plotly::subplot(uk_collab_plot, us_collab_plot, nrows = 2, shareX = T)  %>%
  plotly::layout(annotations = list(
    list(x = 0.5 , y = 1.02, text = "UK Singles Chart 1952-2017", showarrow = F, xref='paper', yref='paper',
         font=list(size=18)),
    list(x = 0.5 , y = 0.48, text = "Billboard Top 100 1958-2017", showarrow = F, xref='paper', yref='paper',
         font=list(size=18)),
    list(x = -0.05 , y = 0.5, text = "Songs on Chart (%)", textangle = -90, showarrow = F, xref='paper', yref='paper',
         font=list(size=16))
    ))


# plot the proportion of collaborations on the chart each week
bind_rows(
  uk_chart %>% mutate(collab= FT| FEAT| FEATURING| AND| AMPERSAND| SLASH| WITH| VS| X) %>%
    group_by(chart_week) %>% summarize(collab_num = sum(collab), chart_num=n()) %>% mutate(chart="UK Singles Chart"),
  us_chart %>% mutate(collab= FT| FEAT| FEATURING| AND| AMPERSAND| SLASH| WITH| VS| X) %>% 
    group_by(chart_week) %>% summarize(collab_num = sum(collab), chart_num=n()) %>% mutate(chart="Billboard Top 100")) %>%
  plot_ly(x=~chart_week, y=~100*collab_num/chart_num, color=~chart, type="scatter", mode= "lines",
                  text = ~paste0(100*round(collab_num/chart_num,3),"%\n",collab_num, " (", chart_num, ")"), hoverinfo="x+name+text") %>%
  plotly::layout(yaxis=list(title= "Collaborations (%)"), xaxis=list(title="", tickfont=list(size=13)),
                 legend = list(x = 0.07, y = 0.97,font=list(size=16)))
