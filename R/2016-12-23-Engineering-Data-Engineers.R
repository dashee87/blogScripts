## if you haven't already installed jobbR
# devtools::install_github("dashee87/jobbR")

## loading the packages we'll need
require(jobbR) # searching indeed API
require(dplyr) # data frame filtering/manipulation
require(rvest) # web scraping
require(stringr) # counting patterns within job descriptions
require(plotly) # interactive plots
require(ggplot2) # vanilla plots
require(tm) # text mining

# collecting data scientist jobs from the Indeed API
dataScientists <- jobSearch(publisher = "appkey", query = "data+scientist", 
                            country = "uk", location = "london", all = TRUE)

# collecting data analyst jobs from the Indeed API
dataAnalysts <- jobSearch(publisher = "appkey", query =  "data+analyst", 
                          country = "uk", location = "london", all = TRUE)

# collecting data engineer jobs from the Indeed API
dataEngineers <- jobSearch(publisher = "appkey", query =  "data+engineer", 
                           country = "uk", location = "london", all = TRUE)

# removing junior and senior roles
dataScientists <- dataScientists[grepl(
  "data scientist", dataScientists$results.jobtitle,ignore.case = TRUE) &
    !grepl("senior|junior|lead|manage|intern|analyst|graduate|chief|engineer",
           dataScientists$results.jobtitle,ignore.case = TRUE),]

dataAnalysts <- dataAnalysts[grepl(
  "data analyst", dataAnalysts$results.jobtitle,ignore.case = TRUE) &
    !grepl("senior|junior|lead|manage|intern|scientist|graduate|chief|engineer",
           dataAnalysts$results.jobtitle,ignore.case = TRUE),]

dataEngineers <- dataEngineers[grepl(
  "data engineer", dataEngineers$results.jobtitle,ignore.case = TRUE) &
    !grepl("senior|junior|lead|manage|intern|scientist|graduate|chief|analyst",
           dataEngineers$results.jobtitle,ignore.case = TRUE),]

# remove duplicates
dataScientists <- dataScientists[! duplicated(dataScientists$results.jobkey),]
dataAnalysts <- dataAnalysts[! duplicated(dataAnalysts$results.jobkey),]
dataEngineers <- dataEngineers[! duplicated(dataEngineers$results.jobkey),]

# number of positions for each job type
data.frame(job_type = c("Data Scientist","Data Analyst","Data Engineer"),
           num_jobs=
             sapply(list(dataScientists,dataAnalysts,dataEngineers),nrow))

# get salary figures for all data scientist positions
dsSalary <- lapply(dataScientists$results.url, function(x)getSalary(x,"GBP"))
dsSalary <- do.call(rbind, dsSalary)

# get salary figures for all data analyst positions
daSalary <- lapply(dataAnalysts$results.url, function(x)getSalary(x,"GBP"))
daSalary <- do.call(rbind, daSalary)

# get salary figures for all data engineer positions
deSalary <- lapply(dataEngineers$results.url, function(x)getSalary(x,"GBP"))
deSalary <- do.call(rbind, deSalary)

# retain jobs with scraped annual salaries
dsSalary <- dsSalary[! is.na(dsSalary$minSal) & dsSalary$period=="year",]
daSalary <- daSalary[! is.na(daSalary$minSal) & daSalary$period=="year",]
deSalary <- deSalary[! is.na(deSalary$minSal) & deSalary$period=="year",]

# take midpoint of salary range
dsSalary$Sal <- mapply(function(x,y){(x+y)/2}, dsSalary$minSal, dsSalary$maxSal)
daSalary$Sal <- mapply(function(x,y){(x+y)/2}, daSalary$minSal, daSalary$maxSal)
deSalary$Sal <- mapply(function(x,y){(x+y)/2}, deSalary$minSal, deSalary$maxSal)

#plot cumulative distribution
dsSalary$type <- "Data Scientist"
daSalary$type <- "Data Analyst"
deSalary$type <- "Data Engineer"
ggplot(rbind(dsSalary, daSalary, deSalary), aes(Sal, colour = type)) + stat_ecdf(size = 1) +
  geom_text(size=8, aes(100000, .4, 
                        label = paste0("Data Analyst (",nrow(daSalary),")"), color = "Data Analyst")) + 
  geom_text(size=8, aes(100000, .3, 
                        label = paste0("Data Scientist (",nrow(dsSalary),")"), 
                        color= "Data Scientist")) + 
  geom_text(size=8, aes(100000, .2, 
                        label = paste0("Data Engineer (",nrow(deSalary),")"), 
                        color= "Data Engineer")) + 
  labs(title = "Annual Salary: Data Analysts v Data Scientists v Data Engineers", 
       x = "Annual Salary (GBP)", y = "Cumulative Proportion") + 
  theme(axis.title = element_text(size = 13,face = "bold"), 
        plot.title = element_text(size = 14,face = "bold"), legend.position = "none",
        axis.text = element_text(size = 11))

# job description plot

plot_ly(rbind(data.frame(job = "Data Scientist",
                         skills = skills$title,
                         prop = round(
                           100*apply(ds_occurs, 2, sum)/nrow(ds_occurs), 2)),
              data.frame(job = "Data Analyst",
                         skills = skills$title, 
                         prop = round(
                           100*apply(da_occurs, 2, sum)/nrow(da_occurs), 2)),
              data.frame(job = "Data Engineer",
                         skills = skills$title, 
                         prop = round(
                           100*apply(de_occurs, 2, sum)/nrow(de_occurs), 2))),
        x = ~skills, y = ~prop, color= ~job, type = 'bar') %>% 
  layout(margin = list(b = 109), 
         xaxis = list(title = "", tickfont = list(size = 12)),
         yaxis = list(title =
                        "<b>Appearance in Job Description (% Job Postings)</b>",
                      titlefont = list(size = 16)),
         title = "<b>Job Description Skills: Data Scientist v Data Analyst v Data Engineer</b>",
         titlefont = list(size=17)) %>% 
  layout(legend=list(font = list(size = 16)))


# scrape job description webpages
ds_job_descripts <- unlist(lapply(dataScientists$results.url, 
                                  function(x){read_html(x) %>% 
                                      html_nodes("#job_summary") %>% 
                                      html_text() %>% tolower() %>%
                                      gsub("\n|/"," ",.) %>%
                                      gsub("'|'","",.) %>%
                                      gsub("[^[:alnum:]///' ]", "", .)}))

da_job_descripts <- unlist(lapply(dataAnalysts$results.url, 
                                  function(x){read_html(x) %>% 
                                      html_nodes("#job_summary") %>% 
                                      html_text() %>% tolower()%>%
                                      gsub("\n|/"," ",.) %>%
                                      gsub("'|'","",.) %>%
                                      gsub("[^[:alnum:]///' ]", "", .)}))

de_job_descripts <- unlist(lapply(dataEngineers$results.url, 
                                  function(x){read_html(x) %>% 
                                      html_nodes("#job_summary") %>% 
                                      html_text() %>% tolower() %>%
                                      gsub("\n|/"," ",.) %>%
                                      gsub("'|'","",.) %>%
                                      gsub("[^[:alnum:]///' ]", "", .)}))

# constructing corpuses

de_corpus <- Corpus(VectorSource(de_job_descripts)) %>% 
  tm_map(function(x){
    removePunctuation(x, preserve_intra_word_dashes = TRUE)}) %>% 
  tm_map(stripWhitespace) %>% tm_map(removeWords,stopwords("english")) %>%
  tm_map(PlainTextDocument)

all_corpus <- Corpus(VectorSource(c(de_job_descripts,
                                    da_job_descripts,ds_job_descripts))) %>% 
  tm_map(function(x){
    removePunctuation(x,preserve_intra_word_dashes = TRUE)}) %>% 
  tm_map(stripWhitespace) %>% tm_map(removeWords,stopwords("english")) %>%
  tm_map(PlainTextDocument)

# converting corpuses to term document matrices

de_tdm <- TermDocumentMatrix(de_corpus)
all_tdm <- TermDocumentMatrix(all_corpus)
de_df <- data.frame(word= row.names(de_tdm),
                    tf = rowSums(ifelse(as.matrix(de_tdm)>0,1,0)),
                    row.names = NULL, stringsAsFactors = FALSE)
all_df <- data.frame(word= row.names(all_tdm),
                     tf = rowSums(ifelse(as.matrix(all_tdm)>0,1,0)),
                     row.names = NULL, stringsAsFactors = FALSE)
# data engineer common words
de_df %>% arrange(-tf) %>% head
# all jobs common words
all_df %>% arrange(-tf) %>% head

# calculating idf and tf-idf

de_df$tf = de_df$tf/max(de_df$tf)
de_idf <- data.frame(word=row.names(all_tdm),
                     idf = log2(length(all_corpus)/rowSums(
                       ifelse(as.matrix(all_tdm)>0,1,0))),
                     row.names = NULL, stringsAsFactors = FALSE)
de_df$tf_idf = de_df$tf * de_idf[match(de_df$word,de_idf$word),]$idf
knitr::kable(de_df %>% inner_join(de_idf,by=c("word"="word")) %>%
               arrange(-tf_idf) %>% mutate(rank=row_number()) %>%
               select(rank,word,tf,idf,tf_idf)  %>% head(40), digits=3)

