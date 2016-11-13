## if you haven't already installed jobbR
# devtools::install_github("dashee87/jobbR")

## loading the packages we'll need
require(jobbR)
require(dplyr)
require(rvest)
require(stringr)
require(plotly)

# collecting data scientist jobs from the Indeed API
dataScientists <- jobSearch(publisher = "publisherkey", query = "data+scientist", 
                            country = "uk", location = "london", all = TRUE)

# collecting data analyst jobs from the Indeed API
dataAnalysts <- jobSearch(publisher = "publisherkey", query =  "data+analyst", 
                          country = "uk", location = "london", all = TRUE)

# removing junior and senior roles
dataScientists <- dataScientists[grepl(
  "data scientist", dataScientists$results.jobtitle,ignore.case = TRUE) &
    !grepl("senior|junior|lead|manage|intern|analyst|graduate|chief",
           dataScientists$results.jobtitle,ignore.case = TRUE),]

dataAnalysts <- dataAnalysts[grepl(
  "data analyst", dataAnalysts$results.jobtitle,ignore.case = TRUE) &
    !grepl("senior|junior|lead|manage|intern|scientist|graduate|chief",
           dataAnalysts$results.jobtitle,ignore.case = TRUE),]

dataScientists <- dataScientists[! duplicated(dataScientists$results.jobkey),]
dataAnalysts <- dataAnalysts[! duplicated(dataAnalysts$results.jobkey),]


# scrape job description webpages
ds_job_descripts <- unlist(lapply(dataScientists$results.url, 
                                  function(x){read_html(x) %>% 
                                      html_nodes("#job_summary") %>% 
                                      html_text() %>% tolower()}))

da_job_descripts <- unlist(lapply(dataAnalysts$results.url, 
                                  function(x){read_html(x) %>% 
                                      html_nodes("#job_summary") %>% 
                                      html_text() %>% tolower()}))

# an example data scientist job description
# I picked 49 as it's one of the shorter ones
ds_job_descripts[49]



skills=data.frame(
  title=c("R", "Python", "SQL", "Excel", "Powerpoint", "KPI", "Dashboard",
          "Matlab", "Tableau", "Qlikview", "D3", "SAS", "SPSS", "BI", "C++",
          "Java", "Javascript", "Ruby", "Scala", "Php", "VBA",
          "Machine Learning", "Big Data", "Modelling", "Communication",
          "Stakeholder", "Masters", "PhD", "Hadoop", "Spark", "Map Reduce",
          "Pig", "Hive", "NoSQL", "MongoDB", "Cassandra", "Mahout",
          "Google Analytics", "Adobe", "API", "NLP", "Maths", "Statistics",
          "Physics", "Computer Science", "Engineering", "Economics",
          "Finance"),
  regex=c("r", "python|numpy|pandas|scikit", "sql|mysql|sql server|mssql",
          "excel","powerpoint|power point", "dashboards?", "kpis?",
          "matlab", "tableau", "qlikview", "d3", "sas", "spss",
          "bi|business intelligence", "c\\+\\+|c/c\\+\\+", "java",
          "javascript", "ruby", "scala", "php", "vba?|visual basic", 
          "machine learning", "big data", "modelling|modeling",
          "communication", "stakeholders?", "masters?|msc","phd", "hadoop",
          "spark", "map reduce|mapreduce|map/reduce", "pig", "hive", "nosql",
          "mongodb", "cassandra", "mahout","google analytics|GA|big query",
          "adobe", "apis?", "nlp|natural language", "math?s|mathematics",
          "statistics|biostatistics", "physics", "computer science",
          "engineering", "economics", "finance"),
  stringsAsFactors = FALSE
)

# count number of occurences of each word in the skills dataframe in 
# the data science job descriptions
ds_occurs <- matrix(unlist(lapply(skills$regex, 
                                  function(x){str_count(ds_job_descripts,
                                                        paste0("\\b", x, "\\b"))})),
                    length(ds_job_descripts), length(skills$title))

# count number of occurences of each word in the skills dataframe in 
# the data science job descriptions
da_occurs <- matrix(unlist(lapply(skills$regex,
                                  function(x){str_count(da_job_descripts,
                                                        paste0("\\b", x, "\\b"))})),
                    length(da_job_descripts), length(skills$title))

head(ds_occurs[,1:10])


ds_occurs <- ifelse(ds_occurs>1, 1, ds_occurs)
da_occurs <- ifelse(da_occurs>1, 1, da_occurs)

plot_ly(rbind(data.frame(job = "Data Scientist",
                         skills = skills$title,
                         prop = round(
                           100*apply(ds_occurs, 2, sum)/nrow(ds_occurs), 2)),
              data.frame(job = "Data Analyst",
                         skills = skills$title, 
                         prop = round(
                           100*apply(da_occurs, 2, sum)/nrow(da_occurs), 2))),
        x = ~skills, y = ~prop, color= ~job, type = 'bar') %>% 
  layout(margin = list(b = 109), 
         xaxis = list(title = "", tickfont = list(size = 12)),
         yaxis = list(title =
                        "<b>Appearance in Job Description (% Job Postings)</b>",
                      titlefont = list(size = 16)),
         title = "<b>Job Description Skills: Data Scientist v Data Analyst</b>",
         titlefont = list(size=17)) %>% 
  layout(legend=list(font = list(size = 16))) %>% 
  layout(autosize = F, width = 1200, height = 800)


ggplot(rbind(data.frame(type = "Data Scientist", 
                        num_skills = apply(ds_occurs,1,sum)),
             data.frame(type = "Data Analyst", 
                        num_skills = apply(da_occurs,1,sum))),
       aes(num_skills, colour = type)) + stat_ecdf(size = 1) +
  geom_text(size=8, aes(20, .3, label = "Data Analyst", color = "Data Analyst")) + 
  geom_text(size=8, aes(20, .2, label = "Data Scientist", color= "Data Scientist")) + 
  labs(title = "# Skills in Job Description: Data Analysts vs Data Scientists",
       x = "Number of Skills", y = "Cumulative Proportion") + 
  theme(axis.title = element_text(size = 14,face = "bold"), 
        plot.title = element_text(size = 16,face = "bold"), legend.position = "none",
        axis.text = element_text(size = 11))


all_jobs <- rbind(dataScientists, dataAnalysts)
all_job_descripts <- c(ds_job_descripts, da_job_descripts)
all_occurs <- rbind(ds_occurs, da_occurs)

# constructing training set (random sample 70 % of the size of the total set)
set.seed(909)
training=sample(1:nrow(all_jobs),floor(7*nrow(all_jobs)/10))

# we determine the class of each training document by checking the row number
# against the number of rows in the training set
# If the row number is greater than the number of rows in the dataScientists
# dataframe, then it's a data analyst position

#calculating proportion of word occurences e.g. P(Maths| Data Scientist)
ds_probs <- apply(all_occurs[training[training <= nrow(dataScientists)],],
                  2,function(x)(1+sum(x>0))/(2+length(x)))
da_probs <- apply(all_occurs[training[training > nrow(dataScientists)],],
                  2,function(x)(1+sum(x>0))/(2+length(x)))

ds_prior <- sum(training <= nrow(dataScientists))/length(training)

all_occurs <- ifelse(all_occurs==0, -1, all_occurs)

training_output=data.frame(
  num=training,
  job_type=training <= nrow(dataScientists),
  naive_ds= apply(t(t(all_occurs[training,])*ds_probs), 1, function(x){
    log(ds_prior) + sum(log(ifelse(x<0, 1+x, x)))}),
  naive_da= apply(t(t(all_occurs[training,])*da_probs), 1, function(x){
    log(1 - ds_prior) + sum(log(ifelse(x<0, 1+x, x)))}))
training_output$naive <- training_output$naive_ds > training_output$naive_da

head(training_output)

table(training_output[c("job_type","naive")])


test_output=data.frame(
  num = as.vector(1:nrow(all_jobs))[-training],
  job_type = all_jobs[-training,]$query=="data scientist",
  naive_ds = apply(t(t(all_occurs[-training,]) * ds_probs),
                   1, function(x){prod(ifelse(x<=0, 1+x, x))}),
  naive_da = apply(t(t(all_occurs[-training,]) * da_probs),
                   1, function(x){prod(ifelse(x<=0, 1+x, x))}))
test_output$naive <- test_output$naive_ds>test_output$naive_da
table(test_output[c("job_type", "naive")])

plot_ly() %>%
  add_pie(data = count(filter(test_output, job_type) %>% 
                         mutate(job_type = "Data Scientist") %>% 
                         mutate(job_type, naive = 
                                  ifelse(naive, "True Positive",
                                         "False Negative")), naive), 
          labels = ~naive, values = ~n, name = "Data Scientist", 
          domain = list(x = c(0, 0.47), y = c(0.2, 1))) %>% 
  add_pie(data = count(filter(test_output, !job_type) %>% 
                         mutate(job_type = "Data Analyst") %>% 
                         mutate(job_type, naive =
                                  ifelse(!naive, "True Negative",
                                         "False Positive")), naive), 
          labels = ~naive, values = ~n, name = "Data Analyst", 
          domain = list(x = c(0.53, 1), y = c(0.2, 1))) %>%
  add_annotations(
    x = c(27,21.6,25.4,20),
    y = c(-0,-0.7,-0.7,-1),
    xref = "x2",
    yref = "y2",
    text = c("","<b>Data Scientist</b>","<b>Data Analyst</b>",""),
    showarrow = FALSE) %>%
  layout(title = "Predictive Accuracy of Naive Bayes Data Scientist Model",
         xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
         yaxis = list(showgrid = F, zeroline = F, showticklabels = F),
         margin = list(b = 0)) 

# false positives
FPs <- filter(test_output,!job_type & naive)$num
# example false positive job description
all_job_descripts[FPs[1]]

FNs <- filter(test_output,job_type & !naive)$num
# example false negative job description
all_job_descripts[FNs[1]]
