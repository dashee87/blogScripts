# For full post, see
# https://dashee87.github.io/data%20science/data-scientists-vs-data-analysts-part-1/

## if you haven't already installed jobbR
# devtools::install_github("dashee87/jobbR")

## loading the packages we'll need
require(jobbR)
require(ggplot2)

# collecting data scientist jobs from the Indeed API
dataScientists <- jobSearch(publisher = "yourpublisherID", query = "data+scientist",
                            country = "uk",location = "london",all = TRUE)

# collecting data analyst jobs from the Indeed API
dataAnalysts <- jobSearch(publisher = "yourpublisherID", query = "data+analyst",
                          country = "uk",location = "london",all = TRUE)

# removing junior and senior roles
dataScientists <- dataScientists[grepl("data scientist",
                                       dataScientists$results.jobtitle, ignore.case = TRUE) &
                                   !grepl("senior|junior|lead|manage|intern|analyst|graduate",
                                          dataScientists$results.jobtitle, ignore.case = TRUE),]

dataAnalysts <- dataAnalysts[grepl("data analyst", 
                                   dataAnalysts$results.jobtitle, ignore.case = TRUE) & 
                               !grepl("senior|junior|lead|manage|intern|scientist|graduate",
                                      dataAnalysts$results.jobtitle,ignore.case = TRUE),]


dataScientists <- dataScientists[! duplicated(dataScientists$results.jobkey),]
dataAnalysts <- dataAnalysts[! duplicated(dataAnalysts$results.jobkey),]


# number of job posts per role
lapply(list(dataScientists,dataAnalysts),nrow)


# get salary figures for all data scientist postions
dsSalary <- lapply(dataScientists$results.url, function(x)getSalary(x,"GBP"))
dsSalary <- do.call(rbind,dsSalary)

# get salary figures for all data analyst postions
daSalary <- lapply(dataAnalysts$results.url, function(x)getSalary(x,"GBP"))
daSalary <- do.call(rbind,daSalary)

# quick look at our salary dataset
head(daSalary)

# filtering out jobs with no advertised salary or retaining those with annual salaries
dsSalary <- dsSalary[!is.na(dsSalary$minSal) & dsSalary$period=="year",]
daSalary <- daSalary[!is.na(daSalary$minSal) & daSalary$period=="year",]

# number of postions with an advertised annual salary
lapply(list(dsSalary, daSalary),nrow)

# plot annual salary cumulative distributions
dsSalary$Sal <- mapply(function(x,y){(x+y)/2}, dsSalary$minSal, dsSalary$maxSal)
daSalary$Sal <- mapply(function(x,y){(x+y)/2}, daSalary$minSal, daSalary$maxSal)
dsSalary$type <- "Data Scientist"
daSalary$type <- "Data Analyst"
ggplot(rbind(dsSalary,daSalary),aes(Sal, colour = type)) + stat_ecdf(size = 1) + 
  geom_text(size = 8, aes(100000, .3, label = "Data Analyst", color = "Data Analyst")) + 
  geom_text(size = 8, aes(100000, .2, label = "Data Scientist", color = "Data Scientist")) + 
  labs(title ="Annual Salary: Data Analysts vs Data Scientists", 
       x = "Annual Salary (GBP)", y = "Cumulative Proportion") + 
  theme(axis.title=element_text(size = 14,face = "bold"),
        plot.title=element_text(size = 16, face = "bold"),
        legend.position = "none",axis.text = element_text(size = 11))
