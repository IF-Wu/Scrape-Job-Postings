library(XML)
library(xml2)
library(RCurl)
library(curl)
library(rvest)
library(stringr)
library(httr)
library(dplyr)
library(ggplot2)

# Make initial queries for different jobs
CC_DS <- htmlParse(getForm("https://www.cybercoders.com/search/", 
                           searchterms = "data scientist", 
                           searchlocation = "", 
                           newsearch = "true", 
                           originalsearch = "true", 
                           sorttype = ""))
CC_DA <- htmlParse(getForm("https://www.cybercoders.com/search/", 
                           searchterms = "data analyst", 
                           searchlocation = "", 
                           newsearch = "true", 
                           originalsearch = "true", 
                           sorttype = ""))
CC_S <- htmlParse(getForm("https://www.cybercoders.com/search/", 
                          searchterms = "statistician", 
                          searchlocation = "", 
                          newsearch = "true", 
                          originalsearch = "true", 
                          sorttype = ""))
CC_DE <- htmlParse(getForm("https://www.cybercoders.com/search/", 
                           searchterms = "data engineer", 
                           searchlocation = "", 
                           newsearch = "true", 
                           originalsearch = "true", 
                           sorttype = ""))

# Check the number of nodes in the first page
length(getNodeSet(CC_DS, "//div[@class = 'job-listing-item']"))
length(getNodeSet(CC_DS, "//div[@class = 'job-status']"))
# We find that one element start with "job-listing-item" is not a job, so we try to use those contain "job-status" as a starting point.

# Function to deal with the full post
FullPost <- function(post){
  header = xpathSApply(post, "//h4[@class = 'section-title']", xmlValue)
  preferred_skills = xpathSApply(post, "//span[@class = 'skill-name']", xmlValue)
  othertext = xpathSApply(post, "//div[@class = 'section-data']", xmlValue)
  body = getNodeSet(post, "//div[@class = 'section-data section-data-title']")
  if(length(body) == 0){
    return(list(header = header, body = "No Full Post", othertext = othertext))
  }
  bodytext = list()
  for(i in 1:length(body)){
    bodytext = append(bodytext, list(xpathSApply(body[[i]], ".//text()", xmlValue, trim = TRUE)))
  }
  list(preferred_skills = preferred_skills, header = header, body = bodytext, othertext = othertext)
}

# Function to get each posts from the result page
CCPost <- function(job){
  title = xpathSApply(job, ".//div[@class = 'job-title']/a", xmlValue)
  date = xpathSApply(job, ".//div[@class = 'posted']/text()", xmlValue)
  location = xpathSApply(job, ".//div[@class = 'location']", xmlValue)
  if(xmlValue(getNodeSet(job, ".//div[@class = 'wage']")) == "Compensation Unspecified"){
    salary = "Not Provided"
    duration = "Not Provided"
  }
  else{
    salary = xpathSApply(job, ".//div[@class = 'wage']/text()", xmlValue)
    duration = xpathSApply(job, ".//div[@class = 'wage']/span", xmlValue)
  }
  link = as.character(getNodeSet(job, ".//div[@class = 'job-title']/a/@href"))
  postURL = getRelativeURL(link, "https://www.cybercoders.com/search/")
  fullpost = FullPost(htmlParse(getURLContent(postURL)))
  list(title = title, date = date, location = location, salary = salary, duration = duration, url = postURL, fullpost = fullpost)
}

# Function to get the next page
CCNextPage <- function(page){
  nextpage = getNodeSet(page, "//a[@class = 'get-search-results next']/@href")
  if(length(nextpage) == 0){
    return(NULL)
  }
  else{
    nextURL = getRelativeURL(as.character(nextpage), "https://www.cybercoders.com/search/")
  }
  return(nextURL)
}

# Function to get all job postings from all pages
getAllPost <- function(CCjob){
  FirstPageNodes = getNodeSet(CCjob, "//div[@class = 'job-listing-item'][.//div[@class = 'job-status']]")
  if(length(FirstPageNodes) == 0){
    return("No Job Posted")
  }
  FirstPagePosts = lapply(FirstPageNodes, CCPost)
  AllPosts = list()
  AllPosts = append(AllPosts, FirstPagePosts)
  if(length(CCNextPage(CCjob)) == 0){
    return(AllPosts)
  }
  NextPageLink = CCNextPage(CCjob)
  while(length(NextPageLink) > 0){
    NextPage = htmlParse(getURLContent(NextPageLink))
    Nodes = getNodeSet(NextPage, "//div[@class = 'job-listing-item'][.//div[@class = 'job-status']]")
    Posts = lapply(Nodes, CCPost)
    AllPosts = append(AllPosts, Posts)
    NextPageLink = CCNextPage(NextPage)
  }
  return(AllPosts)
}

# Function to deal with job duration
JobDuration <- function(job){
  for(i in 1:length(job)){
    if(job[[i]]$duration == "Not Provided"){
      if(length(grep("\\bContract\\b", job[[i]])) > 0){
        job[[i]]$duration = "Contract"
      }
      else{
        job[[i]]$duration = "Full-time"
      }
    }
    else{
      if(length(grep("\\bContract\\b", job[[i]])) > 0){
        job[[i]]$duration = "Contract"
      }
      else{
        job[[i]]$duration = job[[i]]$duration
      }
    }
  }
  return(job)
}

# Function to deal with the salary
JobSalary <- function(job){
  for(i in 1:length(job)){
    if(job[[i]]$salary == "Not Provided"){
      if(length(grep("\\$[0-9]+[kK]* *- *\\$*[0-9]+[kK]*", job[[i]]$fullpost)) > 0){
        text = grep("\\$[0-9]+[kK]* *- *\\$*[0-9]+[kK]*", job[[i]]$fullpost, value = TRUE)
        salary = str_extract(text, "\\$[0-9]+[kK]* *- *\\$*[0-9]+[kK]*")
        job[[i]]$salary = salary
      }
      else{
        job[[i]]$salary = "Not Provided"
      }
    }
    else{
      job[[i]]$salary = job[[i]]$salary
    }
  }
  return(job)
}

# Function to deal with the education level 
JobEducation <- function(job){
  for(i in 1:length(job)){
    if(length(job[[i]]$fullpost$header) > 1){
      index = which(job[[i]]$fullpost$header == "What You Need for this Position")
      post = job[[i]]$fullpost$body[[index]]
      regex1 = "[Bb]achelor[']*s|\\bB[.]*S[.]*\\b|\\bB[.]*S[.]*c[.]*\\b|[Uu]ndergraduate|[Mm]aster[']*s|\\bM[.]*S[.]*\\b|M[.]*S[.]*c[.]*|[Pp]h[.]*[Dd][.]*|[Gg]raduate"
      if(length(grep(regex1, post)) > 0){
        edu = grep(regex1, post, value = TRUE)
        if(length(grep("MS [Ww]indows|MS [Oo]ffice|MS Exchange|MS Work|MS Stack", edu)) > 0){
          if(length(edu) == 1){
            job[[i]]$education = "Not Provided"
          }
          else{
            edu = edu[-which(edu == grep("MS [Ww]indows|MS [Oo]ffice|MS Exchange|MS Work|MS Stack", edu, value = TRUE))]
            job[[i]]$education = edu
          }
        }
        else{
          job[[i]]$education = edu
        }
      }
      else{
        job[[i]]$education = "Not Provided"
      }
    }
    else{
      job[[i]]$education = "Not Provided"
    }
  }
  return(job)
}

# Function to deal with the required skills
JobRequired <- function(job){
  for(i in 1:length(job)){
    if(length(job[[i]]$fullpost$header) > 1){
      index = which(job[[i]]$fullpost$header == "What You Need for this Position")
      post = job[[i]]$fullpost$body[[index]]
      regex2 = "\\bPreferred\\b|\\bPLUS\\b|\\bBONUS\\b|Bonus[-]* *[Ss]kill|Bonus[-]* *[Pp]oint|Nice[-]* *to[-]* *[Hh]ave|NICE[-]* *TO[-]* *HAVE|Plus[-]* *[Ss]kill|Extra[-]* *[Cc]redit"
      if(job[[i]]$education == "Not Provided"){
        skills = post
        if(length(grep(regex2, skills)) > 0){
          preferred_start = which(skills == grep(regex2, skills, value = TRUE)[1])
          job[[i]]$required_skills = skills[1 : (preferred_start - 1)]
        }
        else{
          job[[i]]$required_skills = skills
        }
      }
      else{
        skills = post[-which(post == job[[i]]$education)]
        if(length(grep(regex2, skills)) > 0){
          preferred_start = which(skills == grep(regex2, skills, value = TRUE)[1])
          job[[i]]$required_skills = skills[1 : (preferred_start - 1)]
        }
        else{
          job[[i]]$required_skills = skills
        }
      }
    }
    else{
      job[[i]]$required_skills = "Not Provided"
    }
  }
  return(job)
}

# Using functions above to deal with data scientist jobs
CCDS_JOB <- getAllPost(CC_DS)
CCDS_JOB <- JobDuration(CCDS_JOB)
CCDS_JOB <- JobSalary(CCDS_JOB)
CCDS_JOB <- JobEducation(CCDS_JOB)
CCDS_JOB <- JobRequired(CCDS_JOB)

# Using functions above to deal with data analyst jobs
CCDA_JOB <- getAllPost(CC_DA)
CCDA_JOB <- JobDuration(CCDA_JOB)
CCDA_JOB <- JobSalary(CCDA_JOB)
CCDA_JOB <- JobEducation(CCDA_JOB)
CCDA_JOB <- JobRequired(CCDA_JOB)

# Using functions above to deal with statistician jobs
# We know that there is no statistician job posted
CCS_JOB <- getAllPost(CC_S)

# Using functions above to deal with data engineer jobs
CCDE_JOB <- getAllPost(CC_DE)
CCDE_JOB <- JobDuration(CCDE_JOB)
CCDE_JOB <- JobSalary(CCDE_JOB)
CCDE_JOB <- JobEducation(CCDE_JOB)
CCDE_JOB <- JobRequired(CCDE_JOB)

# Rusults analysis
# Function to get the average salary based on the salary we got before
GetAvgSalary <- function(job){
  for(i in 1:nrow(job)){
    salary = job$salary[i]
    if(salary == "Not Provided"){
      job$average_salary[i] = "Not Provided"
    }
    else{
      if(job$duration[i] == "Contract"){
        lower = unlist(strsplit(gsub(" ", "", salary), " *- *"))[1]
        upper = unlist(strsplit(gsub(" ", "", salary), " *- *"))[2]
        avg = (as.numeric(gsub("\\$|[Kk]", "", upper)) + as.numeric(gsub("\\$|[Kk]", "", lower))) * 0.5
        job$average_salary[i] = paste0("$", avg)
      }
      else{
        lower = unlist(strsplit(gsub(" ", "", salary), " *- *"))[1]
        upper = unlist(strsplit(gsub(" ", "", salary), " *- *"))[2]
        avg = (as.integer(gsub("\\$|[Kk]", "", upper)) + as.integer(gsub("\\$|[Kk]", "", lower))) * 0.5
        job$average_salary[i] = paste0("$", avg, "k")
      }
    }
  }
  return(job)
}

# Function to get the education level for each job
# Since we got a sentence or several sentences for education level in the previous section.
# We need to use text mining method to get the more manageable education level.
GetEducation <- function(job, dataframe){
  bs = "[Bb]achelor[']*s|\\bB[.]*S[.]*\\b|\\bB[.]*S[.]*c[.]*\\b|[Uu]ndergraduate"
  ms = "[Mm]aster[']*s|\\bM[.]*S[.]*\\b|M[.]*S[.]*c[.]*|[Gg]raduate"
  phd = "[Pp]h[.]*[Dd][.]*|[Gg]raduate"
  graduate = "[Mm]aster[']*s|\\bM[.]*S[.]*\\b|M[.]*S[.]*c[.]*|[Pp]h[.]*[Dd][.]*|[Gg]raduate"
  for(i in 1:nrow(dataframe)){
    if(job[[i]]$education == "Not Provided"){
      dataframe$education[i] = "Not Provided"
    }
    else{
      if(length(grep(bs, job[[i]]$education)) > 0){
        if(length(grep(graduate, job[[i]]$education)) > 0){
          dataframe$education[i] = "Bachelors or higher"
        }
        else{
          dataframe$education[i] = "Bachelors"
        }
      }
      else if(length(grep(ms, job[[i]]$education)) > 0){
        dataframe$education[i] = "Masters or higher"
      }
      else{
        dataframe$education[i] = "PhD"
      }
    }
  }
  return(dataframe)
}

# Function to get specific required skills from what we got in the previous section.
GetRequiredSkills <- function(job, dataframe){
  skills = lapply(job, "[[", "required_skills")
  for(i in 1:nrow(dataframe)){
    if(length(grep("\\b[Pp]ython\\b|\\bR\\b|\\bSQL\\b", skills[[i]])) > 0){
      dataframe$required_skills[i] = "Python/R/SQL"
    }
    else{
      dataframe$required_skills[i] = "Other Requirements"
    }
  }
  return(dataframe)
}

# total number of jobs for each query
CCS_JOB # we do not find jobs related to statistician
length(CCDS_JOB) # data scientist
length(CCDA_JOB) # data analyst
length(CCDE_JOB) # data engineer

# create data frame for data scientist
data_scientist <- data.frame(title = unlist(lapply(CCDS_JOB, "[[", "title")))
data_scientist$location <- unlist(lapply(CCDS_JOB, "[[", "location"))
data_scientist$salary <- unlist(lapply(CCDS_JOB, "[[", "salary"))
data_scientist$duration <- unlist(lapply(CCDS_JOB, "[[", "duration"))
data_scientist <- GetAvgSalary(data_scientist)
data_scientist <- GetEducation(CCDS_JOB, data_scientist)
data_scientist <- GetRequiredSkills(CCDS_JOB, data_scientist)

# create data frame for data analyst
data_analyst <- data.frame(title = unlist(lapply(CCDA_JOB, "[[", "title")))
data_analyst$location <- unlist(lapply(CCDA_JOB, "[[", "location"))
data_analyst$salary <- unlist(lapply(CCDA_JOB, "[[", "salary"))
data_analyst$duration <- unlist(lapply(CCDA_JOB, "[[", "duration"))
data_analyst <- GetAvgSalary(data_analyst)
data_analyst <- GetEducation(CCDA_JOB, data_analyst)
data_analyst <- GetRequiredSkills(CCDA_JOB, data_analyst)

# create data frame for data engineer
data_engineer <- data.frame(title = unlist(lapply(CCDE_JOB, "[[", "title")))
data_engineer$location <- unlist(lapply(CCDE_JOB, "[[", "location"))
data_engineer$salary <- unlist(lapply(CCDE_JOB, "[[", "salary"))
data_engineer$duration <- unlist(lapply(CCDE_JOB, "[[", "duration"))
data_engineer <- GetAvgSalary(data_engineer)
data_engineer <- GetEducation(CCDE_JOB, data_engineer)
data_engineer <- GetRequiredSkills(CCDE_JOB, data_engineer)

# relationship between average salary and education
# here we select jobs which have both average salary and education level"
# we also only explore the full-time job
DS_se <- data_scientist[which(data_scientist$average_salary != "Not Provided" 
                              & data_scientist$education != "Not Provided" 
                              & data_scientist$duration == "Full-time"), ]
DS_se$average_salary <- as.numeric(gsub("\\$|k", "", DS_se$average_salary))

DA_se <- data_analyst[which(data_analyst$average_salary != "Not Provided" 
                            & data_analyst$education != "Not Provided" 
                            & data_analyst$duration == "Full-time"), ]
DA_se$average_salary <- as.numeric(gsub("\\$|k", "", DA_se$average_salary))

DE_se <- data_engineer[which(data_engineer$average_salary != "Not Provided" 
                             & data_engineer$education != "Not Provided" 
                             & data_engineer$duration == "Full-time"), ]
DE_se$average_salary <- as.numeric(gsub("\\$|k", "", DE_se$average_salary))

combine_se <- data.frame(jobs = c(rep("Data Scientist", nrow(DS_se)), rep("Data Analyst", nrow(DA_se)), rep("Data Engineer", nrow(DE_se))), 
                         average_salary = c(DS_se$average_salary, DA_se$average_salary, DE_se$average_salary), 
                         education_level = c(DS_se$education, DA_se$education, DE_se$education))
ggplot(combine_se, aes(x = education_level, y = average_salary, shape = jobs)) + 
  geom_point() + 
  xlab("Education level") + 
  ylab("Average Salary (thousand dollars)") + 
  ggtitle("The Average Salary for Different Jobs and Different Education Level")

# relationship between average salary and job location
# here we select jobs which have average salary
# we also only explore the full-time job
# we explore the data engineer as example
DE_sl <- data_engineer[which(data_engineer$average_salary != "Not Provided" 
                             & data_engineer$duration == "Full-time"), ]
DE_sl$average_salary <- as.numeric(gsub("\\$|k", "", DE_sl$average_salary))
DE_sl <- DE_sl[order(DE_sl$average_salary, decreasing = TRUE), ]

ggplot(DE_sl, aes(x = location, y = average_salary)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  xlab("Location") + 
  ylab("Average Salary") + 
  ggtitle("Relationship between Job Location and Average Salary")

# percentage plot for required skills
combine_skills <- data.frame(jobs = c(rep("Data Scientist", length(CCDS_JOB)), rep("Data Analyst", length(CCDA_JOB)), rep("Data Engineer", length(CCDE_JOB))), 
                             skills = c(data_scientist$required_skills, data_analyst$required_skills, data_engineer$required_skills))

ggplot(combine_skills, aes(x = jobs, fill = skills)) + 
  geom_bar(position = "fill") +
  xlab("Jobs") +
  ylab("Percentage") +
  ggtitle("Relationship between Different Skills in Different Jobs") +
  coord_flip()

# top 10 preferred skills for data scientist
All_Preferred_skills_DS <- data.frame(skills = unlist(lapply(lapply(CCDS_JOB, "[[", "fullpost"), "[[", "preferred_skills")))
top10_DS <- All_Preferred_skills_DS %>%
  group_by(skills) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10)
ggplot(data = top10_DS, aes(x = skills, y = count)) + 
  geom_bar(stat="identity") +
  ggtitle("Top 10 Preferred Skills for Data Scientist") + 
  coord_flip()

# top 10 preferred skills for data analyst
All_Preferred_skills_DA <- data.frame(skills = unlist(lapply(lapply(CCDA_JOB, "[[", "fullpost"), "[[", "preferred_skills")))
top10_DA <- All_Preferred_skills_DA %>%
  group_by(skills) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10)
ggplot(data = top10_DA, aes(x = skills, y = count)) + 
  geom_bar(stat="identity") +
  ggtitle("Top 10 Preferred Skills for Data Analyst") + 
  coord_flip()

# top 10 preferred skills for data engineer
All_Preferred_skills_DE <- data.frame(skills = unlist(lapply(lapply(CCDE_JOB, "[[", "fullpost"), "[[", "preferred_skills")))
top10_DE <- All_Preferred_skills_DE %>%
  group_by(skills) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10)
ggplot(data = top10_DE, aes(x = skills, y = count)) + 
  geom_bar(stat="identity") +
  ggtitle("Top 10 Preferred Skills for Data Engineer") + 
  coord_flip()