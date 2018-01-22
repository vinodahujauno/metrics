# Loading Libraries
library(sqldf) # for sql query
library(tidyverse) # for joins
library(ggpubr) # for correlation plotting
library(PerformanceAnalytics) # for correlation plotting
library(corrplot) # for corrplot
library(GGally) # for ggcorr
library(plyr)
library(RColorBrewer) # for color combination in corplot
library(bit64)
# MariaDb Database Connection
library(DBI)
library(RMariaDB)
library(keyringr)


mypwd <- decrypt_dpapi_pw('C:/Password.txt')
# Connecting to Database
connect <- dbConnect(RMariaDB::MariaDB(), user="uno", password=mypwd, dbname="ghtorrent", host="ghdata.sociallycompute.io", client.flag = CLIENT_COMPRESS)
connectlib <- dbConnect(RMariaDB::MariaDB(), user="uno", password=mypwd, dbname="libraries.io", host="ghdata.sociallycompute.io", client.flag = CLIENT_COMPRESS)

# Forming Correlation Table
correlation_table<- data.frame(
  "ProjectName" = character(),
  "ProjectDays" =integer64(),
  "TotalCommits" =integer64(),
  "DistinctCommitters" =integer64(),
  "PullRequestDuration" =integer64(),
  "PullRequestAcceptedDuration"=integer64(),
  "PullRequestCommentDuration" =integer64(), 
  "CommitsDuration" =integer64(),
  "IssueCommentsDuration" =integer64(), 
  "ClosedIssues"= integer64(), 
  "TotalWatchers"= integer64(), 
  "UpstreamDependencies" = integer64(), 
  stringsAsFactors=FALSE)


# Selecting pool of projects
ProjectName1 <- c("rust", "aframe","kubernetes","containerd","rkt")
ProjectOwner1 <- c("rust-lang","aframevr","kubernetes","containerd","rkt")
Project_list<-rbind(ProjectName1, ProjectOwner1)

# Extracting all the relavent data for each project in the pool
for (i in 1:5){
  ProjectName <- paste(Project_list[1,i])
  ProjectOwner <- paste(Project_list[2,i])
  
  #Finding Project ID from GHTorrent data
  project_id <- dbGetQuery(connect, paste("SELECT projects.id FROM projects INNER JOIN users ON projects.owner_id = users.id WHERE projects.name ='",ProjectName,"'AND users.login = '",ProjectOwner,"';",sep = ""))
  ProjectName<-as.character(ProjectName)
  
  # Age of Project in days
  ProjectDays <- dbGetQuery(connect, paste("SELECT DATEDIFF(CURDATE(),created_at) AS ProjectAge FROM projects WHERE id=" ,project_id, ";"))
  
  # pull request duration
  query <- dbGetQuery(connect, paste("select pull_request_history.actor_id, pull_request_history.created_at from pull_request_history join pull_requests on pull_requests.id = pull_request_history.pull_request_id where pull_requests.base_repo_id =" ,project_id, "group by pull_request_history.actor_id, pull_request_history.created_at;"))
  pull_req_duration<-ddply(query, .(actor_id), summarize, first_date=min(created_at), last_date=max(created_at) )
  pull_req_duration$first_date<- as.Date(pull_req_duration$first_date, "%Y-%m-%d")
  pull_req_duration$last_date<- as.Date(pull_req_duration$last_date, "%Y-%m-%d")
  pull_req_duration$Duration<- difftime(pull_req_duration$last_date, pull_req_duration$first_date , units = c("days"))
  # taking average of pull request duration
  #PullRequestDuration<-round(mean(pull_req_duration$Duration))
  PullRequestDuration<-round(median(pull_req_duration$Duration))
  
  
  # pull request accepted duration
  query <- dbGetQuery(connect, paste("select pull_request_history.actor_id, pull_request_history.pull_request_id, pull_request_history.action, pull_request_history.created_at from pull_request_history join pull_requests on pull_requests.id = pull_request_history.pull_request_id where pull_requests.base_repo_id =" ,project_id, "group by  pull_request_history.actor_id, pull_request_history.pull_request_id, pull_request_history.created_at;"))
  merged<-sqldf("select * from query where action='merged' ")
  accepted<-query[(query$pull_request_id %in% merged$pull_request_id),]
  rejected<-query[!(query$pull_request_id %in% merged$pull_request_id),]
  total_rejected<-sqldf('select count(distinct pull_request_id) from rejected')
  pull_req_accepted_duration<-ddply(accepted, .(actor_id), summarize, first_date=min(created_at), last_date=max(created_at) )
  pull_req_accepted_duration$first_date<- as.Date(pull_req_accepted_duration$first_date, "%Y-%m-%d")
  pull_req_accepted_duration$last_date<- as.Date(pull_req_accepted_duration$last_date, "%Y-%m-%d")
  pull_req_accepted_duration$Duration<- difftime(pull_req_accepted_duration$last_date, pull_req_accepted_duration$first_date , units = c("days"))
  pull_req_accepted_duration$Duration<- as.integer(pull_req_accepted_duration$Duration)
  # taking average of pull request duration
  #PullRequestAcceptedDuration<-round(mean(pull_req_accepted_duration$Duration))
  PullRequestAcceptedDuration<-round(median(pull_req_accepted_duration$Duration))
  
  
  # pull request comments duration
  query <- dbGetQuery(connect, paste("select pull_request_comments.user_id, pull_request_comments.created_at from pull_request_comments join pull_requests on pull_requests.id = pull_request_comments.pull_request_id where pull_requests.base_repo_id =" ,project_id, "group by  pull_request_comments.user_id, pull_request_comments.created_at;"))
  pull_req_comments_duration<-ddply(query, .(user_id), summarize, first_date=min(created_at), last_date=max(created_at) )
  pull_req_comments_duration$first_date<- as.Date(pull_req_comments_duration$first_date, "%Y-%m-%d")
  pull_req_comments_duration$last_date<- as.Date(pull_req_comments_duration$last_date, "%Y-%m-%d")
  pull_req_comments_duration$Duration<- difftime(pull_req_comments_duration$last_date, pull_req_comments_duration$first_date , units = c("days"))
  #taking average of pull request comments duration
  #PullRequestCommentDuration<-round(mean(pull_req_comments_duration$Duration))
  PullRequestCommentDuration<-round(median(pull_req_comments_duration$Duration))
  
  # Commits Duration
  query <- dbGetQuery(connect, paste("select author_id, created_at from commits where project_id =",project_id,";"))
  commits_duration<-ddply(query, .(author_id), summarize, first_date=min(created_at), last_date=max(created_at) )
  commits_duration$first_date<- as.Date(commits_duration$first_date, "%Y-%m-%d")
  commits_duration$last_date<- as.Date(commits_duration$last_date, "%Y-%m-%d")
  commits_duration$Duration<- difftime(commits_duration$last_date, commits_duration$first_date , units = c("days"))
  # taking average of commits duration
  #CommitsDuration<-round(mean(commits_duration$Duration))
  CommitsDuration<-round(median(commits_duration$Duration))
  
  # Issue Comments Duration
  query <- dbGetQuery(connect, paste("select issue_comments.user_id, issue_comments.created_at from issue_comments join issues on issues.id = issue_comments.issue_id where issues.repo_id =" ,project_id, "group by  issue_comments.user_id, issue_comments.created_at;"))
  issue_comments_duration<-ddply(query, .(user_id), summarize, first_date=min(created_at), last_date=max(created_at) )
  issue_comments_duration$first_date<- as.Date(issue_comments_duration$first_date, "%Y-%m-%d")
  issue_comments_duration$last_date<- as.Date(issue_comments_duration$last_date, "%Y-%m-%d")
  issue_comments_duration$Duration<- difftime(issue_comments_duration$last_date, issue_comments_duration$first_date , units = c("days"))
  # taking average of issue comments duration
  IssueCommentsDuration<-round(mean(issue_comments_duration$Duration))
  #IssueCommentsDuration<-round(median(issue_comments_duration$Duration))
  
  # Selecting Total Commits
  TotalCommits <- dbGetQuery(connect, paste("select count(id) as TotalCommits from commits where project_id =",project_id,";"))
  
  
  # Count Distinct Committers
  DistinctCommitters <- dbGetQuery(connect, paste("SELECT count(distinct(author_id)) as DistinctCommitters FROM commits WHERE project_id =", project_id, ";"))
  
  
  # Selecting number of closed issues
  ClosedIssues <- dbGetQuery(connect, paste("SELECT count(issue_events.event_id) as 'ClosedIssues' FROM issue_events JOIN issues ON issue_events.issue_id = issues.id WHERE issues.repo_id =", project_id, "AND issue_events.action = 'closed';"))
  
  
  # Selecting total watcher
  TotalWatchers <-dbGetQuery(connect, paste("SELECT count(user_id) as 'Total Watchers' FROM watchers WHERE repo_id =", project_id, ";"))
  
  
  # Selecting dependencies
  DependenciesCount <- dbGetQuery(connectlib, paste("select Name,`Dependent Projects Count` from projects where Name ='",ProjectName,"';",sep = ""))
  UpstreamDependencies <- sum(DependenciesCount$`Dependent Projects Count`)
  
  
  # Joining all the variables and forming correlation table
  correlation_table[nrow(correlation_table)+1,] <- c(ProjectName, ProjectDays, TotalCommits, DistinctCommitters, PullRequestDuration, PullRequestAcceptedDuration, PullRequestCommentDuration, CommitsDuration,  IssueCommentsDuration, ClosedIssues, TotalWatchers, UpstreamDependencies)
}

# Selecting Columns for correlation table
columns <- 2:12 
correlation_table[columns] <- lapply(correlation_table[columns], as.numeric) 

correlation_table

# Correlation Matrix
cor(correlation_table[,columns], method = "pearson")

# Plots
corrplot(cor(correlation_table[,columns]), type = "lower", order = "hclust",  
         tl.col = "black", tl.srt = 0.1,  use="complete.obs", is.corr=FALSE, method = "number",col = brewer.pal(n = 8, name = "RdBu"))

