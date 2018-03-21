# Loading Libraries
library(sqldf) # for sql query
library(tidyverse) # for joins
library(ggpubr) # for correlation plotting
library(PerformanceAnalytics) # for correlation plotting
library(corrplot) # for corrplot
library(GGally) # for ggcorr
library(plyr)
library(RColorBrewer) # for color combination in corplot
library(bit64) # For decelaring table in integer 64 as data imported is in interger64 format
# MariaDb Database Connection
library(DBI) # For Database Connection
library(RMariaDB) # For Database Connection
library(keyringr) ## For Password encryption
library(MASS) # for ANOVA

mypwd <- decrypt_dpapi_pw('C:/Password.txt')
# Connecting to Database
connect <- dbConnect(RMariaDB::MariaDB(), user="uno", password=mypwd, dbname="ghtorrent", host="ghdata.sociallycompute.io", client.flag = CLIENT_COMPRESS)
connectlib <- dbConnect(RMariaDB::MariaDB(), user="uno", password=mypwd, dbname="libraries.io", host="ghdata.sociallycompute.io", client.flag = CLIENT_COMPRESS)

# Forming Correlation Table
correlation_table<- data.frame(
  "ProjectName" = character(),
  "ProjectAge" =integer64(),
  "TotalWatchers"= integer64(), 
  "TotalCommitters" =integer64(),
  "TotalCommits" =integer64(),
  "TotalClosedIssues"= integer64(), 
  "PullRequestDuration" =integer64(),
  "PullRequestCommentDuration" = integer64(),
  "CommitsDuration" =integer64(),
  "IssueCommentsDuration" =integer64(), 
  "PullRequestAcceptedDuration"=integer64(),
  "PullRequestRejectedDuration"=integer64(),
  "AvgPullRequestComments"= integer64(),
  "UpstreamDependencies" = integer64(),
  "DownstreamDependencies"= integer64(),
  "Vindex" = integer64(),
  stringsAsFactors=FALSE)

# Selecting pool of projects
Project_list<- read.csv("G:/My Drive/Thesis/3yearrustprojects.csv")
counter<-203
# Extracting all the relavent data for each project in the pool
for (i in 203:314){
  ProjectOwner <- paste(Project_list[i,7])
  ProjectName <- paste(Project_list[i,8])
  ProjectNameLibraries <- paste(Project_list[i,5])

  connect <- dbConnect(RMariaDB::MariaDB(), user="uno", password=mypwd, dbname="ghtorrent", host="ghdata.sociallycompute.io", client.flag = CLIENT_COMPRESS)
  
  #Finding Project ID from GHTorrent data
  project_id <- dbGetQuery(connect, paste("SELECT projects.id FROM projects INNER JOIN users ON projects.owner_id = users.id WHERE projects.name ='",ProjectName,"'AND users.login = '",ProjectOwner,"';",sep = ""))
  counter <- counter +1
  print(counter)
  if (is.na(project_id[1,1])){
    next
  }
  ProjectName<-as.character(ProjectName)
  
  ## Activity Metrics
  
  # Age of Project in days
  ProjectAge <- dbGetQuery(connect, paste("SELECT DATEDIFF(CURDATE(),created_at) AS ProjectAge FROM projects WHERE id=" ,project_id, ";"))
  
  # Selecting total watcher
  TotalWatchers <-dbGetQuery(connect, paste("SELECT count(user_id) as 'Total Watchers' FROM watchers WHERE repo_id =", project_id, ";"))
  if (is.na(TotalWatchers[1,1])){
    next
  }
  
    # Count Distinct Committers
  TotalCommitters <- dbGetQuery(connect, paste("SELECT count(distinct(author_id)) as DistinctCommitters FROM commits WHERE project_id =", project_id, ";"))
  if (is.na(TotalCommitters[1,1])){
    next
  }
  
  # Selecting Total Commits
  TotalCommits <- dbGetQuery(connect, paste("select count(id) as TotalCommits from commits where project_id =",project_id,";"))
  if (is.na(TotalCommits[1,1])){
    next
  }
  
  # Selecting number of closed issues
  TotalClosedIssues <- dbGetQuery(connect, paste("SELECT count(issue_events.event_id) as 'ClosedIssues' FROM issue_events JOIN issues ON issue_events.issue_id = issues.id WHERE issues.repo_id =", project_id, "AND issue_events.action = 'closed';"))
  if (is.na(TotalClosedIssues[1,1])){
    next
  }
  
  
  # pull request duration
  query <- dbGetQuery(connect, paste("select pull_request_history.actor_id, DATE_FORMAT(pull_request_history.created_at, '%d/%m/%Y') as created_at from pull_request_history join pull_requests on pull_requests.id = pull_request_history.pull_request_id where pull_requests.base_repo_id =" ,project_id, "group by pull_request_history.actor_id, pull_request_history.created_at;"))
  if (is.na(query[1,1])){
    next
  } else {
    query$created_at<- as.Date(query$created_at,'%d/%m/%Y')
    pull_req_duration<-ddply(query, .(actor_id), summarize, first_date=min(created_at), last_date=max(created_at) )
    pull_req_duration$Duration<- difftime(pull_req_duration$last_date, pull_req_duration$first_date , units = c("days"))
    #PullRequestDuration<-round(mean(pull_req_duration$Duration))
    PullRequestDuration<-round(median(pull_req_duration$Duration))
  }
  
  # pull request comments duration
  query <- dbGetQuery(connect, paste("select pull_request_comments.user_id, DATE_FORMAT(pull_request_comments.created_at, '%d/%m/%Y') as created_at  from pull_request_comments join pull_requests on pull_requests.id = pull_request_comments.pull_request_id where pull_requests.base_repo_id =" ,project_id, "group by  pull_request_comments.user_id, pull_request_comments.created_at;"))
  if (is.na(query[1,1])){
    next
  } else {
  query$created_at<- as.Date(query$created_at,'%d/%m/%Y')
  pull_req_comments_duration<-ddply(query, .(user_id), summarize, first_date=min(created_at), last_date=max(created_at) )
  pull_req_comments_duration$Duration<- difftime(pull_req_comments_duration$last_date, pull_req_comments_duration$first_date , units = c("days"))
  #taking average of pull request comments duration
  #PullRequestCommentDuration<-round(mean(pull_req_comments_duration$Duration))
  PullRequestCommentDuration<-round(median(pull_req_comments_duration$Duration))
  }
  
  # Commits Duration
  query <- dbGetQuery(connect, paste("select author_id, DATE_FORMAT(created_at, '%d/%m/%Y') as created_at from commits where project_id =",project_id,";"))
  if (is.na(query[1,1])){
    next
  } else {
  query$created_at<- as.Date(query$created_at,'%d/%m/%Y')
  commits_duration<-ddply(query, .(author_id), summarize, first_date=min(created_at), last_date=max(created_at) )
  commits_duration$Duration<- difftime(commits_duration$last_date, commits_duration$first_date , units = c("days"))
  # taking average of commits duration
  #CommitsDuration<-round(mean(commits_duration$Duration))
  CommitsDuration<-round(median(commits_duration$Duration))
  }
  
  # Issue Comments Duration
  query <- dbGetQuery(connect, paste("select issue_comments.user_id, DATE_FORMAT(issue_comments.created_at, '%d/%m/%Y') as created_at from issue_comments join issues on issues.id = issue_comments.issue_id where issues.repo_id =" ,project_id, "group by  issue_comments.user_id, issue_comments.created_at;"))
  if (is.na(query[1,1])){
    next
  } else {
  query$created_at<- as.Date(query$created_at,'%d/%m/%Y')
  issue_comments_duration<-ddply(query, .(user_id), summarize, first_date=min(created_at), last_date=max(created_at) )
  issue_comments_duration$Duration<- difftime(issue_comments_duration$last_date, issue_comments_duration$first_date , units = c("days"))
  # taking average of issue comments duration
  #IssueCommentsDuration<-round(mean(issue_comments_duration$Duration))
  IssueCommentsDuration<-round(median(issue_comments_duration$Duration))
  }
  
  ## Reward Metrics
  
  # pull request accepted duration
  query <- dbGetQuery(connect, paste("select pull_request_history.actor_id, pull_request_history.pull_request_id, pull_request_history.action, DATE_FORMAT(pull_request_history.created_at, '%d/%m/%Y') as created_at from pull_request_history join pull_requests on pull_requests.id = pull_request_history.pull_request_id where pull_requests.base_repo_id =" ,project_id, "group by  pull_request_history.actor_id, pull_request_history.pull_request_id, pull_request_history.created_at;"))
  if (is.na(query[1,1])){
    PullRequestAcceptedDuration<-0
    PullRequestRejectedDuration<-0
    } else {
  query$created_at<- as.Date(query$created_at,'%d/%m/%Y')
  merged<-sqldf("select * from query where action='merged' ")
  accepted<-query[(query$pull_request_id %in% merged$pull_request_id),]
  if (is.na(accepted[1,1])){
    PullRequestAcceptedDuration<-0
  } else {
    pull_req_accepted_duration<-ddply(accepted, .(pull_request_id), summarize, first_date=min(created_at), last_date=max(created_at) )
    pull_req_accepted_duration$Duration<- difftime(pull_req_accepted_duration$last_date, pull_req_accepted_duration$first_date , units = c("days"))
    # taking average of pull request duration
    #PullRequestAcceptedDuration<-round(mean(pull_req_accepted_duration$Duration))
    PullRequestAcceptedDuration<-round(median(pull_req_accepted_duration$Duration))
    }
  rejected<-query[!(query$pull_request_id %in% merged$pull_request_id),]
  if (is.na(rejected[1,1])){
    PullRequestAcceptedDuration<-0
  } else {
    total_rejected<-sqldf('select count(distinct pull_request_id) from rejected')
    # pull request rejected duration
    pull_req_rejected_duration<-ddply(rejected, .(pull_request_id), summarize, first_date=min(created_at), last_date=max(created_at) )
    pull_req_rejected_duration$Duration<- difftime(pull_req_rejected_duration$last_date, pull_req_rejected_duration$first_date , units = c("days"))
    #PullRequestRejectedDuration<-round(mean(pull_req_rejected_duration$Duration))
    PullRequestRejectedDuration<-round(median(pull_req_rejected_duration$Duration))
    }
  }

  # Average comments on pull request
  query <- dbGetQuery(connect, paste("
                                     select pull_request_comments.pull_request_id, count(pull_request_comments.user_id) as total_comments
                                     from pull_request_comments
                                     join pull_requests
                                     on pull_requests.id=pull_request_comments.pull_request_id
                                     where pull_requests.base_repo_id =" ,project_id, "
                                     group by pull_request_comments.pull_request_id;",sep = ""))
  if (is.na(query[1,1])){
    AvgPullRequestComments<-0
    } else {
  #AvgPullRequestComments<-round(mean(query$total_comments))
  AvgPullRequestComments<-median(query$total_comments)
  AvgPullRequestComments<-as.numeric(AvgPullRequestComments)
    }

  ## Impact Metrics
  connectlib <- dbConnect(RMariaDB::MariaDB(), user="uno", password=mypwd, dbname="libraries.io", host="ghdata.sociallycompute.io", client.flag = CLIENT_COMPRESS)
  

  # Selecting Upstream Dependencies
  UpstreamDependencies <- dbGetQuery(connectlib, paste("select sum(b.Total) 
                                                         from (select a.platform as platfrom, 
                                                              max(a.`Version ID`) as version, 
                                                              a.Count as Total 
                                                              from (select platform, 
                                                                    `Version ID`, 
                                                                    count(`Version ID`) as Count 
                                                                    from dependencies 
                                                                    where `Project Name`='",ProjectNameLibraries,"' group by platform, `Version ID`) as a group by a.platform) b;",sep = ""))
  if (is.na(UpstreamDependencies)){
    UpstreamDependencies<-0
  }
  
  # Selecting Downstream Dependencies
  DependenciesCount <- dbGetQuery(connectlib, paste("select Name,`Dependent Projects Count` from projects where Name ='",ProjectNameLibraries,"';",sep = ""))
  DownstreamDependencies <- sum(DependenciesCount$`Dependent Projects Count`)
  
  
  
  ####Forming V-Index#####
  # Selecting First Order Downstream Dependencies
  FirstOrderList <- dbGetQuery(connectlib, paste("select distinct(`Project ID`)
                                                   from dependencies  
                                                   where `Dependency Name`='",ProjectNameLibraries,"' 
                                                   group by `Project Name`;",sep = ""))
  
  # Selecting Project ID from  First Order Downstream Dependencies 
  ProjectID<-FirstOrderList$`Project ID`
  
  # Forming Second Order data frame
  SecondOrderList <-data.frame(
    "ProjectID" = integer(),
    "DependentsCount" =integer(),
    stringsAsFactors=FALSE)
  
  # Extracting dependencies count for each first order dependency
  for (j in seq_along(ProjectID)){
     query<- dbGetQuery(connectlib, paste("select ID,`Dependent Projects Count` as DependentsCount  
                                                      from projects 
                                                      where ID =",ProjectID[j],";",sep = ""))
     if (is.na(query[1,1])){
       next
     } else {
       SecondOrderList[nrow(SecondOrderList)+1,] <-query
     }
       
  }

  # Sorting second order dependencies
  SecondOrderList <- SecondOrderList[order(-SecondOrderList$DependentsCount),]
  
  # Calculating V-Index
  Vindex<-0
  for (k in seq_along(SecondOrderList$ProjectID)){
    if (k<=SecondOrderList[k,2]){
      Vindex<- Vindex+1
    }
  }
  #####
  
  # Joining all the variables and forming correlation table
  correlation_table[nrow(correlation_table)+1,] <- c(ProjectName, 
                                                     ProjectAge, 
                                                     TotalWatchers, 
                                                     TotalCommitters, 
                                                     TotalCommits, 
                                                     TotalClosedIssues, 
                                                     PullRequestDuration,
                                                     PullRequestCommentDuration,
                                                     CommitsDuration,
                                                     IssueCommentsDuration, 
                                                     PullRequestAcceptedDuration, 
                                                     PullRequestRejectedDuration, 
                                                     AvgPullRequestComments, 
                                                     UpstreamDependencies, 
                                                     DownstreamDependencies,
                                                     Vindex)
}


# To save dataframe in CSV file 
setwd("G:/My Drive/Thesis")
write.csv(correlation_table, file = "rustcorrelationfinal.csv")

# Selecting Columns for correlation table
columns <- 3:17
correlation_table[columns] <- lapply(correlation_table[columns], as.numeric) 

correlation_table1<- correlation_table 
# Removing number and name of projects
correlation_table1<-subset(correlation_table1, select = -c(X, ProjectName ) )

# Summary of Correlation Table
summary(correlation_table1)

# Correlation Matrix
c <-cor(correlation_table1[columns], method = "pearson")
write.csv(c, file = "rustcorrelationoutput.csv")


# Correlation Plots
corrplot(c, type = "lower", order = "hclust",  
         tl.col = "black", tl.srt = 0.1,  is.corr=FALSE, method = "number",col = brewer.pal(n = 8, name = "RdBu"))

# Multiple Linear Regression Example V-Index vs Activity and Reward Metrics
fit <- lm(Vindex ~ ProjectAge+ 
            TotalWatchers+
            TotalCommitters+ 
            TotalCommits+ 
            TotalClosedIssues+
            PullRequestDuration+
            PullRequestCommentDuration+
            CommitsDuration+
            IssueCommentsDuration+
            PullRequestAcceptedDuration+
            PullRequestRejectedDuration+
            AvgPullRequestComments+
            UpstreamDependencies, 
            correlation_table1
          )
summary(fit) # show results
write.csv(fit, file = "rustregressionoutput.csv")

coefficients(fit)
plot(fit)
step <- stepAIC(fit, direction="both")
step$anova # display results

