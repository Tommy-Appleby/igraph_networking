# igraph_networking
#Social Network Analysis of Russian Troll Tweets

df1 <- data.frame(x = seq(2, 8, by = 2),
                  y = seq(30, 15, by = -5))
df2 <- data.frame(x = seq(2, 8, by = 2),
                  y = seq(12, 24, by = 4))

Lines <- ggplot(df1, aes(x, log(y))) + 
         geom_line() +
         geom_line(data = df2, color = "red") # re-define data and overwrite top layer inheritance

plot(Lines)


Lines.alt <- ggplot() + 
              geom_line(data = df2, aes(x, log(y)), color = "red") +
              geom_line(data = df1, aes(x, log(y)), color = "blue")

plot(Lines.alt)

library(gridExtra)
grid.arrange(Lines,Lines.alt , ncol=3, nrow=1) 

#0573j^2HR405r8gD

#mine text for common words:
https://kshaffer.github.io/2017/02/mining-twitter-data-tidy-text-tags/
https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/text-mine-colorado-flood-tweets-science-r/
  
#map where they are coming from
  https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/map-tweet-locations-over-time-r/
  
  
#Find most common web links from a hashtag
  https://pushpullfork.com/mining-twitter-data-tidy-text-tags/
  

#python: entity extraction from russian troll tweets
  
  https://www.lyonwj.com/2017/11/15/entity-extraction-russian-troll-tweets-neo4j/
  
  
library(stringr)
library(tidyverse)


theString <- '@foobar Foobar! and @foo (@bar) but not foo@bar.com'

str_extract_all(string=theString,pattern='(?:^|(?:[^-a-zA-Z0-9_]))@([A-Za-z]+[A-Za-z0-9_]+)')


theString <- '@foobar Foobar! and @fo_o (@bar) but not foo@bar.com'
theString1 <- unlist(strsplit(theString, " "))
regex1 <- "(^|[^@\\w])@(\\w{1,15})\\b" # get strings with @
regex2 <- "[^[:alnum:]@_]"             # remove all punctuation except _ and @
users <- gsub(regex2, "", theString1[grep(regex1, theString1, perl = T)])

users

users.df <- as.data.frame(users)


View(foobar)
thestring2 <- foobar
View(thestring2)
theString3 <- unlist(strsplit(theString2[c(1)], " "))
View(theString3)
users.df.pivot <- unlist(lapply((gsub(regex2, "", theString3[grep(regex1, theString3, perl = T)])), paste, collapse=","))

view(users.df.pivot)

thestring3df <- as.data.frame(theString3)
View(thestring3df)

thestringdf <- as.data.frame(theString)

thestring.atme <- data.frame(To =sapply(str_extract_all(thestring2, "\\@S+"), paste, collapse=","))

#this will parse them into a column... not sure how it would handle multiple rows
thestring.atme <- data.frame(To =sapply(gsub(regex2, "", theString2$V1[grep(regex1, theString2$V1, perl = T)]), paste, collapse=","))



#selecting only the retweets
rts <- grep("^rt @[a-z0-9_]{1,15}", tolower(tweets$text), perl=T, value=T)
# extracting handle names for the senders (those who retweet)
rt.sender <- tolower(as.character(tweets$user_key[grep("^rt @[a-z0-9_]{1,15}", tolower(tweets$text), perl=T)]))
# extracting handle names for the recievers (those who are being retweeted)
rt.receiver<- tolower(regmatches(rts, regexpr("@(?U).*:", rts)))
rt.receiver <- (gsub(":", "", rt.receiver)) #removing ":"
rt.receiver <- (gsub("@", "", rt.receiver)) #removing "@"
### Registering empty entries as missing
rt.sender[rt.sender==""] <- "<NA>"
rt.receiver[rt.receiver==""] <- "<NA>"












####
#
# http://golovchenko.github.io/tutorials/snatrolls.html
#
###
users <- read_csv("~/CEE/russian-troll-tweets/users.csv")
tweets <- read_csv("~/CEE/russian-troll-tweets/tweets.csv")

#selecting only the retweets
rts <- grep("^rt @[a-z0-9_]{1,15}", tolower(tweets$text), perl=T, value=T)
# extracting handle names for the senders (those who retweet)
rt.sender <- tolower(as.character(tweets$user_key[grep("^rt @[a-z0-9_]{1,15}", tolower(tweets$text), perl=T)]))
# extracting handle names for the recievers (those who are being retweeted)
rt.receiver<- tolower(regmatches(rts, regexpr("@(?U).*:", rts)))
rt.receiver <- (gsub(":", "", rt.receiver)) #removing ":"
rt.receiver <- (gsub("@", "", rt.receiver)) #removing "@"
### Registering empty entries as missing
rt.sender[rt.sender==""] <- "<NA>"
rt.receiver[rt.receiver==""] <- "<NA>"

#storing reciever and sender handle names in one dataframe and removing duplicates
handle.all <- unique(as.data.frame(c(rt.sender, rt.receiver))) 
#renaming the handle names variable
handle.all <- handle.all %>% rename(handle = "c(rt.sender, rt.receiver)")

# https://www.r-bloggers.com/generating-graphs-of-retweets-and-messages-on-twitter-using-r-and-gephi/
# importing handle names from the official list release in congress
#trolls_official <-  read.csv("http://golovchenko.github.io/data/trollhandles.txt", stringsAsFactors = F)
# merging the complete list of official troll handle names with the ones in NBC data
data <- tweets
trolls_official <- userofficial
tweets <- tweets %>% rename(handle = user_key) #renaming handle name variable
handles <- tweets %>% select(handle) #selecting only the handles from the data

## Rbind with unmatching number of columns
## https://www.r-bloggers.com/combining-dataframes-when-the-columns-dont-match/
# rbind.match.columns <- function(input1, input2) {
#   n.input1 <- ncol(input1)
#   n.input2 <- ncol(input2)
# 
#   if (n.input2 < n.input1) {
#     TF.names <- which(names(input2) %in% names(input1))
#     column.names <- names(input2[, TF.names])
#   } else {
#     TF.names <- which(names(input1) %in% names(input2))
#     column.names <- names(input1[, TF.names])
#   }
# 
#   return(rbind(input1[, column.names], input2[, column.names]))
# }
# #rbind.match.columns(database.one, database.two)
# handlesfucntion <- rbind.match.columns(trolls_official, handles)


#this doesn't need to be done? 
handles <- rbind(trolls_official, handles)

#this is an alternative but no idea what its really doing, cause its not binding the columns?
#
#handlebind <- dplyr::bind_rows(trolls_official, handles)

handles.u <- unique(handles) #removing duplicates
handles.u$troll <- "troll" #assigning all of these users a trolls
### matching trolls with the complete set of handle names in the retweet network
nodes <- right_join(handles.u, handle.all)
nodes <- replace(nodes, is.na(nodes), "non-troll") # now we have a variable indicating wether a user is a troll

library(igraph)
### Creating a data frame from the sender-receiver objects
rts.df <- data.frame(rt.sender, rt.receiver)
### creating the retweetnetwork based on the sender-receiver df and the node attributes (troll/non-troll)
rts.g <- graph.data.frame(rts.df, directed=T, vertices = nodes)
### removing self-ties
rts.g <-simplify(rts.g, remove.loops = T, remove.multiple = F)
### We then use the edge list to create a network object in igraph, which is essentially our retweet network.
### creating the retweetnetwork based on the sender-receiver df and the node attributes (troll/non-troll)
rts.g <- graph.data.frame(rts.df, directed=T, vertices = nodes)
### removing self-ties
rts.g <-simplify(rts.g, remove.loops = T, remove.multiple = F)

# removing multiple edges between users
g <- simplify(rts.g, remove.multiple = T, remove.loops = T)
# creating a data frame with weighted and unweighted degree centrality for each profile
df <- data.frame(name =V(g)$name,
                 troll= V(g)$troll,indegree=degree(g,mode='in'),
                 indegree_weighted = degree(rts.g, mode ="in"),
                 outdegree=degree(g,mode='out'),
                 outdegree_weighted = degree(rts.g, mode = "out"))
#ranking users by indegree
rank.indegree <- df %>% select(name, troll, indegree,
                               indegree_weighted) %>% arrange(-indegree)

#ranking users b weigted indegree n users * n retweets
rank.indegree.w <- df %>% select(name, troll, indegree,
                                 indegree_weighted) %>% arrange(-indegree_weighted)

library(knitr)
kable(rank.indegree[1:10,], caption = "Top 10 profiles ranked by indegree")


#selecting nodes to exclude
exclude <- V(rts.g)[troll == "non-troll"]
#excluding the nodes
g.troll <- delete.vertices(rts.g, exclude)

### vizualizing the graph
par(bg ="grey10")
plot.igraph(g.troll,layout= layout.fruchterman.reingold(g.troll),
            edge.color="grey",
            edge.curved= .2, vertex.label = NA, vertex.frame.color="#ffffff",
            vertex.size = 2, edge.size = 0.01, edge.arrow.size = 0.01)
