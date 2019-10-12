library(tidyverse)
library(caret)
library(igraph)
library(readr)
library(stringr)


tweets <- read_csv("tweets.csv")
users <- read_csv("users.csv")

data <- tweets

# Grep RT's 
rt <- grep("^rt @[a-z0-9_]{1,15}", tolower(tweets$text), perl=T, value=T)

# Select RT senders 
rt.send <- tolower(as.character(tweets$user_key[grep("^rt @[a-z0-9_]{1,15}", tolower(tweets$text), perl=T)]))
rt.rec <- tolower(regmatches(rt, regexpr("@(?U).*:", rt)))
# Remove @ and :
rt.rec <- (gsub(":", "", rt.rec))
rt.rec <- (gsub("@", "", rt.rec)) 
View(rt.rec)

# Missing Values as NA
rt.send[rt.send==""] <- "<NA>"
rt.rec[rt.rec==""] <- "<NA>"

# Create single df with all users
users.all <- unique(as.data.frame(c(rt.send, rt.rec))) 
#renaming the handle names variable
users.all <- users.all %>% rename(user = "c(rt.send, rt.rec)")


tweets <- tweets %>% rename(user = user_key) #renaming user name variable
tweets.user <- tweets %>% select(user) #selecting only the handles from the data
trolls <- users %>% select(screen_name)
trolls <- trolls %>% rename(user = screen_name)
                            
trolls <- rbind(trolls, tweets.user)

trolls.u <- unique(trolls) #removing duplicates
trolls.u$troll <- "troll" #assigning all of these users a trolls
### matching trolls with the complete set of handle names in the retweet network

nodes <- right_join(trolls.u, users.all)
nodes <- replace(nodes, is.na(nodes), "non-troll") 

# Graph distribution of Trolls and Non-Trolls in the dataset
node.distribution <-  nodes %>% ggplot(aes(x=troll)) + geom_bar(aes(col=troll), alpha=0.5) + 
                        theme(legend.position="none") + scale_color_manual(values=c("#119111", "#ff0505")) + 
                        labs(title="Count of Trolls vs Non-Trolls") + 
                        theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#808484"), axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"), axis.title=element_text(colour="white"))


#############################
# Verify accuracy of counts #
#############################
plot(node.distribution)



#############################
# Network graph creation    #
#############################


rt.df <- data.frame(rt.send, rt.rec)
### creating the retweetnetwork based on the sender-receiver df and the node attributes (troll/non-troll)
rt.g <- graph.data.frame(rt.df, directed=T, vertices = nodes)


bipartite.mapping(rt.g)
V(rt.g)$type <- bipartite_mapping(rt.g)$type
plot(rt.g)


