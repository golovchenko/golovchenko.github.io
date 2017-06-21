### Setting working directory
setwd("D:/movetowork")

### loading vkR
library("vkR")

### Authentication 
vkOAuth(client_id = INSERTYOURCLIEND, 'groups, walls, friends')
setAccessToken(access_token = 'INSERTYOURACESSTOKEN')

### creating an integer object with user id's from following group: https://vk.com/tipical_avdeevka   https://vk.com/public104111409
avd <- getGroupsMembersExecute(group_id = '49726867')
toretsk <- getGroupsMembersExecute(group_id = '104111409')
avdtor <-c(avd, toretsk)  # combining id's from the Toretsk and Avdeevka group


### downloading additional info about the users
info<- getUsersExecute(users_ids = avdtor, fields = "sex,bdate,city,country,deactivated")

### puting relevant variables into objects
id <- info$id 
sex <- info$sex
bdate <- info$bdate
city.id <- info$city$id
city.name <- info$city$title

### combining the objects into a data frame
avdtor.df <- data.frame(id, sex, bdate, city.id, city.name)

### selecting only users who publicaly state that they live in Avdeevka
library("dplyr")
avd.memb <- filter(avdtor.df, city.id == 4920) 

### selecting only users who publicaly state that they live in Avdeevka
tor.memb <- filter(avdtor.df, city.id == 2566) 

### Selecting users who either live in Avdeevka or Toretsk
avdtor.memb <- rbind(avd.memb, tor.memb)

### removing duplicates
avdtor.memb <- distinct(avdtor.memb, id, .keep_all = T)

### creating a friendship network through an adjacency matrix for the users from Avdeevka and Toretsk
#adj <- getArbitraryNetwork(avdtor.memb$id, format = "adjmatrix")
edge <- getArbitraryNetwork(avdtor.memb$id, format = "edgelist")






########################## Network Analysis with Igraph#######################
library(igraph)

g <- graph.data.frame(edge, directed=F, vertices = avdtor.memb)

### subsetting the graph
exclude <- V(g)[degree(g)<25]
g1 <- delete.vertices(g, exclude)

### calculationg degree
deg <- degree(g1)
#eigen<- eigen_centrality(g1)
between <- betweenness(g1, normalized =T)

### Adding colours
colors <- V(g1)$sex
colors1 <- V(g1)$city.id



### vizualizing the graph
plot.igraph(g1,layout= layout.fruchterman.reingold(g1),
            edge.color="grey", vertex.color = colors1, edge.curved= .2, vertex.label = NA, vertex.frame.color="#ffffff",
            vertex.size = deg/50)


