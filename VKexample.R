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

### questions? :)

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









plot.igraph(g1,layout= layout.fruchterman.reingold(g1),
            edge.color="grey", vertex.color = colors1 , edge.curved= .2, vertex.label = NA,
            vertex.size = deg/50)




#### exporting to Gephi 

write.graph(g1, file="example.graphml", format="graphml")




V(g1)$color <-V(g1)$city.id


install.packages("RColorBrewer")
library(RColorBrewer)

colrs <- c("tomato", "gold")
V(g1)$color <- colrs[V(g1)$city.id]

V(g1)$color <- ifelse(city.id[V(g1)], "blue", "red")


plot.igraph(g1,layout= layout.fruchterman.reingold(g1),
            edge.color="orange", vertex.color =V(g1)$color, edge.curved= .2, vertex.label = NA, vertex.frame.color="#ffffff",
            vertex.size = deg/50)


colrs <- c("blue", "red")
colors2 <- colrs[V(g1)$city.id]

V(g1)$color <- colrs[V(g1)$city.name]
color <- colrs[V(g1)$city.name]

V(g1)$color <- 
  
V(g1)$color<-V(g1)$city.name == c('Avdeevka' 'blue', 'red')
#V(g1)$color<-ifelse(V(bsk.network)$name=='CA', 'blue', 'red')

plot.igraph(g1,layout= layout.fruchterman.reingold(g1),
            edge.color="orange", edge.curved= .2, vertex.label = NA, vertex.frame.color="#ffffff",
            vertex.size = deg/10)

# Generate colors base on city:

### plotting the graph
rm(city.name)
plot.igraph(g1,layout= layout.fruchterman.reingold(g1),
            edge.color="orange", edge.curved= .2, vertex.label = NA, vertex.frame.color="#ffffff",
            vertex.size = deg/10)

plot()



exclude<-V(bsk.network)[degree(bsk.network)<3] #identify those vertices part of less than three edges
bsk.network<-delete.vertices(bsk.network, bad.vs) #exclude them from the graph





q















### calculation degree centraliy
deg <- degree(g, mode="all")
V(g)$size <- deg/10#dividing degree by 10 to reduce node size in the final graph and adding degree as node attribute

delete <- V(g)$size < 100
delete

class(deg)
View(deg)

###selecting graph layout algorithm
layout <- layout.fruchterman.reingold(g)

### plotting the friendship network
plot(g, vertex.label=NA, vertex.size = V(g)$size, layout = layout)


## plotting the networing
plot.igraph(g,layout=layout.fruchterman.reingold,
            edge.curved= .2, vertex.label = NA,
            edge.color="orange",
            vertex.color="orange", vertex.frame.color="#ffffff")

plot.igraph(g,layout= layout.auto(g),
            edge.curved= .2, vertex.label = NA,
            edge.color="orange",
            vertex.color="orange", vertex.size =1, vertex.frame.color="#ffffff")

layout.auto()



V(g)
## plotting the networing
plot.igraph(net,layout=layout.fruchterman.reingold, edge.width=E(net)$weight/800,
            edge.curved= .2, vertex.label.color="black",
            edge.color="orange", edge.arrow.size= 0.8,
            vertex.color="orange", vertex.frame.color="#ffffff")


