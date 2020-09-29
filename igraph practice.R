#Load packages#
library(igraphdata)
library(igraph)
library(threejs)
library(tidyverse)
library(plotly)
library(network)

#Set working directory#
setwd("C:/Users/User/Desktop/Software Cheats/R/igraph")

#Load data set#
data("foodwebs")
data("USairports")


##FoodWebs - igraph##
head(foodwebsMI)
foodwebsMI <- foodwebs$Michigan
foodwebsMI <- as.matrix(foodwebsMI)
is.matrix(foodwebsMI)

is.directed(foodwebsMI)

is.data.frame(foodwebsMI)

plot(foodwebsMI, directed = TRUE)

graphjs(foodwebsMI, vertex.size = 1)



#Airports - igraphing#
head(USairports)

airport.mat <- as.matrix(USairports)
is.matrix(airport.mat)

head(airport.mat)

gsize(airport.mat) #23473 edges#
gorder(airport.mat) #755 vertices#

is.directed(airport.mat) #TRUE#

graphjs(airport.mat)

#convert igraph object to a data frame#
Airports <- as_data_frame(USairports)
names(Airports)

table(Airports$Carrier)
jetblue <- Airports %>%
  filter(Carrier == "JetBlue Airways")

head(jetblue)

is.data.frame(jetblue)

g <- graph_from_data_frame(jetblue, directed = FALSE)

g.ec <- eigen_centrality(g)
which.max(g.ec$vector)




plot(g,
     vertex.label.color = "black", 
     vertex.label.cex = 0.6,
     vertex.size = 25*(g.ec$vector),
     edge.color = 'gray88',
     main = "JetBlue Network")


ec <- as.numeric(g.ec$vector)
?graphjs
graphjs(g, vertex.size = 1)
graphjs(g,
        vertex.color = "blue",
        vertex.label.cex = 0.6,
        vertex.label = TRUE,
        edge.color = "white",
        main = "JetBlue Network",
        vertex.size = 5*ec,
        bg = "black",
        brush = TRUE,
        highlight="green",
        lowlight="gray88",
        use.orbitcontrols=TRUE,
        click = list())



table(Airports$Carrier)
homer <- Airports %>%
  filter(Carrier == "Homer Air")


head(homer)

is.data.frame(homer)

h <- graph_from_data_frame(homer, directed = FALSE)

h.ec <- eigen_centrality(h)
which.max(h.ec$vector)




plot(h)

graphjs(h,
        main = "Homer Air Network",
        vertex.label = TRUE)

homer2 <- homer[,c(-1)]

head(homer2)

is.data.frame(homer2)

h1 <- graph_from_data_frame(homer, directed = FALSE)

h1.ec <- eigen_centrality(h1)
which.max(h1.ec$vector)

plot(h1)

graphjs(h1,
        main = "Homer Air Network",
        vertex.label = TRUE)

gd <- edge_density(g)

diameter(g, directed = FALSE)

g.apl <- mean_distance(g, directed = FALSE)
g.apl
#Random Set#
g.random <- erdos.renyi.game(n = gorder(g), p.or.m = gd, type = "gnp")
g.random

plot(g.random)
edge_density(g.random)
mean_distance(g.random, directed = FALSE)

gl <- vector('list', 1000)

for(i in 1:1000){
  gl[[i]] <- erdos.renyi.game(n = gorder(g), p.or.m = gd, type = "gnp")
}

gl.apls <- unlist(lapply(gl, mean_distance, directed = FALSE))

hist(gl.apls, xlim = range(c(1.5, 6)))
abline(v = g.apl, col = "red", lty = 3, lwd = 2)
mean(gl.apls < g.apl)
matrix(triangles(g), nrow = 3)
count_triangles(g, vids="JFK")

# Calculate  the global transitivity of the network.
g.tr <- transitivity(g)
g.tr

# Calculate the local transitivity for vertex BUBBA.
transitivity(g, vids="JFK", type = "local")

# Calculate average transitivity of 1000 random graphs
gl.tr <- lapply(gl, transitivity)
gl.trs <- unlist(gl.tr)

# Get summary statistics of transitivity scores
summary(gl.trs)

# Calculate the proportion of graphs with a transitivity score higher than Forrest Gump's network
mean(gl.trs > gl.tr)

# Identify the largest cliques in the network
largest_cliques(g)

# Determine all maximal cliques in the network and assign to object 'clq'
clq <- max_cliques(g)

# Calculate the size of each maximal clique.
table(unlist(lapply(clq, length)))

# Assign largest cliques output to object 'lc'
lc <- largest_cliques(g)

# Create two new undirected subgraphs, each containing only the vertices of each largest clique.
gs1 <- as.undirected(subgraph(g, lc[[1]]))
gs2 <- as.undirected(subgraph(g, lc[[2]]))


# Plot the two largest cliques side-by-side

par(mfrow=c(1,2)) # To plot two plots side-by-side

plot(gs1,
     vertex.label.color = "black", 
     vertex.label.cex = 0.9,
     vertex.size = 0,
     edge.color = 'gray28',
     main = "Largest Clique 1",
     layout = layout.circle(gs1)
)

plot(gs2,
     vertex.label.color = "black", 
     vertex.label.cex = 0.9,
     vertex.size = 0,
     edge.color = 'gray28',
     main = "Largest Clique 2",
     layout = layout.circle(gs2)
)



