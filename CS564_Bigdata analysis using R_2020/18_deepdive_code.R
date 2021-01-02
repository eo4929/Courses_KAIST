install.packages('igraph')
library(igraph)

install.packages('ggraph') # Create a ggraph plot
library(ggraph)


nodes <- read.csv("Dataset1-nodes.csv", header=T, as.is=T)
edges <- read.csv("Dataset2-edges.csv", header=T, as.is=T)

head(nodes)
head(edges)
nrow(nodes); length(unique(nodes$id))
nrow(edges); nrow(unique(edges[,c("from", "to")]))


#userGraph <- graph.data.frame()
userGraph <- graph_from_data_frame(edges,directed = T, vertices = nodes)

V(userGraph)[types_food == 'Korean cuisine']$color <- 'gold'
V(userGraph)[types_food == 'Western cuisine']$color <- 'green'
V(userGraph)[types_food == 'Chinese cuisine']$color <- 'tomato'
V(userGraph)$size <- V(userGraph)$Preference*0.5
V(userGraph)$frame.color <- "white"

E(userGraph)$width <- 0.2 * E(userGraph)$liking
E(userGraph)[type_relationship == 'official']$color <- 'cyan'
E(userGraph)[type_relationship == 'private']$color <- 'magenta'
E(userGraph)$arrow.size <- 0.85
#E(userGraph)$edge.color <- ifelse( E(userGraph)$type_relationship == "official", "ivory2", "grey79")
#E(userGraph)$edge.color <- "gray80"
#edge_attr(userGraph)

#kam <- layout.kamada.kawai(userGraph)
#plot(userGraph, layout=kam, vertex.label= V(userGraph)$food)
#legend(-1.7,-1, title="users liking in food preference network", legend = c("Korean cuisine", "Western cuisine", "chinese cuisine"), fill = c('gold','green','tomato'))

ggraph(userGraph) + 
  geom_edge_link(aes(colour = type_relationship, width=liking), arrow = arrow(type = "closed", length = unit(3.5, 'mm'), ends="last")) + 
  scale_edge_width(range = c(0.3, 2.5)) + 
  geom_node_point(aes(color = types_food, size = Preference, alpha= Preference)) + 
  geom_node_text(aes(label = food), repel=TRUE) + 
  theme_graph() + 
  labs(title = 'Users Network', 
       subtitle = 'with food preference and social relationship')

ggraph(userGraph) + 
  geom_edge_link(aes(colour = type_relationship, width=liking), arrow = arrow(type = "closed", length = unit(3.5, 'mm'), ends="last")) + 
  scale_edge_width(range = c(0.3, 2.5)) + 
  geom_node_point(aes(shape = types_food, size = Preference, alpha= Preference)) + 
  geom_node_text(aes(label = food), repel=TRUE) + 
  facet_edges(~type_relationship) + 
  theme_graph() + 
  labs(title = 'Users Network2', 
       subtitle = 'with food preference and social relationship')

# Density
edge_density(userGraph, loops=F)
ecount(userGraph)/(vcount(userGraph)*(vcount(userGraph)-1)) # the same result in case of directed graph

#Reciprocity
reciprocity(userGraph)

#Transitivity
transitivity(userGraph, type="global")
transitivity(userGraph, type="local")

#Diameter
diameter(userGraph, directed=F)
diameter(userGraph, directed=T)
diam_undirectedGraph <- get_diameter(userGraph, directed=F)
diam_undirectedGraph
diam_directedGraph <- get_diameter(userGraph, directed=T)
diam_directedGraph

vcol <- rep("gray30", vcount(userGraph))
vcol[diam_undirectedGraph] <- "gold"
ecol <- rep("gray80", ecount(userGraph))
ecol[E(userGraph, path=diam_undirectedGraph)] <- "yellow"
# E(net, path=diam) finds edges along a path
plot(userGraph, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)

#Node degrees
deg <- degree(userGraph, mode="all")
hist(deg, breaks=1:vcount(userGraph)-1, main="Histogram of node degree")

#Degree distribution
deg.dist <- degree_distribution(userGraph, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="gold",
      xlab="Degree", ylab="Cumulative Frequency")


# Centrality(a view of vertices) and centralization(a view of graph).
degree(userGraph)
centr_degree(userGraph, mode="in", normalized=T)

closeness(userGraph, mode="all", weights=NA)
centr_clo(userGraph, mode="all", normalized=T)

eigen_centrality(userGraph, directed=T, weights=NA)
centr_eigen(userGraph, directed=T, normalized=T)

betweenness(userGraph, directed=T, weights=NA)
edge_betweenness(userGraph, directed=T, weights=NA)
centr_betw(userGraph, directed=T, normalized=T)
