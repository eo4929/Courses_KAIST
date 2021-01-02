
library(igraph)
library(ggraph)

#1-1
#graph_car <- graph.data.frame(mtcars)
graph_car <- graph_from_data_frame(mtcars,directed = T)

plot(graph_car,edge.arrow.size=0.2,vertex.label.cex=0.7,vertex.size=15, vertex.color='ivory',edge.arrow.size=0.5,edge.color="deepskyblue")

#1-2
library(NbClust)
scaled_mtcars <- scale(mtcars) %>% as.data.frame()
ds <- dist(scaled_mtcars, method="euclidean")
hcst <- hclust(ds,method="complete")


plot(hcst, labels=rownames(scaled_mtcars), cex=0.8)
#install.packages('networkD3')

#1-3
library(networkD3)
radialNetwork( as.radialNetwork(hcst) )

#1-4
diagonalNetwork( as.radialNetwork(hcst) )

#1-5
dendroNetwork( hcst,textColour = c("red","magenta","blue","green")[cutree(hcst, 4)] )
