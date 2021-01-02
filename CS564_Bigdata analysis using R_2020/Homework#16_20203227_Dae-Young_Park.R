
library(igraph)

#1-1
N = matrix(c(0,1,1,1,0, 1,0,0,1,1, 0,1,0,1,0,
             0,1,1,0,0, 0,1,1,0,0),
           nrow=5, byrow=TRUE)
lab = LETTERS[1:5]
dimnames(N) <- list(lab, lab)
gn <- graph.adjacency(N)
plot(gn,vertex.color=2:6,vertex.size=15,
     edge.color="cornsilk4",edge.arrow.size=0.5)

#1-2
circle <- layout.circle(gn)
grid <- layout.grid(gn)
kam <- layout.kamada.kawai(gn)


plot(gn,layout= circle ,vertex.color=2:6,vertex.size=15,
     edge.color="cornsilk4",edge.arrow.size=0.5)

plot(gn,layout= grid ,vertex.color=2:6,vertex.size=15,
     edge.color="cornsilk4",edge.arrow.size=0.5)

plot(gn,layout= kam ,vertex.color=2:6,vertex.size=15,
     edge.color="cornsilk4",edge.arrow.size=0.5)

#1-3
edge_density(gn, loops=F) # this means the ratio of # of edges and # of all possible edges

#1-4
transitivity(gn, type="global") # this means the probability that the direct neighbors of a vertex are connected

