#########################

# W.L.
# Social Network Analysis
# 07/25/24

#########################

# Social Network Analysis
library(igraph)
g <- graph(c(1, 2, 2, 3, 3, 4, 4, 1),
           directed = F,
           n = 7)
plot(g,
     vertex.color = "purple",
     vertex.size = 40,
     edge.color = "black")

g1 <- graph(c("William", "Max", "Max", "Lorenzo", "Lorenzo", "William",
        "William", "Lorenzo", "Brian", "Lorenzo"),
        directed = T)

plot(g1,
     vertex.color = "purple",
     vertex.size = 40,
     edge.color = "black")

# Network Measures
degree(g1, mode = "all")
degree(g1, mode = "in")
degree(g1, mode = "out")

diameter(g1, directed = F, weights = NA)
edge_density(g1, loops = F)
ecount(g1)/(vcount(g1)*(vcount(g1)-1)) # Total of 5 edges, 4 vertices
reciprocity(g1)
closeness(g1, mode = "all", weights = NA)
betweenness(g1, directed = T, weights = NA)
edge_betweenness(g1, directed = T, weights = NA)

# Read data file
data = read.csv(file.choose(), header = T)
y <- data.frame(data$first, data$second)

# Create network
net <- graph.data.frame(y, directed = T)
V(net)
E(net)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

# Histogram of node degree
hist(V(net)$degree,
     col = "purple",
     main = "Histogram of Node Degree",
     ylab = "Frequency",
     xlab = "Degree of Vertices")

# Network diagram
set.seed(222)
plot(net,
     vertex.color = "purple",
     vertext.size = 2,
     edge.arrow.size = 0.1,
     vertext.label.cex = 0.8)

# Highlighting degree and layouts
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout = layout.fruchterman.reingold)

plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout = layout.graphopt)

plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)

# Hub and authorities
hubScore <- hub_score(net)$vector
authorityScore <- authority.score(net)$vector
par(mfrow = c(1, 2))
set.seed(123)
plot(net,
     vertex.size = hubScore * 30,
     main = "Hubs",
     vertex.color = rainbow(52),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)

plot(net,
     vertex.size = authorityScore * 30,
     main = "Authorities",
     vertex.color = rainbow(52),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)


# Community detection
net <- graph.data.frame(y, directed = F)
community_net <- cluster_edge_betweenness(net)
plot(community_net,
     net,
     vertex.size = 10,
     vertex.label.cex = 0.8)
