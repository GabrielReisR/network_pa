# Loading libraries ====
load_libraries <- function(){
  if (!require("dplyr"))
    install.packages("dplyr"); library(dplyr)
  if (!require("ggraph"))
    install.packages("ggraph"); library(ggraph)
  if (!require("igraph")) # https://cloud.r-project.org/web/packages/igraph/igraph.pdf
    install.packages("igraph"); library(igraph)
  if (!require("onadata"))
    install.packages("onadata"); library(onadata)
}

load_libraries()
#
# Recurring functions ====
rd_color <- "#19c1ce"
#
# Importing data ====
# download the edgelist
g14_edgelist <- read.csv("https://ona-book.org/data/g14_edgelist.csv")
# view head
head(g14_edgelist)

# Creating a simple connected graph ====
g14_graph <- graph_from_data_frame(g14_edgelist,
                                   directed = F)

g14_graph

# Visualizing graph ====
set.seed(123)
ggraph(g14_graph, layout = 'fr') +
  geom_edge_link(aes(label = weight)) +
  geom_node_label(aes(label = name), fill = rd_color) +
  theme_void()
  
# Paths ====
all_simple_paths(g14_graph, from = "9", to = "4")

shortest_9to4 <- all_shortest_paths(g14_graph,
                                    from = "9",
                                    to = "4")
shortest_9to4$res

shortest_9to4_unweighted <- all_shortest_paths(g14_graph,
                                               from = "9",
                                               to = "4",
                                               weights = NA)
shortest_9to4_unweighted$res

# Distances ====
distances(g14_graph)

# We can specify distances between specific vertices
distances(g14_graph,
          v = "9", to = "4",
          weights = NA, # and even specify if we want it weighted
          algorithm = "bellman-ford") # and the algorithm used

# Mean distances
mean(distances(g14_graph)[distances(g14_graph) != 0]) # weighted
mean_distance(g14_graph) # weighted
mean_distance(g14_graph, weights = NA) # unweighted

# Diameter ====
max(distances(g14_graph)[distances(g14_graph) != 0]) # weighted
diameter(g14_graph) # weighted

farthest_vertices(g14_graph) # weighted
farthest_vertices(g14_graph, weights = NA) # unweighted

get_diameter(g14_graph) # weighted
get_diameter(g14_graph, weights = NA) # unweighted

# Density ====
#' Proportion of observed edges when considering all possible edges
edge_density(g14_graph)

#' 19.78% of all possible connections were made

# Example using workfrance graph ====
workfrance_edgelist <- onadata::workfrance_edgelist
workfrance_vertices <- onadata::workfrance_vertices

work_graph <- graph_from_data_frame(workfrance_edgelist,
                                    directed = F,
                                    vertices = workfrance_vertices)

# Putting weight
E(work_graph)$weight <- E(work_graph)$mins

work_graph

ggraph(work_graph, layout = 'fr') +
  geom_edge_link(aes(alpha = weight), show.legend = F) +
  geom_node_label(aes(label = name, fill = dept), size = 3) +
  scale_fill_discrete('Department') +
  theme_void()

# Graph statistics ====
is.connected(work_graph) # do all nodes have at least one connection/edge?
#' It seems that there are no disconnected nodes

edge_density(work_graph)
#' Edge density is only 4.2%, indicating that less than 5% of the possible
#' connections were created

mean_distance(work_graph, weights = NA)
#' The mean distance connecting two nodes is roughly three connections

diameter(work_graph, weights = NA)
#' The biggest separation between two nodes is only 6 connections

farthest_vertices(work_graph, weights = NA) # unweighted
#' The farthest vertices from eachother are nodes 502 and 257

# Connecting vertices ====
#' Let's say we want to connect vertices 3 and 55
V(workfrance)$dept[V(workfrance)$name %in% c("3", "55")]

#' One is from department DMI and the other from SSI

#' Let's look at the distance between the two nodes
distances(workfrance, v = "3", to = "55", weights = NA) 

#' Just two connections!

#' What is the shortest unweighted path between the two nodes?
all_shortest_paths(workfrance, from = "3", to = "55", weight = NA)$res

#' The shortest unweighted path goes from node 3 to node 447 and then 55

#' What is the shortest weighted path between the two nodes?
all_shortest_paths(workfrance, from = "3", to = "55")$res

#' The shortest weighted path also goes from node 3 to 447 and then 55

#'


#
# Connecting other vertices ====
#' Let's say we're interested in connecting nodes 3 and 290
all_shortest_paths(work_graph, from = "3", to = "290",
                   weight = NA)$res

#' We have two possible connections: nodes 859 and 694

# Creating subgraph to analyze these possible connections
subgraph <- induced_subgraph(work_graph,
                             vids = c("3", "290", "694", "859"))

ggraph(subgraph, layout = 'fr') +
  geom_edge_link(aes(alpha = weight, label = weight), show.legend = F) +
  geom_node_label(aes(label = name, fill = dept), size = 3) +
  scale_fill_discrete('Department') +
  theme_void()

#' It seems that node 3 is more connected to node 859 than to node 694, which
#' denotes that this communication would be easier
#' Note that here a higher weight means easier communication between nodes, not
#' the opposite

# Finding distant colleagues ====
# Capturing number of neighbors each node has
# create vectors to capture name and no of neighbors
v_name <- c()
n_neighbors <- c()
# capture name and no of neighbors for every vertex
for (v in V(work_graph)$name) {
  v_name <- append(v_name, v)
  n_neighbors <- append(n_neighbors,
                        length(neighbors(work_graph, v)))
}

# Find the max
v_name[which.max(n_neighbors)]
#' Node 603 has the most number of neighbors

# How many neighbors does node 603 have?
n_neighbors[which.max(n_neighbors)]
#' Node 603 has 28 neighbors

#' Using some programming and graph properties, we would be able to simulate
#' small communities in which we could connect, for example, nodes that aren't
#' very connected to eachother, which would enhance both the quantity of
#' connections (making the average distance smaller and the edge density higher)
#' but also the average strength of connections in our network
