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
  if (!require("patchwork"))
    install.packages("patchwork"); library(patchwork)
  
}

load_libraries()
#
# Recurring functions ====
rd_color <- "#19c1ce"
#
# Importing data ====
# download the edgelist
schoolfriends_edgelist <- onadata::schoolfriends_edgelist
schoolfriends_reported <- schoolfriends_edgelist |>
  dplyr::filter(type == 'reported')

# Creating a directed graph ====
schoolfriends_rp <- graph_from_data_frame(schoolfriends_reported,
                                          directed = T)

schoolfriends_rp

# Visualizing graph ====
set.seed(123)
ggraph(schoolfriends_rp, layout = 'fr') +
  geom_edge_link(color = 'grey',
                 arrow = arrow(length = unit(0.2, 'cm'))) +
  geom_node_point(size = 2, color = rd_color) +
  theme_void()

# Getting weak components using igraph::components() ====
#' *Weak components*:
#' We say 𝐺 is weakly connected if it would be connected when viewed as an
#' undirected graph.
schoolfriends_components <- components(schoolfriends_rp)
schoolfriends_components
# $csize = c(128, 3, 3)
# $no = 3

#' Existem 3 componentes, tendo um 128 pessoas e os outros dois apenas 3 pessoas
#
# Using components as vertices properties ====
V(schoolfriends_rp)$component <- schoolfriends_components$membership

# visualize
set.seed(123)
ggraph(schoolfriends_rp, layout = 'fr') +
  geom_edge_link(color = 'grey',
                 arrow = arrow(length = unit(0.2, 'cm'))) +
  geom_node_point(aes(color = as.factor(component)), size = 2) +
  labs(color = 'Component') +
  theme_void()

#

# Getting strong components using igraph::components() ====
#' *Strong components*:
#' We say that 𝐺 is strongly connected if a path exists from 𝑢 to 𝑣 for any
#' pair of vertices 𝑢 and 𝑣 in 𝐺.

schoolfriends_components <- components(schoolfriends_rp, mode = 'strong')
schoolfriends_components
# $csize = c(3, 1, 1, 3, 1, 117, 4, 1, 3)
# $no = 9

#' Existem 9 componentes fortemente associados, com tamanhos diferentes em cada
#
# Using components as vertices properties ====
V(schoolfriends_rp)$component <- schoolfriends_components$membership

# visualize
set.seed(123)
ggraph(schoolfriends_rp, layout = 'fr') +
  geom_edge_link(color = 'grey',
                 arrow = arrow(length = unit(0.2, 'cm'))) +
  geom_node_point(aes(color = as.factor(component)), size = 2) +
  labs(color = 'Component') +
  theme_void()

# Partitioning a graph ====
# get karate edgelist and create undirected graph
karate_edges <- read.csv("https://ona-book.org/data/karate.csv")
karate <- igraph::graph_from_data_frame(karate_edges, directed = FALSE)

# color John A and Mr Hi differently
V(karate)$leader <- ifelse(
  V(karate)$name %in% c("Mr Hi", "John A"), 1, 0
)

# visualize
set.seed(123)
ggraph(karate, layout = "fr") +
  geom_edge_link(color = "grey") +
  geom_node_point(aes(size = as.factor(leader)), color = rd_color,
                  show.legend = FALSE) +
  theme_void()

# Getting the minimal cut
#' *Minimal cut*
#' Lowest possible value of edges to eliminate in order to get at least 2 graphs
igraph::min_cut(karate, value.only = FALSE)

# Detecting communities using Louvain ====
communities <- cluster_louvain(karate)
communities

# Community sizes
sizes(communities)

# Assign as vertex property
V(karate)$community <- membership(communities)

# Visualizing graph
set.seed(123)
ggraph(karate, layout = "fr") +
  geom_edge_link(color = "grey") +
  geom_node_point(aes(size = as.factor(leader),
                      color = as.factor(community)),
                  show.legend = FALSE) +
  theme_void()

# Finding cliques ====
#' A clique is a subset of vertices in an undirected graph whose induced
#' subgraph is complete. That is, the induced subgraph has an edge density of 1.
#' This is best understood as the most intense possible type of community in an
#' undirected graph.
# Getting cliques
max_cliques(karate, min = 2)
max_cliques(karate, min = 3)
max_cliques(karate, min = 4)
max_cliques(karate, min = 5)

# Getting largest cliques
largest_cliques(karate)

# Getting size of largest clique
clique_num(karate)

# Detecting communities and cliques among Facebook friends ====
# get schoolfriends data
schoolfriends_edgelist <- read.csv(
  "https://ona-book.org/data/schoolfriends_edgelist.csv"
)
schoolfriends_vertices <- read.csv(
  "https://ona-book.org/data/schoolfriends_vertices.csv"
)

# facebook friendships only
schoolfriends_facebook <- schoolfriends_edgelist |>
  dplyr::filter(type == "facebook")

# create undirected graph
schoolfriends_fb <- igraph::graph_from_data_frame(
  d = schoolfriends_facebook,
  vertices = schoolfriends_vertices,
  directed = FALSE
)

# Graph is not connected!
is.connected(schoolfriends_fb)

# remove isolates
isolates <- which(degree(schoolfriends_fb) == 0)
schoolfriends_fb <- schoolfriends_fb |>
  delete.vertices(isolates)

# Graph is connected now!
is.connected(schoolfriends_fb)

# Getting largest cliques ====
cliques <- igraph::largest_cliques(schoolfriends_fb)

cliques

# 6 cliques of 14 people each

# Understanding clique 6
clique6 <- igraph::induced_subgraph(schoolfriends_fb,
                                    vids = cliques[[6]])
data.frame(
  id = V(clique6)$name,
  class = V(clique6)$class,
  gender = V(clique6)$gender
)

#' We see that this clique is distributed over four classes and is mostly male.
#
# create clique property
V(schoolfriends_fb)$clique6 <- ifelse(
  V(schoolfriends_fb) %in% cliques[[6]], 1, 0
)
# visualize
set.seed(123)
ggraph(schoolfriends_fb, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(size = 2, aes(color = as.factor(clique6)),
                  show.legend = FALSE) +
  theme_void()

# Using Louvain to detect communities ====
# get optimal louvain communities
communities <- igraph::cluster_louvain(schoolfriends_fb)

# assign community as a vertex property
V(schoolfriends_fb)$community <- membership(communities)

# how many communities?
length(unique(V(schoolfriends_fb)$community))

# modularity of louvain
modularity(schoolfriends_fb, V(schoolfriends_fb)$community)

# modularity of class structure
modularity(schoolfriends_fb,
           as.integer(as.factor(V(schoolfriends_fb)$class)))

# modularity of gender structure
modularity(schoolfriends_fb,
           as.integer(as.factor(V(schoolfriends_fb)$gender)))


#' The Louvain algorithm modularity outperformed both class and gender factors

# Visualizing two communities at the same time ====
# visualize louvain communities
set.seed(123)
g1 <- ggraph(schoolfriends_fb, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(size = 2, aes(color = as.factor(community)),
                  show.legend = FALSE) +
  theme_void()

# visualize ground truth class communities
set.seed(123)
g2 <- ggraph(schoolfriends_fb, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(size = 2, aes(color = class),
                  show.legend = FALSE) +
  theme_void()

# display side by side
g1 + g2

#' We can see that the Louvain community structure is a much better 
#' representation of dense friendship groups compared to the ground truth class
#' structure, indicating that Facebook friendships tend to span across those
#' class structures

# Detecting politically aligned communities on Twitter ====
# get edgelist and vertex data
ontariopol_edges <- read.csv(
  "https://ona-book.org/data/ontariopol_edgelist.csv"
)

ontariopol_vertices <- read.csv(
  "https://ona-book.org/data/ontariopol_vertices.csv"
)

# create undirected graph
ontariopol <- igraph::graph_from_data_frame(
  d = ontariopol_edges,
  vertices = ontariopol_vertices,
  directed = FALSE
)

ontariopol

# Is the graph connected?
is.connected(ontariopol) # YES!

# find optimal communities
communities <- igraph::cluster_louvain(ontariopol)

# assign as vertex properties
V(ontariopol)$community <- membership(communities)

# how many communities
length(unique(V(ontariopol)$community))

# Comparing modularity
# louvain modularity
modularity(ontariopol,
           membership = V(ontariopol)$community,
           weights = E(ontariopol)$weight)

# political party modularity
modularity(ontariopol,
           membership = as.integer(as.factor(V(ontariopol)$party)),
           weights = E(ontariopol)$weight)

# Modularity is very similar...

# visualize louvain communities
set.seed(123)
g1 <- ggraph(ontariopol, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(size = 2, aes(color = as.factor(community)),
                  show.legend = FALSE) +
  theme_void()

# visualize ground truth political party communities
set.seed(123)
g2 <- ggraph(ontariopol, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(size = 2, aes(color = party),
                  show.legend = FALSE) +
  theme_void()

# display side by side
g1 + g2

#' We see very similar community structures, indicating that the Louvain
#' algorithm has done a good job of identifying political party alignment from
#' the tweet activity of the politicians

# Detecting politically aligned cliques on Twitter ====
# get edgelist and vertex data
ontariopol_edges <- read.csv(
  "https://ona-book.org/data/ontariopol_edgelist.csv"
)

ontariopol_vertices <- read.csv(
  "https://ona-book.org/data/ontariopol_vertices.csv"
)

# create undirected graph
ontariopol <- igraph::graph_from_data_frame(
  d = ontariopol_edges,
  vertices = ontariopol_vertices,
  directed = FALSE
)

ontariopol

# Is the graph connected?
is.connected(ontariopol) # YES!

# get largest cliques
cliques <- igraph::largest_cliques(ontariopol)
cliques

# 24 cliques of size 48

# Visualizing clique 1!
# create clique property
V(ontariopol)$clique1 <- ifelse(
  V(ontariopol) %in% cliques[[1]], 1, 0
)

# visualize clique
set.seed(123)
g1 <- ggraph(ontariopol, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(size = 2, aes(color = as.factor(clique1)),
                  show.legend = FALSE) +
  theme_void()

# visualize ground truth political parties
set.seed(123)
g2 <- ggraph(ontariopol, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.5) +
  geom_node_point(size = 2, aes(color = party)) +
  labs(color = "Party (Right hand graph)") +
  theme_void()

g1 + g2

# Clique 1 is possibly a PCP clique
#