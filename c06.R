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
  if (!require("purrr"))
    install.packages("purrr"); library(purrr)
}

load_libraries()
#
# Recurring functions ====
rd_color <- "#19c1ce"
#
# Importing data ====
# download the edgelist
g14_edgelist <- read.csv("https://ona-book.org/data/g14_edgelist.csv")
g14_unweighted <- g14_edgelist |>
  dplyr::select(-weight)

# Creating a simple connected graph ====
g14_graph <- graph_from_data_frame(g14_unweighted,
                                   directed = F)

g14_graph

# Visualizing graph ====
set.seed(123)
ggraph(g14_graph, layout = 'fr') +
  geom_edge_link() +
  geom_node_label(aes(label = name), fill = rd_color) +
  theme_void()

# Calculate degree centrality ====
igraph::degree(g14_graph) |>
  sort(decreasing = T)

# Calculate ego and ego size ====
igraph::ego(g14_graph, order = 1, nodes = '4')
igraph::ego(g14_graph, order = 2, nodes = '4')
igraph::ego(g14_graph, order = 3, nodes = '4')
igraph::ego(g14_graph, order = 4, nodes = '4')

#' Ego shows how much connection a node has, going from 1st degree connections
#' to all possible ones.

igraph::ego_size(g14_graph, order = 3, nodes = '4')

#' Ego size informs us how many connections a node has in a given order
#
# Calculate closeness centrality ====
igraph::closeness(g14_graph) |> sort(decreasing = T)

# Optionally, we can choose our vertices
igraph::closeness(g14_graph, vids = c('7', '8')) |> sort(decreasing = T)

# Calculate betwenness centrality ====
igraph::betweenness(g14_graph, v = c("8", "9")) |> sort(decreasing = T)

# Calculate eigenvector centrality ====
eigens <- igraph::eigen_centrality(g14_graph, scale = FALSE)
eigens$vector

#' Eigenvector centrality is a measure of the relative influence of a vertex as
#' a function of the influences of the vertices it is connected to
#' That is, how much influence a node has based only on really important
#' network nodes? If in an organization someone has a lot of contacts and 
#' because of that shows a high degree centrality, that does not necessarily
#' mean that this person is influential to our network
#' Eigenvector centrality tries to emulate what influence a node has based on
#' the nodes "that matter" in a network

# Illustrating centrality ====
V(g14_graph)$degree <- degree(g14_graph)
V(g14_graph)$betweenness <- betweenness(g14_graph)
V(g14_graph)$eigen <- eigen_centrality(g14_graph)$vector

set.seed(123)
ggraph(g14_graph, layout = "lgl") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(aes(size = degree), color = "lightblue",
                  show.legend = FALSE) +
  scale_size(range = c(5,15)) +
  geom_node_text(aes(label = name)) +
  theme_void()

# We can also vary node size by its eigenvector centrality
set.seed(123)
ggraph(g14_graph, layout = "lgl") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(size = 6, aes(color = eigen)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  geom_node_text(aes(label = name)) +
  theme_void()

#' Here, we note that nodes to the right of 9 are not as influential as the
#' nodes on its left

# Centrality in the workfrance ====
workfrance_edgelist <- onadata::workfrance_edgelist
workfrance_vertices <- onadata::workfrance_vertices

work_graph <- graph_from_data_frame(workfrance_edgelist,
                                    directed = F,
                                    vertices = workfrance_vertices)


set.seed(123)
ggraph(work_graph, layout = 'fr') +
  geom_edge_link(color = 'grey', alpha = .7) +
  geom_node_point(color = rd_color, size = 4) +
  geom_node_text(aes(label = name), size = 2) +
  theme_void()

# Finding superconnectors ====
#' If one person is added to a department, it might be interesting to connect
#' this person to the highest influential actor in the department and to the
#' organization as a whole, since it's important for the individual to
#' assimilate into their own department and into the workplace as well.

# creating DMI subgraph
dmi_vertices <- V(work_graph)$name[V(work_graph)$dept == 'DMI']
dmi_graph <- igraph::subgraph(work_graph,
                              vids = dmi_vertices)

# visualize
set.seed(123)
ggraph(dmi_graph) +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(color = "lightblue", size = 4) +
  geom_node_text(aes(label = name), size = 2) +
  theme_void()

# get IDs of top 3 betweenness centralities
ranked_betweenness_DMI <- dmi_graph |>
  betweenness() |>
  sort(decreasing = TRUE)

top3_DMI <- names(ranked_betweenness_DMI[1:3])
top3_DMI

#' These are the IDs of the top three superconnectors in the DMI department

# Visualize dmi_graph again with superconnectors ====
# add betweenness vertex property
V(dmi_graph)$betweenness <- betweenness(dmi_graph)
# add top three superconnectors property
V(dmi_graph)$top3 <- ifelse(V(dmi_graph)$name %in% top3_DMI, 1, 0)

# visualize
set.seed(123)
ggraph(dmi_graph) +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(aes(color = as.factor(top3), size = betweenness),
                  show.legend = FALSE) +
  scale_color_manual(values = c("lightblue", "pink")) +
  geom_node_text(aes(label = name), size = 2) +
  theme_void()

# Getting superconnectors for the workfrance graph ====
# get IDs of top 3 betweenness centralities
ranked_betweenness_workfrance <- work_graph |>
  betweenness() |>
  sort(decreasing = TRUE)
#get top 3
top3_workfrance <- names(ranked_betweenness_workfrance[1:3])
top3_workfrance

# add betweenness property
V(work_graph)$betweenness <- betweenness(work_graph)
# label only if a top 3 superconnector
V(work_graph)$btwn_label <- ifelse(
  V(work_graph)$name %in% top3_workfrance,
  V(work_graph)$name,
  ""
)
# visualize
set.seed(123)
ggraph(work_graph) +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(aes(color = dept, size = betweenness),
                  show.legend = FALSE) +
  geom_node_text(aes(label = btwn_label), size = 4) +
  theme_void()

# Identifying influential employees ====
# get IDs of top 3 eigen centrality
ranked_eigen_DMI <- dmi_graph |>
  eigen_centrality() |>
  pluck("vector") |>
  sort(decreasing = TRUE)

#get top 3
top3_DMI_eigen <- names(ranked_eigen_DMI[1:3])
top3_DMI_eigen

# get IDs of top 3 eigen centrality
ranked_eigen_workfrance <- work_graph |>
  eigen_centrality() |>
  pluck("vector") |>
  sort(decreasing = TRUE)

#get top 3
top3_workfrance_eigen <- names(ranked_eigen_workfrance[1:3])
top3_workfrance_eigen

# add betweenness property
V(work_graph)$eigen <- eigen_centrality(work_graph)$vector
# label only if a top3 superconnector
V(work_graph)$eigen_label <- ifelse(
  V(work_graph)$name %in% top3_workfrance_eigen,
  V(work_graph)$name,
  ""
)

# visualize
ggraph(work_graph) +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(aes(color = dept, size = eigen),
                  show.legend = FALSE) +
  geom_node_text(aes(label = eigen_label), size = 4) +
  theme_void()

#' This time, the three most influential are all from the same department,
#' indicating that this department may be strategically important to involve
#' in any planned change initiatives

#