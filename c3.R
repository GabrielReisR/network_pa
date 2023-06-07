# Loading libraries ====
load_libraries <- function(){
  if (!require("dplyr"))
    install.packages("dplyr"); library(dplyr)
  if (!require("ggraph"))
    install.packages("ggraph"); library(ggraph)
  if (!require("ggmap"))
    install.packages("ggmap"); library(ggmap)
  if (!require("igraph")) # https://cloud.r-project.org/web/packages/igraph/igraph.pdf
    install.packages("igraph"); library(igraph)
  if (!require("magrittr"))
    install.packages("magrittr"); library(magrittr) # %<>% operator
  if (!require("networkD3"))
    install.packages("networkD3"); library(networkD3) # %<>% operator
  if (!require("onadata"))
    install.packages("onadata"); library(onadata)
  if (!require("visNetwork"))
    install.packages("visNetwork"); library(visNetwork)
}

load_libraries()
#
# Importing data ====
df <- onadata::karate
karate <- igraph::graph_from_data_frame(df, directed = F)
karate

# We now created a named undirected graph with 34 vertices and 78 edges

# Visualizing graph ====
set.seed(123)
l <- layout_randomly(karate)
plot(karate, layout = l)

# Changing some label properties ====
#' • label: The text of the label
#' • label.family: The font family to be used (default is ‘serif’)
#' • label.font: The font style, where 1 is plain (default), 2 is bold, 3 is italic, 4
#' is bold and italic and 5 is symbol font
#' • label.cex: The size of the label text
#' • label.color: The color of the label text
#' • label.dist: The distance of the label from the vertex, where 0 is centered
#' on the vertex (default) and 1 is beside the vertex
#' • label.degree: The angle at which the label will display relative to the center
#' of the vertex, in radians. The default is -pi/4

# only store a label if Mr Hi or John A
V(karate)$label <- ifelse(V(karate)$name %in% c("Mr Hi", "John A"),
                          V(karate)$name,
                          "")

# change label font color, size and font family
# (selected font family needs to be installed on system)
V(karate)$label.color <- "black"
V(karate)$label.cex <- 0.8
V(karate)$label.family <- "arial"
plot(karate, layout = l)

# Changing some vertices properties ====
#' • size: The size of the vertex
#' • color: The fill color of the vertex
#' • frame.color: The border color of the vertex
#' • shape: The shape of the vertex; multiple shape options are supported
#' including circle, square, rectangle and none
# different colors and shapes for Mr Hi and and John A
V(karate)$color <- ifelse(V(karate)$name %in% c("Mr Hi", "John A"),
                          "lightblue",
                          "pink")
V(karate)$shape <- ifelse(V(karate)$name %in% c("Mr Hi", "John A"),
                          "square",
                          "circle")
plot(karate, layout = l)

# Changing some edges properties ====
#' • color: The color of the edge
#' • width: The width of the edge
#' • arrow.size: The size of the arrow in a directed edge
#' • arrow.width: The width of the arrow in a directed edge
#' • arrow.mode: Whether edges should direct forward (>), backward (<) or both
#' (<>)
#' • lty: Line type of edges, with numerous options including solid, dashed,
#' dotted, dotdash and blank
#' • curved: The amount of curvature to apply to the edge, with zero (default) as
#' a straight edge, negative numbers bending clockwise and positive bending
#' anti-clockwise
# change color and linetype of all edges
E(karate)$color <- "blue"
E(karate)$lty <- "dashed"
plot(karate, layout = l)

# Adding layout property to graph ====
set.seed(123)
karate_grid <- igraph::add_layout_(karate, on_grid())
karate$layout <- karate_grid
# check a few lines of the 'layout' property
head(karate_grid$layout)

#' Other layouts:
#' as_star(), as_tree(), in_circle(), on_grid() and on_sphere()

# circle layout
set.seed(123)
circ <- layout_in_circle(karate)
plot(karate, layout = circ)

# sphere layout
set.seed(123)
sph <- layout_on_sphere(karate)
plot(karate, layout = sph)

# Force-directed graph layouts ====
#' Force-directed graph layouts calculate make connected vertices closer to
#' eachother while repelling vertices that are not connected

# Fruchterman-Reingold algorithm
set.seed(123)
fr <- layout_with_fr(karate)
plot(karate, layout = fr)

# Kamada-Kawai algorithm
set.seed(123)
kk <- layout_with_kk(karate)
plot(karate, layout = kk)

# GEM algorithm
set.seed(123)
gem <- layout_with_gem(karate)
plot(karate, layout = gem)

#' Other force-directed graph layouts:
#' layout_with_dh() uses a simulated annealing algorithm developed for nice
#' graph drawing
#' layout_with_mds() generates vertex coordinates through multidimensional
#' scaling based on shortest path distance
#' layout_with_sugiyama() is suitable for directed graphs and minimizes edge
#' crossings by introducing bends on edges
#' 
#' *For very large graphs (thousands+ vertices)*
#' layout_with_lgl() uses the Large Graph Layout algorithm which tries to
#' identify clusters of vertices and position the clusters before positioning
#' the individual vertices to minimize the chance of hairballs
#' layout_with_drl() and layout_with_graphopt() also use efficient
#' force-directed algorithms which scale well on large graphs

igraph::plot.igraph(karate)
karate$layout <- NULL

# Visualizing graphs using ggraph:: ====
# create graph object
karate <- igraph::graph_from_data_frame(df, directed = FALSE)

# set seed for reproducibility
set.seed(123)
# visualise using ggraph with fr layout
ggraph(karate, layout = "fr") +
  geom_edge_link() +
  geom_node_point()


# Other visualizations using ggraph
set.seed(123)
ggraph(karate, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(color = "blue", size = 5) +
  theme_void() +
  labs(title = "Zachary's Karate Club Network")

# Coloring leaders
V(karate)$leader <- ifelse(
  V(karate)$name %in% c("Mr Hi", "John A"), 1, 0
)
set.seed(123)
ggraph(karate, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(aes(color = as.factor(leader)), size = 5,
                  show.legend = FALSE) +
  theme_void() +
  labs(title = "Zachary's Karate Club Network")

# Using ggraph:: with other dataframes ====
we <- onadata::workfrance_edgelist
wv <- onadata::workfrance_vertices
workfrance <- graph_from_data_frame(
  d = we,
  vertices = workfrance_vertices,
  directed = F
)

# basic visualization
ggraph(workfrance,
       layout = 'fr') +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(color = "blue", size = 5) +
  theme_void()

# adding more to the visualization
ggraph(workfrance,
       layout = 'fr') +
  geom_edge_link(aes(width = mins),
                 color = "grey",
                 alpha = 0.7,
                 show.legend = F) +
  geom_node_point(aes(color = dept), size = 5) +
  labs(color = "Department") +
  theme_void() +
  labs(title = "Spatial co-location of employees in a workplace")

# Visualizing maps and graphs via LondonTube ====
le <- onadata::londontube_edgelist
lv <- onadata::londontube_vertices
lines <- onadata::londontube_edgelist %>% distinct(line, linecolor)
londontube <- graph_from_data_frame(
  d = le,
  vertices = lv,
  directed = T
)

# Visualization
ggraph(londontube,
        layout = 'fr') +
  geom_edge_link(aes(color = line), alpha = .6, width = 1) +
  geom_node_point(color = 'black', size = 1) +
  scale_edge_color_manual(name = 'Line',
                          values = lines$linecolor) +
  theme_void()
  
# # recreate graph object to capture additional edge data
new_edgelist <- londontube_edgelist |>
  dplyr::inner_join(londontube_vertices |>
                      dplyr::select(id, latitude, longitude),
                    by = c("from" = "id")) |>
  dplyr::rename(lat_from = latitude, lon_from = longitude) |>
  dplyr::inner_join(londontube_vertices |>
                      dplyr::select(id, latitude, longitude),
                    by = c("to" = "id")) |>
  dplyr::rename(lat_to = latitude, lon_to = longitude)
# view
head(new_edgelist)

londontube <- igraph::graph_from_data_frame(
  d = new_edgelist,
  vertices = londontube_vertices,
  directed = FALSE
)
# layer a London map (requires Google Maps API key)
londonmap <- get_map(location = "London, UK", source = "google")
# visualize using geolocation
ggmap(londonmap, base_layer = ggraph(tubegraph)) +
  geom_node_point(aes(x = longitude, y = latitude),
                  color = "black", size = 1) +
  geom_edge_link(aes(x = lon_from, y = lat_from,
                     xend = lon_to, yend = lat_to,
                     color = line), width = 1) +
  scale_edge_color_manual(name = "Line",
                          values = lines$linecolor)

#
# Visualizing an interactive graph using visNetwork:: ====
nodes <- data.frame(
  id = 1:4,
  label = c("David", "Zubin", "Suraya", "Jane")
)
edges <- data.frame(
  from = c(1, 1, 1, 4, 4),
  to = c(2, 3, 4, 2, 3)
)
visNetwork(nodes, edges) |>
  visLayout(randomSeed = 123)
#
# Visualizing an interactive graph using an igraph object ====
# create graph object
karate <- igraph::graph_from_data_frame(karate_edgelist,
                                        directed = FALSE)
# different colors and shapes for Mr Hi and and John A
V(karate)$color <- ifelse(V(karate)$name %in% c("Mr Hi", "John A"),
                          "lightblue",
                          "pink")
V(karate)$shape <- ifelse(V(karate)$name %in% c("Mr Hi", "John A"),
                          "square",
                          "circle")
# more visible edges
E(karate)$color = "grey"
E(karate)$width <- 3
# visualize from igraph
visNetwork::visIgraph(karate, layout = "layout_with_fr",
                      randomSeed = 123)

#
# Visualizing a D3 interactive graph ====
# get karate edgelist
karate_edgelist <- read.csv("https://ona-book.org/data/karate.csv")
# visualize
networkD3::simpleNetwork(karate_edgelist)
#
# Visualizing a D3 interactive graph from an igraph object ====
# create igraph object
karate <- igraph::graph_from_data_frame(karate_edgelist,
                                        directed = FALSE)
# give Mr Hi and John A a different group
V(karate)$group <- ifelse(
  V(karate)$name %in% c("Mr Hi", "John A"), 1, 2
)
# translate to networkD3 - creates a list with links and nodes dfs
# links have a source and target column and group if requested
netd3_list <- networkD3::igraph_to_networkD3(karate,
                                             group = V(karate)$group)
# visualize
networkD3::forceNetwork(
  Links = netd3_list$links,
  Nodes = netd3_list$nodes,
  NodeID = "name",
  Source = "source",
  Target = "target",
  Group = "group"
)

# Visualizing a sankey graph ====
# get data
eu_referendum <- read.csv(
  "https://ona-book.org/data/eu_referendum.csv"
)
# aggregate by region
results <- eu_referendum |>
  dplyr::group_by(Region) |>
  dplyr::summarise(Remain = sum(Remain), Leave = sum(Leave)) |>
  tidyr::pivot_longer(-Region, names_to = "result",
                      values_to = "votes")
# create unique regions, "Leave" and "Remain" for nodes dataframe
regions <- unique(results$Region)
nodes <- data.frame(node = c(0:13),
                    name = c(regions, "Leave", "Remain"))
# create edges/links dataframe
results <- results |>
  dplyr::inner_join(nodes, by = c("Region" = "name")) |>
  dplyr::inner_join(nodes, by = c("result" = "name"))
links <- results[ , c("node.x", "node.y", "votes")]
colnames(links) <- c("source", "target", "value")
# visualize using sankeyNetwork
networkD3::sankeyNetwork(
  Links = links, Nodes = nodes, Source = 'source', Target = 'target',
  Value = 'value', NodeID = 'name', units = 'votes', fontSize = 12
)
#