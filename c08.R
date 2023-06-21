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
# Evaluating the nominal assortativity of a graph ====
# Graph assortativity ranges between -1 and 1
#' *Assortativity = 1*
#' Means that two vertices with the same properties have a very high likelihood
#' of being connected
#' *Assortativity = -1*
#' Means that two vertices with the same properties have a very high likelihood
#' of NOT being connected
#' *Assortativity near 0*
#' Means that we can't establish a pattern of connection between two vertices
#' with the same properties
schoolfriends_edgelist <- read.csv(
  "https://ona-book.org/data/schoolfriends_edgelist.csv"
)
schoolfriends_vertices <- read.csv(
  "https://ona-book.org/data/schoolfriends_vertices.csv"
)

# create undirected Facebook friendships graph
schoolfriends_fb <- igraph::graph_from_data_frame(
  d = schoolfriends_edgelist |>
    dplyr::filter(type == "facebook"),
  vertices = schoolfriends_vertices,
  directed = FALSE
)

schoolfriends_fb

# calculate assortativity by class for Facebook friendships
igraph::assortativity_nominal(schoolfriends_fb,
                              as.integer(as.factor(V(schoolfriends_fb)$class)))

# Assortativity = 0.3833667
#' This suggests moderate assortativity, or moderate likelihood that students in
#' the same class will be Facebook friends. Now let’s compare to ‘real’ reported
#' friendships.

# create directed graph of reported friendships
schoolfriends_rp <- igraph::graph_from_data_frame(
  d = schoolfriends_edgelist |>
    dplyr::filter(type == "reported"),
  vertices = schoolfriends_vertices
)

# calculate assortativity by class for reported friendships
igraph::assortativity_nominal(
  schoolfriends_rp,
  as.integer(as.factor(V(schoolfriends_fb)$class))
)

# 0.7188919
#' We see substantially higher assortativity by class for reported friendships,
#' indicating that being in the same class is more strongly associated with
#' developing a reported school friendship

# Evaluating the degree assortativity of a graph ====
#' A high degree assortativity is a measure of preferential attachment in
#' organizations, where highly connected vertices are connected with each other
#' and a large number of vertices with low degree make up the remainder of the
#' network.
#' 
#' *Degree assortativity* asks if people with high degree centrality tend to
#' have connections with people with high degree centrality too.

# degree assortativity of Facebook friendships (undirected)
igraph::assortativity_degree(schoolfriends_fb)

# degree assortativity of reported friendships (directed)
igraph::assortativity_degree(schoolfriends_rp)

#' We see that real-life friendships are moderately assortative in this data,
#' whereas Facebook friendships are approximately neutral. This indicates that
#' more popular students have a stronger tendency in real-life to be friends
#' with other popular students

#' Highly assortative networks demonstrate resilience in that knowledge,
#' community and other social capital are concentrated in a strong core, and
#' disruptions such as departures of actors from those networks are less likely
#' to affect the network as a whole. However, such networks also demonstrate
#' characteristics which are counterproductive to diversity and inclusion and
#' can represent challenging environments for new entrants to adjust to.
#
# Evaluating vertex similarity (how similar are two vertices?) ====
#' The vertex similarity coefficient of a pair of vertices is a measure of how
#' similar are the immediate networks of those two vertices.
#' *Jaccard*: the number of vertices who are neighbors of both 𝑣1 and 𝑣2
#' divided by the number of vertices who are neighbors of at least one of 𝑣1
#' and 𝑣2.
#' *Dice*: twice the number of vertices who are neighbors of both 𝑣1 and 𝑣2
#' divided by the sum of the degree centralities of 𝑣1 and 𝑣2. Thus, common
#' neighbors are double counted in this method.
#' *Inverse log-weighted*: the sum of the inverse logs of the degrees of the
#' common neighbors of 𝑣1 and 𝑣2. This definition asserts that common
#' neighbors that have high degree in the network are ‘less valuable’ in
#' detecting similarity because there is a higher likelihood that they would be
#' a common neighbor simply by chance.

# download edgelist and create unweighted graph
g14_edgelist <- read.csv("https://ona-book.org/data/g14_edgelist.csv")

g14 <- igraph::graph_from_data_frame(d = g14_edgelist |>
                                       dplyr::select(from, to),
                                     directed = FALSE)
g14

# visualize graph
set.seed(123)
g14viz <- ggraph(g14, layout = "lgl") +
    geom_edge_link(color = "grey") +
    geom_node_label(aes(label = name), fill = rd_color) +
    theme_void()

g14viz

# getting jaccard similarity of 7 and 8
#' # of vertices that both are connected = 2
#' # of vertices that 7 and 8 have besides with eachoter = 5
#' Jaccard = 2/5 = 0.4
igraph::similarity(g14, vids = c('7', '8'))

# getting dice similarity
#' # of vertices that both are connected X 2 = 2 X 2 = 4
#' # of vertices that 7 and 8 are connected (sum of degrees) = 7
#' Dice = 4/7 = 0.571
igraph::similarity(g14, vids = c("7", "8"), method = "dice")


igraph::similarity(g14, method = "invlogweighted")

# getting invlogweighted similarity
invsim <- igraph::similarity(g14, method = "invlogweighted")

# rows and cols should be labelled by vertex name before extracting
colnames(invsim) <- V(g14)$name
rownames(invsim) <- V(g14)$name

# extract value for vertices 7 and 8
invsim["7", "8"]

#' This returns the similarity of the selected vertices with all vertices in
#' the network

# Evaluating graph similarity (how similar are two graphs?) ====
#' In organizational network analysis we are usually more concerned with
#' comparing graphs where the vertices represent the same entities (usually
#' people), and where the vertex set is identical but where the edge set may be
#' different.
#' Let 𝐺1 = (𝑉 , 𝐸1) and 𝐺2 = (𝑉 , 𝐸2) be two graphs with the same vertex
#' set. The Jaccard similarity of 𝐺1 and 𝐺2 is the number of edges in both 𝐸1
#' and 𝐸2 divided by the number of edges in at least one of 𝐸1 and 𝐸2.
#' Note that a Jaccard similarity of 1 means that both graphs have identical
#' edge sets and so are identical in structure, and a similarity of 0 means that
#' both graphs have no edges in common. Let’s try this on our schoolfriends 
#' graphs
#' *Jaccard similarity* is the proportion of edges seen in G1 and G2 divided by
#' edges seen only on G1 or G2

# create directed version of Facebook graph
schoolfriends_fb_dir <- igraph::as.directed(schoolfriends_fb)

# getting jaccard similarity
#' [%s%]: intersection of two graphs (all vertices and edges that both graphs
#' have in common)
#' [%u%]: union of two graphs (all unique vertices and edges in both graphs
#' combined)

schoolfriends_fb_dir %s% schoolfriends_rp
schoolfriends_fb_dir %u% schoolfriends_rp

#' Note that the first has 329 vertices and 314 connections, while the second
#' has 329 vertices but 3228 connections total
#' That proportion is the Jaccard similarity:
#' Jaccard = 314/3228 = 0.0972
#' That means that only 9.72% of edges are encountered on both networks,
#' meaning that over 90% of all other edges on G1 and G2 are intrinsic to that
#' graph structure.
#' "We see that there is only about 10% similarity between the Facebook
#' friendships and the ‘real’ reported friendships"

#