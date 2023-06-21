# Loading libraries ====
load_libraries <- function(){
  if (!require("dplyr"))
    install.packages("dplyr"); library(dplyr)
  if (!require("igraph")) # https://cloud.r-project.org/web/packages/igraph/igraph.pdf
    install.packages("igraph"); library(igraph)
  if (!require("magrittr"))
    install.packages("magrittr"); library(magrittr) # %<>% operator
  if (!require("onadata"))
    install.packages("onadata"); library(onadata)
}

load_libraries()

# Creating a graph using igraph ====
koenigsberg <- koenigsberg

graphkoenisberg <- graph_from_data_frame(koenigsberg, directed = FALSE)

l <- layout_nicely(graphkoenisberg)

plot(graphkoenisberg, layout = l)

# Creating an adjacency matrix
as_adjacency_matrix(graphkoenisberg)

as.matrix(as_adjacency_matrix(graphkoenisberg)) # now as an actual matrix

# Checking properties of vertices
V(graphkoenisberg)

# Checking properties of edges
E(graphkoenisberg)

# Creating an undirected graph via edgelist ====
gwork_edgelist <- data.frame(
  from = c("David", "David", "David", "Jane", "Jane"),
  to = c("Zubin", "Suraya", "Jane", "Zubin", "Suraya")
)

gwork_edgelist # dataframe
gwork_edgelist %<>% as.matrix # transform to matrix
gwork_edgelist # matrix

gwork <- graph_from_edgelist(gwork_edgelist, directed = F)
gwork # visualizing gwork output

#' The *UN* at the beginning means "Undirected Named" vertices
#' After the *UN* we have two dashes "--": these are missing properties which
#' will be explained later
#' The *4 5* means 4 vertices and 5 edges
#' And then another two dashes

#' After that, we have the graph's attributes
#' In this case, we have only the name of the vertice (v) of type character (c),
#' which is informed by *name (v/c)*
#' Here, David and Zubin are undirectly connected *(--)*, as well as: David and
#' Suraya, David and Jane, Zubin and Jane, and Suraya and Jane

# Creating a directed graph via edgelist ====
gmanage_edgelist <- data.frame(
  from = c("Suraya", "David", "David"),
  to = c("David", "Zubin", "Jane")
)

gmanage_edgelist # dataframe
gmanage_edgelist %<>% as.matrix # transform to matrix
gmanage_edgelist # matrix

gmanage <- graph_from_edgelist(gmanage_edgelist, directed = T)
gmanage # visualizing gmanage output

#' This time we have a "Directed Named" graph *(DN)* with the vertice attribute
#' *name*, which is a character
#' In it, Suraya leads to David *(->)*, and David leads to Zubin and to Jane

# Creating an directed graph via adjacency matrix ====
adj_flights <- matrix(c(0, 5, 2, 4, 0, 0, 4, 1, 0), nrow = 3, ncol = 3)
rownames(adj_flights) <- c("SFO", "PHL", "TUS")
colnames(adj_flights) <- rownames(adj_flights)

flightgraph <- graph_from_adjacency_matrix(
  adjmatrix = adj_flights,
  mode = "directed"
)
flightgraph # visualizing flightgraph output

# Creating an directed weighted graph via adjacency matrix ====
flightgraph_weighted <- graph_from_adjacency_matrix(
  adjmatrix = adj_flights,
  mode = "directed",
  weighted = TRUE
)

flightgraph_weighted # visualizing flightgraph_weighted output

#' This time, we see *DNW-*, meaning that we've created a "Directed Named
#' Weighted" network
#' Also, our edge list is simplified from the original version

# If we wanted to simplify the repetitive edge list from before...
simplify(flightgraph)

# Creating an directed weighted graph dataframe ====
# edge data frame
edge_df <- data.frame(
  from = c("David", "David", "Jane", "Jane", "Zubin", "Suraya"),
  to = c("Sandra", "Jake", "Mae-Li", "Jake", "Sandra", "Mae-Li")
)
edge_df

# vertex dataframe
vertex_df <- data.frame(
  name = c("David", "Jane", "Zubin", "Suraya",
           "Sandra", "Jake", "Mae-Li"),
  Dept = c(rep("A", 4), rep("B", 3))
)
vertex_df

# create graph
gnew <- graph_from_data_frame(
  d = edge_df,
  directed = FALSE,
  vertices = vertex_df
)
gnew # visualizing flightgraph_weighted output

#' Now we got an Undirected Named graph with 7 vertices and 6 edges, with 2
#' character vertices attributes: "name" and "Dept"

# Adding properties to the vertices and edges ====
# dataframe of edges and properties
edge_transfers <- data.frame(
  from = c("A", "A", "B", "B"),
  to = c("A", "B", "A", "C"),
  cur = c("USD", "USD", "GBP", "GBP"),
  amt = c(150000, 570000, 230000, 175000)
)
edge_transfers
# dataframe of vertices and properties
vertex_transfers <- data.frame(
  name = c("A", "B", "C"),
  loc = c("USA", "UK", "France")
)
vertex_transfers
# create graph
gtransfers <- graph_from_data_frame(
  d = edge_transfers,
  directed = TRUE,
  vertices = vertex_transfers
)
gtransfers # visualizing flightgraph_weighted output

#' In this new Directed Named graph we have two character name attributes of
#' vertices (name and loc) and two edge attributes: one a character type (cur)
#' and another a numeric type (amt)

# Getting the vertex and edge sets ====
V(gtransfers)
V(gtransfers)$name
V(gtransfers)$loc

E(gtransfers)
E(gtransfers)$cur
E(gtransfers)$amt

# Adding weights as an edge property to a graph ====
edge_routes <- data.frame(
  from = c("SFO", "SFO", "PHL", "PHL", "TUS"),
  to = c("PHL", "TUS", "SFO", "TUS", "SFO")
)
edge_routes %<>% as.matrix

flightsgraph <- igraph::graph_from_edgelist(
  el = edge_routes,
  directed = TRUE
)
flightsgraph # visualizing flightsgraph output

E(flightsgraph)$weight # NULL object (THERE IS NO 'WEIGHT')

E(flightsgraph)$weight <- c(4, 4, 5, 1, 2) # Adding weight

E(flightsgraph)$weight # 'WEIGHT' IS ADDED

flightsgraph # visualizing flightsgraph output AGAIN

#' Notice that now the graph is "Weighted"

# Adding department as a vertice property to a graph ====
gnew # visualizing flightsgraph output

V(gnew)$type # NULL object (THERE IS NO 'TYPE')

V(gnew)$type <- V(gnew)$Dept # Adding type

V(gnew)$type # 'TYPE' IS ADDED

gnew # visualizing flightsgraph output

#' Notice that now the graph is "Bipartite"

# Creating a graph using Pizza data ====
glimpse(pizza)

g_df <- pizza %>% select(requester, responder) # vertice-vertice info

pizzagraph <- graph_from_data_frame(
  d = g_df,
  directed = F
)

pizzagraph

#' We created a undirected named graph with 727 vertices and 400 edges
#' That means that out of everyone who asked, some people never got their
#' request attended to
#' 

V(pizzagraph)

E(pizzagraph)
E(pizzagraph)$id <- pizza$request_id
E(pizzagraph)$votes <- pizza$requester_votes
E(pizzagraph)$subreddits <- pizza$requester_subreddits
E(pizzagraph)

# Which ID had the largest number of requester votes?
max(E(pizzagraph)$votes) # maximum number of requester votes
which(E(pizzagraph)$votes == max(E(pizzagraph)$votes)) # which index is this?
idx <- which(E(pizzagraph)$votes == max(E(pizzagraph)$votes))

E(pizzagraph)$id[idx] # request ID "t3_10mo95" had 136567 requester votes

max(E(pizzagraph)$subreddits) # maximum number of requester subreddits
which(E(pizzagraph)$subreddits == max(E(pizzagraph)$subreddits)) # which index is this?
idx <- which(E(pizzagraph)$subreddits == max(E(pizzagraph)$subreddits))

E(pizzagraph)$id[idx] # request ID "t3_1ahfft" had 166 requester subreddits

