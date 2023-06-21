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
    install.packages("networkD3"); library(networkD3)
  if (!require("onadata"))
    install.packages("onadata"); library(onadata)
  if (!require("rvest"))
    install.packages("rvest"); library(rvest)
  if (!require("stringr"))
    install.packages("stringr"); library(stringr)
  if (!require("tools"))
    install.packages("tools"); library(tools)
}

load_libraries()
#
# Recurring functions ====
rd_color <- "#19c1ce"
#
# Importing data ====
# download chinook database tables
chinook_employees <- read.csv(
  "https://ona-book.org/data/chinook_employees.csv"
)
chinook_customers <- read.csv(
  "https://ona-book.org/data/chinook_customers.csv"
)
chinook_invoices <- read.csv(
  "https://ona-book.org/data/chinook_invoices.csv"
)
chinook_items <- read.csv(
  "https://ona-book.org/data/chinook_items.csv"
)
#
# Creating a simple connected graph ====
orgchart <- chinook_employees %>% 
  inner_join(chinook_employees,
             by = c('EmployeeId' = 'ReportsTo'),
             multiple = 'all') %>% 
  select(from = 'FirstName.x', to = 'FirstName.y')

graph_orgchart <- graph_from_data_frame(orgchart,
                                        directed = T)

graph_orgchart

# create management structure as dendrogram (tree)
set.seed(123)
ggraph(orgchart, layout = 'dendrogram', circular = FALSE) +
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()
  
#

# Creating a graph connecting customers to sales reps ====
rep_customer <- chinook_customers %>% 
  mutate(CustomerName = paste(FirstName, LastName)) %>% 
  select(-FirstName, -LastName) %>% 
  left_join(chinook_employees, by = c("SupportRepId" = "EmployeeId")) %>% 
  mutate(RepName = paste(FirstName, LastName)) %>% 
  select(RepName, CustomerName, -FirstName, -LastName, -ReportsTo, -contains('Id'))

repcustomer_graph <- graph_from_data_frame(rep_customer)

V(repcustomer_graph)$type <- ifelse(
  V(repcustomer_graph)$name %in% rep_customer$RepName,
  "Rep",
  "Customer"
)

ggraph(repcustomer_graph, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_label(aes(label = name, color = type), size = 3) +
  scale_color_discrete(name = '') +
  theme_void()

# Connecting customers between themselves ====
rep_customer <- chinook_customers %>% 
  mutate(CustomerName = paste(FirstName, LastName)) %>% 
  select(-FirstName, -LastName) %>% 
  left_join(chinook_employees, by = c("SupportRepId" = "EmployeeId")) %>% 
  mutate(RepName = paste(FirstName, LastName)) %>% 
  select(RepName, CustomerName, -FirstName, -LastName, -ReportsTo, -contains('Id'))

customer_customer <- rep_customer %>% 
  inner_join(rep_customer, by = 'RepName', multiple = 'all') %>% 
  filter(CustomerName.x != CustomerName.y) %>% 
  select(Customer1 = CustomerName.x, Customer2 = CustomerName.y, RepName) %>% 
  distinct()

cust_graph <- graph_from_data_frame(customer_customer,
                                    directed = F)

ggraph(cust_graph) +
  geom_edge_link(aes(color = RepName), alpha = .3) +
  geom_node_point(color = "lightblue", size = 5) +
  scale_color_discrete(name = 'Representante') +
  theme_void()

# Connecting customers to items ====
customer_item <- chinook_invoices %>% 
  left_join(chinook_items, by = "InvoiceId", multiple = 'all') %>% 
  left_join(chinook_customers, by = 'CustomerId') %>% 
  mutate(CustomerName = paste(FirstName, LastName)) %>% 
  select(-InvoiceId, -CustomerId, -FirstName, -LastName, -SupportRepId) %>% 
  distinct()

customeritem_graph <- graph_from_data_frame(customer_item,
                                            directed = F)

V(customeritem_graph)$type <- ifelse(
  V(customeritem_graph)$name %in% customer_item$TrackId,
  "Item",
  "Customer"
)

ggraph(customeritem_graph, layout = 'fr') +
  geom_edge_link(alpha = .3) +
  geom_node_point(aes(color = type), size = 2) +
  scale_color_discrete(name = '') +
  theme_void()

#
# Connecting customers through common purchases ====
customer_item <- chinook_invoices %>% 
  left_join(chinook_items, by = "InvoiceId", multiple = 'all') %>% 
  left_join(chinook_customers, by = 'CustomerId') %>% 
  mutate(CustomerName = paste(FirstName, LastName)) %>% 
  select(-InvoiceId, -CustomerId, -FirstName, -LastName, -SupportRepId) %>% 
  distinct()

customer_customer_item <- customer_item %>% 
  inner_join(customer_item, by = 'TrackId', multiple = 'all') %>% 
  filter(CustomerName.x != CustomerName.y) %>% 
  select(Customer1 = CustomerName.x, Customer2 = CustomerName.y, TrackId) %>% 
  distinct() %>% 
  mutate(Cust1 = pmin(Customer1, Customer2),
         Cust2 = pmax(Customer1, Customer2)) %>% 
  select(-Customer1, -Customer2) %>% 
  distinct() %>% 
  count(Cust1, Cust2, name = 'Items')

custcustitem_graph <- graph_from_data_frame(customer_customer_item,
                                            directed = F)

custcustitem_graph

set.seed(123)
ggraph(custcustitem_graph,
       layout = 'fr') +
  geom_edge_link(aes(color = ordered(Items)), alpha = .5) +
  geom_node_point(color = rd_color, size = 5) +
  labs(edge_color = '# of Common Items') +
  theme_void()
#

# Subsetting for only customers with 2 common purchases ====
edges <- E(custcustitem_graph)[E(custcustitem_graph)$Items >= 2]

# create subgraph using these edges
two_item_graph <- subgraph.edges(custcustitem_graph,
                                 eids = edges)
two_item_graph

set.seed(123)
ggraph(two_item_graph,
       layout = 'fr') +
  geom_edge_link(aes(color = ordered(Items)), alpha = .5) +
  geom_node_point(color = rd_color, size = 5) +
  labs(edge_color = '# of Common Items') +
  theme_void()

# Scraping Friends' first episode script ====
url_string <- "https://fangj.github.io/friends/season/0101.html"
nodes <- xml2::read_html(url_string) %>%
  xml2::as_list() %>%
  unlist()

head(nodes)

# swap lines containing the string 'Scene:' with 'New Scene'
nodes_newscene <- ifelse(grepl("Scene:", nodes), "New Scene", nodes)

# check that there are at least a few 'New Scene' entries now
sum(nodes_newscene == "New Scene")

# outside of 'New Scene' tags extract anything before : in every line
nodes_char <- ifelse(nodes_newscene != "New Scene",
                     str_extract(nodes_newscene,
                                 "^[A-Za-z ]+(?=:)"),
                     nodes_newscene)

# remove NAs
nodes_char_clean1 <- nodes_char[!is.na(nodes_char)]

# remove entries with "all", " and " or "by" irrelevant of the case
nodes_char_clean2 <- nodes_char_clean1[
  !grepl("all| and |by", tolower(nodes_char_clean1))
]

# check
nodes_char_clean2[sample(20)] # parece limpo

# number scene by counting previous "New Scene" entries and adding 1
scene_count <- c()
for (i in 1:length(nodes_char_clean2)) {
  scene_count[i] <- sum(grepl("New Scene", nodes_char_clean2[1:i]))
}

# creating final data frame
results <- data.frame(scene = scene_count,
                      character = nodes_char_clean2) |>
  filter(character != "New Scene") |>
  distinct(scene, character) |>
  mutate(
    character = character |>
      tolower() |>
      toTitleCase() # title case
  )

head(results)

# creating scene pairs
unique_pairs <- function(char_vector = NULL) {
  # ensure unique entries
  vector <- as.character(unique(char_vector))
  # create from-to column dataframe
  df <- data.frame(char1 = character(),
                   char2 = character(),
                   stringsAsFactors = FALSE)
  # iterate over each entry to form pairs
  if (length(vector) > 1) {
    for (i in 1:(length(vector) - 1)) {
      char1 <- rep(vector[i], length(vector) - i)
      char2 <- vector[(i + 1): length(vector)]
      df <- df %>%
        dplyr::bind_rows(
          data.frame(char1 = char1,
                     char2 = char2,
                     stringsAsFactors = FALSE)
        )
    }
  }
  #return result
  df
}

# run unique_pairs by scene
friends_ep101 <- results |>
  dplyr::group_by(scene) |>
  dplyr::summarise(unique_pairs(character)) |>
  dplyr::ungroup()

# check
head(friends_ep101)

# create weight as count of scenes
friends_ep101_edgelist <- friends_ep101 |>
  dplyr::select(-scene) |>
  dplyr::mutate(from = pmin(char1, char2), to = pmax(char1, char2)) |>
  dplyr::count(from, to, name = "weight")

# check
head(friends_ep101_edgelist)

# create igraph object
friends_ep1_network <- igraph::graph_from_data_frame(
  d = friends_ep101_edgelist,
  directed = FALSE
)

friends_ep1_network

# visualizing graph
set.seed(123)
ggraph(friends_ep1_network,
       layout = 'fr') +
  geom_edge_link(aes(width = exp(weight)), color = 'grey', alpha = .3, show.legend = FALSE) +
  geom_node_label(aes(label = name), color = 'blue') +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  theme_void()

#