# Analysis of Canadian airports connectivity

library(igraph)
library(tidyverse)

# 1. load datasets

nodes <- read.csv("nodes.csv")
edges <- read.csv("edges_daily.csv")
flights <- read.csv("flights.csv")
turnarounds <- read.csv("turnarounds.csv")

edges <- edges %>%
  rename(
    origin = dep_icao,
    destination = arr_icao
  )

# 2. preview data

cat("Nodes:\n")
print(head(nodes))
cat("\nEdges:\n")
print(head(edges))

# 3. edge list

edges_clean <- edges %>%
  filter(origin != destination) %>%
  distinct(origin, destination, .keep_all = TRUE)

# 4. graph of airport network
nodes <- nodes %>%
  rename(name = icao)

edges_clean <- edges_clean %>%
  filter(origin %in% nodes$name & destination %in% nodes$name)

g_all <- graph_from_data_frame(d = edges_clean, vertices = nodes, directed = FALSE)

cat("\nSummary of the full network:\n")
print(summary(g_all))

# 5. connectivity metrics
cat("\nConnectivity Metrics:\n")
density_val <- edge_density(g_all)
components_info <- components(g_all)
avg_degree <- mean(degree(g_all))
avg_path <- average.path.length(g_all)

cat(sprintf("Density: %.4f\n", density_val))
cat(sprintf("Average Degree: %.2f\n", avg_degree))
cat(sprintf("Average Path Length: %.2f\n", avg_path))
cat(sprintf("Number of connected components: %d\n", components_info$no))

# 6. centrality
centrality <- data.frame(
  airport = V(g_all)$name,
  degree = degree(g_all),
  betweenness = betweenness(g_all),
  eigen = eigen_centrality(g_all)$vector
)

top_hubs <- centrality %>% arrange(desc(degree)) %>% head(10)
top_bridges <- centrality %>% arrange(desc(betweenness)) %>% head(10)

cat("\nTop 10 Hubs by Degree:\n")
print(top_hubs)
cat("\nTop 10 Bridges by Betweenness:\n")
print(top_bridges)

# 7. visualize degree distribution
hist(degree(g_all),
     main = "Degree Distribution of Canadian Airport Network",
     xlab = "Number of Connections (Degree)",
     col = "steelblue")

# 8. Determine Network Type: Hub-and-Spoke vs Distributed
# Visual cue: few high-degree airports → hub-and-spoke
plot(g_all,
     vertex.size = degree(g_all) / 2,
     vertex.label = NA,
     vertex.color = "lightblue",
     main = "Canadian Domestic Flight Network")

# 9. Compare Airline Subnetworks
compare_metrics <- function(df, name) {
  g <- graph_from_data_frame(d = df, vertices = nodes, directed = FALSE)
  data.frame(
    Airline = name,
    Density = edge_density(g),
    AvgDegree = mean(degree(g)),
    AvgPath = average.path.length(g),
    Clustering = transitivity(g, type = "global")
  )
}

metrics_all <- edges_clean %>%
  group_split(airline) %>%
  map_df(~ compare_metrics(.x, unique(.x$airline)))

cat("\nNetwork Structure Comparison by Airline:\n")
print(metrics_all)

# 10. Average shortest path length per airline
# (Only for connected graphs)
shortest_paths <- edges_clean %>%
  group_split(airline) %>%
  map_df(function(df) {
    g <- graph_from_data_frame(d = df, vertices = nodes, directed = FALSE)
    sp <- if (is.connected(g)) average.path.length(g) else NA
    data.frame(Airline = unique(df$airline), AvgShortestPath = sp)
  })

cat("\nAverage Shortest Path per Airline:\n")
print(shortest_paths)

# 11. export centrality results for visualization
write.csv(centrality, "airport_centrality.csv", row.names = FALSE)
cat("\nCentrality results saved to airport_centrality.csv\n")







