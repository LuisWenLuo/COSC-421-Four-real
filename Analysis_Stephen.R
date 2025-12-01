# ============================================================
# Analysis of Canadian airports connectivity
# (version adapted to your CSV files)
# ============================================================

library(igraph)
library(tidyverse)

# 1. Load datasets -----------------------------------------------------------

nodes <- read.csv("nodes.csv", stringsAsFactors = FALSE)
edges <- read.csv("edges_daily.csv", stringsAsFactors = FALSE)
flights <- read.csv("flights.csv", stringsAsFactors = FALSE)       # loaded for context
turnarounds <- read.csv("turnarounds.csv", stringsAsFactors = FALSE)  # loaded for context

# 2. Clean / standardize column names ---------------------------------------

# nodes: icao, iata, city  → use icao as vertex name
nodes <- nodes %>%
  rename(name = icao)

# edges: date, dep_icao, arr_icao, flights, mean_duration_s, median_duration_s
edges <- edges %>%
  rename(
    origin = dep_icao,
    destination = arr_icao
  )

# 3. Build an edge list aggregated over dates -------------------------------

# remove self-loops, keep only airports we have in nodes,
# and aggregate multiple days into total flights & avg durations
edges_clean <- edges %>%
  filter(origin != destination) %>%
  filter(origin %in% nodes$name & destination %in% nodes$name) %>%
  group_by(origin, destination) %>%
  summarise(
    flights_total = sum(flights, na.rm = TRUE),
    mean_duration_s = mean(mean_duration_s, na.rm = TRUE),
    median_duration_s = mean(median_duration_s, na.rm = TRUE),
    .groups = "drop"
  )

cat("Nodes:\n")
print(head(nodes))
cat("\nEdges (aggregated):\n")
print(head(edges_clean))

# 4. Create the graph -------------------------------------------------------

g_all <- graph_from_data_frame(
  d = edges_clean,
  vertices = nodes,
  directed = FALSE
)

cat("\nSummary of the full network:\n")
print(summary(g_all))

# 5. Connectivity metrics (safe for disconnected graphs) --------------------

cat("\nConnectivity Metrics:\n")

density_val <- edge_density(g_all, loops = FALSE)
components_info <- components(g_all)
avg_degree <- mean(degree(g_all))

# average.path.length() will error if unconnected unless we set unconnected = TRUE
avg_path <- average.path.length(g_all, unconnected = TRUE)

cat(sprintf("Density: %.4f\n", density_val))
cat(sprintf("Average Degree: %.2f\n", avg_degree))
cat(sprintf("Average Path Length (unconnected): %.2f\n", avg_path))
cat(sprintf("Number of connected components: %d\n", components_info$no))

# 6. Centrality -------------------------------------------------------------

centrality <- tibble(
  airport = V(g_all)$name,
  degree = degree(g_all),
  betweenness = betweenness(g_all),
  eigen = eigen_centrality(g_all)$vector,
  strength = strength(g_all, vids = V(g_all), weights = E(g_all)$flights_total)
)

top_hubs <- centrality %>% arrange(desc(degree)) %>% slice(1:10)
top_bridges <- centrality %>% arrange(desc(betweenness)) %>% slice(1:10)

cat("\nTop 10 Hubs by Degree:\n")
print(top_hubs)

cat("\nTop 10 Bridges by Betweenness:\n")
print(top_bridges)

# 7. Visualize degree distribution ------------------------------------------

# 7. Visualize degree distribution (DISCRETE & CLEAR) ------------------------

# 7. Visualize degree distribution (DISCRETE & CLEAR) ------------------------

deg_vals <- degree(g_all)

deg_df <- as.data.frame(table(deg_vals))
colnames(deg_df) <- c("Degree", "Count")
deg_df$Degree <- as.numeric(as.character(deg_df$Degree))

ggplot(deg_df, aes(x = Degree, y = Count)) +
  geom_col(fill = "steelblue", color = "white") +
  labs(
    title = "Degree Distribution of Canadian Airport Network",
    x = "Number of Connections (Degree)",
    y = "Number of Airports"
  ) +
  theme_minimal(base_size = 13)

# Optional: weighted degree (strength) distribution
strength_vals <- strength(g_all, weights = E(g_all)$flights_total)

hist(
  strength_vals,
  main = "Weighted Degree (Traffic Volume) Distribution",
  xlab = "Total Number of Flights",
  col = "steelblue",
  border = "white"
)

# 8. Visualize the giant component only (to avoid massive clutter) ----------

giant_id <- which.max(components_info$csize)
g_giant <- induced_subgraph(
  g_all,
  which(components_info$membership == giant_id)
)

plot(
  g_giant,
  vertex.size = degree(g_giant),
  vertex.label = NA,
  vertex.color = "lightblue",
  edge.width = E(g_giant)$flights_total / max(E(g_giant)$flights_total, na.rm = TRUE) * 3,
  main = "Giant Component of Canadian Airport Network"
)


# 9. Export centrality results ----------------------------------------------

write.csv(centrality, "airport_centrality.csv", row.names = FALSE)
cat("\nCentrality results saved to airport_centrality.csv\n")
