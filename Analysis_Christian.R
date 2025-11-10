library(igraph)
library(tidyverse)
library(lubridate)

edges_daily <- read_csv("edges_daily.csv")
nodes <- read_csv("nodes.csv")
flights <- read_csv("flights.csv")

edges_daily <- edges_daily %>%
  mutate(
    month = month(date),
    day_of_week = wday(date, label = TRUE)
  )

flights <- flights %>%
  mutate(
    date = as.Date(dep_date),
    month = month(date),
    day_of_week = wday(date, label = TRUE)
  )

# QUESTION
# Hub Centrality and Operational Load: Which Canadian airports serve as the most central hubs in the network based on degree centrality, and how does their structural importance correlate with operational flight volume?

g_full <- graph_from_data_frame(
  edges_daily %>% select(dep_icao, arr_icao, flights),
  directed = TRUE,
  vertices = nodes %>% rename(name = icao)
)

E(g_full)$weight <- edges_daily$flights

# Centrality metrics for all airports
airport_centrality <- tibble(
  icao = V(g_full)$name,
  
  degree = degree(g_full, mode = "all"),           
  in_degree = degree(g_full, mode = "in"),         
  out_degree = degree(g_full, mode = "out"),       
  betweenness = betweenness(g_full, directed = TRUE), 
  closeness = closeness(g_full, mode = "all"),    
  strength = strength(g_full, mode = "all"),       
  eigenvector = eigen_centrality(g_full, directed = TRUE)$vector
)

# Total departure volume per airport
departure_load <- edges_daily %>%
  group_by(dep_icao) %>%
  summarise(
    outgoing_flights = sum(flights, na.rm = TRUE),
    n_destinations = n_distinct(arr_icao),
    avg_flights_per_dest = mean(flights, na.rm = TRUE),
    .groups = "drop"
  )

arrival_load <- edges_daily %>%
  group_by(arr_icao) %>%
  summarise(
    incoming_flights = sum(flights, na.rm = TRUE),
    n_origins = n_distinct(dep_icao),
    .groups = "drop"
  )

airport_analysis <- airport_centrality %>%
  left_join(nodes, by = c("icao" = "icao")) %>%
  left_join(departure_load, by = c("icao" = "dep_icao")) %>%
  left_join(arrival_load, by = c("icao" = "arr_icao")) %>%
  mutate(
    outgoing_flights = coalesce(outgoing_flights, 0),
    incoming_flights = coalesce(incoming_flights, 0),
    total_flights = outgoing_flights + incoming_flights,
    n_destinations = coalesce(n_destinations, 0),
    n_origins = coalesce(n_origins, 0)
  ) %>%
  arrange(desc(degree))

# Top airports by centrality
cat("\nTop 10 Airports by degree Centrality\n")
print(airport_analysis %>% 
      select(iata, city, degree, in_degree, out_degree, total_flights, betweenness) %>%
      head(10))

# Correlations between centrality and operational load
cat("\nCorrelation anaylsis\n")
cor_degree_volume <- cor(airport_analysis$degree, 
                         airport_analysis$total_flights, 
                         use = "complete.obs")
cat(sprintf("Degree Centrality vs Total Flights: %.3f\n", cor_degree_volume))

cor_betweenness_volume <- cor(airport_analysis$betweenness, 
                               airport_analysis$total_flights, 
                               use = "complete.obs")
cat(sprintf("Betweenness vs Total Flights: %.3f\n", cor_betweenness_volume))

cor_strength_volume <- cor(airport_analysis$strength, 
                           airport_analysis$total_flights, 
                           use = "complete.obs")
cat(sprintf("Weighted Degree vs Total Flights: %.3f\n", cor_strength_volume))

# Network summary statistics
cat("\nNetwork summary statistics\n")
cat(sprintf("Num of airports: %d\n", vcount(g_full)))
cat(sprintf("Num of routes: %d\n", ecount(g_full)))
cat(sprintf("Network density: %.4f\n", edge_density(g_full)))
cat(sprintf("Avg degree: %.2f\n", mean(degree(g_full, mode = "all"))))
cat(sprintf("Avg path length: %.2f\n", mean_distance(g_full, directed = TRUE)))
cat(sprintf("Clustering coefficient: %.4f\n", transitivity(g_full, type = "global")))

# Degree Centrality vs Total Flights
p1 <- ggplot(airport_analysis, aes(x = degree, y = total_flights)) +
  geom_point(aes(size = betweenness), color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_text(data = airport_analysis %>% head(8), 
            aes(label = iata), vjust = -1, size = 3.5, fontface = "bold") +
  theme_minimal() +
  labs(
    title = "Airport Degree Centrality vs Operational Flight Volume",
    x = "Degree Centrality",
    y = "Total Flight Volume",
    size = "Betweenness\nCentrality"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p1)

cat("\nCorrelation analysis\n")
cor_degree_volume <- cor(airport_analysis$degree, 
                         airport_analysis$total_flights, 
                         use = "complete.obs")
cat(sprintf("Degree Centrality vs Total Flights: %.3f\n", cor_degree_volume))


# Top 10 Airports by Degree Centrality
p2 <- ggplot(airport_analysis %>% head(10), 
       aes(x = reorder(iata, degree), y = degree, fill = total_flights)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue", 
                      name = "Total\nFlights") +
  theme_minimal() +
  labs(
    title = "Top 10 Most Connected Airports (Hub Centrality)",
    x = "Airport",
    y = "Degree Centrality"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p2)

# In-Degree vs Out-Degree
p3 <- ggplot(airport_analysis, aes(x = in_degree, y = out_degree)) +
  geom_point(aes(size = total_flights, color = degree), alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_text(data = airport_analysis %>% head(8), 
            aes(label = iata), vjust = -1, size = 3) +
  scale_color_gradient(low = "yellow", high = "green", name = "Total\nDegree") +
  scale_size_continuous(name = "Flight\nVolume") +
  theme_minimal() +
  labs(
    title = "Directional Connectivity: Incoming vs Outgoing Connections",
    x = "In-Degree",
    y = "Out-Degree"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p3)

# Hub Classification
airport_analysis <- airport_analysis %>%
  mutate(
    hub_type = case_when(
      degree >= quantile(degree, 0.75) & total_flights >= quantile(total_flights, 0.75) ~ "Major Hub",
      degree >= quantile(degree, 0.75) & total_flights < quantile(total_flights, 0.75) ~ "Connector Hub",
      degree < quantile(degree, 0.75) & total_flights >= quantile(total_flights, 0.75) ~ "High Traffic Spoke",
      TRUE ~ "Regional Airport"
    )
  )

p4 <- ggplot(airport_analysis, aes(x = degree, y = total_flights, color = hub_type)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(data = airport_analysis %>% filter(hub_type %in% c("Major Hub", "Connector Hub")), 
            aes(label = iata), vjust = -1, size = 3) +
  scale_color_manual(values = c("Major Hub" = "red", 
                                 "Connector Hub" = "orange",
                                 "High Traffic Spoke" = "blue",
                                 "Regional Airport" = "gray")) +
  theme_minimal() +
  labs(
    title = "Airport Hub Classification",
    x = "Degree Centrality",
    y = "Total Flight Volume",
    color = "Hub Type"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "right")

print(p4)

# Hub types
cat("\nSummary of hub types\n")
hub_summary <- airport_analysis %>%
  group_by(hub_type) %>%
  summarise(
    n_airports = n(),
    avg_degree = mean(degree),
    avg_flights = mean(total_flights),
    total_system_flights = sum(total_flights)
  ) %>%
  arrange(desc(avg_degree))

print(hub_summary)

# Linear regression
cat("\nLinear regression: Degree Centrality predicting Flight Volume\n")
lm_model <- lm(total_flights ~ degree + betweenness, data = airport_analysis)
print(summary(lm_model))