suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(igraph)
  library(purrr)
  library(stringr)
})

# Data load
edges_daily <- read_csv("edges_daily.csv", show_col_types = FALSE)
flights      <- read_csv("flights.csv",      show_col_types = FALSE)
nodes        <- read_csv("nodes.csv",        show_col_types = FALSE)
turns        <- read_csv("turnarounds.csv",  show_col_types = FALSE)


# Build directed graph, with weight = total flights and distances as inverset frequency.

edges_total <- edges_daily %>%
  group_by(dep_icao, arr_icao) %>%
  summarise(weight = sum(flights, na.rm = TRUE), .groups = "drop") %>%
  filter(weight > 0)

g <- graph_from_data_frame(edges_total,
                           directed = TRUE,
                           vertices = nodes %>% rename(name = icao))

E(g)$weight    <- edges_total$weight
E(g)$distance  <- 1 / E(g)$weight  # shortest paths / betweenness / closeness


# degree, strength, betweeness, closeness, pagerank, and eigenvector
centrality <- tibble(icao = V(g)$name) %>%
  mutate(
    degree_in   = degree(g, mode = "in"),
    degree_out  = degree(g, mode = "out"),
    strength_in = strength(g, mode = "in",  weights = E(g)$weight),
    strength_out= strength(g, mode = "out", weights = E(g)$weight),
    betweenness = betweenness(g, directed = TRUE, weights = E(g)$distance, normalized = TRUE),
    closeness   = closeness(g, mode = "out", weights = E(g)$distance, normalized = TRUE),
    pagerank    = page_rank(g, directed = TRUE, weights = E(g)$weight)$vector,
    eigen_undirected = eigen_centrality(as.undirected(g, mode = "collapse"),
                                        weights = E(as.undirected(g, "collapse"))$weight)$vector
  ) %>%
  left_join(nodes, by = c("icao" = "icao")) %>% # attach iata/city
  relocate(iata, city, .after = icao)

# z score (positive = longer than normal gate-gate time for the city pair)
robust_z <- function(x) {
  med <- median(x, na.rm = TRUE)
  madx <- mad(x, center = med, constant = 1.4826, na.rm = TRUE)
  if (is.na(madx) || madx == 0) return((x - med) / (sd(x, na.rm = TRUE) + 1e-9))
  (x - med) / madx
}

flights_anom <- flights %>%
  mutate(route = paste(dep_icao, arr_icao, sep = "->")) %>%
  group_by(dep_date, route) %>%
  mutate(
    duration_z = robust_z(duration_s)
  ) %>%
  ungroup() %>%
  select(icao24, callsign, dep_icao, arr_icao, dep_date,
         dep_time_unix, arr_time_unix, duration_s, duration_z)

# Map turnarounds to flight anomalies by aircraft (if an arriving leg is long, does the subsequent leg also run long?)
tau <- 1.0  # threshold for "meaningfully long" relative to that route-day

prev_legs <- turns %>%
  select(icao24, ground_icao, prev_arr_icao, prev_arr_time_unix) %>%
  left_join(
    flights_anom %>%
      select(icao24, arr_icao, arr_time_unix, duration_s, duration_z) %>%
      rename(prev_arr_icao = arr_icao,
             prev_arr_time_unix = arr_time_unix,
             prev_duration_s = duration_s,
             prev_z = duration_z),
    by = c("icao24", "prev_arr_icao", "prev_arr_time_unix")
  )

next_legs <- turns %>%
  select(icao24, ground_icao, next_dep_icao, next_dep_time_unix) %>%
  left_join(
    flights_anom %>%
      select(icao24, dep_icao, dep_time_unix, duration_s, duration_z) %>%
      rename(next_dep_icao = dep_icao,
             next_dep_time_unix = dep_time_unix,
             next_duration_s = duration_s,
             next_z = duration_z),
    by = c("icao24", "next_dep_icao", "next_dep_time_unix")
  )

turn_pairs <- turns %>%
  select(icao24, ground_icao, prev_arr_icao, next_dep_icao,
         prev_arr_time_unix, next_dep_time_unix, turnaround_min) %>%
  left_join(prev_legs, by = c("icao24","ground_icao","prev_arr_icao","prev_arr_time_unix")) %>%
  left_join(next_legs, by = c("icao24","ground_icao","next_dep_icao","next_dep_time_unix")) %>%
  # Keep one row per physical turnaround with matched anomalies:
  distinct(icao24, ground_icao, prev_arr_time_unix, next_dep_time_unix, .keep_all = TRUE)

# For all airports:
# Propagation rate, amplification, local correlation
N_min <- 20L  # minimum pairs to compute per-airport correlation

propag_stats <- turn_pairs %>%
  filter(!is.na(prev_z), !is.na(next_z)) %>%
  mutate(
    severe_prev = prev_z > tau,
    severe_next = next_z > tau,
    propagated  = severe_prev & severe_next
  ) %>%
  group_by(ground_icao) %>%
  summarise(
    n_pairs          = n(),
    n_severe_prev    = sum(severe_prev, na.rm = TRUE),
    n_propagated     = sum(propagated, na.rm = TRUE),
    propagation_rate = ifelse(n_severe_prev > 0, n_propagated / n_severe_prev, NA_real_),
    amplification    = ifelse(n_propagated > 0,
                              mean(next_z - prev_z, na.rm = TRUE),
                              NA_real_),
    rho_spearman     = if (n() >= N_min) suppressWarnings(cor(prev_z, next_z, method = "spearman", use = "complete.obs")) else NA_real_
  ) %>%
  ungroup() %>%
  left_join(nodes, by = c("ground_icao" = "icao")) %>%
  rename(icao = ground_icao) %>%
  relocate(iata, city, .after = icao)

# Combine into one table
airport_panel <- centrality %>%
  left_join(propag_stats, by = c("icao","iata","city")) %>%
  # traffic scale for regression controls:
  mutate(
    strength_total = strength_in + strength_out,
    degree_total   = degree_in + degree_out,
    log_strength   = log1p(strength_total)
  )

# Global spearman rank association
metrics <- c("degree_in","degree_out","degree_total",
             "strength_in","strength_out","strength_total",
             "betweenness","closeness","pagerank","eigen_undirected")

spearman_tbl <- map_dfr(metrics, function(m) {
  df <- airport_panel %>% filter(!is.na(propagation_rate), !is.na(.data[[m]]))
  if (nrow(df) < 3) {
    tibble(metric = m, rho = NA_real_, p_value = NA_real_, n = nrow(df))
  } else {
    ct <- suppressWarnings(cor.test(df$propagation_rate, df[[m]], method = "spearman", exact = FALSE))
    tibble(metric = m, rho = unname(ct$estimate), p_value = ct$p.value, n = nrow(df))
  }
})

# Also test with airport-level ρ(prev_z, next_z) as the dependent variable
spearman_tbl_rho <- map_dfr(metrics, function(m) {
  df <- airport_panel %>% filter(!is.na(rho_spearman), !is.na(.data[[m]]))
  if (nrow(df) < 3) {
    tibble(metric = m, rho = NA_real_, p_value = NA_real_, n = nrow(df), outcome = "rho(prev,next)")
  } else {
    ct <- suppressWarnings(cor.test(df$rho_spearman, df[[m]], method = "spearman", exact = FALSE))
    tibble(metric = m, rho = unname(ct$estimate), p_value = ct$p.value, n = nrow(df), outcome = "rho(prev,next)")
  }
})

# Weighted regression (does centrality predict propagation rate controlling?)
reg_df <- airport_panel %>%
  filter(!is.na(propagation_rate), !is.na(n_severe_prev), n_severe_prev > 0) %>%
  mutate(
    z_betweenness = scale(betweenness)[,1],
    z_pagerank    = scale(pagerank)[,1],
    z_closeness   = scale(closeness)[,1],
    z_degree      = scale(degree_total)[,1],
    z_strength    = scale(log_strength)[,1]
  )

if (nrow(reg_df) >= 5) {
  fit <- glm(propagation_rate ~ z_pagerank + z_betweenness + z_strength,
             data = reg_df,
             weights = n_severe_prev,
             family = gaussian())
  reg_summary <- summary(fit)
} else {
  fit <- NULL
  reg_summary <- NULL
}

# Print
print(airport_panel %>%
        select(icao, iata, city,
               propagation_rate, amplification, rho_spearman,
               degree_total, strength_total, betweenness, closeness, pagerank) %>%
        arrange(desc(propagation_rate)))

print(spearman_tbl %>% arrange(p_value))
print(spearman_tbl_rho %>% arrange(p_value))

if (!is.null(reg_summary)) {
  print(reg_summary)
}

#csv output
write_csv(airport_panel, "airport_centrality_delay_panel.csv")
write_csv(spearman_tbl, "centrality_vs_propagation_rate_spearman.csv")
write_csv(spearman_tbl_rho, "centrality_vs_prev_next_rho_spearman.csv")
