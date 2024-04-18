
# Installing packages -----------------------------------------------------
install.packages("igraph")
install.packages("ggnetwork")
install.packages("ggplot2")
install.packages("dplyr")


library(igraph)
library(ggnetwork)
library(sna)
library(ggplot2)
library(network)
library(plotly)
library(dplyr)
library(tidyr)  # For data manipulation
library(stringr)

# Reading csv file --------------------------------------------------------


# Define the file paths for the CSV files in the Downloads folder
catalogue_file <- "~/Downloads/extract_21022024/cast_catalogue_21022023.csv"
charities_file <- "~/Downloads/extract_21022024/cast_charities_21022023.csv"
prev_smr_file <- "~/Downloads/extract_21022024/cast_prev_smr_27032023.csv"

# Read data from the CSV files
cast_catalogue <- read.csv(catalogue_file)
cast_charities <- read.csv(charities_file)
cast_prev_smr <- read.csv(prev_smr_file)

View(cast_catalogue)
View(cast_charities)
View(cast_prev_smr)

# Identify common phecodes across all three files
common_phecodes <- Reduce(intersect, list(cast_catalogue$phecode, cast_charities$phecode, cast_prev_smr$phecode))

# Filter data frames to include only rows with common phecodes
catalogue_filtered <- cast_catalogue[cast_catalogue$phecode %in% common_phecodes, ]
cast_charities_filtered <- cast_charities[cast_charities$phecode %in% common_phecodes, ]
cast_prev_smr_filtered <- cast_prev_smr[cast_prev_smr$phecode %in% common_phecodes, ]

# Now, cast_catalogue_filtered, cast_charities_filtered, and cast_prev_smr_filtered showing 123 entries across all files
# contain only the rows with phecodes that are common across all three files.

str(cast_charities_filtered)

# Cleaning the data -------------------------------------------------------

# Define a function to replace 'narrow' with 'Narrow'
replace_narrow <- function(x) {
  str_replace(x, fixed('narrow'), 'Narrow')
}

# Apply the replacement function to specified columns using mutate_at()
cast_charities_filtered <- cast_charities_filtered %>%
  mutate_at(vars(Focus1:Focus5), ~ replace_narrow(.))

# Check unique values in the specified columns
unique_values <- sapply(cast_charities_filtered[, c("Focus1", "Focus2", "Focus3", "Focus4", "Focus5")], unique)
print(unique_values)




# Basic visual showing the count of charities -----------------------------

# Prepare data for visualization (count occurrences of each charity for each health condition)
charity_counts <- table(stack(cast_charities_filtered[, c("Charity.1", "Charity.name.2", "Charity.name.3", "Charity.name.4", "Charity.name.5")]))
charity_counts <- as.data.frame(charity_counts)

# Plot a bar chart showing the top charities by health condition
ggplot(charity_counts, aes(x = ind, y = Freq, fill = ind)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Health Condition", y = "Count of Charities", title = "Top Charities by Health Condition") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Network Diagram of Health Conditions and Charities ----------------------

# Extract relevant columns
charities_1 <- cast_charities_filtered[, c("nicename", "Charity.1", "Charity.name.2", "Charity.name.3", "Charity.name.4", "Charity.name.5")]

# Create a vector of unique nodes (health conditions and charities)
nodes <- unique(c(unlist(charities_1[, 1]), unlist(charities_1[, 2:6])))

# Initialize an empty edge list
edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)

# Iterate over each row of the dataframe to create edges
for (i in 1:nrow(charities_1)) {
  row_edges <- expand.grid(from = charities_1[i, 1], to = unlist(charities_1[i, 2:6]))
  edges <- rbind(edges, row_edges)
}

# Remove NA values and duplicates
edges <- na.omit(edges)
edges <- unique(edges)
# Create an igraph graph object
g <- graph_from_data_frame(edges, directed = FALSE)

# Set node attributes (e.g., color, size) based on node type (health condition or charity)
node_type <- ifelse(nodes %in% cast_charities_filtered$nicename, "health_condition", "charity")
node_color <- ifelse(node_type == "health_condition", "skyblue", "orange")

# Plot the network graph
plot(g, layout = layout_with_fr(g), 
     vertex.size = 8, vertex.label = nodes, 
     vertex.color = node_color, vertex.frame.color = "gray", 
     edge.color = "gray", edge.arrow.size = 0.5,
     main = "Network Diagram of Health Conditions and Charities")


# Error Filter speciality to focus on Medical Oncology, cardio, neuro -----------

# Install and load required packages
install.packages(c("igraph", "dplyr"))
library(igraph)
library(dplyr)

# Define target medical specialties
target_specialties <- c("Cardiology", "Medical Oncology", "Neurology")

# Filter catalogue_filtered to include only phecodes associated with the target specialties
filtered_phecodes <- catalogue_filtered %>%
  filter(speciality %in% target_specialties) %>%
  pull(phecode)

# Filter cast_charities_filtered to include only relevant diseases (nicenames) and charities (columns Charity.1 to Charity.name.5)
filtered_data <- cast_charities_filtered %>%
  filter(phecode %in% filtered_phecodes) %>%
  select(nicename, Charity.1, Charity.name.2, Charity.name.3, Charity.name.4, Charity.name.5)

# Create a vector of unique nodes (health conditions and charities)
nodes <- unique(c(as.character(unlist(filtered_data[, "nicename"])), 
                  as.character(unlist(filtered_data[, 2:6]))))

# Create an igraph graph object
g <- graph.empty(n = length(nodes), directed = FALSE)

# Add vertex names to the graph
V(g)$name <- nodes

# Iterate over each row of the filtered data to create edges between diseases and associated charities
for (i in 1:nrow(filtered_data)) {
  disease <- as.character(filtered_data[i, "nicename"])
  charities <- as.character(unlist(filtered_data[i, 2:6]))
  
  # Check if disease is a valid vertex in the graph
  if (disease %in% nodes) {
    # Iterate over each charity and add edges if valid vertices in the graph
    for (charity in charities) {
      if (charity %in% nodes) {
        # Add edges between the disease node and associated charity nodes
        g <- add_edges(g, c(disease, charity))
      }
    }
  }
}

# Set node attributes based on node type (health condition or charity) and specialty
node_type <- ifelse(nodes %in% filtered_data$nicename, "health_condition", "charity")
node_color <- ifelse(node_type == "health_condition", "lightblue", "orange")

# Color-code nodes based on medical specialty
specialty_colors <- ifelse(nodes %in% as.character(filtered_data$nicename), 
                           ifelse(filtered_data$nicename %in% target_specialties, "lightblue", "orange"), 
                           "gray")

# Plot the network graph
plot(g, 
     layout = layout_with_fr(g),  # Fruchterman-Reingold layout
     vertex.size = 8,             # Node size
     vertex.label.cex = 0.8,      # Label size
     vertex.color = specialty_colors,   # Node color based on specialty
     vertex.label.color = "black",# Label color
     edge.color = "gray",         # Edge color
     edge.arrow.size = 0.5,       # Arrow size
     main = "Network Diagram for Multiple Specialties")

# Legend for node colors
legend_items <- unique(specialty_colors)
legend_labels <- c("Health Condition", "Charity", target_specialties)

legend("topright", legend = legend_labels, fill = legend_items, title = "Node Type")


# Caterpillar plot between prevalence + disease ---------------------------

# Merge cast_charities_filtered and cast_prev_smr_filtered by phecode
merged_data <- inner_join(cast_charities_filtered, cast_prev_smr_filtered, by = "phecode")

# Filter out rows with missing prevalence data
merged_data <- merged_data %>%
  filter(!is.na(SMR_prev) & !is.na(ci_left_SMR_prev) & !is.na(ci_right_SMR_prev))

# Create the caterpillar plot using ggplot2
ggplot(merged_data, aes(x = nicename, y = SMR_prev)) +
  geom_point(size = 3, color = "blue") +  # Point estimates
  geom_errorbar(aes(ymin = ci_left_SMR_prev, ymax = ci_right_SMR_prev), width = 0.2, color = "purple") +  # Confidence intervals
  coord_flip() +  # Flip the axes for horizontal plot
  labs(
    x = "Disease (Nicename)",
    y = "Mortality Rates",
    title = "Caterpillare Plot Comparison of Mortality Rates to Diseases",
    caption = "Error bars represent 95% confidence intervals"
  ) +
  theme_minimal()  # Minimal theme for better readability





# Caterpillar plot: Mortality vs Speciality -------------------------------

# Merge cast_prev_smr_filtered with catalogue_filtered by phecode
merged_data <- inner_join(cast_prev_smr_filtered, catalogue_filtered, by = "phecode")

# Filter out rows with missing data (if necessary)
merged_data <- merged_data %>%
  filter(!is.na(SMR_prev) & !is.na(speciality))  # Filter out rows with missing SMR_prev or speciality

# View the merged dataset
head(merged_data)

# Load required package
library(ggplot2)

# Create a caterpillar plot of disease prevalence by specialty
ggplot(merged_data, aes(x = speciality, y = SMR_prev)) +
  geom_point(size = 3, color = "purple") +  # Point estimates
  geom_errorbar(aes(ymin = SMR_prev - ci_left_SMR_prev, ymax = SMR_prev + ci_right_SMR_prev), width = 0.2, color = " blue") +  # Confidence intervals
  coord_flip() +  # Flip the axes for horizontal plot
  labs(
    x = "Specialty",
    y = "Mortality Estimate",
    title = "Mortality rates by Specialty",
    caption = "Error bars represent 95% confidence intervals"
  ) +
  theme_minimal()  # Minimal theme for better readability

# Different Visualisations of igraph --------------------------------------

# Extract unique disease names from the dataframe
disease_names <- unique(cast_charities_filtered$nicename)

# Create an empty graph
g <- graph.empty()

# Add disease nodes to the graph
g <- add_vertices(g, n = length(disease_names), name = disease_names, type = "disease")

# Iterate over each row to add charity nodes and edges
for (i in 1:nrow(cast_charities_filtered)) {
  disease <- cast_charities_filtered[i, "nicename"]
  
  # Iterate over Charity columns (Charity.1 to Charity.5)
  for (j in 1:5) {
    charity_col <- paste0("Charity.", j)
    charity <- cast_charities_filtered[i, charity_col]
    focus_col <- paste0("Focus", j)
    focus <- cast_charities_filtered[i, focus_col]
    
    # Check if charity is not NA and is a non-empty string
    if (!is.na(charity) && !is.null(charity) && nchar(trimws(charity, "both")) > 0 &&
        !is.na(focus) && !is.null(focus) && nchar(trimws(focus, "both")) > 0) {
      focus <- tolower(focus)
      edge_weight <- ifelse(focus == "narrow", 1, 2)  # Adjust weights based on focus
      
      # Add charity node if it doesn't already exist
      if (!charity %in% V(g)$name) {
        g <- add_vertices(g, n = 1, name = charity, type = "charity")
      }
      
      # Add edge between disease and charity
      g <- add_edges(g, edges = c(disease, charity), weight = edge_weight)}}}

# Fruchterman-Reingold layout ---------------------------------------------


# Plot the graph
plot(g,
     layout = layout_with_fr(g),  # Fruchterman-Reingold layout
     vertex.label.dist = 0.5,     # Adjust label distance from nodes
     vertex.label.cex = 0.8,      # Adjust label size
     vertex.color = ifelse(V(g)$type == "disease", "lightblue", "lightgreen"),  # Node color by type
     vertex.frame.color = "gray", # Node border color
     edge.width = E(g)$weight,    # Edge width by weight
     main = "Disease-Charity Relationships:Focus = Narrow")  # Main title for the plot

# Add legend
legend("topright", legend = c("Disease", "Charity"),
       col = c("lightblue", "lightgreen"), pch = 19,
       title = "Node Type", cex = 0.8, bty = "n")

# Kamada-Kawai layout -----------------------------------------------------


plot(g,
     layout = layout_with_kk(g),  # Kamada-Kawai layout
     vertex.label.dist = 0.5,
     vertex.label.cex = 0.8,
     vertex.color = ifelse(V(g)$type == "disease", "lightblue", "lightgreen"),
     vertex.frame.color = "gray",
     edge.width = E(g)$weight,
     main = "Disease-Charity Relationships (Kamada-Kawai Layout)")

# Add legend
legend("topright", legend = c("Disease", "Charity"),
       col = c("lightblue", "lightgreen"), pch = 19,
       title = "Node Type", cex = 0.8, bty = "n")

# Random layout -----------------------------------------------------------


plot(g,
     layout = layout_randomly(g),  # Random layout
     vertex.label.dist = 0.5,
     vertex.label.cex = 0.8,
     vertex.color = ifelse(V(g)$type == "disease", "purple", "lightblue"),
     vertex.frame.color = "gray",
     edge.width = E(g)$weight,
     main = "Disease-Charity Relationships (Random Layout)")

# Add legend
legend("topright", legend = c("Disease", "Charity"),
       col = c("purple", "lightblue"), pch = 19,
       title = "Node Type", cex = 0.8, bty = "n")

# Circular layout ---------------------------------------------------------


plot(g,
     layout = layout_in_circle(g),  # Circular layout
     vertex.label.dist = 0.5,
     vertex.label.cex = 0.8,
     vertex.color = ifelse(V(g)$type == "disease", "lightblue", "lightgreen"),
     vertex.frame.color = "gray",
     edge.width = E(g)$weight,
     main = "Disease-Charity Relationships (Circular Layout)")


# Add legend
legend("topright", legend = c("Disease", "Charity"),
       col = c("lightblue", "lightgreen"), pch = 19,
       title = "Node Type", cex = 0.8, bty = "n")


# #Bar chart showing the charities, disease and Focus - basic visu --------

# Reshape the data for plotting (from wide to long format)
charity_long <- cast_charities_filtered %>%
  select(nicename, Focus1:Focus5) %>%
  pivot_longer(cols = starts_with("Focus"), names_to = "Focus_Category", values_to = "Focus_Level") %>%
  filter(!is.na(Focus_Level))  # Filter out NA values

# Create a bar plot with color-coded focus levels within disease categories
ggplot(charity_long, aes(x = nicename, fill = Focus_Level)) +
  geom_bar() +
  labs(title = "Disease Focus of Charities",
       x = "Disease",
       y = "Count of Charities",
       fill = "Focus Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Broad" = "skyblue", "Narrow" = "salmon", "NA" = "grey"),
                    na.value = "grey")  # Customize fill colors


# Stacked Bar Chart for Disease Focus Levels ------------------------------
charity_long <- cast_charities_filtered %>%
  select(nicename, Focus1:Focus5) %>%
  pivot_longer(cols = starts_with("Focus"), names_to = "Focus_Category", values_to = "Focus_Level") %>%
  filter(!is.na(Focus_Level))  # Filter out NA values

# Count the proportion of Broad and Narrow focus levels by disease category
focus_counts <- charity_long %>%
  group_by(nicename, Focus_Level) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))  # Calculate proportion within each disease category

# Create a stacked bar chart
ggplot(focus_counts, aes(x = nicename, y = Proportion, fill = Focus_Level)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Proportion of Broad vs Narrow Focus by Disease",
       x = "Disease",
       y = "Proportion",
       fill = "Focus Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Broad" = "skyblue", "Narrow" = "salmon"))  # Customize fill colors


# Heatmap Charity Distribution by Specialty and Disease ---------------------------

# Standardize focus levels function
standardize_focus <- function(x) {
  tolower(gsub("\\s+", "", x))  # Remove spaces and convert to lowercase
}

# Merge DataFrames based on common identifier (phecode)
merged_data <- cast_charities_filtered %>%
  inner_join(catalogue_filtered, by = "phecode")

# Filter and reshape the merged data for plotting
merged_long <- merged_data %>%
  select(nicename, Focus1:Focus5, speciality) %>%
  pivot_longer(cols = starts_with("Focus"), names_to = "Focus_Category", values_to = "Focus_Level") %>%
  filter(!is.na(Focus_Level))  # Filter out NA values

# Apply the standardize_focus function to Focus_Level
merged_long$Focus_Level <- sapply(merged_long$Focus_Level, standardize_focus)

# Count the number of charities for each combination of specialty, disease, and focus level
heatmap_data <- merged_long %>%
  group_by(speciality, nicename, Focus_Level) %>%
  summarise(Count = n())

# Create a heatmap
ggplot(heatmap_data, aes(x = speciality, y = nicename, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Charity Distribution by Specialty and Disease",
       x = "Specialty",
       y = "Disease",
       fill = "Count of Charities") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")  # Adjust legend position



# PCA ---------------------------------------------------------------------

# Perform k-means clustering on the PCA scores
set.seed(123)  # Set seed for reproducibility
num_clusters <- 3  # Specify the number of clusters
cluster_results <- kmeans(pca_scores, centers = num_clusters)

# Extract cluster assignments
cluster_assignments <- cluster_results$cluster

# Convert cluster assignments to factor for color coding
cluster_colors <- factor(cluster_assignments)

# Plot PCA results with color-coded clusters
library(ggplot2)

# Create a scatter plot of PCA scores with color-coded clusters
ggplot(pca_scores, aes(x = PC1, y = PC2, color = cluster_colors, label = rownames(pca_scores))) +
  geom_point(size = 3) +
  geom_text(hjust = 0, vjust = 0) +  # Add labels to data points
  labs(title = "PCA Plot with Color-Coded Clusters",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  scale_color_manual(values = palette()[1:num_clusters]) +  # Customize cluster colors
  theme_minimal()





# Population prevalence visualisation -------------------------------------------
# Load required libraries
library(dplyr)
library(ggplot2)

# Assume all necessary DataFrames have been read into R: cast_charities_filtered, catalogue_filtered, cast_prev_smr_filtered

# Merge DataFrames based on common identifier (phecode)
merged_data <- cast_charities_filtered %>%
  inner_join(catalogue_filtered, by = "phecode") %>%
  inner_join(cast_prev_smr_filtered, by = "phecode")  # Merge with population rates data

# Select relevant columns and standardize focus levels
merged_data <- merged_data %>%
  select(speciality, Focus1, prev_pop, ci_left_prev_pop, ci_right_prev_pop) %>%
  mutate(Focus1 = ifelse(tolower(gsub("\\s+", "", Focus1)) %in% c("narrow", "na"), "Narrow", "Broad"))

# Create a scatter plot with error bars
ggplot(merged_data, aes(x = Focus1, y = prev_pop)) +
  geom_point(aes(color = speciality), size = 3, alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_left_prev_pop, ymax = ci_right_prev_pop), width = 0.2) +
  labs(title = "Specialty Charities' Focus and Prevalence Rates",
       x = "Focus Level",
       y = "Population Rate",
       color = "Specialty") +
  scale_color_viridis_d() +  # Use a color scale for specialties
  theme_minimal()




# Population prevalence Distribution by Specialty and Focus Level  --------


# Merge DataFrames based on common identifier (phecode)
merged_data <- cast_charities_filtered %>%
  inner_join(catalogue_filtered, by = "phecode") %>%
  inner_join(cast_prev_smr_filtered, by = "phecode")  # Merge with population rates data

# Select relevant columns and standardize focus levels
merged_data <- merged_data %>%
  select(speciality, Focus1, prev_pop, ci_left_prev_pop, ci_right_prev_pop) %>%
  mutate(Focus_Level = ifelse(tolower(gsub("\\s+", "", Focus1)) %in% c("narrow", "na"), "Narrow", "Broad"))

# Create a box plot with error bars for each focus level and specialty
ggplot(merged_data, aes(x = speciality, y = prev_pop, fill = Focus_Level)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +  # Remove outliers and adjust transparency
  geom_errorbar(aes(ymin = ci_left_prev_pop, ymax = ci_right_prev_pop), width = 0.2, position = position_dodge(0.75)) +  # Add error bars
  labs(title = "Population prevalence Distribution by Specialty and Focus Level (Box Plot)",
       x = "Specialty",
       y = "Population Rate",
       fill = "Focus Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")  # Adjust legend position



# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)  # For data manipulation

# Assume all necessary DataFrames have been read into R: cast_charities_filtered, catalogue_filtered, cast_prev_smr_filtered

# Merge DataFrames based on common identifier (phecode)
merged_data <- cast_charities_filtered %>%
  inner_join(catalogue_filtered, by = "phecode") %>%
  inner_join(cast_prev_smr_filtered, by = "phecode")  # Merge with population rates data

# Select relevant columns and standardize focus levels
focus_columns <- c("Focus1", "Focus2", "Focus3", "Focus4", "Focus5")

merged_data <- merged_data %>%
  select(speciality, all_of(focus_columns), prev_pop) %>%
  pivot_longer(cols = starts_with("Focus"), names_to = "Focus_Category", values_to = "Focus_Level") %>%
  mutate(Focus_Level = ifelse(tolower(gsub("\\s+", "", Focus_Level)) %in% c("narrow", "na"), "Narrow", "Broad"))

# Create a grouped box plot for each focus level and specialty
ggplot(merged_data, aes(x = speciality, y = prev_pop, fill = Focus_Level)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA, position = position_dodge(width = 0.9)) +  # Remove outliers and adjust position
  labs(title = "Population Rate Distribution by Specialty and Focus Level",
       x = "Specialty",
       y = "Population Rate",
       fill = "Focus Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")  # Adjust legend position


# #Bar chart --------------------------------------------------------------

 
# Merge DataFrames based on common identifier (phecode)
merged_data <- cast_charities_filtered %>%
  inner_join(catalogue_filtered, by = "phecode") %>%
  inner_join(cast_prev_smr_filtered, by = "phecode")  # Merge with population rates data

# Select relevant columns and standardize focus levels
merged_data <- merged_data %>%
  select(speciality, Focus1, prev_pop, ci_left_prev_pop, ci_right_prev_pop) %>%
  mutate(Focus_Level = ifelse(tolower(gsub("\\s+", "", Focus1)) %in% c("narrow", "na"), "Narrow", "Broad"))

# Calculate mean population rate and confidence interval for each focus level and specialty
summary_data <- merged_data %>%
  group_by(speciality, Focus_Level) %>%
  summarise(Mean_Population_Rate = mean(prev_pop),
            CI_Left = min(ci_left_prev_pop),
            CI_Right = max(ci_right_prev_pop)) %>%
  ungroup()

# Create a grouped bar chart
ggplot(summary_data, aes(x = speciality, y = Mean_Population_Rate, fill = Focus_Level)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_errorbar(aes(ymin = CI_Left, ymax = CI_Right), position = position_dodge(width = 0.6), width = 0.3) +
  labs(title = " Population prevalence by Specialty and Focus Level",
       x = "Specialty",
       y = "Mean Population Rate",
       fill = "Focus Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")  # Adjust legend position



# Clusters using igraph ---------------------------------------------------


#This is to show the Clusters between Disease Charity but separated using the speciality 

# Extract unique disease names and charities from the dataframe
disease_names <- unique(cast_charities_filtered$nicename)

# Create an empty directed graph
g <- graph.empty(directed = TRUE)

# Add disease nodes to the graph
g <- add_vertices(g, n = length(disease_names), name = disease_names, type = "disease")

# Iterate over each row to add charity nodes and edges
for (i in 1:nrow(cast_charities_filtered)) {
  disease <- cast_charities_filtered[i, "nicename"]
  
  # Iterate over Charity columns (Charity.1 to Charity.5)
  for (j in 1:5) {
    charity_col <- paste0("Charity.", j)
    charity <- cast_charities_filtered[i, charity_col]
    focus_col <- paste0("Focus", j)
    focus <- cast_charities_filtered[i, focus_col]
    
    # Check if charity and focus are not NA and non-empty strings
    if (!is.na(charity) && !is.null(charity) && nchar(trimws(charity, "both")) > 0 &&
        !is.na(focus) && !is.null(focus) && nchar(trimws(focus, "both")) > 0) {
      focus <- tolower(trimws(focus))
      edge_weight <- ifelse(focus == "narrow", 1, 2)  # Adjust weights based on focus
      
      # Add charity node if it doesn't already exist
      if (!charity %in% V(g)$name) {
        g <- add_vertices(g, n = 1, name = charity, type = "charity")
      }
      
      # Add directed edge from disease to charity with focus attribute
      g <- add_edges(g, edges = c(disease, charity), weight = edge_weight, focus = focus)}}}

# Perform community detection (clustering) using the walktrap algorithm
cluster_membership <- cluster_walktrap(g)

# Get unique cluster IDs and their sizes
clusters <- sizes(cluster_membership)

# Filter clusters to include only those with significant size (adjust threshold as needed)
significant_clusters <- clusters[clusters > 1]  # Consider clusters with at least 2 nodes

# Extract node names for significant clusters
significant_nodes <- names(V(g))[membership(cluster_membership) %in% names(significant_clusters)]

# Subgraph with significant clusters
g_sub <- induced_subgraph(g, significant_nodes)

# Determine node colors based on type (disease vs. charity) and edge colors based on focus
V(g_sub)$color <- ifelse(V(g_sub)$type == "disease", "lightblue", "lightgreen")
E(g_sub)$color <- ifelse(E(g_sub)$focus == "narrow", "red", "blue")

# Plot the graph with colored nodes and edges
plot(g_sub,
     layout = layout_with_fr(g_sub),  # Fruchterman-Reingold layout for subgraph
     vertex.label.dist = 0.5,         # Adjust label distance from nodes
     vertex.label.cex = 0.8,           # Adjust label size
     vertex.color = V(g_sub)$color,    # Node color by type (disease vs. charity)
     vertex.frame.color = "gray",      # Node border color
     edge.width = E(g_sub)$weight,     # Edge width by weight
     edge.color = E(g_sub)$color,      # Edge color by focus (narrow vs. broad)
     main = "Significant Disease-Charity Clusters")  # Main title for the plot

# Add legend for node types and edge focus, position it further away from the plot
legend("topright", legend = c("Disease (Light Blue)", "Charity (Light Green)", "Narrow Focus (Red)", "Broad Focus (Blue)"),
       fill = c("lightblue", "lightgreen", "red", "blue"), cex = 0.8, bty = "n", title = "Legend", 
       x = max(igraph::layout_with_fr(g_sub)[,1]) + 1, y = max(igraph::layout_with_fr(g_sub)[,2]) + 1)



# Clusters between Disease Charity + specialty using 2 files igraph  ---------------------------------


#This is to show the Clusters between Disease Charity but separated using the speciality 
# Extract unique specialties from the catalogue_filtered dataframe
specialties <- unique(catalogue_filtered$speciality)

# Create an empty directed graph
g <- graph.empty(directed = TRUE)

# Add specialty nodes to the graph
g <- add_vertices(g, n = length(specialties), name = specialties, type = "specialty")

# Iterate over each row in cast_charities_filtered to add disease nodes and edges
for (i in 1:nrow(cast_charities_filtered)) {
  disease <- cast_charities_filtered[i, "nicename"]
  phecode <- cast_charities_filtered[i, "phecode"]
  
  # Find the corresponding specialty for the disease based on phecode
  specialty <- catalogue_filtered$speciality[catalogue_filtered$phecode == phecode]
  
  if (length(specialty) > 0) {
    specialty <- unique(specialty)  # Ensure unique specialty
    
    # Add disease node if it doesn't already exist
    if (!disease %in% V(g)$name) {
      g <- add_vertices(g, n = 1, name = disease, type = "disease")}
    
    # Add edges from disease to specialty
    g <- add_edges(g, edges = c(disease, specialty), weight = 1)
    
    # Iterate over Charity columns (Charity.1 to Charity.5)
    for (j in 1:5) {
      charity_col <- paste0("Charity.", j)
      charity <- cast_charities_filtered[i, charity_col]
      focus_col <- paste0("Focus", j)
      focus <- cast_charities_filtered[i, focus_col]
      
      # Check if charity and focus are not NA and non-empty strings
      if (!is.na(charity) && !is.null(charity) && nchar(trimws(charity, "both")) > 0 &&
          !is.na(focus) && !is.null(focus) && nchar(trimws(focus, "both")) > 0) {
        focus <- tolower(trimws(focus))
        edge_weight <- ifelse(focus == "narrow", 1, 2)  # Adjust weights based on focus
        
        # Add charity node if it doesn't already exist
        if (!charity %in% V(g)$name) {
          g <- add_vertices(g, n = 1, name = charity, type = "charity")}
        
        # Add directed edge from specialty to charity with focus attribute
        g <- add_edges(g, edges = c(specialty, charity), weight = edge_weight, focus = focus)}}}}

# Perform community detection (clustering) using the walktrap algorithm
cluster_membership <- cluster_walktrap(g)

# Get unique cluster IDs and their sizes
clusters <- sizes(cluster_membership)

# Filter clusters to include only those with significant size (adjust threshold as needed)
significant_clusters <- clusters[clusters > 1]  # Consider clusters with at least 2 nodes

# Extract node names for significant clusters
significant_nodes <- names(V(g))[membership(cluster_membership) %in% names(significant_clusters)]

# Subgraph with significant clusters
g_sub <- induced_subgraph(g, significant_nodes)

# Determine node colors based on type (disease vs. charity) and edge colors based on focus
V(g_sub)$color <- ifelse(V(g_sub)$type == "disease", "lightblue", "lightgreen")
E(g_sub)$color <- ifelse(E(g_sub)$focus == "narrow", "red", "blue")

# Plot the graph with colored nodes and edges
plot(g_sub,
     layout = layout_with_fr(g_sub),  # Fruchterman-Reingold layout for subgraph
     vertex.label.dist = 0.5,         # Adjust label distance from nodes
     vertex.label.cex = 0.8,           # Adjust label size
     vertex.color = V(g_sub)$color,    # Node color by type (disease vs. charity)
     vertex.frame.color = "gray",      # Node border color
     edge.width = E(g_sub)$weight,     # Edge width by weight
     edge.color = E(g_sub)$color,      # Edge color by focus (narrow vs. broad)
     main = "Significant Specialty-Charity Clusters")  # Main title for the plot

# Add legend for node types and edge focus, position it outside the plot area
legend("topright", legend = c("Specialty (Light Blue)", "Charity (Light Green)", "Narrow Focus (Red)", "Broad Focus (Blue)"),
       fill = c("lightblue", "lightgreen", "red", "blue"), cex = 0.8, bty = "n", title = "Legend", 
       x = max(layout_with_fr(g_sub)[,1]) + 1, y = max(layout_with_fr(g_sub)[,2]) + 1)

















# These code below give errors --------------------------------------------



summary(cast_prev_smr$prev_pop)
summary(cast_charities$nicename) 


library(qgraph)

# Check the unique values of the charity variable
unique_charities <- unique(cast_charities$nicename)

# Print unique charities
print(unique_charities)

# Create a dataframe with the relevant variables
data <- data.frame(Prevalence = cast_prev_smr$prev_pop, Charity = cast_charities$nicename)

# Create a correlation matrix
correlation_matrix <- cor(data)

# Plot the caterpillar network
qgraph(correlation_matrix, layout = "spring", labels = colnames(correlation_matrix), label.scale = FALSE)




library(ggnetwork)
library(ggplot2)


# Check the unique values of the charity variable
unique_charities <- unique(cast_charities$nicename)

# Create a dataframe with the relevant variables
data <- data.frame(Prevalence = cast_prev_smr_filtered$prev_pop, Disease = cast_charities_filtered$Charity.1)

# Create a dataframe for nodes (charities)
nodes <- data.frame(id = 1:length(unique_charities), label = unique_charities)

# Create a dataframe for edges (connections between charities and prevalence)
edges <- data.frame(from = rep(1, length(unique_charities)), to = 1:length(unique_charities), Prevalence = NA)

# Create a ggplot object
gg <- ggplot() +
  geom_edges(data = edges, aes(x = from_id, y = to_id, color = Prevalence), 
             arrow = arrow(type = "closed", length = unit(0.15, "inches")), size = 0.5) +
  geom_nodes(data = nodes, aes(x = x, y = y, label = label), color = "skyblue", size = 5) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_void()



# ggnetwork errrors -------------------------------------------------------


# Load required libraries
library(network)
library(ggplot2)
library(ggrepel)

# Assuming `cast_charities_filtered` is your data containing charity information
# Replace this with your actual dataset or load it from your source

# Extract relevant columns for charities and focus categories
charity_cols <- c("Charity.1", "Charity.name.2", "Charity.name.3", "Charity.name.4", "Charity.name.5")
focus_cols <- c("Focus1", "Focus2", "Focus3", "Focus4", "Focus5")

# Initialize an empty network
n <- network.initialize(0, directed = FALSE)

# Extract unique diseases (nicenames) and populate as nodes
diseases <- unique(cast_charities_filtered$nicename)
add.vertices(n, diseases)

# Iterate over each row in cast_charities_filtered to populate the network
for (i in 1:nrow(cast_charities_filtered)) {
  # Extract valid charities and focuses from the current row
  charities <- unlist(cast_charities_filtered[i, charity_cols], use.names = FALSE)
  focuses <- unlist(cast_charities_filtered[i, focus_cols], use.names = FALSE)
  disease <- cast_charities_filtered$nicename[i]  # Current disease (nicename)
  
  # Remove NA values and ensure non-empty values
  valid_charities <- na.omit(charities)
  valid_focuses <- na.omit(focuses)
  
  # Connect each valid charity to its corresponding valid focus category and disease
  for (charity in valid_charities) {
    for (focus in valid_focuses) {
      if (!is.na(charity) && !is.na(focus)) {
        # Add edges only if the vertex names are not NA or empty
        if (vertex.exists(n, charity) && vertex.exists(n, focus)) {
          add.edge(n, charity, focus)  # Connect charity to focus
          add.edge(n, disease, charity)  # Connect disease to charity
        }
      }
    }
  }
  }

# Plot the network graph
ggn <- ggnetwork(n)
ggplot(ggn, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50") +
  geom_nodes() +  # Customize node aesthetics as needed
  theme_blank()


# Load required libraries
library(network)
library(ggnetwork)
library(ggplot2)
library(ggrepel)  # For geom_nodetext_repel and geom_nodelabel_repel

# Assuming `cast_charities_filtered` is your data containing charity information
# Replace this with your actual dataset or load it from your source
charity_data <- cast_charities_filtered  # Replace `cast_charities_filtered` with your data frame

# Define the number of charities per disease and focus columns
num_charities <- 5
num_focus <- 5

# Initialize an empty network
n <- network.initialize(0, directed = FALSE)

# Iterate over each row in the charity data to gather names and focus categories
for (i in 1:nrow(charity_data)) {
  # Extract the charity names and focus categories for this row
  charities <- unlist(charity_data[i, paste0("Charity.", j)], use.names = FALSE)
  focuses <- unlist(charity_data[i, paste0("Focus", j)], use.names = FALSE)
  
  # Filter out NA values in both charities and focuses
  valid_charities <- charities[!is.na(charities) & charities != ""]
  valid_focuses <- focuses[!is.na(focuses) & focuses != ""]
  
  # Check if valid_charities is not empty
  if (length(valid_charities) > 0) {
    # Add unique charities as vertices to the network
    n <- add.vertices(n, unique(valid_charities))
    
    # Add edges between each charity and its corresponding focus category
    for (j in seq_along(valid_charities)) {
      if (!is.na(valid_focuses[j]) && valid_focuses[j] != "") {
        add.edge(n, valid_charities[j], valid_focuses[j])
      }
    }
  }
}

# Create a ggnetwork object with Fruchterman-Reingold layout from igraph
ggn <- ggnetwork(n, layout = "fr")

# Plot the network graph with custom aesthetics
ggplot(ggn, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50") +
  geom_nodes(aes(color = vertex.names, size = 4)) +  # Use vertex names as node labels
  geom_nodetext_repel(aes(label = vertex.names), box.padding = unit(0.3, "lines")) +
  scale_color_manual(values = c("blue")) +  # Customize node color
  theme_blank()


library(tidyverse)
library(ggnetwork)

# Select relevant columns and convert NA values to empty strings
df <- cast_charities_filtered %>%
  select(nicename, Charity.1, Charity.name.2, Charity.name.3, Charity.name.4, Charity.name.5,
         Focus1, Focus2, Focus3, Focus4, Focus5) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", as.character(.))))

# Reshape data for nodes
nodes <- df %>%
  pivot_longer(cols = starts_with("Charity"),
               names_to = "Charity_Number",
               values_to = "Charity") %>%
  distinct(Charity) %>%
  rename(label = Charity)

# Reshape data for edges
edges <- df %>%
  pivot_longer(cols = starts_with("Charity"),
               names_to = "Charity_Number",
               values_to = "Charity") %>%
  filter(Charity != "") %>%
  mutate(nicename = trimws(nicename)) %>%
  left_join(nodes, by = "Charity_Number") %>%
  mutate(from = nicename, to = label, focus = paste("Focus", Charity_Number, sep = ""))





# ggplot with nodes edges repel but errors -------------------------------------------


  
library(tidyverse)

# Extract relevant columns for diseases and charities
edges <- cast_charities_filtered %>%
  select(nicename, Charity.1:Charity.name.5) %>%
  pivot_longer(cols = starts_with("Charity"), names_to = "Charity_Number", values_to = "Charity") %>%
  filter(!is.na(Charity)) %>%
  mutate(Charity_Number = parse_number(Charity_Number))

# Create a dataframe for nodes (diseases and charities)
nodes <- edges %>%
  distinct(Charity) %>%
  rename(label = Charity, type = "Charity") %>%
  bind_rows(edges %>% select(nicename) %>% distinct() %>% rename(label = nicename, type = "nicename"))

# Define colors for node types
node_colors <- c(Disease = "red", Charity = "blue")

# Plot network using ggplot2
ggplot() +
  geom_segment(data = edges, aes(x = nicename, xend = Charity, y = 0, yend = 1), color = "gray", size = 1, alpha = 0.5) +
  geom_point(data = nodes, aes(x = label, y = 0, color = type), size = 5) +
  scale_color_manual(values = node_colors) +
  theme_minimal() +
  labs(title = "Relationship Between Diseases and Charities") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
