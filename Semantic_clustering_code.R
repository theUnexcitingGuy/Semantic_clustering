
# libraries ---------------------------------------------------------------
library(rvest)
library(caret)
library(dplyr)
library(readxl)
library(tidyverse)
library(MarshMellow) #importing my own package to fecth automatically categories from Wikipedia
library(packcircles) #used to plot clusters as circles
library(visNetwork) #to visualize the clusters

# data import -------------------------------------------------------------
entities <- read_xlsx('Entities.xlsx')

# fetching all categories for entities ------------------------------------
entity_categ <- entities %>% 
  mutate(categorie = map(Entity, WikiCateg)) %>% 
  unnest(categorie)

# creating the clustering packing (FIRST ATTEMPT) -------------------------
data <- entity_categ %>% 
  count(categorie)

#packing
packing <- circleProgressiveLayout(data$n, sizetype='area')

#binding
data <- cbind(data, packing)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot
ggplot() + 
  
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  
  # Add text in the center of each bubble + control its size
  geom_text(data = data, aes(x, y, size=n, label = categorie)) +
  scale_size_continuous(range = c(1,4)) +
  
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()


# custering (SECOND ATTEMPT) ----------------------------------------------
#for each entity i search which cluster it is associated to, if there is more than one cluster than I pick only the most numerous cluster
cluster_numerosity <- entity_categ %>% 
  count(categorie) %>% 
  rename(categ = categorie)

#filtering the clusters of each entity in the entity_categ dataframe
final_clustering_data <- entity_categ %>% 
  group_by(Entity) %>% 
  filter(categorie %in% (cluster_numerosity %>% filter(categ %in% categorie) %>% arrange(desc(n)) %>% head(1))) %>% 
  ungroup() %>% 
  rename(cluster = categorie)


# Plotting again the data -------------------------------------------------
data_final <- final_clustering_data %>% 
  count(cluster)

#packing
packing_final <- circleProgressiveLayout(data_final$n, sizetype='area')

#binding
data_final <- cbind(data_final, packing_final)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing_final, npoints=50)

# Make the plot
ggplot() + 
  
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  
  # Add text in the center of each bubble + control its size
  geom_text(data = data_final, aes(x, y, size=10, label = cluster)) +
  scale_size_continuous(range = c(1,4)) +
  
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()


# Displaying the elements within the clusters --------------
edges <- final_clustering_data %>% 
  rename(from = cluster, to = Entity) 

#create the weight column
value_column <- rbind(col1, col2) %>% 
  count(id) %>% 
  rename(value = n)

col1 <- final_clustering_data %>% 
  select(Entity) %>% 
  rename(id = Entity)
col2 <- final_clustering_data %>% 
  select(cluster) %>% 
  rename(id = cluster)
nodes <- rbind(col1, col2) %>% 
  unique() %>% 
  mutate(label = id)

#merging with the value column
nodes_merged <- merge(nodes, value_column, by = 'id', all = T) %>% 
  mutate(font.size = value*4)

#creating the visNetwork graph to display the clusters
visNetwork(nodes_merged, edges) %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visPhysics(solver = "barnesHut", stabilization = list(enabled = FALSE))

