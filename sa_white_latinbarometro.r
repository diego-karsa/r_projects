## Ethnic groups in Latin America

# Load required libraries
library(rvest) # data scrapper
library(ggplot2) # draw maps
library(maps) # map outlines
library(mapproj) # map projections
library(dplyr) # data manipulation tools
library(stringr) # string tools
library(ggtext) # Markdown format for strings
library(janitor) # name cleaning

# Get data from Wikipedia
table <- 
  # Read html
  read_html("https://en.wikipedia.org/wiki/Ethnic_groups_in_Latin_America") %>%
  # Get the table node
  html_node(xpath = "/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[4]") %>%
  # Convert the html table element into a data frame
  html_table(fill = TRUE)

# Edit table
table <- table %>%
  # Drop last row (Total)
  slice(1:n()-1) %>%
  clean_names() %>% 
  # Change % strings to numeric
  mutate(across(2:ncol(table), function(x) str_replace(x, "%", ""))) %>%
  mutate(across(2:ncol(table), function(x) as.numeric(x) / 100 ))

# Vector with country names in the table
south_america_data <- table %>%
  select(1) %>%
  pull()

# Create a vector with names of all Latin American countries
south_america <-
  # Read html
  read_html("https://en.wikipedia.org/wiki/List_of_countries_and_territories_by_the_United_Nations_geoscheme") %>%
  # Get the table node
  html_node(xpath = "/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[1]") %>%
  # Convert the html table element into a data frame
  html_table(fill = TRUE) %>%
  # Filter countries in Latin America and remove names not matching with map_data
  filter(`Geographical subregion` %in% c("South America", "Central America", "Caribbean")) %>% 
  # Select Country column and turn it into a vector
  select(`Country or Area`) %>%
  rename(Country = `Country or Area`) %>% 
  pull()

# Vectors
map_data_world <- map_data("world", region = ".") %>%
  select(region) %>%
  distinct(region) %>%
  pull()

# Vector of Latin American countries with map data
south_america_clean <- intersect(map_data_world, south_america)
# Latin American countries without ethnic data
south_america_miss <- setdiff(south_america_clean, south_america_data)
# Vector of Latin American countries to map
south_america_map <- union(south_america_data, south_america_miss)

# Get map data for Latin America
map_data <- map_data("world", region = south_america_map) %>%
  left_join(table, by = c("region" = "country")) %>% 
  mutate(across(mestizos:other_race, ~ .*100))

# Draw map
map <- ggplot(map_data) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = whites), color = "gray35", linewidth = 0.4) +
  scale_fill_gradientn(limits = c(0,100),
                       colours = c("ivory", "darkblue"),
                       breaks = seq(10, 60, 10),
                       guide = guide_colorsteps()) +
  coord_map("mercator") +
  labs(title = "**Percentage of respondents who self-identify as white**",
       subtitle = "Based on the question *What race do you consider yourself to belong to?*",
       caption = "Source: Informe Latinbarómetro 2018") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "#cddee7"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.position = c(0.875, 0.15),
        legend.title = element_blank(),
        plot.title = ggtext::element_markdown(family = "Ubuntu"),
        plot.subtitle = ggtext::element_markdown(family = "Ubuntu"),
        plot.caption = ggtext::element_markdown(family = "Ubuntu"),
        legend.text =  ggtext::element_markdown(family = "Ubuntu")
  )

# Export map
ggsave(
  "white_south_america.png",
  plot = map,
  device = png,
  scale = 1,
  dpi = 320
)