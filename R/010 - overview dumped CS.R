library(sf)
library(dplyr)
library(giscoR)
library(ggplot2)


csense <- st_read("./data/cs_data_dump.gpkg") %>% 
   filter(climate_related == "1") %>% # who cares about climate unrelated misinformation?
   mutate(date = as.Date(review_date)) %>% 
   # date sanity check...
   filter(date >= as.Date("2000-01-01") &
             date <= Sys.Date())

# entire world as basemap, 1: 20M
world <- gisco_get_countries(resolution = "20")

# spatial overview
ggplot() +
   geom_sf(data = world, fill = NA, color = "gray45") +
   geom_sf(data = csense, color = "red", alpha = 1/125, size = 1, shape = 16) +
   coord_sf(crs = st_crs("ESRI:54019")) +
   theme_minimal() +
   labs(title = paste0("Places [", length(csense$review),"] mentioned in\nclimate related claim [", length(unique(csense$claim)),"] reviews [", length(unique(csense$review)),"]"))

ggsave("./output/spatial overview.png",
       width = 2000, height = 1500, units = "px")


# temporal overview
csense %>% 
   st_drop_geometry() %>% 
   select(date, review) %>% 
   unique() %>% 
   group_by(date) %>% 
   summarise(count = n()) %>% 
   mutate(
   m_avg = slider::slide_index_dbl(
      count,                       
      .i = date,      
      .f = ~mean(.x, na.rm = TRUE),
      .before = lubridate::days(180),
      .after = lubridate::days(180),)) %>%         
   ggplot(aes(x = date, y = count)) + 
   geom_point(pch = 4, alpha = 1/4) +
#   geom_smooth(se = F, color = "red", method = stats::loess) +
   geom_line(color = "red", aes(y = m_avg), linewidth = 1) +
   scale_x_date(date_breaks = "2 years",
                date_labels = "%Y") +
   scale_y_continuous(limits = c(0, 30)) +
   theme_minimal() +
   theme(axis.title = element_blank(),
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
   labs(title = paste0("Daily count of climate related claim [", length(unique(csense$claim)),"] reviews [", length(unique(csense$review)),"]"),
        subtitle = "with moving average smoothing applied")

ggsave("./output/temporal overview.png",
       width = 2000, height = 1500, units = "px")

# org overview
csense %>% 
   st_drop_geometry() %>% 
   select(org, review) %>%
   unique() %>% 
   group_by(org) %>% 
   summarise(count = n()) %>% 
   filter(count > 100) %>% 
   ggplot(aes(x = reorder(org, count), y = count)) +
   geom_col(aes(fill = org), show.legend = F) +
   coord_flip() +
   theme_minimal() +
   theme(axis.title = element_blank()) +
   labs(title = paste0("Major fact checking orgs / 100+ reviews"))

ggsave("./output/organizational overview.png",
       width = 2000, height = 1500, units = "px")