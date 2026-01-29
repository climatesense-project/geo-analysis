library(sf)
library(dplyr)
library(giscoR)
library(ggplot2)


csense <- st_read("./data/cs_data_dump.gpkg") %>% 
   mutate(review_date = as.Date(review_date)) %>% 
   # date sanity check...
   filter(review_date >= as.Date("2015-01-01") &
             review_date <= Sys.Date()) %>% 
   mutate(climate_related = climate_related == "1") 

# entire world as basemap, 1: 20M
world <- gisco_get_countries(resolution = "01") %>% 
   rmapshaper::ms_simplify(keep = 1/100, keep_shapes = T)

# spatial overview
ggplot() +
   geom_sf(data = world, fill = NA, color = "gray45") +
   geom_sf(data = csense, aes(color = climate_related), alpha = 1/125, size = 1, shape = 16) +
   coord_sf(crs = st_crs("ESRI:54019")) +
   scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
   theme_minimal() +
   labs(title = paste0("Places [",
                       length(csense$review),
                       "] mentioned in\nclimate related [",
                       length(unique(csense$claim[csense$climate_related])),
                       "] and unrelated [",
                       length(unique(csense$claim[!csense$climate_related])),
                       "] claim reviews [", length(unique(csense$review)),"]")) +
   theme(legend.position="bottom") +
   guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))

ggsave("./output/spatial overview.png",
       width = 2000, height = 1500, units = "px")

world_climate_pct <- world %>% 
   st_join(csense) %>% 
   st_drop_geometry() %>% 
   group_by(ISO3_CODE, climate_related) %>% 
   summarise(count = n()) %>% 
   tidyr::pivot_wider(id_cols = ISO3_CODE,
                      names_from = climate_related,
                      values_from = count,
                      values_fill = 0) %>% 
   mutate(climate_related_pct = `TRUE` / (`TRUE` + `FALSE` + `NA`)) %>% 
   select(ISO3_CODE, climate_related_pct)
   
# spatial share = climate related out of country total
world %>% 
   left_join(world_climate_pct) %>% 
   ggplot() +
   geom_sf(aes(fill = climate_related_pct), color = NA) +
   scale_fill_viridis_c(labels = scales::label_percent(),
                        limits = c(0, 1)) +
   coord_sf(crs = st_crs("ESRI:54019")) +
   theme_minimal() +
   labs(title = paste0("Share of places [",
                       length(csense$review),
                       "] per state or equivalent [",
                       length(world$CNTR_ID),
                       "] mentioned in\nclimate related [",
                       length(unique(csense$claim[csense$climate_related])),
                       "] and unrelated [",
                       length(unique(csense$claim[!csense$climate_related])),
                       "] claim reviews [", length(unique(csense$review)),"]")) +
   theme(legend.position="bottom")

ggsave("./output/spatial share.png",
       width = 2000, height = 1500, units = "px")

# temporal overview
csense %>% 
   st_drop_geometry() %>% 
#   filter(!climate_related) %>% 
   select(review_date, review, climate_related) %>% 
   unique() %>% 
   group_by(review_date, climate_related) %>% 
   summarise(count = n()) %>% 
   filter(review_date != as.Date("2016-06-13")) %>%  # breaks axes, adds little value
   ggplot(aes(x = review_date, y = count)) + 
   geom_point(pch = 4, alpha = 1/4) +
   geom_smooth(se = F, aes(color = climate_related)) +
   scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
#   geom_line(color = "red", aes(y = m_avg), linewidth = 1) +
   scale_x_date(date_breaks = "1 years",
                date_labels = "%Y") +
#   scale_y_continuous(limits = c(0, 30)) +
   facet_wrap(~climate_related, ncol = 1, scales = "free_y") +
   theme_minimal() +
   theme(axis.title = element_blank(),
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
   labs(title = paste0("Daily count of climate related [",
                       length(unique(csense$claim[csense$climate_related])),
                       "]\nand unrelated [",
                       length(unique(csense$claim[!csense$climate_related])),
                       "] claim reviews [", length(unique(csense$review)),"]")) +
   theme(legend.position="bottom")

ggsave("./output/temporal overview.png",
       width = 2000, height = 1500, units = "px")

# temporal overview - relative
csense %>% 
   st_drop_geometry() %>% 
   select(review_date, review, climate_related) %>% 
   unique() %>% 
   group_by(review_date, climate_related) %>% 
   summarise(count = n()) %>% 
   tidyr::pivot_wider(id_cols = review_date,
                      names_from = climate_related,
                      values_from = count,
                      values_fill = 0) %>% 
   mutate(climate_related_pct = `TRUE` / (`TRUE` + `FALSE`)) %>% 
   ggplot(aes(x = review_date, y = climate_related_pct)) + 
   geom_point(pch = 4, alpha = 1/4) +
   geom_smooth(se = F, color = "red") +
   scale_x_date(date_breaks = "1 years",
                date_labels = "%Y") +
   theme_minimal() +
   theme(axis.title = element_blank(),
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
   labs(title = paste0("Daily share of climate related [",
                       length(unique(csense$claim[csense$climate_related])),
                       "]\nand unrelated [",
                       length(unique(csense$claim[!csense$climate_related])),
                       "] claim reviews [", length(unique(csense$review)),"]")) +
   theme(legend.position="bottom")

ggsave("./output/temporal share.png",
       width = 2000, height = 1500, units = "px")

# org overview
csense %>% 
   st_drop_geometry() %>% 
   select(org, review, climate_related) %>%
   unique() %>% 
   group_by(org, climate_related) %>% 
   summarise(count = n()) %>% 
   filter(count > 300) %>% 
   ggplot(aes(x = reorder(org, count), y = count)) +
   geom_col(aes(fill = org), show.legend = F) +
   coord_flip() +
   facet_wrap(~climate_related, nrow = 1, scales = "free_x") +
   theme_minimal() +
   theme(axis.title = element_blank()) +
   labs(title = paste0("Major fact checking orgs / 300+ reviews"))

ggsave("./output/organizational overview.png",
       width = 2000, height = 1500, units = "px")