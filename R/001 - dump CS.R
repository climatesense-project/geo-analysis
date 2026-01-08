library(glitter) # SPARQL query from R
library(dplyr)
library(stringr)
library(sf)

start_time <- Sys.time()
vystup <- "./output/benchmark_cs.txt"

cat(paste(Sys.time(), "start\n"), file = vystup, append = T)

# the real thing ... 
query_basis = spq_init(
   endpoint = "http://data.climatesense-project.eu/sparql"
)

# iterate over history of interest
for (year in 2000:lubridate::year(Sys.Date())) {

   # compose the query
   query <- query_basis %>%
      spq_prefix(prefixes = c("schema" = "http://schema.org/",
                              "dbr" = "http://dbpedia.org/resource/",
                              "geo" = "http://www.w3.org/2003/01/geo/wgs84_pos",
                              "cs" = "http://data.climatesense-project.eu/ontology#",
                              "dbp" = "http://dbpedia.org/property/")) %>% 
      spq_add("?review rdf:type schema:ClaimReview") %>%
      spq_add("?review schema:mentions ?mentioned") %>% 
      spq_add("?review schema:datePublished ?date") %>% 
      spq_add("?review schema:author ?reviewer") %>% 
      spq_add("?reviewer schema:name ?org") %>% 
      spq_add("?review schema:itemReviewed ?claim") %>% 
      spq_add("?claim cs:isClimateRelated true") %>% # who cares about climate unrelated claims?
      spq_add("?mentioned geo:geometry ?geo") %>% 
      spq_set(year_review = paste0("'", year, "'")) %>%  # current year of iteration
      spq_filter(str_sub(as.character(date), 1, 4) == year_review) %>% 
      spq_select(.spq_duplicate = "distinct") # no duplicities thank you
      
   
   result <- spq_perform(query) # let the magic happen!
   
   # digest the results & save for future use
   result %>% 
      unique() %>% # just to make certain that the select distinct worked 
      mutate(claim = str_remove(claim, "http://data.climatesense-project.eu/claim/")) %>% 
      mutate(mentioned = str_remove(mentioned, "http://dbpedia.org/resource/")) %>% 
      mutate(review = str_remove(review, "http://data.climatesense-project.eu/claim-review/")) %>% 
      select(-reviewer) %>% 
      st_as_sf(wkt = "geo", crs = 4326) %>% 
      st_write("./data/cs_data_dump.gpkg", append = (year != 2000)) # first year = new db, all others append
   
   cat(paste(Sys.time(), "| year:", year, "| raw results:", nrow(result), "| unique results:", nrow(unique(result)), "| unique reviews:", length(unique(result$review)),  "\n"), file = vystup, append = T)


}

cat(paste(Sys.time(), "finish\nelapsed time", lubridate::as.duration(Sys.time() - start_time), "\n"), file = vystup, append = T)