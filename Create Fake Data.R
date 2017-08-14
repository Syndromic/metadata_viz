## Prep ----

library(tigris)
library(dplyr)
library(tidyr)

data(fips_codes)

## Timeliness - create fake data set ----

Counties_Skel <- left_join(fips_codes, Timeliness_Extended, c("state" = "Site_State", "county" = "County_Name")) %>% 
                    select(Site_State = state, Geographic_Level, County_Name = county) %>%
                    mutate(Geographic_Level = "County")

State_Abb_List <- distinct(fips_codes, state)

States_Skel <- State_Abb_List %>% 
                select(Site_State = state) %>% 
                mutate(Geographic_Level = "State", County_Name = NA)

Timeliness_Skel <- bind_rows(Counties_Skel, States_Skel) 

n <- nrow(Timeliness_Skel)

Timeliness_Fake <- Timeliness_Skel %>% 
                    mutate(pLessThan24 = runif(n, 0.0, 100.0), 
                           pLessThan48 = runif(n, 0.0, 100.0), 
                           d_p80 = runif(n, 0.0, 10.0)
                           )


## Completeness - create fake data set ----

dqvar_names <- distinct(Completeness_Extended, dqvar)

# Notice that I cross() with Timeliness_Skel. That's OK because I'm just using the skeleton.
Completeness_Skel <- crossing(Timeliness_Skel, dqvar_names) 

n <- nrow(Completeness_Skel)

Completeness_Fake <- Completeness_Skel %>% mutate(pvisits = runif(n, 0.0, 100.0))


## Generate .rds files to readRDS() in other script ----

saveRDS(Timeliness_Fake, "Timeliness_Fake.rds")
saveRDS(Completeness_Fake, "Completeness_Fake.rds")
