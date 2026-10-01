library(tidyverse)
library(glmmTMB)
library(performance)

home <- here::here()

d <- readr::read_csv(paste0(home, "/data/clean/trawl.csv")) |> 
  filter(depth > 0) |> 
  drop_na(temp) |> 
  mutate(temp_sc = as.numeric(scale(temp)),
         depth = log(depth), 
         depth_sc = as.numeric(scale(depth)),
         depth_sq = depth_sc*depth_sc,
         quarter_f = as.factor(quarter),
         year_f = as.factor(year))


ggplot(d, aes(temp, density)) + 
  geom_point()

m <- glmmTMB(density ~ s(temp) + (1|year_f),  
             data = d,
             family = t_family)

summary(m)

performance::r2(m)

performance::check_singularity(m)

rebuild_Tlim <- glmmTMB(((Tlim))~ s(spr0,k=4)+(SRR)+(Scenario)+(1|Species),
                        family = t_family(link = "identity"), data = brf)