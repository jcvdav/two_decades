
library(here)
library(tidyverse)

clean_fish <- read_csv(file = here("data", "processed_data", "clean_fish_transects.csv")) %>% 
  select(year, abundance, site, species, community, zone, id, diver) %>% 
  mutate(group = "Fish")
clean_inverts <- read_csv(file = here("data", "processed_data", "clean_invertebrate_transects.csv")) %>% 
  select(year, abundance, site, species, community, zone, id, diver) %>% 
  mutate(group = "Invertebrates")

data <- rbind(clean_fish,
      clean_inverts) 

n_orgs_cum <- data %>% 
  filter(abundance > 0) %>% 
  mutate(site = paste(site, zone)) %>% 
  group_by(year) %>% 
  summarize(Organisms = sum(abundance),
            n_res = n_distinct(site),
            n_div = n_distinct(diver)) %>% 
  ungroup() %>% 
  mutate(Organisms = cumsum(Organisms) / 1e3) %>% 
  ggplot(aes(x = year, y = Organisms, fill = n_div)) +
  geom_line() +
  geom_point(aes(size = n_res), shape = 21) +
  scale_x_continuous(labels = seq(2006, 2020, by = 2), breaks = seq(2006, 2020, by = 2)) +
  theme_bw() +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.background = element_blank()) +
  labs(x = "Year", y = "Cumulative # of\norganisms recorded (thousands)") +
  guides(size = guide_legend("# Sites surveyed"),
         fill = guide_legend("# Community members\nparticipating")) +
  scale_fill_gradientn(colors = carto.pal(pal1 = "blue.pal"))


ggsave(n_orgs_cum,
       filename = here("20_res", "img", "n_orgs_cum.pdf"),
       width = 6,
       height = 4)


f <- c("Alejandra Meza", "Alondra Moran", "Araceli Acevedo", "Ashley Greenley", "Bertha", "Bertha Aguirre", "Carmen Valdez",
       "Cindy Dawson", "Elba Lopez", "Elsa Cuellar", "Esmeralda Albanes", "Fernanda Perez", "Guadalupe Lopez Ruiz",
       "Leticia Ortega", "Lizbeth Tamayo", "Magdalena Precoma", "Mariana Walther", "Megan Wehrenberg", "Mitzi Leal",
       "Sulema Garcia", "Yarrreni Perera")

data %>% 
  mutate(mf = ifelse(diver %in% f, "Female", "Male")) %>% 
  count(year, mf, diver) %>% 
  count(year, mf) %>% 
  ggplot(aes(x = year, y = n, group = mf)) +
  geom_line() +
  geom_point() 








