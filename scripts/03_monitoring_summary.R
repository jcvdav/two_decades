
library(here)
library(cartography)
library(cowplot)
library(tidyverse)

clean_fish <- read_csv(file = here("data", "processed_data", "clean_fish_transects.csv")) %>% 
  select(year, abundance, site, species, community, zone, id, diver) %>% 
  mutate(group = "Fish",
         id = paste("F", id))

clean_inverts <- read_csv(file = here("data", "processed_data", "clean_invertebrate_transects.csv")) %>% 
  select(year, abundance, site, species, community, zone, id, diver) %>% 
  mutate(group = "Invertebrates",
         id = paste("I", id))

iucn_status <- read_csv(here("data", "processed_data", "iucn_status.csv"), show_col_types = F)


female <- c("Alejandra Meza", "Alondra Moran", "Araceli Acevedo", "Bertha", "Bertha Aguirre", "Carmen Valdez",
            "Elba Lopez", "Elsa Cuellar", "Esmeralda Albanes", "Guadalupe Lopez Ruiz",
            "Leticia Ortega", "Lizbeth Tamayo", "Mitzi Leal",
            "Sulema Garcia", "Yarrreni Perera", "Fernanda Perez", "Hanela Ancona", "Yareni Perera")

cobi <- c("Alfonso Romero", "Alejandra Meza", "Antonio Gomez", "Arturo Hernandez", "Ashley Greenley",
          "Cindy Dawson", "Craig Shuman", "Eduardo Diaz", "Fernanda Perez",
          "Ernesto Gastelum", "Jacobo Caamal", "Juan Carlos Villasenor", "Leonardo Vazquez", "Magdalena Precoma",
          "Mariana Walther", "Mario Rojo", "Megan Wehrenberg", "Philip Ericsson", "Phillip Erickson",
          "Sergio Marcos", "Stuart Fulton", "Alvin Suarez", "Araceli Acevedo", "Carmen Valdez", "Raziel Hernandez", "Yareni Perera")

data <- rbind(clean_fish,
              clean_inverts) %>% 
  mutate(diver = case_when(diver == "Allan Larraniaga" ~ "Allan Larranaga",
                           diver == "Francisco Valdivieso" ~ "Francisco Valdiviezo",
                           diver == "Miguel Jarillo Garcia" ~ "Miguel Jarillo",
                           diver == "Miguel Jesus Tun Catzim" ~ "Miguel Tun",
                           diver == "Pabtlo Catzim" ~ "Pablo Catzi Pech",
                           diver == "Philip Ericksson" ~ "Philip Erickson",
                           diver == "Hugo Valdivieso" ~ "Hugo Valdiviezo",
                           diver == "Jaime Medina" ~ "Jaime Alberto Medina Alavez",
                           diver == "HaÒela Ancona" ~ "Hanela Ancoma",
                           T ~ diver)) %>% 
  mutate(female = diver %in% female,
         cobi = diver %in% cobi)


divers_in_time <- data %>% 
  filter(!cobi) %>% 
  select(year, diver, female) %>% 
  distinct() %>% 
  group_by(year) %>% 
  summarize(n_div = n_distinct(diver),
            Female = sum(female),
            Male = n_div - Female) %>% 
  select(-n_div) %>% 
  pivot_longer(contains("ale"), names_to = "group", values_to = "n")

n_divers <- ggplot(data = divers_in_time, aes(x = year, y = n, fill = group)) + 
  geom_area(color = "black") +
  scale_x_continuous(labels = seq(2006, 2020, by = 2), breaks = seq(2006, 2020, by = 2), expand = c(0, 0)) +
  theme_bw() +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.background = element_blank()) +
  labs(x = "Year", y = "Community members\nparticipating") +
  guides(fill = guide_legend("Gender")) +
  scale_fill_manual(values = carto.pal(pal1 = "blue.pal")) +
  scale_y_continuous(expand = c(0, 0))



n_orgs_cum <- data %>% 
  filter(abundance > 0) %>% 
  mutate(site = paste(site, zone)) %>% 
  group_by(year) %>% 
  summarize(Organisms = sum(abundance),
            n_res = n_distinct(site),
            n_trans = n_distinct(id)) %>% 
  ungroup() %>% 
  mutate(Organisms = cumsum(Organisms) / 1e3) %>% 
  ggplot(aes(x = year, y = Organisms, fill = n_trans)) +
  geom_line() +
  geom_point(aes(size = n_res), shape = 21) +
  scale_x_continuous(labels = seq(2006, 2020, by = 2), breaks = seq(2006, 2020, by = 2)) +
  theme_bw() +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.background = element_blank(),
        legend.box = "horizontal") +
  labs(x = "Year",
       y = "Cumulative records \n(thousands)") +
  guides(size = guide_legend("Unique sites surveyed"),
         fill = guide_legend("Transects")) +
  scale_fill_gradientn(colors = carto.pal(pal1 = "blue.pal", n1 = 10))

iucn <-
iucn_status %>% 
  count(iucn_cat) %>% 
  mutate(n = n / sum(n)) %>% 
  mutate(iucn_cat = fct_relevel(iucn_cat, c("DD", "LC", "NT", "VU", "EN", "CR"))) %>% 
  ggplot(aes(x = iucn_cat, y = n)) +
  geom_col(fill = "steelblue",
           color = "black") +
  theme_bw() +
  labs(x = "IUCN Category",
       y = "% Species\n") +
  scale_y_continuous(labels = scales::percent)


p <- plot_grid(iucn, n_orgs_cum, n_divers, ncol = 1, rel_heights = c(0.75, 1, 1), labels = "auto")


ggsave(p,
       filename = here("results", "img", "data_in_numbers.jpg"),
       width = 5,
       height = 8)









