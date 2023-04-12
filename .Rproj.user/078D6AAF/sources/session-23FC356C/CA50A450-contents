eq %>% 
  left_join(locations %>% distinct(admin3pcode, 
                                   sub_district = admin3name_en, 
                                   governorate = admin1name_en), 
            by = "admin3pcode") %>% 
  filter(wounded_dead > 0) %>% 
  mutate(sub_district = fct_reorder(sub_district, wounded_dead)) %>% 
  ggplot(aes(x = wounded_dead, y = sub_district)) + 
  geom_col(aes(fill = governorate)) + 
  geom_text(aes(label = comma(wounded_dead, accuracy = 1)), hjust = "inward") + 
  scale_fill_viridis_d(option = "plasma", begin = 0.2, end = .8) + 
  scale_x_continuous(labels = comma) + 
  labs(title = "Dead and wounded per sub-district", 
       x = "Number of dead and wounded", 
       y = "") + 
  
  eq %>% 
  left_join(locations %>% distinct(admin3pcode, 
                                   sub_district = admin3name_en, 
                                   governorate = admin1name_en), 
            by = "admin3pcode") %>% 
  filter(wounded_dead > 0) %>% 
  mutate(sub_district = fct_reorder(sub_district, wounded_dead), 
         wounded_dead_100k = round(wounded_dead_100k, digits = 2)) %>% 
  ggplot(aes(x = wounded_dead_100k, y = sub_district)) + 
  geom_col(aes(fill = governorate)) + 
  geom_text(aes(label = wounded_dead_100k), hjust = "inward") + 
  scale_fill_viridis_d(option = "plasma", begin = 0.2, end = .8) + 
  scale_x_continuous(labels = comma) + 
  labs(title = "Dead and wounded per 100,000 persons", 
       x = "Dead and wounded per 100k", 
       y = "") + 
  
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

ggsave("./img/dead_wounded_sub_district.png", dpi = 300, height = 8.27, width = 11.69, units = "in")

eq %>% 
  left_join(locations %>% distinct(admin3pcode, 
                                   sub_district = admin3name_en, 
                                   governorate = admin1name_en), 
            by = "admin3pcode") %>% 
  filter(damaged_houses > 0) %>% 
  mutate(sub_district = fct_reorder(sub_district, damaged_houses)) %>% 
  ggplot(aes(x = damaged_houses, y = sub_district)) + 
  geom_col(aes(fill = governorate)) + 
  geom_text(aes(label = comma(damaged_houses, accuracy = 1)), hjust = "inward") + 
  scale_fill_viridis_d(option = "plasma", begin = 0.2, end = .8) + 
  scale_x_continuous(labels = comma) + 
  labs(title = "Damaged houses per sub-district", 
       x = "Number of damaged houses", 
       y = "") + 
  
  eq %>% 
  left_join(locations %>% distinct(admin3pcode, 
                                   sub_district = admin3name_en, 
                                   governorate = admin1name_en), 
            by = "admin3pcode") %>% 
  filter(damaged_houses > 0) %>% 
  mutate(sub_district = fct_reorder(sub_district, damaged_houses), 
         damaged_houses_100k = round(damaged_houses_100k, digits = 2)) %>% 
  ggplot(aes(x = damaged_houses_100k, y = sub_district)) + 
  geom_col(aes(fill = governorate)) + 
  geom_text(aes(label = damaged_houses_100k), hjust = "inward") + 
  scale_fill_viridis_d(option = "plasma", begin = 0.2, end = .8) + 
  scale_x_continuous(labels = comma) + 
  labs(title = "Damaged houses per 100,000 persons", 
       x = "Damaged houses per 100k", 
       y = "") + 
  
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

ggsave("./img/damaged_houses_sub_district.png", dpi = 300, height = 8.27, width = 11.69, units = "in")

eq %>% 
  filter(admin3pcode %in% nw_pcode3 & 
           district %in% c("Harim", "Idleb", "Jisr-Ash-Shugur",
                           "Afrin", "Jebel Saman", "A'zaz", "Jarablus", "Al Bab")) %>% 
  left_join(hno %>% 
              filter(admin3pcode %in% nw_pcode3 &
                       admin2name_en %in% c("Harim", "Idleb", "Jisr-Ash-Shugur",
                                            "Afrin", "Jebel Saman", "A'zaz", "Jarablus", "Al Bab")) %>%
              group_by(district = admin2name_en) %>%
              summarise(population = sum(total_population, na.rm = TRUE)), by = "district") %>% 
  mutate(wounded_dead = casualties + injuries, 
         damaged_houses = totally_damaged + partially_damaged, 
         wounded_dead_100k = round(wounded_dead / population * 100000, digits = 2), 
         damaged_houses_100k = round(damaged_houses / population * 100000, digits = 2)) %>% 
  select(governorate, district, wounded_dead, wounded_dead_100k, 
         damaged_houses, damaged_houses_100k) %>% 
  flextable() %>% 
  theme_zebra() %>% 
  set_table_properties(layout = "autofit", width = .99) %>% 
  set_caption("Casualties and damaged houses, absolute figures and per 100,000 persons") %>% 
  footnote(i = 1, j = 3:6, part = "header",
           as_paragraph("Data from the Assistance Coordination Unit, Syria 20230307"))