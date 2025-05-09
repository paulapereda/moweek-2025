pacman::p_load(hrbrthemes, tidyverse, here)

# (1) Vestimenta

vestimenta <- read_rds(here("data", "previa", "clean", "product_clean_clothing.rds")) 

sizes_clothes <- vestimenta %>% 
  pivot_longer(size_1:size_13, names_to = "size", values_to = "size_value") %>% 
  filter(!is.na(size_value)) %>% 
  filter(!(size_value == ",")) %>% 
  group_by(grupo, size_value) %>% 
  summarise(size = n()) %>% 
  arrange(grupo, desc(size)) %>% 
  ungroup() 

# (a) Outerwear

sizes_clothes %>% 
  filter(grupo == "Outerwear") %>% 
  ggplot(aes(x = reorder(size_value, size), y = size)) +
  geom_point(shape = 21, fill = "#936A4E", color = "#333333", size = 15) +
  geom_text(aes(label = size), 
            color = "white", 
            fontface = "bold", 
            size = 5,
            family = "Roboto Condensed") +
  labs(x = "Talle", 
       y = "Cantidad",
       title = "Outerwear | Distribución de talles",
       caption = "Fuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood") +
  theme_ipsum_rc(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 25),
    plot.caption = element_text(size = 14),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 14, margin = margin(r = 12)),
    axis.text.y = element_text(size = 14, margin = margin(r = 12)),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) 

ggsave(here("output", "15_talles_outerwear.jpg"), width = 13, height = 8)

# (b) Tops

sizes_clothes %>% 
  filter(grupo == "Tops") %>% 
  ggplot(aes(x = reorder(size_value, size), y = size)) +
  geom_point(shape = 21, fill = "#936A4E", color = "#333333", size = 15) +
  geom_text(aes(label = size), 
            color = "white", 
            fontface = "bold", 
            size = 5,
            family = "Roboto Condensed") +
  labs(x = "Talle", 
       y = "Cantidad",
       title = "Tops | Distribución de talles",
       caption = "Fuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood") +
  theme_ipsum_rc(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 25),
    plot.caption = element_text(size = 14),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 14, margin = margin(r = 12)),
    axis.text.y = element_text(size = 14, margin = margin(r = 12)),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) 

ggsave(here("output", "16_talles_tops.jpg"), width = 13, height = 8)

# (c) One piece

sizes_clothes %>% 
  filter(grupo == "One piece") %>% 
  ggplot(aes(x = reorder(size_value, size), y = size)) +
  geom_point(shape = 21, fill = "#936A4E", color = "#333333", size = 15) +
  geom_text(aes(label = size), 
            color = "white", 
            fontface = "bold", 
            size = 5,
            family = "Roboto Condensed") +
  labs(x = "Talle", 
       y = "Cantidad",
       title = "One piece | Distribución de talles",
       caption = "Fuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood") +
  theme_ipsum_rc(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 25),
    plot.caption = element_text(size = 14),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 14, margin = margin(r = 12)),
    axis.text.y = element_text(size = 14, margin = margin(r = 12)),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) 

ggsave(here("output", "17_talles_one_piece.jpg"), width = 13, height = 8)

# (d) Layering/Knit

sizes_clothes %>% 
  filter(grupo == "Layering/Knit") %>% 
  ggplot(aes(x = reorder(size_value, size), y = size)) +
  geom_point(shape = 21, fill = "#936A4E", color = "#333333", size = 15) +
  geom_text(aes(label = size), 
            color = "white", 
            fontface = "bold", 
            size = 5,
            family = "Roboto Condensed") +
  labs(x = "Talle", 
       y = "Cantidad",
       title = "Layering/Knit | Distribución de talles",
       caption = "Fuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood") +
  theme_ipsum_rc(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 25),
    plot.caption = element_text(size = 14),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 14, margin = margin(r = 12)),
    axis.text.y = element_text(size = 14, margin = margin(r = 12)),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) 

ggsave(here("output", "18_talles_layering_knit.jpg"), width = 13, height = 8)

# (2) Calzado

(shoes <- read_rds(here("data", "previa", "clean", "product_clean_shoes.rds")) %>% 
  pivot_longer(size_1:size_11, names_to = "size", values_to = "size_value") %>% 
  filter(!is.na(size_value)) %>% 
  filter(!(size_value == ",")) %>%
  filter(!(size_value %in% c("26", "27", "29", "31", "28", "45", "S", "M", "46", "43", "44"))) %>%
  group_by(size_value) %>% 
  summarise(size = n()) %>%
  arrange(desc(size)) %>% 
  ungroup() %>% 
  ggplot(aes(x = reorder(size_value, size), y = size)) +
  geom_point(shape = 21, fill = "#936A4E", color = "#333333", size = 15) +
  geom_text(aes(label = size), 
            color = "white", 
            fontface = "bold", 
            size = 5,
            family = "Roboto Condensed") +
  labs(x = "Talle", 
       y = "Cantidad",
       title = "Calzado | Distribución de talles",
       caption = "Fuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood") +
  theme_ipsum_rc(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 25),
    plot.caption = element_text(size = 14),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 14, margin = margin(r = 12)),
    axis.text.y = element_text(size = 14, margin = margin(r = 12)),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) 
)

ggsave(here("output", "19_talles_calzado.jpg"), width = 13, height = 8)

