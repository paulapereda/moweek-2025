pacman::p_load(hrbrthemes, tidyverse, here)

# (1) Vestimenta

vestimenta <- read_rds(here("data", "previa", "clean", "product_clean_clothing.rds")) 

prices_category_all <- vestimenta %>% 
  summarise(mean_price = round(median(price)))

prices_category <- vestimenta %>% 
  group_by(grupo) %>% 
  summarise(mean_price = round(median(price))) 

vestimenta %>% 
  ggplot(aes(fct_reorder(grupo, price, .desc = F), price)) +
  geom_point(aes(color = grupo), alpha = .6, size = 4) +
  stat_summary(fun = "median", geom = "point", shape = 16, alpha = .8, size = 8, color = "#172829", show.legend = TRUE) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = paste0("$", format(round(..y..), big.mark = ".", scientific = FALSE))),
    vjust = -1.3,
    size = 5,
    color = "#333333",
    fontface = "bold",
    family = "Roboto Condensed"
  ) +
  scale_color_manual(values = rep(c("#936A4E"), 17)) +
  scale_y_continuous(limits = c(0, 40000),
                     breaks = seq(0, 40000, by = 5000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     expand = c(.02, .2)) + 
  coord_flip() +
  labs(x = "Categoría",
       y = "Precio",
       title = "Vestimenta | Dispersión de precios por categoría", 
       caption = "Nota: El punto verde representa la mediana por categoría.\nFuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood") +
  theme_ipsum_rc(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 25),
    plot.caption = element_text(size = 14),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

ggsave(here("output", "13_vestimenta_dispersion_categoria.jpg"), width = 13, height = 8)

# (2) Carteras

bags <- read_rds(here("data", "previa", "clean", "product_clean_bags.rds")) 

bags_category_all <- bags %>% 
  summarise(mean_price = round(median(price)))

prices_category_bags <- bags %>% 
  group_by(category) %>% 
  summarise(mean_price = round(median(price)))

bags %>% 
  ggplot(aes(fct_reorder(category, price, .desc = F), price)) +
  geom_point(aes(color = category), alpha = .6, size = 4) +
  stat_summary(fun = "median", geom = "point", shape = 16, alpha = .8, size = 8, color = "#172829", show.legend = TRUE) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = paste0("$", format(round(..y..), big.mark = ".", scientific = FALSE))),
    vjust = -1.3,
    size = 5,
    color = "#333333",
    fontface = "bold",
    family = "Roboto Condensed"
  ) +
  scale_color_manual(values = rep(c("#936A4E"), 17)) +
  scale_y_continuous(limits = c(0, 20000),
                    breaks = seq(0, 20000, by = 5000),
                    labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                    expand = c(.02, .2)) + 
  coord_flip() +
  labs(x = "Categoría",
       y = "Precio",
       title = "Carteras & Bolsos | Dispersión de precios por categoría", 
       caption = "Nota: El punto verde representa la mediana por categoría.\nFuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood") +
  theme_ipsum_rc(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 25),
    plot.caption = element_text(size = 14),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

ggsave(here("output", "14_carteras_dispersion_categoria.jpg"), width = 13, height = 8)

# (3) Calzado

shoes <- read_rds(here("data", "previa", "clean", "product_clean_shoes.rds")) %>% 
  mutate(category = ifelse(category == "Botas Y Botines", "Botas & Botines", category))
  
shoes_category_all <- shoes %>% 
  summarise(mean_price = round(median(price)))

prices_category_shoes <- shoes %>% 
  group_by(category) %>% 
  summarise(mean_price = round(median(price)))

shoes %>% 
  ggplot(aes(fct_reorder(category, price, .desc = F), price)) +
  geom_point(aes(color = category), alpha = .6, size = 4) +
  stat_summary(fun = "median", geom = "point", shape = 16, alpha = .8, size = 8, color = "#172829", show.legend = TRUE) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = paste0("$", format(round(..y..), big.mark = ".", scientific = FALSE))),
    vjust = -1.3,
    size = 5,
    color = "#333333",
    fontface = "bold",
    family = "Roboto Condensed"
  ) +
  scale_color_manual(values = rep(c("#936A4E"), 17)) +
  scale_y_continuous(limits = c(0, 15000),
                     breaks = seq(0, 15000, by = 5000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     expand = c(.02, .2)) + 
  coord_flip(ylim = c(0, 15000)) +
  labs(x = "Categoría",
       y = "Precio",
       title = "Calzado | Dispersión de precios por categoría", 
       caption = "Nota: El punto verde representa la mediana por categoría.\nFuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood") +
  theme_ipsum_rc(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 25),
    plot.caption = element_text(size = 14),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

ggsave(here("output", "14_calzado_dispersion_categoria.jpg"), width = 13, height = 8)

