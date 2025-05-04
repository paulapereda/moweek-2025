pacman::p_load(hrbrthemes, tidyverse, here)

# (1) Vestimenta 

vestimenta <- read_rds(here("data", "previa", "clean", "product_clean_clothing.rds"))

# Pivot longer
df_long <- vestimenta %>% 
  pivot_longer(
    cols = starts_with("color_"),
    names_to = "colors",
    values_to = "color"
  ) %>% 
  filter(!is.na(color))

# (1) Overall percentage of each color
overall_pct <- df_long %>% 
  count(color, name = "n") %>% 
  mutate(N = sum(n), 
         pct = round((n / N)*100, 1),
         label = paste0(pct, "%"))

# Colores destacados
colores_destacados <- c("Chocolate", "Beige", "Cognac", "Tostado", "Cobre", "Marrón")

# Preparar los datos
top20_colors <- df_long %>% 
  count(color, name = "n") %>% 
  mutate(
    N = sum(n), 
    pct = round((n / N) * 100, 1),
    label = paste0(pct, "%")
  ) %>%
  arrange(desc(pct)) %>%
  slice_max(pct, n = 20) %>%
  mutate(
    color = if_else(color == "Verde oliva", "Verde\noliva", color),
    color = fct_reorder(color, pct),
    fill_color = if_else(color %in% colores_destacados, "#936A4E", "#C6B7A4")
    
  )

# Plot
ggplot(top20_colors, aes(x = color, y = pct, fill = fill_color)) +
  geom_col() +
  scale_fill_identity() +
  geom_text(aes(label = label), 
            vjust = -0.3, 
            color = "#333333",
            fontface = "bold",
            size = 5) +
  labs(
    x = NULL,
    y = "Porcentaje (%)",
    title = "Vestimenta | Top 20 Colores",
    caption = "Fuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood"
  ) +
  theme_ipsum_rc(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 25),
    plot.caption = element_text(size = 14),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 14)
  )

ggsave(here("output", "01_vestimenta_chocolate.jpg"), dpi = 300, width = 16, height = 10)

# (2) Calzado 

calzado <- read_rds(here("data", "previa", "clean", "product_clean_shoes.rds"))

# Pivot longer
df_long <- calzado %>% 
  pivot_longer(
    cols = starts_with("color_"),
    names_to = "colors",
    values_to = "color"
  ) %>% 
  filter(!is.na(color))

# (1) Overall percentage of each color
overall_pct <- df_long %>% 
  count(color, name = "n") %>% 
  mutate(N = sum(n), 
         pct = round((n / N)*100, 1),
         label = paste0(pct, "%"))

# Colores destacados
colores_destacados <- c("Chocolate", "Beige", "Cognac", "Tostado", "Cobre", "Marrón")

# Preparar los datos
top20_colors <- df_long %>% 
  count(color, name = "n") %>% 
  mutate(
    N = sum(n), 
    pct = round((n / N) * 100, 1),
    label = paste0(pct, "%")
  ) %>%
  arrange(desc(pct)) %>%
  slice_max(pct, n = 20) %>%
  mutate(
    color = if_else(color == "Verde oliva", "Verde\noliva", color),
    color = fct_reorder(color, pct),
    fill_color = if_else(color %in% colores_destacados, "#936A4E", "#C6B7A4")
    
  )

# Plot
ggplot(top20_colors, aes(x = color, y = pct, fill = fill_color)) +
  geom_col() +
  scale_fill_identity() +
  geom_text(aes(label = label), 
            vjust = -0.3, 
            color = "#333333",
            fontface = "bold",
            size = 5) +
  labs(
    x = NULL,
    y = "Porcentaje (%)",
    title = "Calzado | Top 20 Colores",
    caption = "Fuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood"
  ) +
  theme_ipsum_rc(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 25),
    plot.caption = element_text(size = 14),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 14)
  )

ggsave(here("output", "02_calzado_chocolate.jpg"), dpi = 300, width = 16, height = 10)

# (3) Carteras & Bolsos 

carteras <- read_rds(here("data", "previa", "clean", "product_clean_bags.rds"))

# Pivot longer
df_long <- carteras %>% 
  pivot_longer(
    cols = starts_with("color_"),
    names_to = "colors",
    values_to = "color"
  ) %>% 
  filter(!is.na(color))

# (1) Overall percentage of each color
overall_pct <- df_long %>% 
  count(color, name = "n") %>% 
  mutate(N = sum(n), 
         pct = round((n / N)*100, 1),
         label = paste0(pct, "%"))

# Colores destacados
colores_destacados <- c("Chocolate", "Beige", "Cognac", "Tostado", "Cobre", "Marrón")

# Preparar los datos
top20_colors <- df_long %>% 
  count(color, name = "n") %>% 
  mutate(
    N = sum(n), 
    pct = round((n / N) * 100, 1),
    label = paste0(pct, "%")
  ) %>%
  arrange(desc(pct)) %>%
  slice_max(pct, n = 20) %>%
  mutate(
    color = if_else(color == "Verde oliva", "Verde\noliva", color),
    color = fct_reorder(color, pct),
    fill_color = if_else(color %in% colores_destacados, "#936A4E", "#C6B7A4")
    
  )

# Plot
ggplot(top20_colors, aes(x = color, y = pct, fill = fill_color)) +
  geom_col() +
  scale_fill_identity() +
  geom_text(aes(label = label), 
            vjust = -0.3, 
            color = "#333333",
            fontface = "bold",
            size = 5) +
  labs(
    x = NULL,
    y = "Porcentaje (%)",
    title = "Carteras y bolsos | Top 20 Colores",
    caption = "Fuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood"
  ) +
  theme_ipsum_rc(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 25),
    plot.caption = element_text(size = 14),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 14)
  )

ggsave(here("output", "03_carteras_chocolate.jpg"), dpi = 300, width = 16, height = 10)
