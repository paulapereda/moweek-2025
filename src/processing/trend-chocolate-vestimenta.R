pacman::p_load(hrbrthemes, tidyverse, here)

# 1. Leer y pivotear igual que antes
vestimenta <- read_rds(here("data", "previa", "clean", "product_clean_clothing.rds"))

df_long <- vestimenta %>% 
  pivot_longer(
    cols  = starts_with("color_"),
    names_to  = "colors",
    values_to = "color"
  ) %>% 
  filter(!is.na(color))

# 2. TABLA: % de cada color *dentro* de cada categoría -------------
colores_destacados <- c("Chocolate", "Beige", "Cognac", "Tostado", "Cobre", "Marrón")

pct_color_por_cat <- df_long %>% 
  group_by(grupo, color) %>% 
  summarise(n = n(), .groups = "drop_last") %>%      # cuenta por color en cada categoría
  mutate(
    N_cat = sum(n),                                  # total de ítems de esa categoría
    pct   = round((n/N_cat) * 100, 1),
    label = paste0(pct, "%")
  ) %>% 
  ungroup()                                          # volvemos a plano

# 3. (Opcional) Quedarte con el top‑20 de cada categoría ----------
top20_por_cat <- pct_color_por_cat %>% 
  group_by(grupo) %>% 
  slice_max(order_by = pct, n = 20, with_ties = FALSE) %>% 
  mutate(
    color = case_when(
      color == "Verde agua" ~ "Verde\nagua",
      color == "Azul marino" ~ "Azul\nmarino",
      color == "Verde oliva" ~ "Verde\noliva",
      color == "Rosado pastel" ~ "Rosado\npastel",
      color == "Azul piedra" ~ "Azul\npiedra",
      color == "Off white" ~ "Off\nwhite",
      .default = color),
    fill_color = if_else(color %in% colores_destacados,
                         "#936A4E",                  # tono marrón destacado
                         "#C6B7A4")                # resto
  
)


# 4. GRÁFICOS
### (a) Outerwear

ggplot(top20_por_cat %>% filter(grupo == "Outerwear") %>% 
         mutate(color = fct_reorder(color, pct)),
       aes(x = color, y = pct, fill = fill_color, label = label)) +
  geom_col(width = 0.8, show.legend = FALSE) +
  geom_text(aes(label = label), 
            vjust = -0.3, 
            color = "#333333",
            fontface = "bold",
            size = 5) +
  scale_fill_identity() +
  labs(
    x = NULL,
    y = "Porcentaje (%)",
    title = "Outerwear | Top 20 Colores",
    caption = "Sub-categoría que incluye: Blazers y Chaquetas, Camperas y Tapados, Capas y Ponchos\nFuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood"
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

ggsave(here("output", "04_outerwear.jpg"), dpi = 300, width = 16, height = 10)

### (b) Tops

ggplot(top20_por_cat %>% filter(grupo == "Tops") %>% 
         mutate(color = fct_reorder(color, pct)),
       aes(x = color, y = pct, fill = fill_color, label = label)) +
  geom_col(width = 0.8, show.legend = FALSE) +
  geom_text(aes(label = label), 
            vjust = -0.3, 
            color = "#333333",
            fontface = "bold",
            size = 5) +
  scale_fill_identity() +
  labs(
    x = NULL,
    y = "Porcentaje (%)",
    title = "Tops | Top 20 Colores",
    caption = "Sub-categoría que incluye: Blusas y Tops, Camisas, Remeras, Buzos y Sacos\nFuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood"
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

ggsave(here("output", "05_tops.jpg"), dpi = 300, width = 16, height = 10)

### (c) Bottoms

ggplot(top20_por_cat %>% filter(grupo == "Bottoms") %>% 
         mutate(color = fct_reorder(color, pct)),
       aes(x = color, y = pct, fill = fill_color, label = label)) +
  geom_col(width = 0.8, show.legend = FALSE) +
  geom_text(aes(label = label), 
            vjust = -0.3, 
            color = "#333333",
            fontface = "bold",
            size = 5) +
  scale_fill_identity() +
  labs(
    x = NULL,
    y = "Porcentaje (%)",
    title = "Bottoms | Top 20 Colores",
    caption = "Sub-categoría que incluye: Jeans, Pantalones, Shorts y Bermudas, Polleras\nFuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood"
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

ggsave(here("output", "06_bottoms.jpg"), dpi = 300, width = 16, height = 10)


### (d) One piece

ggplot(top20_por_cat %>% filter(grupo == "One piece") %>% 
         mutate(color = fct_reorder(color, pct)),
       aes(x = color, y = pct, fill = fill_color, label = label)) +
  geom_col(width = 0.8, show.legend = FALSE) +
  geom_text(aes(label = label), 
            vjust = -0.3, 
            color = "#333333",
            fontface = "bold",
            size = 5) +
  scale_fill_identity() +
  labs(
    x = NULL,
    y = "Porcentaje (%)",
    title = "One piece | Top 20 Colores",
    caption = "Sub-categoría que incluye: Vestidos, Monos, Conjuntos\nFuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood"
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

ggsave(here("output", "07_one_piece.jpg"), dpi = 300, width = 16, height = 10)

### Layering/Knit

ggplot(top20_por_cat %>% filter(grupo == "Layering/Knit") %>% 
         mutate(color = fct_reorder(color, pct)),
       aes(x = color, y = pct, fill = fill_color, label = label)) +
  geom_col(width = 0.8, show.legend = FALSE) +
  geom_text(aes(label = label), 
            vjust = -0.3, 
            color = "#333333",
            fontface = "bold",
            size = 5) +
  scale_fill_identity() +
  labs(
    x = NULL,
    y = "Porcentaje (%)",
    title = "Layering/Knit | Top 20 Colores",
    caption = "Sub-categoría que incluye: Chalecos y Kimonos, Ruanas y Chales\nFuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood"
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

ggsave(here("output", "08_layering_knits.jpg"), dpi = 300, width = 16, height = 10)

# 
# 
# 
# # 5. (Opcional) Tabla resumen de la familia marrón ----------------
# familia_marron <- pct_color_por_cat %>% 
#   mutate(grupo_marron = color %in% colores_destacados) %>% 
#   group_by(grupo, grupo_marron) %>% 
#   summarise(pct = sum(pct), .groups = "drop") %>% 
#   mutate(
#     grupo = if_else(grupo_marron, "Familia marrón", "Otros colores")
#   ) %>% 
#   select(-grupo_marron) %>% 
#   pivot_wider(names_from = grupo, values_from = pct)
# 
# familia_marron
