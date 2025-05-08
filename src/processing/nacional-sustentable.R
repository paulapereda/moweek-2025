pacman::p_load(hrbrthemes, tidyverse, here)

# Sustentable: este producto fue producido con materiales sostenibles.
# Nacional: este producto fue producido en Uruguay.

vestimenta <- read_rds(here("data", "previa", "clean", "product_clean_clothing.rds"))

# (1) Sustentable

sustainable <- vestimenta %>%
  group_by(brand) %>%
  summarise(sustainable = sum(sustainable) / n(), .groups = "drop")

# Separar por criterio
sust_mayor_50 <- sustainable %>% filter(sustainable >= 0.5)
sust_menor_50 <- sustainable %>% filter(sustainable < 0.5)

# Gráfico lollipop
  sust_mayor_50 %>%
    ggplot(aes(x = fct_reorder(brand, sustainable), y = sustainable)) +
    geom_segment(aes(xend = brand, y = 0, yend = sustainable), color = "#dccccc") +
    geom_point(size = 4, color = "#936A4E") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    labs(x = NULL, 
         y = "% de prendas sustentables dentro de la colección F/W25", 
         title = "Vestimenta | 50% o más de la colección es sustentable",
         caption = "Sustentable: este producto fue producido con materiales sostenibles.\nFuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood"
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
      axis.text.x = element_text(size = 14, margin = margin(r = 12)),
      axis.text.y = element_text(size = 14, margin = margin(r = 12)),
      axis.title.x = element_text(size = 12)
    )

ggsave(here("output", "09_sustentable_1.jpg"), dpi = 300, width = 16, height = 10)
  
  
# Gráfico: Histograma

  sustainable %>% 
    ggplot(aes(x = sustainable)) +
    geom_histogram(binwidth = .08, fill = "#936A4E") +
    scale_x_continuous(labels = scales::percent) +
    labs(x = "% prendas sustentables", 
         y = "Nº de marcas",
         title = "Casi 7 de cada 10 marcas no tiene ninguna prenda sustentable",
         caption = "Sustentable: este producto fue producido con materiales sostenibles.\nFuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood"
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
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    )

  ggsave(here("output", "10_sustentable_2.jpg"), dpi = 300, width = 16, height = 10)
  
  # (1) Ncional
  
  nacional <- vestimenta %>%
    group_by(brand) %>%
    summarise(uruguayan_made = sum(uruguayan_made) / n(), .groups = "drop")
  
  # Separar por criterio
  nac_mayor_50 <- nacional %>% filter(uruguayan_made >= 0.5)
  nac_menor_50 <- nacional %>% filter(uruguayan_made < 0.5)
  
  # Gráfico lollipop
  nac_mayor_50 %>%
    ggplot(aes(x = fct_reorder(brand, uruguayan_made), y = uruguayan_made)) +
    geom_segment(aes(xend = brand, y = 0, yend = uruguayan_made), color = "#dccccc") +
    geom_point(size = 3, color = "#936A4E") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    labs(x = NULL, 
         y = "% de prendas nacionales dentro de la colección F/W25", 
         title = "Vestimenta | 50% o más de la colección es producción nacional",
         caption = "Nacional: este producto fue producido en Uruguay.\nFuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood"
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
      axis.text.x = element_text(size = 14, margin = margin(r = 12)),
      axis.text.y = element_text(size = 10, margin = margin(r = 15)),
      axis.title.x = element_text(size = 12)
    )
  
  ggsave(here("output", "11_nacional_1.jpg"), dpi = 300, width = 17, height = 10)
  
  
  # Gráfico: Histograma
  
  nacional %>% 
    ggplot(aes(x = uruguayan_made)) +
    geom_histogram(binwidth = .08, fill = "#936A4E") +
    scale_x_continuous(labels = scales::percent) +
    labs(x = "% prendas sustentables", 
         y = "Nº de marcas",
         title = "Más de 7 de cada 10 marcas producen mayoritariamente en Uruguay",
         caption = "Nacional: este producto fue producido en Uruguay.\nFuente: elaboración propia en base a scraping Moweek, 2025 | @paubgood"
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
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    )
  
  ggsave(here("output", "12_nacional_2.jpg"), dpi = 300, width = 16, height = 10)
  
  