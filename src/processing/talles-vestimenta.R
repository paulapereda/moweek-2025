pacman::p_load(hrbrthemes, tidyverse, here)

# (1) Vestimenta 

vestimenta <- read_rds(here("data", "previa", "clean", "product_clean_clothing.rds"))

# Pivot longer
df_long <- vestimenta %>% 
  pivot_longer(
    cols = starts_with("size_"),
    names_to = "sizes",
    values_to = "size"
  ) %>% 
  filter(!is.na(size)) %>% 
  mutate(grupo = case_when(
    category %in% c("Blazers Y Chaquetas", "Camperas Y Tapados", "Capas Y Ponchos") ~ "Outerwear",
    category %in% c("Blusas Y Tops", "Camisas", "Remeras", "Buzos Y Sacos") ~ "Tops",
    category %in% c("Jeans", "Pantalones", "Shorts Y Bermudas", "Polleras") ~ "Bottoms",
    category %in% c("Vestidos", "Monos", "Conjuntos") ~ "One piece",
    category %in% c("Chalecos Y Kimonos", "Ruanas Y Chales") ~ "Layering/Knit")) %>% 
  filter(!is.na(size)) %>% 
  group_by(grupo, brand, size) %>% 
  summarise(n = n()) %>% 
  ungroup()

### Fijarse pa campańa que esta2 sacando talle mal

### (1) Outerwear: Blazers Y Chaquetas, Camperas Y Tapados, Capas Y Ponchos

outerwear <- df_long %>% 
  filter(grupo == "Outerwear")

aux <- tibble(size_value = c("U", 
                             "0", "1", "2", "3", "4", "5", "6",
                             "S/M", "M/L", "L/XL",
                             "XXS", "XS", "S", "M", "L", "XL", "XXL", "XXXL", "4XL",
                             "22", "23", "24", "25", "26", "27", 
                             "28", "29", "30", "31", "32", "33", 
                             "34", "36", "38", "40", "42", "44"),
              x = lag(cumsum(c(2L, 
                               2L, 2L, 2L, 2L, 2L, 2L, 2L,
                               3L, 3L, 3L, 
                               3L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 
                               2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                               2L, 2L, 2L, 2L, 2L, 2L, 1L)), default = 0))