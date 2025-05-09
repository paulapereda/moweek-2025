pacman::p_load(tidyverse, here)

# (1) Vestimenta

vestimenta_01 <- read_rds(here("data", "previa", "product_01_clothing.rds")) %>% 
  distinct(name, price, brand, type, category, characteristics, sizes, colors, description)

vestimenta_02 <- read_rds(here("data", "previa", "product_02_clothing.rds")) %>% 
  distinct(name, price, brand, type, category, characteristics, sizes, colors, description)

vestimenta_03 <- read_rds(here("data", "previa", "product_03_clothing.rds")) %>% 
  distinct(name, price, brand, type, category, characteristics, sizes, colors, description)

vestimenta_04 <- read_rds(here("data", "previa", "product_04_clothing.rds")) %>% 
  distinct(name, price, brand, type, category, characteristics, sizes, colors, description)


clean_vestimenta <- vestimenta_01 %>%
  bind_rows(vestimenta_02) %>% 
  bind_rows(vestimenta_03) %>% 
  bind_rows(vestimenta_04) %>% 
  distinct(name, price, brand, type, category, characteristics, sizes, colors, description) %>% 
  filter(!(price %in% c("$ 5.200\n\n                                                    $ 4.160"))) %>%
  mutate(

    image_name = name %>%
      str_to_lower() %>%                         # lowercase
      str_replace_all("[^a-z0-9]+", "_") %>%     # replace anything that's not a-z, 0-9 with _
      str_replace_all("_+", "_") %>%             # collapse multiple underscores
      str_remove("^_|_$"),                       # remove leading or trailing underscores
    
    # Paso 1: limpiar price
    price = price %>%
      str_remove_all("\\.") %>% # eliminar puntos
      str_remove_all("\\$") %>% # eliminar $
      str_trim() %>%
      as.numeric(),

    # Paso 2: limpiar brand

    brand = brand %>%
      str_remove("^by\\s+") %>% # eliminar "by " y espacios
      str_squish(), # eliminar espacios extra

    # Paso 3: crear uruguayan_made y sustainable

    uruguayan_made = if_else(str_detect(characteristics, regex("Hecho en Uruguay", ignore_case = TRUE)), 1, 0),
    sustainable = if_else(str_detect(characteristics, regex("Sustentable", ignore_case = TRUE)), 1, 0),
    sizes = if_else(sizes == "", "U", sizes)
  ) %>% 
  distinct(name, price, brand, type, category, characteristics, sizes, colors, description, uruguayan_made, sustainable) %>%
  select(- characteristics, - description) 

rm(vestimenta_01, vestimenta_02, vestimenta_03, vestimenta_04)

# Paso 4: separar sizes
# Primero separar los tamaños usando salto de línea y limpiar

sizes_split <- clean_vestimenta$sizes %>%
  map(
    ~ .x %>%
      str_split("\\n") %>%
      unlist() %>%
      str_trim() %>%
      discard(~ .x == "") # eliminar vacíos
  )

# ¿Cuál es el máximo número de talles en una fila?
max_sizes <- max(map_int(sizes_split, length))

# Crear columnas size_1, size_2, ..., size_n
sizes_df <- sizes_split %>%
  map(~ c(.x, rep(NA, max_sizes - length(.x)))) %>%
  map_dfr(~ set_names(as.list(.x), paste0("size_", 1:max_sizes)))

# Paso 5: separar colors
colors_split <- clean_vestimenta$colors %>%
  map(~ .x %>%
    str_split("\\n") %>%
    unlist() %>%
    str_trim() %>%
    discard(~ .x == ""))

# ¿Cuál es el máximo número de colores en una fila?
max_colors <- max(map_int(colors_split, length))

# Crear columnas color_1, color_2, ..., color_n
colors_df <- colors_split %>%
  map(~ c(.x, rep(NA, max_colors - length(.x)))) %>%
  map_dfr(~ set_names(as.list(.x), paste0("color_", 1:max_colors)))

# Paso 6: 
#### - Juntar todo,
#### - Eliminar filas repetidas
final_data <- clean_vestimenta %>%
  select(-sizes, -colors) %>% # borrar las originales
  bind_cols(sizes_df, colors_df) %>%
  distinct(
    name, price, brand, type, category, uruguayan_made, sustainable,
    size_1, size_2, size_3, size_4, size_5, size_6, size_7, size_8,
    size_9, size_10, size_11, size_12, size_13,
    color_1, color_2, color_3, color_4, color_5, color_6, color_7, color_8,
    color_9, color_10, color_11, color_12, color_13, color_14, color_15,
    color_16
  ) %>% 
  mutate(grupo = case_when(
    category %in% c("Blazers Y Chaquetas", "Camperas Y Tapados", "Capas Y Ponchos") ~ "Outerwear",
    category %in% c("Blusas Y Tops", "Camisas", "Remeras", "Buzos Y Sacos") ~ "Tops",
    category %in% c("Jeans", "Pantalones", "Shorts Y Bermudas", "Polleras") ~ "Bottoms",
    category %in% c("Vestidos", "Monos", "Conjuntos") ~ "One piece",
    category %in% c("Chalecos Y Kimonos", "Ruanas Y Chales") ~ "Layering/Knit"))

rm(
  clean_vestimenta, sizes_df, sizes_split, colors_df, colors_split
  )

write_rds(final_data, here("data", "previa", "clean", "product_clean_clothing.rds"))
