pacman::p_load(tidyverse, here)

# Joyería

joyeria <- read_rds(here("data", "previa", "product_jewelry.rds"))

clean_joyeria <- joyeria %>% 
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
    sustainable = if_else(str_detect(characteristics, regex("Sustentable", ignore_case = TRUE)), 1, 0)
  ) %>%
  select(- characteristics, - description) 

# Paso 4: separar colors
colors_split <- clean_joyeria$colors %>%
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
final_data <- clean_joyeria %>%
  select(-sizes, -colors) %>% # borrar las originales
  bind_cols(colors_df) %>%
  distinct(
    name, price, brand, type, category, uruguayan_made, sustainable,
    color_1, color_2, color_3, color_4, color_5, color_6, color_7, color_8,
    color_9, color_10, color_11, color_12, color_13, color_14, color_15,
    color_16, color_17, color_18, color_19, color_20, color_21, color_22,
    color_23, color_24, color_25, color_26, color_27
  )     

write_rds(final_data, here("data", "previa", "clean", "product_clean_jewelry.rds"))
