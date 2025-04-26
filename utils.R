###############################################################################
###############################################################################
###################################  UTILS ####################################
###############################################################################
###############################################################################

pacman::p_load(tidyverse)

# (1) Count downloaded images per folder (category)

## Count files in a folder and subfolders
folder_path <- "images-previa"

## List all files (with full paths)
file_list <- list.files(path = folder_path, recursive = TRUE, full.names = TRUE)

## Keep only the files (remove directories)
only_files <- file_list[file.info(file_list)$isdir == FALSE]

## Create a tibble to count files per folder
file_counts <- tibble(
  path = only_files,
  folder = dirname(only_files)
)  %>% 
  count(folder, name = "n_files")  %>% 
  arrange(desc(n_files)) %>%
  mutate(
    folder_clean = str_remove(folder, "^images-previa/_"),  # Remove "images-previa/_" at the start
    folder_clean = str_remove(folder_clean, "_50$"),        # Remove "_50" at the end
    category = str_extract(folder_clean, "^[^_]+"),         # Everything before the first "_"
    subcategory = str_remove(folder_clean, "^[^_]+_")       # Everything after the first "_"
  ) %>%
  select(category, subcategory, n_files)

file_counts %>% 
  group_by(category) %>% 
  summarise(n = sum(n_files))


