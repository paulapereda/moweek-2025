pacman::p_load(tidyverse, rvest, here, httr)

## (1) IMAGES

# Imágenes - Vestimenta, calzado, carteras, joyería, belleza
 source(here("src", "extract-images-previa", "extract-previa.R"))

## (2) INFORMATION

# Info - Vestimenta
source(here("src", "extract-info-previa", "extract-previa-vestimenta.R"))

# Info - Calzado
source(here("src", "extract-info-previa", "extract-previa-calzado.R"))

# Info - Carteras
source(here("src", "extract-info-previa", "extract-previa-carteras.R"))

# Info - Joyería
source(here("src", "extract-info-previa", "extract-previa-joyeria.R"))

# Info - Belleza
source(here("src", "extract-info-previa", "extract-previa-belleza.R"))





