pacman::p_load(tidyverse, rvest, here, httr)

# (!) CAMBIAR EL 01 AL 09 PARA NO SOBREESCRIBIR ARCHIVOS 

## (1) INFORMATION

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

## (2) IMAGES

# Info - Vestimenta, calzado, carteras, joyería, belleza
source(here("src", "extract-images-previa", "extract-previa.R"))

### Code:
#### - 01: previa moweek ✓
#### - 02: domingo pre moweek ✓
#### - 03: lunes pre moweek ✓
#### - 04: martes pre moweek
#### - 05: miércoles pre moweek
#### - 06: jueves pre moweek
#### - 07: viernes de moweek (día 1)
#### - 08: viernes de moweek (día 2)
#### - 09: viernes de moweek (día 3)