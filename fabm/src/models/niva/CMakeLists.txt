add_library(fabm_models_niva OBJECT
            niva_model_library.F90
            oxydep/oxydep.F90
            brom/brom_bio.F90
            brom/brom_bioplast.F90
            brom/brom_carb.F90
            brom/brom_eqconst.F90
            brom/brom_redox.F90
            brom/brom_salt.F90
            domcast/domcast.F90
            roms/niva_roms_npzd_Franks.F90
           )

target_link_libraries(fabm_models_niva PRIVATE fabm_base)

#-----------------------------------------------------------------------
# Copyright (C) 2014 - Karsten Bolding and Jorn Bruggeman (BB)         !
#-----------------------------------------------------------------------
