add_library(fabm_models_jrc OBJECT
            jrc_model_library.F90
            bsem/bsem.F90
            med_ergom/med_ergom.F90
           )

target_link_libraries(fabm_models_jrc PRIVATE fabm_base)

#-----------------------------------------------------------------------
# Copyright (C) 2016 - Karsten Bolding and Jorn Bruggeman (BB)         !
#-----------------------------------------------------------------------
