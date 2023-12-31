










MODULE par_sed
   !!======================================================================
   !!                        ***  par_sed  ***
   !! Sediment :   set sediment parameter
   !!======================================================================
   !! History :
   !!        !  06-12  (C. Ethe)  Orignal
   !!----------------------------------------------------------------------
   !! $Id: par_sed.F90 10222 2018-10-25 09:42:23Z aumont $

   !! Domain characteristics
   USE par_kind
   USE par_oce , ONLY :       &
      jpi      =>   jpi   ,  & !: first  dimension of grid --> i
      jpj      =>   jpj   ,  & !: second dimension of grid --> j
      jpim1    =>   jpim1 ,  & !: jpi - 1
      jpjm1    =>   jpjm1 ,  & !: jpj - 1
      jpij     =>   jpij  ,  & !: jpi x jpj
      jp_tem   =>   jp_tem,  & !: indice of temperature
      jp_sal   =>   jp_sal     !: indice of salintity

   INTEGER, PARAMETER :: jpdta = 17

   ! Vertical sediment geometry
   INTEGER, PUBLIC   ::      &
      jpksed   = 11 ,        &
      jpksedm1 = 10

   ! sediment tracer species
   INTEGER, PARAMETER ::    &
      jpsol =  8,           &  !: number of solid component
      jpwat = 10,           &   !: number of pore water component
      jpwatp1 = jpwat +1,   &
      jpsol1 = jpsol - 1

   
   ! pore water components       
   INTEGER, PARAMETER :: &
      jwsil  = 1,        & !: silic acid
      jwoxy  = 2,        & !: oxygen
      jwdic  = 3,        & !: dissolved inorganic carbon
      jwno3  = 4,        & !: nitrate
      jwpo4  = 5,        & !: phosphate
      jwalk  = 6,        & !: alkalinity
      jwnh4  = 7,        & !: Ammonium
      jwh2s  = 8,        & !: Sulfate
      jwso4  = 9,        & !: H2S
      jwfe2  = 10          !: Fe2+

   ! solid components       
   INTEGER, PARAMETER ::  &
      jsopal  = 1,        & !: opal sediment
      jsclay  = 2,        & !: clay
      jspoc   = 3,        & !: organic carbon
      jscal   = 4,        & !: calcite
      jspos   = 5,        & !: semi-ref POC
      jspor   = 6,        & !: refractory POC
      jsfeo   = 7,        & !: iron hydroxides
      jsfes   = 8           !: FeS

   INTEGER, PARAMETER ::  &
      jptrased   = jpsol + jpwat , &
      jpdia3dsed = 2             , &
      jpdia2dsed = 12

END MODULE par_sed
