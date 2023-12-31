










MODULE trcwri_age
   !!======================================================================
   !!                       *** MODULE trcwri ***
   !!    age :   Output of age tracers
   !!======================================================================
   !! History :   1.0  !  2009-05 (C. Ethe)  Original code
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !! trc_wri_age   :  outputs of concentration fields
   !!----------------------------------------------------------------------
   USE par_age     
   USE trc         
   USE iom

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_wri_age 

CONTAINS

   SUBROUTINE trc_wri_age
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_wri_trc  ***
      !!
      !! ** Purpose :   output passive tracers fields 
      !!---------------------------------------------------------------------
      CHARACTER (len=20)   :: cltra
      INTEGER              :: jn
      !!---------------------------------------------------------------------

      ! write the tracer concentrations in the file

      cltra = TRIM( ctrcnm(jp_age) )                  ! short title for tracer
      CALL iom_put( cltra, trn(:,:,:,jp_age) )

      !
   END SUBROUTINE trc_wri_age


   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcwri_age.F90 10070 2018-08-28 14:30:54Z nicolasmartin $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!======================================================================
END MODULE trcwri_age
