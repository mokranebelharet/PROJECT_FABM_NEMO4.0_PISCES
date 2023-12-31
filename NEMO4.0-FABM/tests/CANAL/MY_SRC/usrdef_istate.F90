MODULE usrdef_istate
   !!======================================================================
   !!                     ***  MODULE usrdef_istate   ***
   !!
   !!                      ===  CANAL configuration  ===
   !!
   !! User defined : set the initial state of a user configuration
   !!======================================================================
   !! History :  NEMO ! 2017-11  (J. Chanut) Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!  usr_def_istate : initial state in Temperature and salinity
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean space and time domain
   USE dom_oce        
   USE phycst         ! physical constants
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   !   
   USE usrdef_nam
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   usr_def_istate   ! called by istate.F90

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: usrdef_istate.F90 13284 2020-07-09 15:12:23Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
  
   SUBROUTINE usr_def_istate( pdept, ptmask, pts, pu, pv, pssh )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE usr_def_istate  ***
      !! 
      !! ** Purpose :   Initialization of the dynamics and tracers
      !!                Here CANAL configuration 
      !!
      !! ** Method  :   Set a gaussian anomaly of pressure and associated
      !!                geostrophic velocities
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   pdept   ! depth of t-point               [m]
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   ptmask  ! t-point ocean mask             [m]
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts), INTENT(  out) ::   pts     ! T & S fields      [Celsius ; g/kg]
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(  out) ::   pu      ! i-component of the velocity  [m/s] 
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(  out) ::   pv      ! j-component of the velocity  [m/s] 
      REAL(wp), DIMENSION(jpi,jpj)         , INTENT(  out) ::   pssh    ! sea-surface height
      !
      INTEGER  :: ji, jj, jk, jl  ! dummy loop indices
      REAL(wp) :: zx, zy, zP0, zumax, zlambda, zr_lambda2, zn2, zf0, zH, zrho1, za, zf, zdzF
      REAL(wp) :: zpsurf, zdyPs, zdxPs
      REAL(wp) :: zdt, zdu, zdv
      REAL(wp) :: zjetx, zjety, zbeta
      REAL(wp), DIMENSION(jpi,jpj)  ::   zrandom
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'usr_def_istate : CANAL configuration, analytical definition of initial state'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~   '
      !
      zjetx = ABS(rn_ujetszx)/2.
      zjety = ABS(rn_ujetszy)/2.
      !
      zf0   = 2._wp * omega * SIN( rad * rn_ppgphi0 )
      !
      SELECT CASE(nn_initcase)

      CASE(-1)    ! stratif at rest

         ! sea level:
         pssh(:,:) = 0.
         ! temperature:
         pts(:,:,1,jp_tem) = 25. !!30._wp
         pts(:,:,2:jpk,jp_tem) = 22. !!24._wp
         ! salinity:  
         pts(:,:,:,jp_sal) = 35._wp
         ! velocities:
         pu(:,:,:) = 0.
         pv(:,:,:) = 0.

      CASE(0)    ! rest
         
         ! sea level:
         pssh(:,:) = 0.
         ! temperature:
         pts(:,:,:,jp_tem) = 10._wp
         ! salinity:  
         pts(:,:,:,jp_sal) = 35._wp
         ! velocities:
         pu(:,:,:) = 0.
         pv(:,:,:) = 0.
         
      CASE(1)    ! geostrophic zonal jet from -zjety to +zjety

         ! sea level:
         SELECT CASE( nn_fcase )
         CASE(0)    ! f = f0
            ! sea level: ssh = - fuy / g
            WHERE( ABS(gphit) <= zjety )
               pssh(:,:) = - ff_t(:,:) * rn_uzonal * gphit(:,:) * 1.e3 / grav
            ELSEWHERE
               pssh(:,:) = - ff_t(:,:) * rn_uzonal * SIGN(zjety, gphit(:,:)) * 1.e3 / grav
            END WHERE
         CASE(1)    ! f = f0 + beta*y
            ! sea level: ssh = - u / g * ( fy + 0.5 * beta * y^2 )
            zbeta = 2._wp * omega * COS( rad * rn_ppgphi0 ) / ra
            WHERE( ABS(gphit) <= zjety )
               pssh(:,:) = - rn_uzonal / grav * ( zf0 * gphit(:,:) * 1.e3 + 0.5 * zbeta * gphit(:,:) * gphit(:,:) * 1.e6 )
            ELSEWHERE
               pssh(:,:) = - rn_uzonal / grav * ( zf0 * SIGN(zjety, gphit(:,:)) * 1.e3   &
                  &                             + 0.5 * zbeta * zjety * zjety * 1.e6 )
            END WHERE
         END SELECT
         ! temperature:
         pts(:,:,:,jp_tem) = 10._wp
         ! salinity:  
         pts(:,:,jpk,jp_sal) = 0.
         DO jk=1, jpkm1
            WHERE( ABS(gphit) <= zjety )
!!$            WHERE( ABS(gphit) <= zjety*0.5 .AND. ABS(glamt) <= zjety*0.5 ) ! for a square of salt
               pts(:,:,jk,jp_sal) = 35.
            ELSEWHERE
               pts(:,:,jk,jp_sal) = 30.
            END WHERE                    
         END DO
         ! velocities:
         pu(:,:,:) = 0.
         DO jk=1, jpkm1
            WHERE( ABS(gphit) <= zjety ) pu(:,:,jk) = rn_uzonal
         END DO
         pv(:,:,:) = 0.
         !                  
      CASE(2)    ! geostrophic zonal current shear
      
         ! sea level:
         SELECT CASE( nn_fcase )
         CASE(0)    ! f = f0
            ! sea level: ssh = - fuy / g
            WHERE( ABS(gphit) <= zjety )
               pssh(:,:) = - ff_t(:,:) * rn_uzonal * ABS(gphit(:,:)) * 1.e3 / grav
            ELSEWHERE
               pssh(:,:) = - ff_t(:,:) * rn_uzonal * zjety * 1.e3 / grav
            END WHERE
         CASE(1)    ! f = f0 + beta*y
            ! sea level: ssh = - u / g * ( fy + 0.5 * beta * y^2 )
            zbeta = 2._wp * omega * COS( rad * rn_ppgphi0 ) / ra
            WHERE( ABS(gphit) <= zjety )
               pssh(:,:) = - SIGN(rn_uzonal, gphit(:,:)) / grav   &
                  &        * ( zf0 * gphit(:,:) * 1.e3 + 0.5 * zbeta * gphit(:,:) * gphit(:,:) * 1.e6 )
            ELSEWHERE
               pssh(:,:) = - SIGN(rn_uzonal, gphit(:,:)) / grav   &
                  &        * ( zf0 * SIGN(zjety, gphit(:,:)) * 1.e3 + 0.5 * zbeta * zjety * zjety * 1.e6 )
            END WHERE
         END SELECT
         ! temperature:
         pts(:,:,:,jp_tem) = 10._wp
         ! salinity:  
         pts(:,:,:,jp_sal) = 30.
         DO jk=1, jpkm1
            WHERE( ABS(gphiv) <= zjety ) pts(:,:,jk,jp_sal) = 30. + SIGN(1.,gphiv(:,:))
         END DO
         ! velocities:
         pu(:,:,:) = 0.
         DO jk=1, jpkm1
            WHERE( ABS(gphiv) <= zjety ) pu(:,:,jk) = SIGN(rn_uzonal,gphit(:,:))*SIGN(1.,rn_uzonal)
            WHERE( ABS(gphiv) == 0.    ) pu(:,:,jk) = 0.  
         END DO
         pv(:,:,:) = 0.
         !                  
      CASE(3)    ! gaussian zonal currant

         ! zonal current
         DO jk=1, jpkm1
            ! gphit and lambda are both in km
            pu(:,:,jk) = rn_uzonal * EXP( - 0.5 * gphit(:,:)**2 / rn_lambda**2 )
         END DO
         
         ! sea level:
         pssh(:,1) = - ff_t(:,1) / grav * pu(:,1,1) * e2t(:,1)
         DO jl=1, jpnj
            DO jj=nldj, nlej
               DO ji=nldi, nlei
                  pssh(ji,jj) = pssh(ji,jj-1) - ff_t(ji,jj) / grav * pu(ji,jj,1) * e2t(ji,jj)
               END DO
            END DO
            CALL lbc_lnk( 'usrdef_istate', pssh, 'T',  1. )
         END DO
         
         ! temperature:
         pts(:,:,:,jp_tem) = 10._wp
         ! salinity:  
         DO jk=1, jpkm1
            pts(:,:,jk,jp_sal) = pssh(:,:)
         END DO
         ! velocities:
         pv(:,:,:) = 0.
         !            
      CASE(4)    ! geostrophic zonal pulse
   
         DO jj=1, jpj
            DO ji=1, jpi
               IF ( ABS(glamt(ji,jj)) <= zjetx ) THEN
                  zdu = rn_uzonal
               ELSEIF ( ABS(glamt(ji,jj)) <= zjetx + 100. ) THEN
                  zdu = rn_uzonal * ( ( zjetx-ABS(glamt(ji,jj)) )/100. + 1. )
               ELSE
                  zdu = 0.
               END IF
               IF ( ABS(gphit(ji,jj)) <= zjety ) THEN
                  pssh(ji,jj) = - ff_t(ji,jj) * zdu * gphit(ji,jj) * 1.e3 / grav
                  pu(ji,jj,:) = zdu
                  pts(ji,jj,:,jp_sal) = zdu / rn_uzonal + 1.
               ELSE
                  pssh(ji,jj) = - ff_t(ji,jj) * zdu * SIGN(zjety,gphit(ji,jj)) * 1.e3 / grav 
                  pu(ji,jj,:) = 0.
                  pts(ji,jj,:,jp_sal) = 1.
               END IF
            END DO
         END DO
         
         ! temperature:
         pts(:,:,:,jp_tem) = 10._wp * ptmask(:,:,:)        
         pv(:,:,:) = 0.
         
         
       CASE(5)    ! vortex
                  !
         zf0   = 2._wp * omega * SIN( rad * rn_ppgphi0 )
         zumax = rn_vtxmax * SIGN(1._wp, zf0) ! Here Anticyclonic: set zumax=-1 for cyclonic
         zlambda = SQRT(2._wp)*rn_lambda*1.e3       ! Horizontal scale in meters 
         zn2 = 3.e-3**2
         zH = 0.5_wp * 5000._wp
         !
         zr_lambda2 = 1._wp / zlambda**2
         zP0 = rau0 * zf0 * zumax * zlambda * SQRT(EXP(1._wp)/2._wp)
         !
         DO jj=1, jpj
            DO ji=1, jpi
               zx = glamt(ji,jj) * 1.e3
               zy = gphit(ji,jj) * 1.e3
               ! Surface pressure: P(x,y,z) = F(z) * Psurf(x,y)
               zpsurf = zP0 * EXP(-(zx**2+zy**2)*zr_lambda2) - rau0 * ff_t(ji,jj) * rn_uzonal * zy
               ! Sea level:
               pssh(ji,jj) = 0.
               DO jl=1,5
                  zdt = pssh(ji,jj)
                  zdzF = (1._wp - EXP(zdt-zH)) / (zH - 1._wp + EXP(-zH))   ! F'(z)
                  zrho1 = rau0 * (1._wp + zn2*zdt/grav) - zdzF * zpsurf / grav    ! -1/g Dz(P) = -1/g * F'(z) * Psurf(x,y)
                  pssh(ji,jj) = zpsurf / (zrho1*grav) * ptmask(ji,jj,1)   ! ssh = Psurf / (Rho*g)
               END DO
               ! temperature:
               DO jk=1,jpk
                  zdt =  pdept(ji,jj,jk) 
                  zrho1 = rau0 * (1._wp + zn2*zdt/grav)
                  IF (zdt < zH) THEN
                     zdzF = (1._wp-EXP(zdt-zH)) / (zH-1._wp + EXP(-zH))   ! F'(z)
                     zrho1 = zrho1 - zdzF * zpsurf / grav    ! -1/g Dz(P) = -1/g * F'(z) * Psurf(x,y)
                  ENDIF
                  !               pts(ji,jj,jk,jp_tem) = (20._wp + (rau0-zrho1) / 0.28_wp) * ptmask(ji,jj,jk)
                  pts(ji,jj,jk,jp_tem) = (10._wp + (rau0-zrho1) / 0.28_wp) * ptmask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
         ! salinity:  
         pts(:,:,:,jp_sal) = 35._wp * ptmask(:,:,:) 
         !
         ! velocities:
         za = 2._wp * zP0 / zlambda**2
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               zx = glamu(ji,jj) * 1.e3
               zy = gphiu(ji,jj) * 1.e3
               DO jk=1, jpk
                  zdu = 0.5_wp * (pdept(ji,jj,jk) + pdept(ji+1,jj,jk))
                  IF (zdu < zH) THEN
                     zf = (zH-1._wp-zdu+EXP(zdu-zH)) / (zH-1._wp+EXP(-zH))
                     zdyPs = - za * zy * EXP(-(zx**2+zy**2)*zr_lambda2) - rau0 * ff_t(ji,jj) * rn_uzonal
                     pu(ji,jj,jk) = - zf / ( rau0 * ff_t(ji,jj) ) * zdyPs * ptmask(ji,jj,jk) * ptmask(ji+1,jj,jk)
                  ELSE
                     pu(ji,jj,jk) = 0._wp
                  ENDIF
               END DO
            END DO
         END DO
         !
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               zx = glamv(ji,jj) * 1.e3
               zy = gphiv(ji,jj) * 1.e3
               DO jk=1, jpk
                  zdv = 0.5_wp * (pdept(ji,jj,jk) + pdept(ji,jj+1,jk))
                  IF (zdv < zH) THEN
                     zf = (zH-1._wp-zdv+EXP(zdv-zH)) / (zH-1._wp+EXP(-zH))
                     zdxPs = - za * zx * EXP(-(zx**2+zy**2)*zr_lambda2)
                     pv(ji,jj,jk) = zf / ( rau0 * ff_f(ji,jj) ) * zdxPs * ptmask(ji,jj,jk) * ptmask(ji,jj+1,jk)
                  ELSE
                     pv(ji,jj,jk) = 0._wp
                  ENDIF
               END DO
            END DO
         END DO
         !            
         CALL lbc_lnk_multi( 'usrdef_istate', pu, 'U', -1., pv, 'V', -1. )

      END SELECT

      IF (ln_sshnoise) THEN
         CALL RANDOM_SEED()
         CALL RANDOM_NUMBER(zrandom)
         pssh(:,:) = pssh(:,:) + ( 0.1  * zrandom(:,:) - 0.05 )
      END IF
  
   END SUBROUTINE usr_def_istate

   !!======================================================================
END MODULE usrdef_istate
