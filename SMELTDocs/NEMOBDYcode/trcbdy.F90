MODULE trcbdy
   !!======================================================================
   !!                       ***  MODULE  bdytrc  ***
   !! Ocean tracers:   Apply boundary conditions for tracers in TOP component
   !!======================================================================
   !! History :  1.0  !  2005-01  (J. Chanut, A. Sellar)  Original code
   !!            3.0  !  2008-04  (NEMO team)  add in the reference version
   !!            3.4  !  2011     (D. Storkey) rewrite in preparation for OBC-BDY merge
   !!            3.5  !  2012     (S. Mocavero, I. Epicoco) Optimization of BDY communications
   !!            3.6  !  2015     (T. Lovato) Adapt BDY for tracers in TOP component
   !!----------------------------------------------------------------------
#if defined key_bdy && key_top
   !!----------------------------------------------------------------------
   !!   'key_bdy'                     Unstructured Open Boundary Conditions
   !!----------------------------------------------------------------------
   !!   trc_bdy            : Apply open boundary conditions to T and S
   !!   trc_bdy_frs        : Apply Flow Relaxation Scheme
   !!----------------------------------------------------------------------
   USE timing                       ! Timing
   USE oce_trc                      ! ocean dynamics and tracers variables
   USE par_trc
   USE trc                          ! ocean space and time domain variables 
   USE bdylib                       ! for orlanski library routines
   USE lbclnk                       ! ocean lateral boundary conditions (or mpp link)
   USE in_out_manager               ! I/O manager
   USE bdy_oce, only: idx_bdy, OBC_INDEX, BDYTMASK       ! ocean open boundary conditions
   USE bdy_par, only: lk_bdy
   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_bdy      ! routine called in trcnxt.F90 
   PUBLIC trc_bdy_dmp  ! routine called in trcstp.F90 

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.6 , NEMO Consortium (2015)
   !! $Id$ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_bdy( kt )
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE trc_bdy  ***
      !!
      !! ** Purpose : - Apply open boundary conditions for tracers in TOP component
      !!                and scale the tracer data
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) :: kt     ! Main time step counter
      !!
      INTEGER               :: ib_bdy, jn ! Loop indeces
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 ) CALL timing_start('trc_bdy')
      !
      !WRITE(numout,*) "In trc_bdy at A: nstop=", nstop
      DO jn = 1, jptra
         DO ib_bdy=1, nb_bdy

            SELECT CASE( trcdta_bdy(jn,ib_bdy)%cn_obc )
            CASE('none')
               CYCLE
            CASE('frs')
               CALL bdy_trc_frs( jn, idx_bdy(ib_bdy), trcdta_bdy(jn,ib_bdy), kt )
            CASE('specified')
               CALL bdy_trc_spe( jn, idx_bdy(ib_bdy), trcdta_bdy(jn,ib_bdy), kt )
            CASE('neumann')
               CALL bdy_trc_nmn( jn, idx_bdy(ib_bdy), trcdta_bdy(jn,ib_bdy), kt )
            CASE('orlanski')
               CALL bdy_trc_orlanski( jn, idx_bdy(ib_bdy), trcdta_bdy(jn,ib_bdy), ll_npo=.false. )
            CASE('orlanski_npo')
               CALL bdy_trc_orlanski( jn, idx_bdy(ib_bdy), trcdta_bdy(jn,ib_bdy), ll_npo=.true. )
            CASE DEFAULT
               WRITE(numout,*) "jn=", jn, "ib_bdy=", ib_bdy, "trcdta_bdy(jn,ib_bdy)%cn_obc=", trcdta_bdy(jn,ib_bdy)%cn_obc
               CALL ctl_stop( 'trc_bdy : unrecognised option for open boundaries for T and S' )
            END SELECT

            !WRITE(numout,*) "In trc_bdy at B: nstop=", nstop
            ! Boundary points should be updated
            CALL lbc_bdy_lnk( tra(:,:,:,jn), 'T', 1., ib_bdy )
            !WRITE(numout,*) "In trc_bdy at C: nstop=", nstop
         ENDDO
      ENDDO
      !
      IF( nn_timing == 1 ) CALL timing_stop('trc_bdy')

   END SUBROUTINE trc_bdy

   SUBROUTINE bdy_trc_frs( jn, idx, dta, kt )
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE bdy_trc_frs  ***
      !!                    
      !! ** Purpose : Apply the Flow Relaxation Scheme for tracers at open boundaries.
      !! 
      !! Reference : Engedahl H., 1995, Tellus, 365-382.
      !!----------------------------------------------------------------------
      INTEGER,         INTENT(in) ::   kt
      INTEGER,         INTENT(in) ::   jn   ! Tracer index
      TYPE(OBC_INDEX), INTENT(in) ::   idx  ! OBC indices
      TYPE(OBC_DATA),  INTENT(in) ::   dta  ! OBC external data
      !! 
      REAL(wp) ::   zwgt           ! boundary weight
      INTEGER  ::   ib, ik, igrd   ! dummy loop indices
      INTEGER  ::   ii, ij         ! 2D addresses
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 ) CALL timing_start('bdy_trc_frs')
      ! 
      igrd = 1                       ! Everything is at T-points here
      DO ib = 1, idx%nblen(igrd)
         DO ik = 1, jpkm1
            ii = idx%nbi(ib,igrd)
            ij = idx%nbj(ib,igrd)
            zwgt = idx%nbw(ib,igrd)
            tra(ii,ij,ik,jn) = ( tra(ii,ij,ik,jn) + zwgt * ( ( dta%trc(ib,ik) * dta%rn_fac)  & 
                        &  - tra(ii,ij,ik,jn) ) ) * tmask(ii,ij,ik)
         END DO
      END DO 
      !
      IF( kt .eq. nit000 ) CLOSE( unit = 102 )
      !
      IF( nn_timing == 1 ) CALL timing_stop('bdy_trc_frs')
      !
   END SUBROUTINE bdy_trc_frs
  
   SUBROUTINE bdy_trc_spe( jn, idx, dta, kt )
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE bdy_trc_frs  ***
      !!                    
      !! ** Purpose : Apply a specified value for tracers at open boundaries.
      !! 
      !!----------------------------------------------------------------------
      INTEGER,         INTENT(in) ::   kt
      INTEGER,         INTENT(in) ::   jn   ! Tracer index
      TYPE(OBC_INDEX), INTENT(in) ::   idx  ! OBC indices
      TYPE(OBC_DATA),  INTENT(in) ::   dta  ! OBC external data
      !! 
      REAL(wp) ::   zwgt           ! boundary weight
      INTEGER  ::   ib, ik, igrd   ! dummy loop indices
      INTEGER  ::   ii, ij         ! 2D addresses
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 ) CALL timing_start('bdy_trc_spe')
      !
      igrd = 1                       ! Everything is at T-points here
      DO ib = 1, idx%nblenrim(igrd)
         ii = idx%nbi(ib,igrd)
         ij = idx%nbj(ib,igrd)
         DO ik = 1, jpkm1
            tra(ii,ij,ik,jn) = dta%trc(ib,ik) * dta%rn_fac * tmask(ii,ij,ik)
         END DO
      END DO
      !
      IF( kt .eq. nit000 ) CLOSE( unit = 102 )
      !
      IF( nn_timing == 1 ) CALL timing_stop('bdy_trc_spe')
      !
   END SUBROUTINE bdy_trc_spe

   SUBROUTINE bdy_trc_nmn( jn, idx, dta, kt )
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE bdy_trc_nmn  ***
      !!                    
      !! ** Purpose : Duplicate the value for tracers at open boundaries.
      !! 
      !!----------------------------------------------------------------------
      INTEGER,         INTENT(in) ::   kt
      INTEGER,         INTENT(in) ::   jn   ! Tracer index
      TYPE(OBC_INDEX), INTENT(in) ::   idx  ! OBC indices
      TYPE(OBC_DATA),  INTENT(in) ::   dta  ! OBC external data
      !! 
      REAL(wp) ::   zwgt           ! boundary weight
      INTEGER  ::   ib, ik, igrd   ! dummy loop indices
      INTEGER  ::   ii, ij, zcoef, zcoef1, zcoef2, ip, jp   ! 2D addresses
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 ) CALL timing_start('bdy_trc_nmn')
      !
      !WRITE(numout,*) "Called bdy_trc_nmn"
      igrd = 1                       ! Everything is at T-points here
      DO ib = 1, idx%nblenrim(igrd)
         ii = idx%nbi(ib,igrd)
         ij = idx%nbj(ib,igrd)
         DO ik = 1, jpkm1
            ! search the sense of the gradient
            zcoef1 = bdytmask(ii-1,ij  ) +  bdytmask(ii+1,ij  )
            zcoef2 = bdytmask(ii  ,ij-1) +  bdytmask(ii  ,ij+1)
            IF ( zcoef1+zcoef2 == 0) THEN
               ! corner
               zcoef = tmask(ii-1,ij,ik) + tmask(ii+1,ij,ik) +  tmask(ii,ij-1,ik) +  tmask(ii,ij+1,ik)
               tra(ii,ij,ik,jn) = tra(ii-1,ij  ,ik,jn) * tmask(ii-1,ij  ,ik) + &
                 &                tra(ii+1,ij  ,ik,jn) * tmask(ii+1,ij  ,ik) + &
                 &                tra(ii  ,ij-1,ik,jn) * tmask(ii  ,ij-1,ik) + &
                 &                tra(ii  ,ij+1,ik,jn) * tmask(ii  ,ij+1,ik)
               tra(ii,ij,ik,jn) = ( tra(ii,ij,ik,jn) / MAX( 1, zcoef) ) * tmask(ii,ij,ik)
            ELSE
               ip = bdytmask(ii+1,ij  ) - bdytmask(ii-1,ij  )
               jp = bdytmask(ii  ,ij+1) - bdytmask(ii  ,ij-1)
               tra(ii,ij,ik,jn) = tra(ii+ip,ij+jp,ik,jn) * tmask(ii+ip,ij+jp,ik)
            ENDIF
         END DO
      END DO
      !
      IF( kt .eq. nit000 ) CLOSE( unit = 102 )
      !
      IF( nn_timing == 1 ) CALL timing_stop('bdy_trc_nmn')
      !
   END SUBROUTINE bdy_trc_nmn
 

   SUBROUTINE bdy_trc_orlanski( jn, idx, dta, ll_npo )
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE bdy_trc_orlanski  ***
      !!             
      !!              - Apply Orlanski radiation to tracers of TOP component. 
      !!              - Wrapper routine for bdy_orlanski_3d
      !! 
      !!
      !! References:  Marchesiello, McWilliams and Shchepetkin, Ocean Modelling vol. 3 (2001)    
      !!----------------------------------------------------------------------
      INTEGER,                      INTENT(in) ::   jn      ! Tracer index
      TYPE(OBC_INDEX),              INTENT(in) ::   idx     ! OBC indices
      TYPE(OBC_DATA),               INTENT(in) ::   dta     ! OBC external data
      LOGICAL,                      INTENT(in) ::   ll_npo  ! switch for NPO version

      INTEGER  ::   igrd                                    ! grid index
      !!----------------------------------------------------------------------

      IF( nn_timing == 1 ) CALL timing_start('bdy_trc_orlanski')
      !
      igrd = 1      ! Orlanski bc on tracers; 
      !            
      CALL bdy_orlanski_3d( idx, igrd, trb(:,:,:,jn), tra(:,:,:,jn), (dta%trc * dta%rn_fac), ll_npo )
      !
      IF( nn_timing == 1 ) CALL timing_stop('bdy_trc_orlanski')
      !

   END SUBROUTINE bdy_trc_orlanski

   SUBROUTINE trc_bdy_dmp( kt )
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE trc_bdy_dmp  ***
      !!                    
      !! ** Purpose : Apply damping for tracers at open boundaries.
      !!             It currently applies the damping to all tracers!!!
      !! 
      !!----------------------------------------------------------------------
      INTEGER,         INTENT(in) ::   kt
      !! 
      INTEGER  ::   jn             ! Tracer index
      REAL(wp) ::   zwgt           ! boundary weight
      REAL(wp) ::   zta, zsa, ztime
      INTEGER  ::   ib, ik, igrd   ! dummy loop indices
      INTEGER  ::   ii, ij         ! 2D addresses
      INTEGER  ::   ib_bdy         ! Loop index
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 ) CALL timing_start('trc_bdy_dmp')
      !
      DO jn = 1, jptra
         DO ib_bdy=1, nb_bdy
            IF ( trcdta_bdy(jn, ib_bdy)%dmp ) THEN
               igrd = 1                       ! Everything is at T-points here
               DO ib = 1, idx_bdy(ib_bdy)%nblen(igrd)
                  ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
                  ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
                  zwgt = idx_bdy(ib_bdy)%nbd(ib,igrd)
                  DO ik = 1, jpkm1
                     zta = zwgt * ( trcdta_bdy(jn, ib_bdy)%trc(ib,ik) - trb(ii,ij,ik,jn) ) * tmask(ii,ij,ik)
                     tra(ii,ij,ik,jn) = tra(ii,ij,ik,jn) + zta
                  END DO
               END DO
            ENDIF
         ENDDO
      ENDDO
      !
      IF( nn_timing == 1 ) CALL timing_stop('trc_bdy_dmp')
      !
   END SUBROUTINE trc_bdy_dmp
 
#else
   !!----------------------------------------------------------------------
   !!   Dummy module                   NO Unstruct Open Boundary Conditions
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_bdy(kt)      ! Empty routine
      WRITE(*,*) 'trc_bdy: You should not have seen this print! error?', kt
   END SUBROUTINE trc_bdy

   SUBROUTINE trc_bdy_dmp(kt)      ! Empty routine
      WRITE(*,*) 'trc_bdy_dmp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_bdy_dmp

#endif

   !!======================================================================
END MODULE trcbdy
