MODULE trcbc
   !!======================================================================
   !!                     ***  MODULE  trcdta  ***
   !! TOP :  module for passive tracer boundary conditions
   !!=====================================================================
   !!----------------------------------------------------------------------
#if  defined key_top 
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP model 
   !!----------------------------------------------------------------------
   !!   trc_dta    : read and time interpolated passive tracer data
   !!----------------------------------------------------------------------
   USE par_trc       !  passive tracers parameters
   USE oce_trc       !  shared variables between ocean and passive tracers
   USE trc           !  passive tracers common variables
   USE iom           !  I/O manager
   USE lib_mpp       !  MPP library
   USE fldread       !  read input fields
#if defined key_bdy
   USE bdy_oce, only: nb_bdy , idx_bdy, ln_coords_file, rn_time_dmp, rn_time_dmp_out
#endif
   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_bc_init    ! called in trcini.F90 
   PUBLIC   trc_bc_read    ! called in trcstp.F90 or within

   INTEGER  , SAVE, PUBLIC                             :: nb_trcobc   ! number of tracers with open BC
   INTEGER  , SAVE, PUBLIC                             :: nb_trcsbc   ! number of tracers with surface BC
   INTEGER  , SAVE, PUBLIC                             :: nb_trccbc   ! number of tracers with coastal BC
   INTEGER  , SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:)  :: n_trc_indobc ! index of tracer with OBC data
   INTEGER  , SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:)  :: n_trc_indsbc ! index of tracer with SBC data
   INTEGER  , SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:)  :: n_trc_indcbc ! index of tracer with CBC data
   !INTEGER  , SAVE, PUBLIC                             :: ntra_obc     ! MAX( 1, nb_trcxxx ) to avoid compilation error with bounds checking
   !INTEGER  , SAVE, PUBLIC                             :: ntra_sbc     ! MAX( 1, nb_trcxxx ) to avoid compilation error with bounds checking
   !INTEGER  , SAVE, PUBLIC                             :: ntra_cbc     ! MAX( 1, nb_trcxxx ) to avoid compilation error with bounds checking
   REAL(wp) , SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:)  :: rf_trofac   ! multiplicative factor for OBCtracer values
   TYPE(FLD), SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:), TARGET  :: sf_trcobc   ! structure of data input OBC (file informations, fields read)
   REAL(wp) , SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:)  :: rf_trsfac   ! multiplicative factor for SBC tracer values
   TYPE(FLD), SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:)  :: sf_trcsbc   ! structure of data input SBC (file informations, fields read)
   REAL(wp) , SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:)  :: rf_trcfac   ! multiplicative factor for CBC tracer values
   TYPE(FLD), SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:)  :: sf_trccbc   ! structure of data input CBC (file informations, fields read)
   TYPE(MAP_POINTER), ALLOCATABLE, DIMENSION(:) :: nbmap_ptr   ! array of pointers to nbmap
   
   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: trcbc.F90 5215 2015-04-15 16:11:56Z nicolasmartin $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_bc_init(ntrc)
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_bc_init  ***
      !!                    
      !! ** Purpose :   initialisation of passive tracer BC data 
      !! 
      !! ** Method  : - Read namtsd namelist
      !!              - allocates passive tracer BC data structure 
      !!----------------------------------------------------------------------
      !
      INTEGER,INTENT(IN) :: ntrc                           ! number of tracers
      INTEGER            :: jl, jn, ib, ibd, ii, ij, ik    ! dummy loop indices
      INTEGER            :: ierr0, ierr1, ierr2, ierr3     ! temporary integers
      INTEGER            ::  ios                           ! Local integer output status for namelist read
      INTEGER            :: nblen, igrd                    ! support arrays for BDY
      CHARACTER(len=100) :: clndta, clntrc
      !
      CHARACTER(len=100) :: cn_dir_sbc, cn_dir_cbc, cn_dir_obc
      TYPE(FLD_N), ALLOCATABLE, DIMENSION(:) :: slf_i  ! local array of namelist informations on the fields to read
      TYPE(FLD_N), DIMENSION(jpmaxtrc) :: sn_trcobc    ! open
      TYPE(FLD_N), DIMENSION(jpmaxtrc) :: sn_trcsbc    ! surface
      TYPE(FLD_N), DIMENSION(jpmaxtrc) :: sn_trccbc    ! coastal
      REAL(wp)   , DIMENSION(jpmaxtrc) :: rn_trofac    ! multiplicative factor for tracer values
      REAL(wp)   , DIMENSION(jpmaxtrc) :: rn_trsfac    ! multiplicative factor for tracer values
      REAL(wp)   , DIMENSION(jpmaxtrc) :: rn_trcfac    ! multiplicative factor for tracer values
      !!
      NAMELIST/namtrc_bc/ cn_dir_sbc, cn_dir_cbc, sn_trcsbc, rn_trsfac, sn_trccbc, rn_trcfac
#if defined key_bdy
      NAMELIST/namtrc_bdy/ cn_trc_dflt, cn_trc, nn_trcdmp_bdy
      NAMELIST/nambdy_bc/ cn_dir_obc, sn_trcobc, rn_trofac
#endif
      !!----------------------------------------------------------------------
      IF( nn_timing == 1 )  CALL timing_start('trc_bc_init')
      !
      IF( lwp ) THEN
         WRITE(numout,*) ' '
         WRITE(numout,*) 'trc_bc_init : Tracers Boundary Conditions (BC)'
         WRITE(numout,*) '~~~~~~~~~~~ '
         
         !WRITE(numout,*) "In trc_bc_init at A: nstop=", nstop
      ENDIF
      !
      !  Initialisation and local array allocation
      ierr0 = 0  ;  ierr1 = 0  ;  ierr2 = 0  ;  ierr3 = 0  
      ALLOCATE( slf_i(ntrc), STAT=ierr0 )
      IF( ierr0 > 0 ) THEN
         CALL ctl_stop( 'trc_bc_init: unable to allocate local slf_i' )   ;   RETURN
      ENDIF

      ! Compute the number of tracers to be initialised with open, surface and boundary data
      ALLOCATE( n_trc_indobc(ntrc), STAT=ierr0 )
      IF( ierr0 > 0 ) THEN
         CALL ctl_stop( 'trc_bc_init: unable to allocate n_trc_indobc' )   ;   RETURN
      ENDIF
      nb_trcobc      = 0
      n_trc_indobc(:) = 0
      !
      ALLOCATE( n_trc_indsbc(ntrc), STAT=ierr0 )
      IF( ierr0 > 0 ) THEN
         CALL ctl_stop( 'trc_bc_init: unable to allocate n_trc_indsbc' )   ;   RETURN
      ENDIF
      nb_trcsbc      = 0
      n_trc_indsbc(:) = 0
      !
      ALLOCATE( n_trc_indcbc(ntrc), STAT=ierr0 )
      IF( ierr0 > 0 ) THEN
         CALL ctl_stop( 'trc_bc_init: unable to allocate n_trc_indcbc' )   ;   RETURN
      ENDIF
      nb_trccbc      = 0
      n_trc_indcbc(:) = 0
      !
      !DO jn = 1, ntrc
      !   IF( ln_trc_obc(jn) ) THEN
      !       nb_trcobc       = nb_trcobc + 1 
      !       n_trc_indobc(jn) = nb_trcobc 
      !   ENDIF
      !   IF( ln_trc_sbc(jn) ) THEN
      !       nb_trcsbc       = nb_trcsbc + 1
      !       n_trc_indsbc(jn) = nb_trcsbc
      !   ENDIF
      !   IF( ln_trc_cbc(jn) ) THEN
      !       nb_trccbc       = nb_trccbc + 1
      !       n_trc_indcbc(jn) = nb_trccbc
      !   ENDIF
      !ENDDO
      !ntra_obc = MAX( 1, nb_trcobc )   ! To avoid compilation error with bounds checking
      !IF( lwp ) WRITE(numout,*) ' '
      !IF( lwp ) WRITE(numout,*) ' Number of passive tracers to be initialized with open boundary data :', nb_trcobc
      !IF( lwp ) WRITE(numout,*) ' '
      !ntra_sbc = MAX( 1, nb_trcsbc )   ! To avoid compilation error with bounds checking
      !IF( lwp ) WRITE(numout,*) ' '
      !IF( lwp ) WRITE(numout,*) ' Number of passive tracers to be initialized with surface boundary data :', nb_trcsbc
      !IF( lwp ) WRITE(numout,*) ' '
      !ntra_cbc = MAX( 1, nb_trccbc )   ! To avoid compilation error with bounds checking
      !IF( lwp ) WRITE(numout,*) ' '
      !IF( lwp ) WRITE(numout,*) ' Number of passive tracers to be initialized with coastal boundary data :', nb_trccbc
      !IF( lwp ) WRITE(numout,*) ' '

      ! Read Boundary Conditions Namelists
      REWIND( numnat_ref )              ! Namelist namtrc_bc in reference namelist : Passive tracer data structure
      READ  ( numnat_ref, namtrc_bc, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namtrc_bc in reference namelist', lwp )

      REWIND( numnat_cfg )              ! Namelist namtrc_bc in configuration namelist : Passive tracer data structure
      READ  ( numnat_cfg, namtrc_bc, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namtrc_bc in configuration namelist', lwp )
      IF(lwm) WRITE ( numont, namtrc_bc )

#if defined key_bdy
      REWIND( numnat_ref )              ! Namelist namtrc_bc in reference namelist : Passive tracer data structure
      READ  ( numnat_ref, namtrc_bdy, IOSTAT = ios, ERR = 903)
903   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namtrc_bdy in reference namelist', lwp )

      REWIND( numnat_cfg )              ! Namelist namtrc_bc in configuration namelist : Passive tracer data structure
      READ  ( numnat_cfg, namtrc_bdy, IOSTAT = ios, ERR = 904 )
904   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namtrc_bdy in configuration namelist', lwp )
      IF(lwm) WRITE ( numont, namtrc_bdy )
      ! setup up preliminary informations for BDY structure
      REWIND( numnat_ref )
      REWIND( numnat_cfg )
      DO ib = 1, nb_bdy
         DO jn = 1, ntrc
            ! Set type of obc in BDY data structure (around here we may plug user override of obc type from nml)
            !WRITE(numout,*) "ln_trc_obc(jn)=", ln_trc_obc(jn)
            !WRITE(numout,*) "jn=", jn, " ib=", ib, "cn_trc(ib)=", cn_trc(ib)
            !WRITE(numout,*) "jn=", jn, " ib=", ib, "cn_trc_dflt(ib)=", cn_trc_dflt(ib)
            IF ( ln_trc_obc(jn)) THEN
               trcdta_bdy(jn,ib)%cn_obc = TRIM( cn_trc(ib) )
            ELSE
               trcdta_bdy(jn,ib)%cn_obc = TRIM( cn_trc_dflt(ib) )
            ENDIF
            ! set damping use in BDY data structure
            trcdta_bdy(jn,ib)%dmp = .false.
            IF(nn_trcdmp_bdy(ib) .EQ. 1 .AND. ln_trc_obc(jn) ) trcdta_bdy(jn,ib)%dmp = .true.
            IF(nn_trcdmp_bdy(ib) .EQ. 2 ) trcdta_bdy(jn,ib)%dmp = .true.
            IF(trcdta_bdy(jn,ib)%cn_obc == 'frs' .AND. nn_trcdmp_bdy(ib) .NE. 0 )  &
                & CALL ctl_stop( 'Use FRS OR relaxation' )
            IF (nn_trcdmp_bdy(ib) .LT. 0 .OR. nn_trcdmp_bdy(ib) .GT. 2)            THEN
                 WRITE(numout,*) "nn_trcdmp_bdy=", nn_trcdmp_bdy(ib), " ib=", ib  
                 CALL ctl_stop( 'Not a valid option for nn_trcdmp_bdy. Allowed: 0,1,2.' )
            ENDIF
         ENDDO
      ENDDO

#else
      ! Force all tracers OBC to false if bdy not used
      ln_trc_obc = .false.
#endif
      ! compose BC data indexes
      DO jn = 1, ntrc
         IF( ln_trc_obc(jn) ) THEN
             nb_trcobc       = nb_trcobc + 1  ; n_trc_indobc(jn) = nb_trcobc
         ENDIF
         IF( ln_trc_sbc(jn) ) THEN
             nb_trcsbc       = nb_trcsbc + 1  ; n_trc_indsbc(jn) = nb_trcsbc
         ENDIF
         IF( ln_trc_cbc(jn) ) THEN
             nb_trccbc       = nb_trccbc + 1  ; n_trc_indcbc(jn) = nb_trccbc
         ENDIF
      ENDDO
      ! print some information for each 
      !IF( lwp ) THEN
      !   DO jn = 1, ntrc
      !      IF( ln_trc_obc(jn) )  THEN    
      !         clndta = TRIM( sn_trcobc(jn)%clvar ) 
      !         IF(lwp) WRITE(numout,*) 'Preparing to read OBC data file for passive tracer number :', jn, ' name : ', clndta, & 
      !         &               ' multiplicative factor : ', rn_trofac(jn)
      !      ENDIF
      !      IF( ln_trc_sbc(jn) )  THEN    
      !         clndta = TRIM( sn_trcsbc(jn)%clvar ) 
      !         IF(lwp) WRITE(numout,*) 'Preparing to read SBC data file for passive tracer number :', jn, ' name : ', clndta, & 
      !         &               ' multiplicative factor : ', rn_trsfac(jn)
      !      ENDIF
      !      IF( ln_trc_cbc(jn) )  THEN    
      !         clndta = TRIM( sn_trccbc(jn)%clvar ) 
      !         IF(lwp) WRITE(numout,*) 'Preparing to read CBC data file for passive tracer number :', jn, ' name : ', clndta, & 
      !         &               ' multiplicative factor : ', rn_trcfac(jn)
      !      ENDIF
      !   END DO
      !ENDIF
      !
      ! The following code is written this way to reduce memory usage and repeated for each boundary data
      ! MAV: note that this is just a placeholder and the dimensions must be changed according to 
      !      what will be done with BDY. A new structure will probably need to be included
      !
      ! OPEN Lateral boundary conditions
            ! Print summmary of Boundary Conditions
      IF( lwp ) THEN
         WRITE(numout,*) ' '
         WRITE(numout,'(a,i3)') '   Total tracers to be initialized with SURFACE BCs data:', nb_trcsbc
         IF ( nb_trcsbc > 0 ) THEN
            WRITE(numout,*) '   #trc        NAME        Boundary     Mult.Fact. '
            DO jn = 1, ntrc
               IF ( ln_trc_sbc(jn) ) WRITE(numout,9001) jn, TRIM( sn_trcsbc(jn)%clvar ), 'SBC', rn_trsfac(jn)
            ENDDO
         ENDIF
         WRITE(numout,'(2a)') '   SURFACE BC data repository : ', TRIM(cn_dir_sbc)

         WRITE(numout,*) ' '
         WRITE(numout,'(a,i3)') '   Total tracers to be initialized with COASTAL BCs data:', nb_trccbc
         IF ( nb_trccbc > 0 ) THEN
            WRITE(numout,*) '   #trc        NAME        Boundary     Mult.Fact. '
            DO jn = 1, ntrc
               IF ( ln_trc_cbc(jn) ) WRITE(numout, 9001) jn, TRIM( sn_trccbc(jn)%clvar ), 'CBC', rn_trcfac(jn)
            ENDDO
         ENDIF
         WRITE(numout,'(2a)') '   COASTAL BC data repository : ', TRIM(cn_dir_cbc)

         WRITE(numout,*) ' '
         WRITE(numout,'(a,i3)') '   Total tracers to be initialized with OPEN BCs data:', nb_trcobc
#if defined key_bdy
         IF ( nb_trcobc > 0 ) THEN
            WRITE(numout,*) '   #trc        NAME        Boundary     Mult.Fact.   OBC Settings'
            DO jn = 1, ntrc
               IF ( ln_trc_obc(jn) )  WRITE(numout, 9001) jn, TRIM( sn_trcobc(jn)%clvar ), 'OBC', rn_trofac(jn), (trcdta_bdy(jn,ib)%cn_obc,ib=1,nb_bdy)
               IF ( .NOT. ln_trc_obc(jn) )  WRITE(numout, 9002) jn, 'Set data to IC and use default condition', (trcdta_bdy(jn,ib)%cn_obc,ib=1,nb_bdy)
            ENDDO
            WRITE(numout,*) ' '
            DO ib = 1, nb_bdy
                IF (nn_trcdmp_bdy(ib) .EQ. 0) WRITE(numout,9003) '   Boundary ',ib,' -> NO damping of tracers'
                IF (nn_trcdmp_bdy(ib) .EQ. 1) WRITE(numout,9003) '   Boundary ',ib,' -> damping ONLY for tracers with external data provided'
                IF (nn_trcdmp_bdy(ib) .EQ. 2) WRITE(numout,9003) '   Boundary ',ib,' -> damping of ALL tracers'
                IF (nn_trcdmp_bdy(ib) .GT. 0) THEN
                   WRITE(numout,9003) '     USE damping parameters from nambdy for boundary ', ib,' : '
                   WRITE(numout,'(a,f10.2,a)') '     - Inflow damping time scale  : ',rn_time_dmp(ib),' days'
                   WRITE(numout,'(a,f10.2,a)') '     - Outflow damping time scale : ',rn_time_dmp_out(ib),' days'
                ENDIF
            ENDDO
         ENDIF
#endif
         WRITE(numout,'(2a)') '   OPEN BC data repository : ', TRIM(cn_dir_obc)
      ENDIF
9001  FORMAT(2x,i5, 3x, a15, 3x, a5, 6x, e11.3, 4x, 10a13)
9002  FORMAT(2x,i5, 3x, a41, 3x, 10a13)
9003  FORMAT(a, i5, a)
#if defined key_bdy
      ! OPEN Lateral boundary conditions
      IF( nb_trcobc > 0 ) THEN 
         ALLOCATE ( sf_trcobc(nb_trcobc*nb_bdy), rf_trofac(nb_trcobc*nb_bdy), nbmap_ptr(nb_trcobc*nb_bdy), STAT=ierr1 )
         IF( ierr1 > 0 ) THEN
            CALL ctl_stop( 'trc_bc_init: unable to allocate sf_trcobc structure' )   ;   RETURN
         ENDIF

         igrd = 1                       ! Everything is at T-points here

         DO ib = 1, nb_bdy
            READ  ( numnat_ref, nambdy_bc, IOSTAT = ios, ERR = 905)
905         IF( ios /= 0 ) CALL ctl_nam ( ios , 'nambdy_bc in reference namelist', lwp )

            READ  ( numnat_cfg, nambdy_bc, IOSTAT = ios, ERR = 906 )
906         IF( ios /= 0 ) CALL ctl_nam ( ios , 'nambdy_bc in configuration namelist', lwp )
            IF(lwm) WRITE ( numont, nambdy_bc )

            nblen = idx_bdy(ib)%nblen(igrd)
            !WRITE(numout,*) 'nblen=', nblen
            
            DO jn = 1, ntrc
               IF ( ln_trc_obc(jn)) THEN
               ! Initialise from external data
                  jl = n_trc_indobc(jn)
                  slf_i(jl)    = sn_trcobc(jn)
                  rf_trofac(jl+(ib-1)*nb_trcobc) = rn_trofac(jn)
                                               ALLOCATE( sf_trcobc(jl+(ib-1)*nb_trcobc)%fnow(nblen,1,jpk)   , STAT=ierr2 )
                  IF( sn_trcobc(jn)%ln_tint )  ALLOCATE( sf_trcobc(jl+(ib-1)*nb_trcobc)%fdta(nblen,1,jpk,2) , STAT=ierr3 )
                  IF( ierr2 + ierr3 > 0 ) THEN
                    CALL ctl_stop( 'trc_bc_init : unable to allocate passive tracer OBC data arrays' )   ;   RETURN
                  ENDIF
                  trcdta_bdy(jn,ib)%trc => sf_trcobc(jl+(ib-1)*nb_trcobc)%fnow(:,1,:)
                  trcdta_bdy(jn,ib)%rn_fac = rf_trofac(jl+(ib-1)*nb_trcobc)
                  ! create OBC mapping array
                  nbmap_ptr(jl+(ib-1)*nb_trcobc)%ptr => idx_bdy(ib)%nbmap(:,igrd)
                  nbmap_ptr(jl+(ib-1)*nb_trcobc)%ll_unstruc = ln_coords_file(igrd)
               ELSE
               ! Initialise obc arrays from initial conditions
                  ALLOCATE ( trcdta_bdy(jn,ib)%trc(nblen,jpk) )
                  DO ibd = 1, nblen
                     DO ik = 1, jpkm1
                        ii = idx_bdy(ib)%nbi(ibd,igrd)
                        ij = idx_bdy(ib)%nbj(ibd,igrd)
                        trcdta_bdy(jn,ib)%trc(ibd,ik) = trn(ii,ij,ik,jn) * tmask(ii,ij,ik)
                     END DO
                  END DO
                  trcdta_bdy(jn,ib)%rn_fac = 1._wp
               ENDIF
            ENDDO
            CALL fld_fill( sf_trcobc((1+(ib-1)*nb_trcobc):(nb_trcobc+(ib-1)*nb_trcobc)), slf_i, cn_dir_obc, &
            &             'trc_bc_init', 'Passive tracer OBC data', 'namtrc_bc' )
         ENDDO

         ENDIF
#endif
      !IF( nb_trcobc > 0 ) THEN       !  allocate only if the number of tracer to initialise is greater than zero
      !   ALLOCATE( sf_trcobc(nb_trcobc), rf_trofac(nb_trcobc), STAT=ierr1 )
      !   IF( ierr1 > 0 ) THEN
      !      CALL ctl_stop( 'trc_bc_init: unable to allocate  sf_trcobc structure' )   ;   RETURN
      !   ENDIF
      !   !
      !   DO jn = 1, ntrc
      !      IF( ln_trc_obc(jn) ) THEN      ! update passive tracers arrays with input data read from file
      !         jl = n_trc_indobc(jn)
      !         slf_i(jl)    = sn_trcobc(jn)
      !         rf_trofac(jl) = rn_trofac(jn)
      !                                      ALLOCATE( sf_trcobc(jl)%fnow(jpi,jpj,jpk)   , STAT=ierr2 )
      !         IF( sn_trcobc(jn)%ln_tint )  ALLOCATE( sf_trcobc(jl)%fdta(jpi,jpj,jpk,2) , STAT=ierr3 )
      !         IF( ierr2 + ierr3 > 0 ) THEN
      !           CALL ctl_stop( 'trc_bc_init : unable to allocate passive tracer OBC data arrays' )   ;   RETURN
      !         ENDIF
      !      ENDIF
      !      !   
      !   ENDDO
      !   !                         ! fill sf_trcdta with slf_i and control print
      !   CALL fld_fill( sf_trcobc, slf_i, cn_dir, 'trc_bc_init', 'Passive tracer OBC data', 'namtrc_bc' )
      !   !
      !ENDIF
      !
      ! SURFACE Boundary conditions
      IF( nb_trcsbc > 0 ) THEN       !  allocate only if the number of tracer to initialise is greater than zero
         ALLOCATE( sf_trcsbc(nb_trcsbc), rf_trsfac(nb_trcsbc), STAT=ierr1 )
         IF( ierr1 > 0 ) THEN
            CALL ctl_stop( 'trc_bc_init: unable to allocate  sf_trcsbc structure' )   ;   RETURN
         ENDIF
         !
         DO jn = 1, ntrc
            IF( ln_trc_sbc(jn) ) THEN      ! update passive tracers arrays with input data read from file
               jl = n_trc_indsbc(jn)
               slf_i(jl)    = sn_trcsbc(jn)
               rf_trsfac(jl) = rn_trsfac(jn)
                                            ALLOCATE( sf_trcsbc(jl)%fnow(jpi,jpj,1)   , STAT=ierr2 )
               IF( sn_trcsbc(jn)%ln_tint )  ALLOCATE( sf_trcsbc(jl)%fdta(jpi,jpj,1,2) , STAT=ierr3 )
               IF( ierr2 + ierr3 > 0 ) THEN
                 CALL ctl_stop( 'trc_bc_init : unable to allocate passive tracer SBC data arrays' )   ;   RETURN
               ENDIF
            ENDIF
            !   
         ENDDO
         !                         ! fill sf_trcsbc with slf_i and control print
         CALL fld_fill( sf_trcsbc, slf_i, cn_dir_sbc, 'trc_bc_init', 'Passive tracer SBC data', 'namtrc_bc' )
         !
      ENDIF
      
      !COASTAL Boundary conditions
      IF( nb_trccbc > 0 ) THEN       !  allocate only if the number of tracer to initialise is greater than zero
         ALLOCATE( sf_trccbc(nb_trccbc), rf_trcfac(nb_trccbc), STAT=ierr1 )
         IF( ierr1 > 0 ) THEN
            CALL ctl_stop( 'trc_bc_ini: unable to allocate  sf_trccbc structure' )   ;   RETURN
         ENDIF
         !
         DO jn = 1, ntrc
            IF( ln_trc_cbc(jn) ) THEN      ! update passive tracers arrays with input data read from file
               jl = n_trc_indcbc(jn)
               slf_i(jl)    = sn_trccbc(jn)
               rf_trcfac(jl) = rn_trcfac(jn)
                                            ALLOCATE( sf_trccbc(jl)%fnow(jpi,jpj,1)   , STAT=ierr2 )
               IF( sn_trccbc(jn)%ln_tint )  ALLOCATE( sf_trccbc(jl)%fdta(jpi,jpj,1,2) , STAT=ierr3 )
               IF( ierr2 + ierr3 > 0 ) THEN
                 CALL ctl_stop( 'trc_bc_ini : unable to allocate passive tracer CBC data arrays' )   ;   RETURN
               ENDIF
            ENDIF
            !   
         ENDDO
         !                         ! fill sf_trccbc with slf_i and control print
         CALL fld_fill( sf_trccbc, slf_i, cn_dir_cbc, 'trc_bc_init', 'Passive tracer CBC data', 'namtrc_bc' )
         !
      ENDIF
 
      DEALLOCATE( slf_i )          ! deallocate local field structure
      
      !WRITE(numout,*) "In trc_bc_init at B: nstop=", nstop
      IF( nn_timing == 1 )  CALL timing_stop('trc_bc_init')

   END SUBROUTINE trc_bc_init


   SUBROUTINE trc_bc_read(kt, jit)
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_bc_init  ***
      !!
      !! ** Purpose :  Read passive tracer Boundary Conditions data
      !!
      !! ** Method  :  Read BC inputs and update data structures using fldread
      !!              
      !!----------------------------------------------------------------------
   
      ! NEMO
      USE fldread
      
      !! * Arguments
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index
      INTEGER, INTENT( in ), OPTIONAL ::   jit   ! subcycle time-step index (for timesplitting option)
      INTEGER            ::  ib
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('trc_bc_read')

      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'trc_bc_read : Surface boundary conditions for passive tracers.'
         IF(lwp) WRITE(numout,*) '~~~~~~~ '
      ENDIF
      !WRITE(numout,*) "In trc_bc_read at A1: nstop=", nstop
      IF ( PRESENT(jit) ) THEN 
      
#if defined key_bdy
         ! OPEN boundary conditions (use time_offset=+1 as they are applied at the end of the step)
         IF( nb_trcobc > 0 ) THEN
           if (lwp) write(numout,'(a,i5,a,i10)') '   reading OBC data for ', nb_trcobc ,' variable(s) at step ', kt
           DO ib = 1, nb_bdy
             CALL fld_read(kt=kt, kn_fsbc=1, sd=sf_trcobc((1+(ib-1)*nb_trcobc):(nb_trcobc+(ib-1)*nb_trcobc)), &
             &             map=nbmap_ptr((1+(ib-1)*nb_trcobc):(nb_trcobc+(ib-1)*nb_trcobc)), kit=jit, kt_offset=+1)
           END DO
         ENDIF
#endif

         ! SURFACE boundary conditions
         IF( nb_trcsbc > 0 ) THEN
           if (lwp) write(numout,'(a,i5,a,i10)') '   reading SBC data for ', nb_trcsbc ,' variable(s) at step ', kt
           CALL fld_read(kt=kt, kn_fsbc=1, sd=sf_trcsbc, kit=jit)
         ENDIF

         ! COASTAL boundary conditions
         IF( nb_trccbc > 0 ) THEN
           if (lwp) write(numout,'(a,i5,a,i10)') '   reading CBC data for ', nb_trccbc ,' variable(s) at step ', kt
           CALL fld_read(kt=kt, kn_fsbc=1, sd=sf_trccbc, kit=jit)
         ENDIF

      ELSE
      
#if defined key_bdy
         ! OPEN boundary conditions (use time_offset=+1 as they are applied at the end of the step)
         IF( nb_trcobc > 0 ) THEN
           if (lwp) write(numout,'(a,i5,a,i10)') '   reading OBC data for ', nb_trcobc ,' variable(s) at step ', kt
           DO ib = 1, nb_bdy
             CALL fld_read(kt=kt, kn_fsbc=1, sd=sf_trcobc((1+(ib-1)*nb_trcobc):(nb_trcobc+(ib-1)*nb_trcobc)), &
             &             map=nbmap_ptr((1+(ib-1)*nb_trcobc):(nb_trcobc+(ib-1)*nb_trcobc)), kt_offset=+1)
           END DO
         ENDIF
#endif

         ! SURFACE boundary conditions
         IF( nb_trcsbc > 0 ) THEN
           if (lwp) write(numout,'(a,i5,a,i10)') '   reading SBC data for ', nb_trcsbc ,' variable(s) at step ', kt
           CALL fld_read(kt=kt, kn_fsbc=1, sd=sf_trcsbc)
         ENDIF

         ! COASTAL boundary conditions
         IF( nb_trccbc > 0 ) THEN
           if (lwp) write(numout,'(a,i5,a,i10)') '   reading CBC data for ', nb_trccbc ,' variable(s) at step ', kt
           CALL fld_read(kt=kt, kn_fsbc=1, sd=sf_trccbc)
         ENDIF

      ENDIF 
      !
      
      !WRITE(numout,*) "In trc_bc_read at A: nstop=", nstop
      IF( nn_timing == 1 )  CALL timing_stop('trc_bc_read')
      !       

   END SUBROUTINE trc_bc_read
#else
   !!----------------------------------------------------------------------
   !!   Dummy module                              NO 3D passive tracer data
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_bc_read( kt )        ! Empty routine
      WRITE(*,*) 'trc_bc_read: You should not have seen this print! error?', kt
   END SUBROUTINE trc_bc_read
#endif

   !!======================================================================
END MODULE trcbc
