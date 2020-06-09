program main
  use omp_lib; use prec; use csv_module; use iso_fortran_env, only: wp => real64
  use parameters; use globals; use globals_smm; use process
  implicit none

  type(csv_file) :: f
  character(len=30),dimension(:),allocatable :: header
  real(wp),dimension(:),allocatable :: c1,c2,c3,c4
  logical :: status_ok
  integer,dimension(:),allocatable :: itypes

  type(param) p
  integer t,tstart,tfinal,ji00,jrii,jrff,jlbb
  real(dp) mss,x(tt,ncols)

  tt1 = omp_get_wtime()

  ! Get data from csv file
  call f%read('data/export001.csv',header_row=1,status_ok=status_ok)
  call f%get_header(header,status_ok)
  call f%variable_types(itypes,status_ok)

  call f%get(1 ,c1 ,status_ok)
  call f%get(2 ,c2 ,status_ok)
  call f%get(3 ,c3 ,status_ok)
  call f%get(4 ,c4 ,status_ok)

  call f%close(status_ok)
  
  ! Which estimation vector?
  tstart = -1
  if(c3(1).gt.-100d0)then
     tstart = int(c1(1))
  else
     do t=2,size(c1)
        if(c3(t).gt.-100d0.and.c3(t-1).le.-100d0)then
           tstart = int(c1(t))
           exit
        end if
     end do
  end if
  if(tstart.lt.0) stop "Error - invalid data on tstart"

  if(2.eq.1)then
     ! choose automatically tfinal
     tfinal = -1
     if(c3(size(c1)).gt.-100d0)then
        tfinal = int(c1(size(c1)))
     else
        do t=size(c1)-1,1,-1
           if(c3(t).gt.-100d0.and.c3(t+1).le.-100d0)then
              tfinal = int(c1(t))
              exit
           end if
        end do
     end if
     if(tfinal.lt.0) stop "Error - invalid data on tfinal"
  else
     ! choose manually tfinal
     tfinal = 83
  end if

  ! Run estimation
  p%p0     = 56d0*10**6

  p%i0     = 149d0*22.5d0
  p%ri     = 1.79d0
  p%rf     = 0.52d0
  p%lambda = 0.014d0

  
  ! !Uncomment/Comment for results without revisions
  ! smm%min_i00 = 1.000d0;	smm%max_i00 = 50d0
  ! smm%min_rii = 8.000d0;	smm%max_rii = 9.90d0
  ! smm%min_rff = 0.400d0; 	smm%max_rff = 0.62d0
  ! smm%min_lbb = 0.085d0;	smm%max_lbb = 0.105d0

  ! !Uncomment/Comment for results with revisions
  smm%min_i00 = 100.0d0;	smm%max_i00 = 700d0
  smm%min_rii = 4.000d0;	smm%max_rii = 8.50d0
  smm%min_rff = 0.200d0; 	smm%max_rff = 0.52d0
  smm%min_lbb = 0.055d0;	smm%max_lbb = 0.085d0

  
  call get_eqspace(ni00,smm%min_i00,smm%max_i00,smm%grid_i00)
  call get_eqspace(nrii,smm%min_rii,smm%max_rii,smm%grid_rii)
  call get_eqspace(nrff,smm%min_rff,smm%max_rff,smm%grid_rff)
  call get_eqspace(nlbb,smm%min_lbb,smm%max_lbb,smm%grid_lbb)

  !write(*,'(4a13,a5,1a15)')'I0','Rinit','Rfinit','Lambda','::','distance'
  do jrii=1,nrii
     !print*,jrii
     do ji00=1,ni00
        do jrff=1,nrff
           do jlbb=1,nlbb
              p%i0     = smm%grid_i00(ji00)
              p%ri     = smm%grid_rii(jrii)
              p%rf     = smm%grid_rff(jrff)
              p%lambda = smm%grid_lbb(jlbb)

              
              ! !Uncomment/Comment for results without revisions
              !call get_mss(p,tstart,tfinal,c1,c2,smm%dist(ji00,jrii,jrff,jlbb))
              ! !Uncomment/Comment for results with revisions
              call get_mss(p,tstart,tfinal,c1,c3,smm%dist(ji00,jrii,jrff,jlbb))

              
              ! write(*,'(4f13.4,a5,f15.5)') &
              !      p%i0,p%ri,p%rf,p%lambda, &
              !      '::', &
              !      smm%dist(ji00,jrii,jrff,jlbb)
           end do
        end do
     end do
  end do
  !write(*,'(4a13,a5,1a15)')'I0','Rinit','Rfinit','Lambda','::','distance'
  print*,""
  smm%imindist = minloc(smm%dist)
  print*,"Mininum at: "
  write(*,'(4i13)') smm%imindist
  print*,""
  p%i0     = smm%grid_i00(smm%imindist(1))
  p%ri     = smm%grid_rii(smm%imindist(2))
  p%rf     = smm%grid_rff(smm%imindist(3))
  p%lambda = smm%grid_lbb(smm%imindist(4))

  
  ! !Uncomment/Comment for results without revisions
  !call get_mss(p,tstart,tfinal,c1,c2,mss)
  ! !Uncomment/Comment for results with revisions
  call get_mss(p,tstart,tfinal,c1,c3,mss)

  
  ! !Uncomment/Comment for results without revisions
  !open(1,file='data/Res_00.txt',position="rewind")
  ! !Uncomment/Comment for results with revisions
  open(1,file='data/Res_13.txt',position="rewind")

  
  write(*,'(4a13,a5,1a15)')'I0','Rinit','Rfinit','Lambda','::','distance'
  write(*,'(4f13.4,a5,2f15.5)') &
       p%i0,p%ri,p%rf,p%lambda, &
       '::', &
       mss,smm%dist(smm%imindist(1),smm%imindist(2),smm%imindist(3),smm%imindist(4))
  write(1,'(4a13,a5,1a15)')'I0','Rinit','Rfinit','Lambda','::','distance'
  write(1,'(4f13.4,a5,2f15.5)') &
       p%i0,p%ri,p%rf,p%lambda, &
       '::', &
       mss,smm%dist(smm%imindist(1),smm%imindist(2),smm%imindist(3),smm%imindist(4))
  close(1)

  ! Export model sim to csv
  call get_simulation(p,tt,x)

  
  ! !Uncomment/Comment for results without revisions
  !open(1,file='data/Res_simul_00.txt',position="rewind")
  ! !Uncomment/Comment for results with revisions
  open(1,file='data/Res_simul_13.txt',position="rewind")

  
  do t=1,tt
     write(1,'(15f17.2)')x(t,:)
  end do
  close(1)
  
  tt2 = omp_get_wtime()
  print*,""
  print*,'Program execution time: ',tt2-tt1
  print*,""
end program main
