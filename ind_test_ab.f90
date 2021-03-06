program tst_ind_ab
  use iso_fortran_env
  implicit none
  integer, parameter :: nloc=8, nhl=2, ivsize=nloc+2*nhl
  type tempv
    real, allocatable :: v(:)[:]
  end type tempv
  ! real    :: xv(ivsize)[*]
  type(tempv) :: xv
  real, allocatable :: tv(:)
  integer :: rmt_idx(2,2), loc_idx(2,2)
  integer, allocatable :: xchg(:)
  integer :: nrcv, me, np, nxch, i, ip, iv
  character(len=120) :: fmt

  me = this_image()
  np = num_images()
  
  if (np==1) then
    !    allocate(xchg(0))
    xchg = [ integer :: ]
    
  else if (me == 1) then
    xchg = [me+1]
  else if (me == np) then
    xchg = [me-1]
  else
    xchg = [me-1, me+1]
  end if
  nxch = size(xchg)
  nrcv = nxch * nhl 
  allocate(xv%v(ivsize)[*])
  allocate(tv(ivsize))

  
  tv(1:nloc) = [(i,i=(me-1)*nloc+1,me*nloc)]
  xv%v(1:nloc) = tv(1:nloc)
  iv = nloc + 1
  do ip=1, nxch
    loc_idx(1:nhl,ip) = [ (i,i=iv,iv+nhl-1) ]
    if (xchg(ip) == me-1) then
      rmt_idx(1:nhl,ip) = [ (i,i=nloc-nhl+1,nloc) ]
    else
      rmt_idx(1:nhl,ip) = [ (i,i=1,nhl) ]
    end if
    iv = iv + nhl
  end do

  sync images(xchg)
  iv = nloc + 1
  do ip=1, nxch
    !xv%v(loc_idx(1:nhl,ip)) = xv%v(rmt_idx(1:nhl,ip))[xchg(ip)]
    !tv(loc_idx(1:nhl,ip)) = xv%v(rmt_idx(1:nhl,ip))[xchg(ip)]
    tv(iv:iv+nhl-1) = xv%v(rmt_idx(1:nhl,ip))[xchg(ip)]
    iv = iv + nhl
  end do
  
  do ip=1, np
    sync all
    if (ip == me) then
      write(fmt,*) '( i0,a,',nloc,'(f5.0,1x),a,',nrcv,'(f5.0,1x) )'
      ! write(*,fmt) me,': From halo exchange :',xv%v(1:nloc),':',xv%v(nloc+1:nloc+nrcv)
      write(*,fmt) me,': From halo exchange :',tv(1:nloc),':',tv(nloc+1:nloc+nrcv)
    end if
  end do
  stop

end program tst_ind_ab
