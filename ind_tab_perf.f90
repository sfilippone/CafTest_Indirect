program tst_ind_ab
  use iso_fortran_env
  implicit none
  integer, parameter :: nloc=32*128*128, nhl=4*128
  type tempv
    real, allocatable :: v(:)[:]
  end type tempv
  ! real    :: xv(ivsize)[*]
  real,allocatable  :: xx(:)[:]
  type(tempv) :: xv
  real, allocatable :: tv(:)
  integer, allocatable :: rmt_idx(:,:), loc_idx(:,:)
  integer, allocatable :: xchg(:)
  integer :: nrcv, ivsize, me, np, nxch, i, ip, iv
  integer :: icnt1, icnt2, icr
  real :: t1, t2
  character(len=120) :: fmt

  me = this_image()
  np = num_images()

  call system_clock(count_rate=icr)
  
  if (np==1) then
    !    allocate(xchg(0))
    xchg = [ integer :: ]
    
  else if (me == 1) then
    xchg = [me+1]
  else if (me == np) then
    xchg = [me-1]
  else
    if (mod(me,2) == 0) then 
      xchg = [me-1, me+1]
    else
      xchg = [me+1, me-1]
    end if
  end if
  nxch = size(xchg)
  nrcv = nxch * nhl 
  ivsize = nloc + nrcv
  call co_max(ivsize)

  allocate(xv%v(ivsize)[*])
  !allocate(xx(ivsize)[*])
  allocate(tv(ivsize))
  allocate(rmt_idx(nhl,nxch),loc_idx(nhl,nxch))
  write(*,*) me,' My size :',nxch,nrcv, ivsize

  
  tv(1:nloc) = [(i,i=(me-1)*nloc+1,me*nloc)]
  xv%v(1:nloc) = tv(1:nloc)
  !xx(1:nloc) = tv(1:nloc)
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
  write(*,*) me,' Syncng with :',xchg, icr
  sync all 

  call system_clock(count=icnt1)
  sync images(xchg)
  iv = nloc + 1
  do ip=1, nxch
    !xv%v(loc_idx(1:nhl,ip)) = xv%v(rmt_idx(1:nhl,ip))[xchg(ip)]
    !tv(loc_idx(1:nhl,ip)) = xv%v(rmt_idx(1:nhl,ip))[xchg(ip)]
    if (.false.) then 
      tv(iv:iv+nhl-1)   = xv%v(rmt_idx(1:nhl,ip))[xchg(ip)]
    else
      if (xchg(ip) == me-1) then
        tv(iv:iv+nhl-1)   = xv%v(nloc-nhl+1:nloc)[xchg(ip)]
        !tv(iv:iv+nhl-1)   = xx(nloc-nhl+1:nloc)[xchg(ip)]
      else
        tv(iv:iv+nhl-1)   = xv%v(1:nhl)[xchg(ip)]
        !tv(iv:iv+nhl-1)   = xx(1:nhl)[xchg(ip)]
      end if
    end if
    xv%v(iv:iv+nhl-1) = tv(iv:iv+nhl-1)
    !xx(iv:iv+nhl-1) = tv(iv:iv+nhl-1) 
    iv = iv + nhl
  end do
  call system_clock(count=icnt2)
  t1 = real(icnt2-icnt1)/real(icr)

  write(*,*) me,'Completed exchange', t1,icr
 deallocate(xv%v)
 ! deallocate(xx)
  
end program tst_ind_ab
