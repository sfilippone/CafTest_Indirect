program tst_ind_1
  use iso_fortran_env
  implicit none
  real, allocatable    :: xv(:)[:]
  integer, allocatable :: rmt_idx(:,:), loc_idx(:,:), xchg(:)
#ifndef IS_INTEL
  type(event_type), allocatable   :: ready(:)[:]
#endif
  integer, parameter :: nloc=128*128, nhl=128
  integer :: nrcv, ivsize, me, np, nxch, i, ip, iv

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
  nrcv = nhl * nxch
  ivsize = nloc + nrcv
  call co_max(ivsize)
  allocate(xv(ivsize)[*])
#ifndef IS_INTEL
  allocate(ready(np)[*])
#endif
  allocate(rmt_idx(nhl,nxch),loc_idx(nhl,nxch))
  write(*,*) me,' My size :',nxch,nrcv, ivsize

  xv(1:nloc) = me*[(i,i=1,nloc)]
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
  write(*,*) me,' Syncng with :',xchg
    
#ifdef IS_INTEL
  sync images(xchg)
#else
  do ip=1,nxch
    event post (ready(me)[xchg(ip)])
  end do
#endif
  
  iv = nloc + 1
  do ip=1, nxch
#ifndef  IS_INTEL
    event wait (ready(xchg(ip)))
#endif
    xv(loc_idx(1:nhl,ip)) = xv(rmt_idx(1:nhl,ip))[xchg(ip)]
  end do

  write(*,*) me,'Completed exchange'
!!$  sync all
!!$  do ip=1, np
!!$    if (ip == me) &
!!$         &  write(*,*) me,'From halo exchange :',xv(1:nloc),':',xv(nloc+1:nloc+nrcv)
!!$    sync all
!!$  end do
  
  stop

#ifdef IS_INTEL
contains
  subroutine co_max(sz)
    use iso_fortran_env
    implicit none
    integer :: sz
    integer, allocatable, save :: csz[:]
    integer :: me, np, ip

    if (.not.allocated(csz)) then
      allocate(csz[*])
    end if
    csz = sz

    me = this_image()
    np = num_images() 
   
    if (me == 1) then
      do ip = 2, np
        csz = max(csz,csz[ip])
      end do
    end if
    sync all
    sz = csz[1]
  end subroutine co_max
#endif
end program tst_ind_1
