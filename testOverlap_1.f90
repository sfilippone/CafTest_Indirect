program testOverlap
  implicit none
  include 'mpif.h'
  integer :: me,np,i,req,ierr
  integer :: status(MPI_STATUS_SIZE)
  real(kind=8) :: e_t, s_t,m_t=0.d0,s_c=0.d0,e_c=0.d0,res=20.d0
  real(kind=8),allocatable :: x(:)[:]
  integer, parameter :: n = 20000000

  ! interface
  !    function WALLTIME() bind(C, name = "WALLTIME")
  !      real(kind=8) :: WALLTIME
  !    end function WALLTIME
  ! end interface

  allocate(x(n)[*])

  x = 2

  me = this_image()
  np = num_images()

  sync all

  do i=1,5
     if(me==1) then
        s_t = MPI_Wtime()
        x(1:2000)[np] = x(1:2000)
        e_t = MPI_Wtime()
     else
	s_c = MPI_Wtime()
        call computing(res,n)
	e_c = MPI_Wtime()
     endif

     if (me==1)  write(*,*) me, e_t-s_t,"this time should be less than the one printed by the last image"
     if (me==np) write(*,*) me, e_c-s_c
     sync all
  enddo

  if(me == np) write(*,*) res

end program testOverlap

subroutine computing(res,n)
  implicit none
  integer, intent(in) :: n
  real(kind=8),intent(out) :: res
  integer 	      :: i
  res = 0.d0
  do i=1,2*n
    res = res + log(res)/(1984/i*2)
    res = res - sqrt(res)
  enddo

end subroutine
