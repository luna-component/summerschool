module heat
  use iso_fortran_env, only : REAL64
  implicit none

  integer, parameter :: dp = REAL64


  type field 
      integer :: nx, ny
      real(dp) :: dx, dy
      real(dp), allocatable :: dpoints(:,:)
      
  end type

contains

  subroutine init_metadata(n, m, field0, dx, dy)
    implicit none
    integer :: n, m
    real(dp) :: dx, dy
    type(field), intent(out):: field0
    
    field0 % nx = n
    field0 % ny = m

    allocate(field0 % dpoints(n, m))

  end subroutine init_metadata


end module heat
