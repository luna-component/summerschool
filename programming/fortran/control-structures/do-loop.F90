program loops
  implicit none
  integer, parameter :: nx = 10, ny = 10
  real :: squaresize = 1.0, stepsize, x, y
  real :: A(nx, ny)
  ! TODO define parameters nx and ny
  ! TODO: define real-valued array A
  integer :: i, i, step = 1
  
  ! TODO initialize array A here
  stepsize = squaresize / nx

  do i = 1, nx, step
     x = 0.0
     
     do j = 1, ny, step
        y = 0.0
        A(i, j) = x**2 + y**2
        y = y + stepsize
     end do

     x = x + stepsize

  end do
  !--------------------------------------------------
  ! Print out the array
  ! the ':' syntax means the whole row, see the Fortran arrays lecture
  do i = 1, nx
     write(*, '(12F6.1)') A(i,:)
  end do

end program loops
