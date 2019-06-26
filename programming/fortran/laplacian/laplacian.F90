program laplacian
  use iso_fortran_env, only : REAL64
  implicit none

  integer, parameter :: dp = REAL64
  real(dp), dimension(:,:), allocatable :: A, L
  real(dp) :: dx, dy, x, y
  integer :: alloc_stat
  integer :: nx, ny, i, j

  write (*,*)  'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny
  ! Grid spacing
  dx = 1.0/real(nx-1)
  dy = 1.0/real(ny-1)
  
  ! TODO: allocate matrices
  allocate (A(nx,ny), stat=alloc_stat)
  if (alloc_stat /= 0) stop

  allocate (L(nx,ny), stat=alloc_stat)
  if (alloc_stat /= 0) stop


  ! initialize array A(x,y) = (x^2 + y^2) in the domain [0:1,0:1]
  y = 0.0
  do j = 1, ny
     x = 0.0
     do i = 1, nx
        A(i,j) =  x**2 + y**2
        x = x + dx
     end do
     y = y + dy
  end do

  ! TODO: Compute Laplacian of A and save it to array L
  !L(0,0) = A(0,0)
  L = 0.0
  do i = 2, ny-1
     !x = 0.0
     do j = 2, nx-1
        L(i,j) = ( A(i-1, j) - 2.0*( A(i,j)  ) + A(i+1, j) ) / (dx**2) &
            + ( A(i, j-1) - 2.0*( A(i,j)  ) + A(i, j+1) ) / (dy**2)
     end do
  end do
  

  ! TODO: Printing of the arrays
  write(*,*) "Original array:"
  do i = 2, nx-1
    print *, 'row', i
    write(*, '(ES10.4)') A(i, 2:nx-1)
    !write(*, '(12F6.2)') A(i, 2:nx-1)
  end do
  

  write(*,*) "Laplacian of the array:"
  do i = 2, nx-1
    print *, 'row', i
    write(*, '(ES10.4)') L(i, 2:nx-1)
    !write(*, '(12F6.2)') L(i, 2:nx-1)
  end do

  !write(*,*) 'Shape of L:', shape(L)
  !write(*,*) 'nx-1, dx:', real(nx-1), dx, 1.0/10
  ! Analytically, the Laplacian of the function is nabla^2 A(x,y) = 4


end program laplacian
