program arrays
  implicit none
  real, allocatable :: A(:,:)
  !real, dimension(:,:), allocatable :: A
  !Define the array A
  real :: x, y, dx, dy
  integer :: nx, ny, i, j, alloc_stat

  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny
  dx = 1.0/real(nx-1)
  dy = 1.0/real(ny-1)

  ! allocate the array A
  allocate (A(nx,ny), stat=alloc_stat)
  if (alloc_stat /= 0) stop

  ! initalize the array A
  y = 0.0
  do j = 1, ny
    x = 0.0
    do i = 1, nx
        A(i,j) = x**2 + y**2 
        x = x + dx
    end do
    y = y + dy 
  end do    
  ! Print out the array
  write (*,*) 'A', A(1,:) 
  !write (*,'(*(F6.2))') 'A', A(1,:) 

end program arrays
