module laplacian_mod
  use iso_fortran_env, only : REAL64
  implicit none

  integer, parameter :: dp = REAL64
  real(dp), parameter :: dx = 0.01, dy = 0.01


contains

  subroutine initialize(field0)
    ! TODO: implement a subroutine that initializes the input array
    implicit none
    real(dp), dimension(:,:), intent(out) :: field0
    real(dp) :: x, y
    integer :: nx, ny, i, j

    nx = size(field0, 1)
    ny = size(field0, 2)

     ! initialize array A(x,y) = (x^2 + y^2) in the domain [0:1,0:1]
     y = 0.0
     do j = 1, ny
        x = 0.0
        do i = 1, nx
            field0(i,j) =  x**2 + y**2
            x = x + dx
        end do
        y = y + dy
     end do

  end subroutine initialize


  subroutine laplacian(curr, prev)
    ! TODO: insert a subroutine that computes a laplacian of the
    ! array "prev" and returns it as an array "curr"
    implicit none
    real(dp), dimension(:,:), intent(in) :: prev
    !real(dp), dimension(:,:) :: prev
    real(dp), dimension(:,:), intent(out) :: curr
    real(dp) :: x, y
    !integer :: alloc_stat
    integer :: nx, ny, i, j

    nx = size(prev, 1)
    ny = size(prev, 2)
    
    curr = 0.0
    do i = 2, ny-1
       do j = 2, nx-1
          curr(i,j) = ( prev(i-1, j) - 2.0*( prev(i,j)  ) + prev(i+1, j) ) / (dx**2) &
              + ( prev(i, j-1) - 2.0*( prev(i,j)  ) + prev(i, j+1) ) / (dy**2)
       end do
    end do

  end subroutine laplacian


  subroutine write_field(array)
    ! TODO: write a subroutine that prints "array" on screen
    implicit none
    real(dp), dimension(:,:), intent(in) :: array
    integer :: i, j, nx, ny

    i = 1
    j = 1
    nx = size(array, 1)
    ny = size(array, 2)

    write(*,*) "Rows of the array:"
    do i = 2, nx-1
      print *, 'row', i
      write(*, '(12F6.2)' ) array(i, 2:nx-1)
    end do

  end subroutine write_field


end module laplacian_mod
