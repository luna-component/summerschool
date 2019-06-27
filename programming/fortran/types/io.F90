module io

  use iso_fortran_env, only : REAL64, REAL32


contains

  ! Reads the temperature distribution from an input file
  subroutine read_field(field, filename)
    implicit none

    real(kind=REAL64), dimension(:,:), allocatable, intent(out) :: field
    character(len=*), intent(in) :: filename
    integer :: nx, ny, i 
    
    !open file and read the first line to read the dimensions (integers nx, ny)
    open(11, file=filename, status='old')
    read(11, fmt='(1x2i4)') nx, ny
   
    ! allocalte the matrix the values will be written in 
    allocate(field(nx,ny))
    
    ! initiate the matrix, each line contains nx elements, they're written in a column of matrix
    field = 0.0
    do i= 1, ny
        read(11, *) field(i, :)
    end do

    close(unit=11, status='keep')


  end subroutine read_field

  ! Output routine, saves the temperature distribution as a png image
  subroutine write_field(field, iter)
    use iso_fortran_env, only : REAL64
    use pngwriter
    implicit none

    integer, parameter :: dp = REAL64
    real(kind=REAL64), intent(in) :: field(:,:)
    integer, intent(in) :: iter

    character(len=85) :: filename
    integer :: nx, ny, stat

    nx = size(field, 1)
    ny = size(field, 2)


    write(filename,'(A5,I4.4,A4,A)')  'heat_', iter, '.png'
    stat = save_png(real(field, kind=dp), nx, ny, filename)
    if (stat == 0) then
       write (*,*) 'Wrote the png file ', filename
       write (*,*) 'Use e.g. "eog" to open it.'
    end if
  end subroutine write_field

end module io
