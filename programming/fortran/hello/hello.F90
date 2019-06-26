program hello
  implicit none
  real :: y, y2
  integer :: x
  complex :: z, z2

  character(len=80) :: weekday
  character(len=80) :: month = 'June'

  logical :: test1 = .true.

  write (*,*) 'Hello world from Fortran!'
  write (*,*) 'Goodbye!'
  write (*,*) ''

  write (*,*) 'Power = '
  read (*,*) x
  write (*,*) 'Give a complex number'
  read (*,*) z
  write (*,*) 'Given a complex number:', z

  write (*,*) 'Enter a weekday'
  read (*,*) weekday
  write (*,*) 'A nice ', weekday, ' in ', month

  

  y = 2**x
  y2 = y*z
  z2 = y*z

  write (*,*) '2 to the power of', x, ' is equal to', y
  write (*,*) 'Multiplied: ', y2, z2
  write (*,*) 'Is it a Wednesday?', test1
  
end program hello
