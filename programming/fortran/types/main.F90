program init
    use heat
    use io
    implicit none

    type(field) :: field0
    integer :: nx, ny

    call read_field(field0 % dpoints, 'bottle.dat')
    nx = size(field0 % dpoints, 1) 
    ny = size(field0 % dpoints, 2) 
    
    call write_field(field0 % dpoints, 1)

    call init_metadata(nx, ny, field0, 0.1_dp, 0.1_dp)

    write (*,*) field0 % nx 

end program init
