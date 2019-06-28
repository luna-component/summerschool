program coll_exer
  use mpi_f08
  implicit none

  integer, parameter :: n_mpi_tasks = 4

  integer :: ntasks, rank, ierr, color, nntasks, nrank
  integer, dimension(2*n_mpi_tasks) :: sendbuf, recvbuf
  integer, dimension(2*n_mpi_tasks**2) :: printbuf
!  integer, dimension(n_mpi_tasks) :: scount, displs
  type(mpi_comm) :: newcomm0

  call mpi_init(ierr)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)

  if (ntasks /= n_mpi_tasks) then
     if (rank == 0) then
        print *, "Run this program with ", n_mpi_tasks, " tasks."
     end if
     call mpi_abort(MPI_COMM_WORLD, -1, ierr)
  end if

  ! Initialize message buffers
  call init_buffers

  ! Print data that will be sent
  call print_buffers(sendbuf)

  ! assign color to each process
  if (rank < 2) then
      color = 1 
  else
      color = 2
  end if
  
  ! split the world communicator into two new comms, processes get assigned due to color
  call mpi_comm_split(MPI_COMM_WORLD, color, rank, newcomm0, ierr)
  call mpi_comm_rank(newcomm0, nrank, ierr)
  write (*,*) 'Rank', rank,  'in MY_COMM_WORLD, but ', nrank, 'in ', color,  'in newcomm0. '

  ! excercise done here
  call mpi_reduce(sendbuf, recvbuf, 2*n_mpi_tasks, MPI_INTEGER, MPI_SUM, 0, newcomm0)
  call print_buffers(recvbuf)


  call mpi_finalize(ierr)

contains

  subroutine init_buffers
    implicit none
    integer :: i

    do i = 1, 2*n_mpi_tasks
       recvbuf(i) = -1
       sendbuf(i) = i + 2*n_mpi_tasks * rank - 1
    end do
  end subroutine init_buffers


  subroutine print_buffers(buffer)
    implicit none
    integer, dimension(:), intent(in) :: buffer
    integer, parameter :: bufsize = 2*n_mpi_tasks
    integer :: i
    character(len=40) :: pformat

    write(pformat,'(A,I3,A)') '(A4,I2,":",', bufsize, 'I3)'

    call mpi_gather(buffer, bufsize, MPI_INTEGER, &
         & printbuf, bufsize, MPI_INTEGER, &
         & 0, MPI_COMM_WORLD, ierr)

    if (rank == 0) then
       do i = 1, ntasks
          write(*,pformat) 'Task', i - 1, printbuf((i-1)*bufsize+1:i*bufsize)
       end do
       print *
    end if
  end subroutine print_buffers

end program coll_exer
