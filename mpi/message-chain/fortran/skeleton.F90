program basic
  use mpi_f08
  use iso_fortran_env, only : REAL64

  implicit none
!  integer, parameter :: msgsize = 10000000
  integer, parameter :: msgsize = 1000
  integer :: rc, myid, ntasks
  integer :: message(msgsize)
  integer :: receiveBuffer(msgsize)
  type(mpi_status) :: status

  real(REAL64) :: t0, t1
  integer :: getfrom, sendto

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)

  message = myid
  
!  ! Start measuring the time spent in communication
!  call mpi_barrier(mpi_comm_world, rc)
!  t0 = mpi_wtime()
!
!  ! a part. ends up in deadlock since sendig anf receiving at the same time?
!  if (myid < ntasks-1) then
!     call mpi_send(message, msgsize, MPI_INTEGER, myid+1, myid+1, MPI_COMM_WORLD, rc)
!     write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
!          ' Sent elements: ', msgsize, &
!          '. Tag: ', myid+1, '. Receiver: ', myid+1
!  end if
!
!  if (myid > 0) then
!     call mpi_recv(receiveBuffer, msgsize, MPI_INTEGER, myid-1, myid, MPI_COMM_WORLD, status, rc) 
!     write(*,'(A10,I3,A,I3)') 'Receiver: ', myid, &
!          ' First element: ', receiveBuffer(1)
!  end if

!  ! part b
!  if ((myid < ntasks-1) .and. (myid > 0)) then
!     call mpi_sendrecv(message, msgsize, MPI_INTEGER, myid+1, myid+1, receiveBuffer, &
!      msgsize, MPI_INTEGER, myid-1, MPI_ANY_TAG, MPI_COMM_WORLD, status, rc)
!     write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
!          ' Sent elements: ', msgsize, &
!          '. Tag: ', myid+1, '. Receiver: ', myid+1
!
!  else if (myid < ntasks-1) then
!     call mpi_send(message, msgsize, MPI_INTEGER, myid+1, myid+1, MPI_COMM_WORLD, rc)
!     write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
!          ' Sent elements: ', msgsize, &
!          '. Tag: ', myid+1, '. Receiver: ', myid+1
!
!  else if (myid > 0) then
!     call mpi_recv(receiveBuffer, msgsize, MPI_INTEGER, myid-1, myid, MPI_COMM_WORLD, status, rc) 
!     write(*,'(A10,I3,A,I3)') 'Receiver: ', myid, &
!          ' First element: ', receiveBuffer(1)
!  end if

  ! part c

  if (myid < ntasks-1) then
    sendto = myid + 1
  else 
    sendto = MPI_PROC_NULL
  end if 

  if (myid > 0) then
    getfrom = myid -1
  else
    getfrom = MPI_PROC_NULL
  end if

  call mpi_sendrecv(message, msgsize, MPI_INTEGER, sendto, myid+1, receiveBuffer, &
      msgsize, MPI_INTEGER, getfrom, MPI_ANY_TAG, MPI_COMM_WORLD, status, rc)
     write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
          ' Sent elements: ', msgsize, &
          '. Tag: ', myid+1, '. Receiver: ', myid+1

  ! Finalize measuring the time and print it out
  t1 = mpi_wtime()
  call mpi_barrier(mpi_comm_world, rc)
  call flush(6)

  write(*, '(A20, I3, A, F6.3)') 'Time elapsed in rank', myid, ':', t1-t0

  call mpi_finalize(rc)

end program basic
