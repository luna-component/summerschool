program exchange
  use mpi_f08
  implicit none
  integer, parameter :: msgsize = 100
  integer :: rc, myid, ntasks, sometag
  type(mpi_status) :: status
  integer :: message(msgsize)
  integer :: receiveBuffer(msgsize)

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)
  call mpi_barrier(MPI_COMM_WORLD, rc)

  message = myid
  sometag = 1


  if (myid == 0) then
     call mpi_send(message, msgsize, MPI_INTEGER, myid+1, sometag, MPI_COMM_WORLD, rc)
     call mpi_recv(receiveBuffer, msgsize, MPI_INTEGER, ntasks-1, MPI_ANY_TAG, MPI_COMM_WORLD, status, rc)
     write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
          ' received ', receiveBuffer(1)

  else if (myid == ntasks-1) then
     call mpi_recv(receiveBuffer, msgsize, MPI_INTEGER, myid-1, MPI_ANY_TAG, MPI_COMM_WORLD, status, rc)
     call mpi_send(message, msgsize, MPI_INTEGER, 0, sometag, MPI_COMM_WORLD, rc)
     write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
          ' received ', receiveBuffer(1)

  else if (myid > 0 .and. mod(myid,2) == 0) then
     call mpi_send(message, msgsize, MPI_INTEGER, myid+1, sometag, MPI_COMM_WORLD, rc)
     call mpi_recv(receiveBuffer, msgsize, MPI_INTEGER, myid-1, MPI_ANY_TAG, MPI_COMM_WORLD, status, rc)
     write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
          ' received ', receiveBuffer(1)

  else if (myid > 0 .and. myid < ntasks-1) then 
     call mpi_recv(receiveBuffer, msgsize, MPI_INTEGER, myid-1, sometag, MPI_COMM_WORLD, status, rc)
     call mpi_send(message, msgsize, MPI_INTEGER, myid+1, sometag, MPI_COMM_WORLD, rc)
     write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
          ' received ', receiveBuffer(1)
  end if
  
  call mpi_barrier(MPI_COMM_WORLD, rc)
  call mpi_finalize(rc)

end program exchange
