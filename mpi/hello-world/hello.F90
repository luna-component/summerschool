program hello
    use mpi_f08
    implicit none
    integer :: r, nsize, myrank

      call MPI_Init(r)

      call MPI_Comm_size(MPI_COMM_WORLD, nsize, r)
      call MPI_Comm_rank(MPI_COMM_WORLD, myrank )
     
!      call MPI_Comm_Barrier(MPI_COMM_WORLD, r)

      ! prints my rank nr of the process
      write(*,*) 'Hello', myrank

      if (myrank == 0) then
        write (*,*) 'Total MPI processes: ', nsize
      end if  

      call MPI_Finalize(r)



end program
