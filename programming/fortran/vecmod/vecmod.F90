module vector_algebra
  use iso_fortran_env, only : REAL64
  implicit none
  type vector_t
     real(REAL64) :: x, y, z
  end type vector_t

  interface abs
    module procedure v_2norm
  end interface abs

  interface operator(+)
      module procedure v_sum
  end interface operator(+)

  interface operator(-)
      module procedure v_subtr
  end interface operator(-)

  interface operator(*)
      module procedure v_dot
  end interface operator(*)

!  interface (.x.)
!      module procedure v_cross
!  end interface (.x.)

  contains
  
    function v_2norm(v1) result(v3)
        implicit none
        type(vector_t), intent(in) :: v1
        real(REAL64) :: v3
        
        v3 = sqrt( (v1%x * v1%x) + (v1%y * v1%y) + (v1%z * v1%z) )
  
    end function v_2norm
  
  
    function v_sum(v1, v2) result(v3)
        type(vector_t), intent(in) :: v1, v2
        type(vector_t) :: v3
  
        v3%x = (v1%x + v2%x) 
        v3%y = (v1%y + v2%y)
        v3%z = (v1%z + v2%z)
  
    end function v_sum
  
  
    function v_subtr(v1, v2) result(v3)
        type(vector_t), intent(in) :: v1, v2
        type(vector_t) :: v3
  
        v3%x = (v1%x - v2%x) 
        v3%y = (v1%y - v2%y)
        v3%z = (v1%z - v2%z)
  
    end function v_subtr
  
  
    function v_dot(v1, v2) result(v3)
        type(vector_t), intent(in) :: v1, v2
        real(REAL64) :: v3
        
        v3 =  (v1%x * v2%x) + (v1%y * v2%y) + (v1%z * v2%z) 
  
    end function v_dot


!    function v_cross(v1, v2) result(v3)
!        type(vector_t), intent(in) :: v1, v2
!        type(vector_t) :: v3
!  
!        v3%x = (v1%y*v2%z - v1%z*v2%y) 
!        v3%y = (v1%z*v2%x - v1%x*v2%z)
!        v3%z = (v1%x*v2%y - v1%y*v2%x)
!    end function v_cross

end module vector_algebra
