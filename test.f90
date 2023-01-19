PROGRAM Triangle
  IMPLICIT NONE
  REAL :: a, b, c, area_val, Area
  PRINT *, 'Welcome, please enter the&
       &lengths of the 3 sides.'
  READ *, a, b, c
  area_val = Area(a,b,c)
END PROGRAM Triangle
FUNCTION Area(x,y,z)
  IMPLICIT NONE
  REAL :: Area            ! function type
  REAL, INTENT( IN ) :: x, y, z
  REAL :: theta, height
  theta = ACOS((x**2+y**2-z**2)/(2.0*x*y))
  height = x*SIN(theta); Area = 0.5*y*height
END FUNCTION Area
SUBROUTINE Dummy(a,b,c)
  IMPLICIT NONE
  REAL, INTENT( IN ) :: a, b
  REAL, INTENT( INOUT ) :: c
  REAL, d
  d = a+b*c
END SUBROUTINE Dummy
