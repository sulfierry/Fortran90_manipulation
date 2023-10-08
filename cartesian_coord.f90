module Cartesian2DModule
  implicit none

  type :: Point2D
    real :: x, y
  end type Point2D

contains

  real function calculateDistance(pointA, pointB)
    implicit none
    type(Point2D) :: pointA, pointB
    calculateDistance = sqrt((pointA%x - pointB%x)**2 + (pointA%y - pointB%y)**2)
  end function calculateDistance

  subroutine assignValues(p, x_val, y_val)
    implicit none
    type(Point2D), intent(inout) :: p
    real, intent(in) :: x_val, y_val
    p%x = x_val
    p%y = y_val
  end subroutine assignValues

  subroutine displayContent(R)
    implicit none
    type(Point2D), intent(in) :: R
    print*, R%x, R%y
  end subroutine displayContent

  subroutine translatePoint(P, x_offset, y_offset)
    implicit none
    type(Point2D), intent(inout) :: P
    real, intent(in) :: x_offset, y_offset
    P%x = P%x + x_offset
    P%y = P%y + y_offset
  end subroutine translatePoint

end module Cartesian2DModule

program DerivedTypeExperiment
  use Cartesian2DModule
  implicit none
  type(Point2D) :: origin, pointA, pointB
  real :: distAB, distBA

  origin = Point2D(0.0, 0.0)
  call assignValues(pointA, 3.0, 3.0)

  print*, "Origin coordinates"
  call displayContent(origin)

  print*, "Coordinates of point A"
  call displayContent(pointA)

  pointB = pointA

  print*, "Coordinates of point B"
  call displayContent(pointB)

  call translatePoint(pointB, 3.0, 4.0)

  print*, "Coordinates of translated point B"
  call displayContent(pointB)

  distAB = calculateDistance(pointA, pointB)
  print*, "Distance between A and B =", distAB

  distBA = calculateDistance(pointB, pointA)
  print*, "Distance between B and A =", distBA

end program DerivedTypeExperiment
