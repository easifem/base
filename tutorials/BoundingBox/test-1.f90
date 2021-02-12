program main
  use basetype
  use basemethod
  implicit none

  type( boundingbox_ ) :: obj1, obj2, obj3, obj4

  obj1 = boundingbox( 2, [0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP] )
  obj2 = boundingbox( 2, [-0.5_DFP, 0.5_DFP, 0.5_DFP, 1.5_DFP, 0.0_DFP, 0.0_DFP] )
  
  call display( obj1, "obj1=")

  !write( *, * ) "is intersect:: ", obj1 .isIntersect. obj2
  !obj3 = obj1 .intersection. obj2
  !CALL Obj3 % Display( )
  !obj4 = obj3 .union. obj1
  !CALL obj4 % Display( )
  !write( * , * ) (Obj1 .contains. [1.0_DFP, 1.0_DFP])
end program main
