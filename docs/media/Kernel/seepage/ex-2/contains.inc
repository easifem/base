CONTAINS

PURE FUNCTION DS_LINE(x) RESULT( ans )
  REAL( DFP ), INTENT( IN ) :: x(:)
  REAL( DFP ) :: ans

  ans = x(1)-16.0_DFP
END FUNCTION DS_LINE

PURE FUNCTION InitialPressure( x ) RESULT( Ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: x( : )
  REAL( DFP ) :: Ans
  Ans = rho_w*9.81*10.0_dfp
END FUNCTION InitialPressure

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION upstreamBC( x ) RESULT( Ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: x( : )
  REAL( DFP ) :: Ans
  Ans = rho_w*9.81*10.0_dfp
END FUNCTION upstreamBC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION downstreamBC( x ) RESULT( Ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: x( : )
  REAL( DFP ) :: Ans
  Ans = rho_w*9.81*2.0_dfp
END FUNCTION downstreamBC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION freeSurfaceBC( x ) RESULT( Ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: x( : )
  REAL( DFP ) :: Ans

  if( (x(2)-2.0_DFP) .GT. 0.00001_DFP )then
    Ans = rho_w*9.81*x(2)
  else
    Ans = rho_w*9.81*2.0_DFP
  end if
END FUNCTION freeSurfaceBC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE FINAL_SHAPE
  REAL( DFP ), ALLOCATABLE :: x( :, : )
  TYPE(gpf) :: gp
  TYPE(File_) :: afile

  ! INTEGER( I4B ) :: TOP
  ! TOP = 4

  ! x = dom%nodes(1:2, dom%mdBoundary(TOP)%ptr%nptrs)
  ! call gp%options( &
  !   & 'set terminal postscript eps enhanced color font "Helvetica,10"')
  ! call gp%options( 'set output "./graphs/final_shape.eps"' )
  ! call gp%title( string='Final shape' )
  ! call gp%xlabel( string='x (m)' )
  ! call gp%ylabel( string='y (m)')
  ! call gp%options( 'unset grid')

  ! call gp%options( &
  !   & 'set xrange[0:' // trim(str(maxval(x(1,:))*1.1)) //']')

  ! call gp%options( &
  !   & 'set yrange[0:' // trim(str(maxval(x(2,:))*1.1)) //']')

  ! call gp%filename('./graphs/final_shape.gp')
  ! call gp%plot(x1=x(1, :), y1=x(2, :), &
  !   & ls1='w lp lt 2 lw 1 lc rgb "black" pt 7 ps 1' )
  ! call gp%reset()

  ! call openFileToWrite(afile, './graphs/', 'final_shape', '.txt')
  ! call writeData(afile, x, trans=.true.)
  ! call closeFile(afile)
  ! DEALLOCATE( x )
END SUBROUTINE FINAL_SHAPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE TIME_VS_ITERATIONS
  TYPE(gpf) :: gp
  TYPE(file_) :: afile

  ! post processing
  call gp%options( &
    & 'set terminal postscript eps enhanced color font "Helvetica,10"')
  call gp%options( 'set output "./graphs/iteration_vs_time.eps"' )
  call gp%title( string='Number of iteration' )
  call gp%xlabel( string='Time (hour)' )
  call gp%ylabel( string='Total iterations')
  call gp%options( 'unset grid')

  call gp%options( &
    & 'set xrange[0:' // trim(str(maxval(times)*1.1)) //']')

  call gp%options( &
    & 'set yrange [0:' // trim(str(MAXITER, .true.)) //'];' )
  call gp%filename('./graphs/iteration_vs_time.gp')
  call gp%plot(x1=times, y1=real(iters, dfp), &
    & ls1='w lp lt 2 lw 1 lc rgb "black" pt 7 ps 1' )
  call gp%reset()

  call openFileToWrite(afile, './graphs/', 'iteration_vs_time', '.txt')
  call writeData(afile, times, real(iters, dfp))
  call closeFile(afile)
END SUBROUTINE TIME_VS_ITERATIONS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE TIME_VS_ANGLE
  TYPE(gpf) :: gp
  TYPE(FILE_) :: afile

  ! post processing
  call gp%options( &
    & 'set terminal postscript eps enhanced color font "Helvetica,10"')
  call gp%options( 'set output "./graphs/angle_vs_time.eps"' )
  call gp%title( string='Angle between velocity and normal' )
  call gp%xlabel( string='Time (hour)' )
  call gp%ylabel( string='Angle (degrees)')
  call gp%options( 'unset grid')

  call gp%options( &
    & 'set xrange[0:' // trim(str(maxval(times)*1.1)) //']')

  call gp%options( &
    & 'set yrange[0:'&
    & // trim(str(90.0)) &
    & //'];' )
  call gp%filename('./graphs/angle_vs_time.gp')
  call gp%plot(x1=times(1:), y1=Angle(1:), &
    & ls1='w lp lt 2 lw 1 lc rgb "black" pt 7 ps 1' )
  call gp%reset()

  call openFileToWrite(afile, './graphs/', 'angle_vs_time', '.txt')
  call writeData(afile, times, Angle)
  call closeFile(afile)
END SUBROUTINE TIME_VS_ANGLE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE TIME_VS_VERT_NODE
  TYPE(gpf) :: gp
  TYPE(file_) :: afile

  ! post processing
  call gp%options( &
    & 'set terminal postscript eps enhanced color font "Helvetica,10"')
  call gp%options( 'set output "./graphs/y_vs_time.eps"' )
  call gp%title( string='Trajectory of vertical node' )
  call gp%xlabel( string='Time (hour)' )
  call gp%ylabel( string='y (m)')
  call gp%options( 'unset grid')

  call gp%options( &
    & 'set xrange[0:' // trim(str(maxval(times)*1.1)) //']')

  call gp%options( &
    & 'set yrange[0:'&
    & // trim(str(24.0)) &
    & //'];' )
  call gp%filename('./graphs/y_vs_time.gp')
  call gp%plot(x1=times(1:), y1=VertPos(1:),  ls1='w lp lt 2 lw 1 lc rgb "black" pt 7 ps 1')
  call gp%reset()

  call openFileToWrite(afile, './graphs/', 'y_vs_time', '.txt')
  call writeData(afile, times, VertPos)
  call closeFile(afile)

END SUBROUTINE TIME_VS_VERT_NODE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE TIME_VS_MESH_QUALITY_MAX
  TYPE(gpf) :: gp
  TYPE(File_) :: afile

  ! post processing
  call gp%options( &
    & 'set terminal postscript eps enhanced color font "Helvetica,10"')
  call gp%options( 'set output "./graphs/qmax_vs_time.eps"' )
  call gp%title( string='Mesh quality' )
  call gp%xlabel( string='Time (hour)' )
  call gp%ylabel( string='Quality')
  call gp%options( 'unset grid')

  call gp%options( &
    & 'set xrange[0:' // trim(str(maxval(times)*1.1)) //']')

  call gp%options( &
    & 'set yrange[0:'&
    & // trim(str( (INT(MAXVAL(qmax))+1),.true.)) &
    & //'];' )
  call gp%filename('./graphs/qmax_vs_time.gp')
  call gp%plot(x1=times, y1=qmax, &
    & ls1='w lp lt 2 lw 1 lc rgb "black" pt 7 ps 1' )
  call gp%reset()

  call openFileToWrite(afile, './graphs/', 'qmax_vs_time', '.txt')
  call writeData(afile, times, qmax)
  call closeFile(afile)
END SUBROUTINE TIME_VS_MESH_QUALITY_MAX