SUBMODULE ( RealVector_Method ) Constructor
USE BaseMethod
USE h5fortran

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE get_shape
  IF( ALLOCATED( Obj % Val ) ) THEN
    Ans(1) = SIZE( Obj % Val )
  ELSE
    Ans = 0
  END IF
END PROCEDURE get_shape

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE get_size
  !Define internal variables
  INTEGER( I4B ), ALLOCATABLE :: S( : )
  S = Shape( Obj )
  IF( PRESENT( Dims ) ) THEN
    Ans = S( Dims )
  ELSE
    Ans = PRODUCT( S )
  END IF
  DEALLOCATE( S )
END PROCEDURE get_size

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE allocate_data
  IF( ALLOCATED( Obj % Val ) ) THEN
    IF( SIZE( Obj % Val ) .NE. Dims ) THEN
      DEALLOCATE( Obj % Val )
      ALLOCATE( Obj % Val( Dims ) )
      Obj % Val = 0.0_DFP
    ELSE
      Obj % Val = 0.0_DFP
    END IF
  ELSE
    ALLOCATE( Obj % Val( Dims ) )
    Obj % Val = 0.0_DFP
  END IF
  CALL setTotalDimension( Obj, 1_I4B )
END PROCEDURE allocate_data

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocate_data
  IF( ALLOCATED( Obj % Val ) ) DEALLOCATE( Obj % Val )
END PROCEDURE deallocate_data

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE RealVectorDisplay
  INTEGER( I4B ) :: i, max_size
  type(hdf5_file) :: h5f
  type(File_) :: aFile
  INTEGER( I4B ) :: sizes(SIZE(Obj))
  REAL( DFP ) :: val( SIZE( Obj ) )

  DO i = 1, SIZE( Obj )
    sizes(i) = SIZE(Obj(i))
  END DO

  max_size = MAXVAL( sizes )

  IF( PRESENT( UnitNo ) ) THEN
    CALL Write_data( UnitNo )
    RETURN
  END IF

  IF( PRESENT( filename ) ) THEN
    SELECT CASE( TRIM( extension ) )
    CASE( '.hdf5' )
      call ExecuteCommand( 'mkdir -p '//trim(path), &
        & __FILE__ // "Line num :: " // TRIM(INT2STR(__LINE__)) &
        & // "  RealVectorDisplay()" )

      call h5f%initialize( &
        & filename= trim(path)//trim(filename)//trim(extension), &
        & status='new', action='w', comp_lvl=1)

      DO i = 1, SIZE(Obj)
        call h5f%write( '/' // TRIM(msg) // '/comp[' &
          & // TRIM(INT2STR(i)) // ']', Obj(i)%Val )
      END DO
      call h5f%finalize()

    CASE( '.txt' )
      CALL OpenFileToWrite(Obj=afile, filename=filename, path=path, &
        & extension='.txt')
      CALL Write_data( afile%UnitNo )
      CALL CloseFile(afile)

    CASE( '.md' )
      CALL Display( __FILE__, 'ERROR in File :: ' )
      CALL Display( __LINE__, '          in LINE :: ' )
      CALL Display( '        Message :: Cannot write to .txt file')
      STOP
    END SELECT

    RETURN

  END IF

  CALL Write_data( stdout )

  CONTAINS
  SUBROUTINE Write_data( unitno )
    INTEGER( I4B ), INTENT( IN ) :: unitno
    INTEGER( I4B ) :: i, j

    DO i = 1, max_size
      val = 0.0_DFP
      DO j = 1, SIZE( Obj )
        IF( i .LE. sizes( j ) ) val( j ) = Obj(j)%Val(i)
      END DO

      WRITE( UnitNo, * ) val

    END DO
  END SUBROUTINE

END PROCEDURE RealVectorDisplay

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE RealscalarDisplay
  IF( PRESENT( UnitNo ) ) THEN
    CALL Display(Vec=Obj%Val, UnitNo = UnitNo, msg=msg )
    RETURN
  END IF

  IF( PRESENT( filename ) ) THEN
    CALL Display(Vec=Obj%Val, msg=msg, filename=filename, &
      & extension=extension, path=path )
    RETURN
  END IF

  CALL Display(Vec=Obj%Val, msg=msg)

END PROCEDURE RealscalarDisplay

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj
  CALL AllocateData( Obj, tSize )
END PROCEDURE initiate_obj

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj_vector
  INTEGER( I4B ) :: n, i

  n = SIZE( tSize )

  IF( ALLOCATED( Obj ) ) THEN
    IF( SIZE( Obj ) .NE. n ) THEN
      DEALLOCATE( Obj )
      ALLOCATE( Obj( n ) )
    END IF
  ELSE
    ALLOCATE( Obj( n ) )
  END IF

  DO i = 1, n
    CALL AllocateData( Obj( i ), tSize( i ) )
  END DO

END PROCEDURE initiate_obj_vector

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj_ab
  IF( ALLOCATED( Obj % Val ) ) DEALLOCATE( Obj % Val )
  ALLOCATE( Obj % Val( a:b ) )
  Obj % Val = 0.0_DFP
  CALL setTotalDimension( Obj, 1_I4B )
END PROCEDURE initiate_obj_ab

!----------------------------------------------------------------------------
!                                                             Random_Number
!----------------------------------------------------------------------------

MODULE PROCEDURE Random_Number_obj
  CALL Initiate( Obj=Obj, tSize=tSize )
  CALL RANDOM_NUMBER( Obj%Val )
END PROCEDURE Random_Number_obj

!----------------------------------------------------------------------------
!                                                             Random_Number
!----------------------------------------------------------------------------

MODULE PROCEDURE Random_Number_obj_vec
  INTEGER( I4B ) :: ii, n

  n = SIZE( tSize )
  IF( ALLOCATED( Obj ) ) THEN
    IF( SIZE( Obj ) .NE. n ) THEN
      DEALLOCATE( Obj )
      ALLOCATE( Obj( n ) )
    END IF
  ELSE
    ALLOCATE( Obj( n ) )
  END IF

  DO ii = 1, n
    CALL Initiate( Obj=Obj(ii), tSize=tSize(ii) )
    CALL RANDOM_NUMBER( Obj(ii)%Val )
  END DO

END PROCEDURE Random_Number_obj_vec

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  CALL AllocateData( Obj, tSize )
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_1
  ALLOCATE( Obj )
  CALL AllocateData( Obj, tSize )
END PROCEDURE Constructor_1

!----------------------------------------------------------------------------
!                                                    Vector and Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_Int8
  ALLOCATE( Obj )
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE Constructor_Int8

MODULE PROCEDURE ConstructorInt8
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE ConstructorInt8

MODULE PROCEDURE Constructor_Int16
  ALLOCATE( Obj )
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE Constructor_Int16

MODULE PROCEDURE ConstructorInt16
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE ConstructorInt16

MODULE PROCEDURE Constructor_Int32
  ALLOCATE( Obj )
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE Constructor_Int32

MODULE PROCEDURE ConstructorInt32
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE ConstructorInt32

MODULE PROCEDURE Constructor_Int64
  ALLOCATE( Obj )
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE Constructor_Int64

MODULE PROCEDURE ConstructorInt64
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE ConstructorInt64

MODULE PROCEDURE Constructor_Real32
  ALLOCATE( Obj )
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE Constructor_Real32

MODULE PROCEDURE ConstructorReal32
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE ConstructorReal32

MODULE PROCEDURE Constructor_Real64
  ALLOCATE( Obj )
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE Constructor_Real64

MODULE PROCEDURE ConstructorReal64
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE ConstructorReal64

END SUBMODULE Constructor