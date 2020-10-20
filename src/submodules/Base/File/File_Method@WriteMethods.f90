SUBMODULE(File_Method) WriteMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE write_data_ascii_scalar
  SELECT TYPE( Val )
  TYPE IS( CHARACTER( * ))
    WRITE( Obj%UnitNo, "(A)") TRIM(Val)
  TYPE IS( Real(Real64) )
    WRITE( Obj%UnitNo, FReal64 ) Val
  TYPE IS( Real(Real32) )
    WRITE( Obj%UnitNo, FReal32 ) Val
  TYPE IS( Integer(INT32) )
    WRITE( Obj%UnitNo, FInt32 ) Val
  TYPE IS( Integer(INT64) )
    WRITE( Obj%UnitNo, FInt64 ) Val
  END SELECT
END PROCEDURE write_data_ascii_scalar
!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE write_data_ascii_r1
  INTEGER( I4B ) :: ii, l
  TYPE( String ) :: fmt

  SELECT TYPE( Val )
  TYPE IS ( Integer(Int32) )
    IF( .NOT. transpose ) THEN
      DO ii =1, SIZE(Val)
        WRITE(Obj%UnitNo, FInt32) Val( ii )
      END DO
    ELSE
      l = SIZE(Val)
      fmt = "("//trim(str(l, no_sign=.true.)) // trim(FInt32)//")"
      WRITE(Obj%UnitNo, fmt%chars()) Val
    END IF

  TYPE IS ( Integer(Int64) )
    IF( .NOT. transpose ) THEN
      DO ii =1, SIZE(Val)
        WRITE(Obj%UnitNo, FInt64) Val( ii )
      END DO
    ELSE
      l = SIZE(Val)
      fmt = "("//trim(str(l, no_sign=.true.)) // trim(FInt64)//")"
      WRITE(Obj%UnitNo, fmt%chars()) Val
    END IF

  TYPE IS ( Real(Real32) )
    IF( .NOT. transpose ) THEN
      DO ii =1, SIZE(Val)
        WRITE(Obj%UnitNo, FReal32) Val( ii )
      END DO
    ELSE
      l = SIZE(Val)
      fmt = "("//trim(str(l, no_sign=.true.)) // trim(FReal32)//")"
      WRITE(Obj%UnitNo, fmt%chars()) Val
    END IF

  TYPE IS ( Real(Real64) )
        IF( .NOT. transpose ) THEN
      DO ii =1, SIZE(Val)
        WRITE(Obj%UnitNo, FReal64) Val( ii )
      END DO
    ELSE
      l = SIZE(Val)
      fmt = "("//trim(str(l, no_sign=.true.)) // trim(FReal64)//")"
      WRITE(Obj%UnitNo, fmt%chars()) Val
    END IF
  END SELECT

END PROCEDURE write_data_ascii_r1

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE write_data_ascii_r2
  INTEGER( I4B ) :: ii, l
  TYPE( String ) :: fmt

  SELECT TYPE( Val )
  TYPE IS ( Integer(Int32) )
    ! print column wise
    IF(transpose) THEN
      l = SIZE(val, 1)
      fmt = "("//trim(str(l, no_sign=.true.)) // trim(FInt32)//")"
      DO ii =1, SIZE(val,2)
        WRITE(Obj%UnitNo, fmt%chars()) val( :, ii )
      END DO
    ELSE
      ! print rowwise
      l = SIZE(val,2)
      fmt = "("//trim(str(l, no_sign=.true.)) // trim(FInt32)//")"
      DO ii =1, SIZE(val,1)
        WRITE(Obj%UnitNo, fmt%chars()) val( ii, : )
      END DO
    END IF

  TYPE IS ( Integer(Int64) )
    ! print column wise
    IF(transpose) THEN
      l = SIZE(val, 1)
      fmt = "("//trim(str(l, no_sign=.true.)) // trim(FInt64)//")"
      DO ii =1, SIZE(val,2)
        WRITE(Obj%UnitNo, fmt%chars()) val( :, ii )
      END DO
    ELSE
      ! print rowwise
      l = SIZE(val,2)
      fmt = "("//trim(str(l, no_sign=.true.)) // trim(FInt64)//")"
      DO ii =1, SIZE(val,1)
        WRITE(Obj%UnitNo, fmt%chars()) val( ii, : )
      END DO
    END IF

  TYPE IS ( Real(Real32) )
    ! print column wise
    IF(transpose) THEN
      l = SIZE(val, 1)
      fmt = "("//trim(str(l, no_sign=.true.)) // trim(FReal32)//")"
      DO ii =1, SIZE(val,2)
        WRITE(Obj%UnitNo, fmt%chars()) val( :, ii )
      END DO
    ELSE
      ! print rowwise
      l = SIZE(val,2)
      fmt = "("//trim(str(l, no_sign=.true.)) // trim(FReal32)//")"
      DO ii =1, SIZE(val,1)
        WRITE(Obj%UnitNo, fmt%chars()) val( ii, : )
      END DO
    END IF

  TYPE IS ( Real(Real64) )
    ! print column wise
    IF(transpose) THEN
      l = SIZE(val, 1)
      fmt = "("//trim(str(l, no_sign=.true.)) // trim(FReal64)//")"
      DO ii =1, SIZE(val,2)
        WRITE(Obj%UnitNo, fmt%chars()) val( :, ii )
      END DO
    ELSE
      ! print rowwise
      l = SIZE(val,2)
      fmt = "("//trim(str(l, no_sign=.true.)) // trim(FReal64)//")"
      DO ii =1, SIZE(val,1)
        WRITE(Obj%UnitNo, fmt%chars()) val( ii, : )
      END DO
    END IF
  END SELECT

END PROCEDURE write_data_ascii_r2

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

END SUBMODULE WriteMethods