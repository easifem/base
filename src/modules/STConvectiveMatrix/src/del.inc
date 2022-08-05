
!----------------------------------------------------------------------------
!                                       STConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix in rank-4 array

INTERFACE
  MODULE PURE FUNCTION Mat4_STConvectiveMatrix_1(test, trial, c, &
      & term1, term2, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: c
    !! convective velocity
    INTEGER(I4B), INTENT(IN) :: term1
    !! term1 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z => space derivative
    !! DEL_t => time derivative
    INTEGER(I4B), INTENT(IN) :: term2
    !! term2 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z => space derivative
    !! DEL_t => time derivative
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
    !! "trial" take projection of C on trial
    !! "test" take projection of C on test
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finite element matrix.
  END FUNCTION Mat4_STConvectiveMatrix_1
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_1
END INTERFACE STConvectiveMatrix

!----------------------------------------------------------------------------
!                                       STConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix in rank-4 array

INTERFACE
  MODULE PURE FUNCTION Mat4_STConvectiveMatrix_2(test, trial, c1, c2, &
      & term1, term2, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! Scalar FE variable
    TYPE(FEVariable_), INTENT(IN) :: c2
    !! convective velocity, vector FEVariable,
    INTEGER(I4B), INTENT(IN) :: term1
    !! term1 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z, DEL_X_ALL => space derivative
    !! DEL_t => time derivative
    INTEGER(I4B), INTENT(IN) :: term2
    !! term2 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z, DEL_X_ALL => space derivative
    !! DEL_t => time derivative
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
    !! "trial" take projection of C on trial
    !! "test" take projection of C on test
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finite element matrix.
  END FUNCTION Mat4_STConvectiveMatrix_2
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_2
END INTERFACE STConvectiveMatrix

!----------------------------------------------------------------------------
!                                       STConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix in rank-4 array

INTERFACE
  MODULE PURE FUNCTION Mat4_STConvectiveMatrix_3(test, trial, c, &
      & term1, term2, ncopy, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: c
    !! convective velocity
    INTEGER(I4B), INTENT(IN) :: term1
    !! term1 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z => space derivative
    !! DEL_t => time derivative
    INTEGER(I4B), INTENT(IN) :: term2
    !! term2 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z => space derivative
    !! DEL_t => time derivative
    INTEGER( I4B ), INTENT( IN ) :: ncopy
    !! number of diagonal copies
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
    !! "trial" take projection of C on trial
    !! "test" take projection of C on test
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finite element matrix.
  END FUNCTION Mat4_STConvectiveMatrix_3
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_3
END INTERFACE STConvectiveMatrix


!----------------------------------------------------------------------------
!                                       STConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix in rank-4 array

INTERFACE
  MODULE PURE FUNCTION Mat4_STConvectiveMatrix_4(test, trial, c1, c2, &
      & term1, term2, ncopy, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! Scalar FE variable
    TYPE(FEVariable_), INTENT(IN) :: c2
    !! convective velocity, vector FEVariable,
    INTEGER(I4B), INTENT(IN) :: term1
    !! term1 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z, DEL_X_ALL => space derivative
    !! DEL_t => time derivative
    INTEGER(I4B), INTENT(IN) :: term2
    !! term2 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z, DEL_X_ALL => space derivative
    !! DEL_t => time derivative
    INTEGER( I4B ), INTENT( IN ) :: ncopy
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
    !! "trial" take projection of C on trial
    !! "test" take projection of C on test
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finite element matrix.
  END FUNCTION Mat4_STConvectiveMatrix_4
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_4
END INTERFACE STConvectiveMatrix



!----------------------------------------------------------------------------
!                                         ConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix

INTERFACE
  MODULE PURE FUNCTION Mat2_STConvectiveMatrix_1(test, trial, c, crank, &
    & term1, term2, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: c
    INTEGER(I4B), INTENT(IN) :: term1
    INTEGER(I4B), INTENT(IN) :: term2
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION Mat2_STConvectiveMatrix_1
END INTERFACE

INTERFACE ConvectiveMatrix
  MODULE PROCEDURE Mat2_STConvectiveMatrix_1
END INTERFACE ConvectiveMatrix

!----------------------------------------------------------------------------
!                                         ConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-06
! update: 2021-12-06
! summary: Returns the space-time convective matrix

INTERFACE
  MODULE PURE FUNCTION Mat2_STConvectiveMatrix_2(test, trial, c1, c2, &
      & term1, term2, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    INTEGER(I4B), INTENT(IN) :: term1
    INTEGER(I4B), INTENT(IN) :: term2
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION Mat2_STConvectiveMatrix_2
END INTERFACE

INTERFACE ConvectiveMatrix
  MODULE PROCEDURE Mat2_STConvectiveMatrix_2
END INTERFACE ConvectiveMatrix

!----------------------------------------------------------------------------
!                                         ConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix

INTERFACE
  MODULE PURE FUNCTION Mat2_STConvectiveMatrix_3(test, trial, c, &
    & term1, term2, ncopy, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: c
    INTEGER(I4B), INTENT(IN) :: term1
    INTEGER(I4B), INTENT(IN) :: term2
    INTEGER( I4B ), INTENT( IN ) :: ncopy
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION Mat2_STConvectiveMatrix_3
END INTERFACE

INTERFACE ConvectiveMatrix
  MODULE PROCEDURE Mat2_STConvectiveMatrix_3
END INTERFACE ConvectiveMatrix

!----------------------------------------------------------------------------
!                                         ConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-06
! update: 2021-12-06
! summary: Returns the space-time convective matrix

INTERFACE
  MODULE PURE FUNCTION Mat2_STConvectiveMatrix_4(test, trial, c1, c2, &
      & term1, term2, ncopy, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    INTEGER(I4B), INTENT(IN) :: term1
    INTEGER(I4B), INTENT(IN) :: term2
    INTEGER(I4B), INTENT(IN) :: ncopy
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION Mat2_STConvectiveMatrix_4
END INTERFACE

INTERFACE ConvectiveMatrix
  MODULE PROCEDURE Mat2_STConvectiveMatrix_4
END INTERFACE ConvectiveMatrix

!----------------------------------------------------------------------------
!                                         ConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix

INTERFACE
  MODULE PURE FUNCTION Mat2_STConvectiveMatrix_5(test, trial, c, &
    & term1, term2, opt) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: c
    INTEGER(I4B), INTENT(IN) :: term1
    INTEGER(I4B), INTENT(IN) :: term2
    INTEGER( I4B ), INTENT( IN ) :: opt(1)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION Mat2_STConvectiveMatrix_5
END INTERFACE

INTERFACE ConvectiveMatrix
  MODULE PROCEDURE Mat2_STConvectiveMatrix_5
END INTERFACE ConvectiveMatrix

!----------------------------------------------------------------------------
!                                         ConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-06
! update: 2021-12-06
! summary: Returns the space-time convective matrix

INTERFACE
  MODULE PURE FUNCTION Mat2_STConvectiveMatrix_6(test, trial, c1, c2, &
      & term1, term2, opt) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    INTEGER(I4B), INTENT(IN) :: term1
    INTEGER(I4B), INTENT(IN) :: term2
    INTEGER(I4B), INTENT(IN) :: opt(1)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION Mat2_STConvectiveMatrix_6
END INTERFACE

INTERFACE ConvectiveMatrix
  MODULE PROCEDURE Mat2_STConvectiveMatrix_6
END INTERFACE ConvectiveMatrix



! !----------------------------------------------------------------------------
! !                                                           ConvectiveMatrix
! !----------------------------------------------------------------------------

! MODULE PROCEDURE Mat2_STConvectiveMatrix_1
! REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
! m4 = STConvectiveMatrix(test=test, trial=trial, term1=term1, &
!   & term2=term2, c=c, projecton=projecton)
! CALL convert(from=m4, to=ans)
! DEALLOCATE (m4)
! END PROCEDURE Mat2_STConvectiveMatrix_1

! !----------------------------------------------------------------------------
! !                                                           ConvectiveMatrix
! !----------------------------------------------------------------------------

! MODULE PROCEDURE Mat2_STConvectiveMatrix_2
! REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
! m4 = STConvectiveMatrix(test=test, trial=trial, term1=term1, &
!   & term2=term2, c1=c1, c2=c2, projecton=projecton)
! CALL convert(from=m4, to=ans)
! DEALLOCATE (m4)
! END PROCEDURE Mat2_STConvectiveMatrix_2

! !----------------------------------------------------------------------------
! !                                                           ConvectiveMatrix
! !----------------------------------------------------------------------------

! MODULE PROCEDURE Mat2_STConvectiveMatrix_3
! REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
! m4 = STConvectiveMatrix(test=test, trial=trial, term1=term1, &
!   & term2=term2, ncopy=ncopy, c=c, projecton=projecton)
! CALL convert(from=m4, to=ans)
! DEALLOCATE (m4)
! END PROCEDURE Mat2_STConvectiveMatrix_3

! !----------------------------------------------------------------------------
! !                                                           ConvectiveMatrix
! !----------------------------------------------------------------------------

! MODULE PROCEDURE Mat2_STConvectiveMatrix_4
! REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
! m4 = STConvectiveMatrix(test=test, trial=trial, &
!   & c1=c1, c2=c2, term1=term1, &
!   & term2=term2, ncopy=ncopy, projecton=projecton)
! CALL convert(from=m4, to=ans)
! DEALLOCATE (m4)
! END PROCEDURE Mat2_STConvectiveMatrix_4


!----------------------------------------------------------------------------
!                                       STConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix in rank-4 array

INTERFACE
  MODULE PURE FUNCTION Mat4_STConvectiveMatrix_5(test, trial, c, crank, &
      & term1, term2, opt, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), INTENT(IN) :: c
    !! convective velocity
    TYPE(FEVariableVector_), INTENT( IN ) :: crank
    !! vector fevariable
    INTEGER(I4B), INTENT(IN) :: term1
    !! term1 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z => space derivative
    !! DEL_t => time derivative
    INTEGER(I4B), INTENT(IN) :: term2
    !! term2 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z => space derivative
    !! DEL_t => time derivative
    INTEGER( I4B ), INTENT( IN ) :: opt
    !! number of diagonal copies
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
    !! "trial" take projection of C on trial
    !! "test" take projection of C on test
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finite element matrix.
  END FUNCTION Mat4_STConvectiveMatrix_5
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_5
END INTERFACE STConvectiveMatrix

!----------------------------------------------------------------------------
!                                       STConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix in rank-4 array

INTERFACE
  MODULE PURE FUNCTION Mat4_STConvectiveMatrix_6(test, trial, c, crank, &
      & term1, term2, ncopy, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), INTENT(IN) :: c
    !! scalar fevariable
    TYPE(FEVariableScalar_), INTENT( IN ) :: crank
    !! scalar fevariable
    INTEGER(I4B), INTENT(IN) :: term1
    !! term1 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z => space derivative
    !! DEL_t => time derivative
    INTEGER(I4B), INTENT(IN) :: term2
    !! term2 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z => space derivative
    !! DEL_t => time derivative
    INTEGER( I4B ), INTENT( IN ) :: ncopy
    !! number of diagonal copies
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
    !! "trial" take projection of C on trial
    !! "test" take projection of C on test
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finite element matrix.
  END FUNCTION Mat4_STConvectiveMatrix_6
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_6
END INTERFACE STConvectiveMatrix

!----------------------------------------------------------------------------
!                                       STConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix in rank-4 array

INTERFACE
  MODULE PURE FUNCTION Mat4_STConvectiveMatrix_7(test, trial, &
      & term1, term2, ncopy, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    INTEGER(I4B), INTENT(IN) :: term1
    !! term1 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z => space derivative
    !! DEL_t => time derivative
    INTEGER(I4B), INTENT(IN) :: term2
    !! term2 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z => space derivative
    !! DEL_t => time derivative
    INTEGER( I4B ), INTENT( IN ) :: ncopy
    !! number of diagonal copies
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
    !! "trial" take projection of C on trial
    !! "test" take projection of C on test
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finite element matrix.
  END FUNCTION Mat4_STConvectiveMatrix_7
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_7
END INTERFACE STConvectiveMatrix

!----------------------------------------------------------------------------
!                                       STConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix in rank-4 array

INTERFACE
  MODULE PURE FUNCTION Mat4_STConvectiveMatrix_8(test, trial, c1, c2, &
      & c1rank, c2rank, term1, term2, ncopy, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! Scalar FE variable
    TYPE(FEVariable_), INTENT(IN) :: c2
    !! convective velocity, vector FEVariable,
    TYPE(FEVariableScalar_), INTENT( IN ) :: c1rank
    TYPE(FEVariableVector_), INTENT( IN ) :: c2rank
    INTEGER(I4B), INTENT(IN) :: term1
    !! term1 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z, DEL_X_ALL => space derivative
    !! DEL_t => time derivative
    INTEGER(I4B), INTENT(IN) :: term2
    !! term2 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z, DEL_X_ALL => space derivative
    !! DEL_t => time derivative
    INTEGER( I4B ), INTENT( IN ) :: ncopy
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
    !! "trial" take projection of C on trial
    !! "test" take projection of C on test
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finite element matrix.
  END FUNCTION Mat4_STConvectiveMatrix_8
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_8
END INTERFACE STConvectiveMatrix
