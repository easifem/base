!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   ContinuumSpin_Class.F90
!                   Last Update :   Dec-29-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!   TYPE :: Module
!
!   DESCRIPTION
!       -   Rank2Tensor class is extended to Define ContinuumSpin Class
!==============================================================================

 MODULE ContinuumSpin_Class
    USE GlobalData
    USE IO
    USE Rank2Tensor_Class
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: ContinuumSpin_, ContinuumSpin, ContinuumSpin_Pointer

!------------------------------------------------------------------------------
!                                                               ContinuumSpin_
!------------------------------------------------------------------------------

 TYPE, EXTENDS( Rank2Tensor_ ) :: ContinuumSpin_

 END TYPE ContinuumSpin_

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    INTERFACE ContinuumSpin
        MODULE PROCEDURE Constructor1, Constructor2
    END INTERFACE

    INTERFACE ContinuumSpin_Pointer
        MODULE PROCEDURE Constructor_1, Constructor_2
    END INTERFACE

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

 CONTAINS

!------------------------------------------------------------------------------
!                                                               Constructor_1
!------------------------------------------------------------------------------

 FUNCTION Constructor_1(  )

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1.  Empty Velocity Gradient constructor
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


    ! Define intent of dummy varialbes
    CLASS( ContinuumSpin_ ), POINTER :: Constructor_1

    ALLOCATE( Constructor_1 )

 END FUNCTION Constructor_1

!------------------------------------------------------------------------------
!                                                               Constructor_2
!------------------------------------------------------------------------------

 FUNCTION Constructor_2( L )

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1.  Empty Velocity Gradient constructor
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    USE VelocityGradient_Class

    ! Define intent of dummy varialbes
    CLASS( ContinuumSpin_ ), POINTER :: Constructor_2
    TYPE( VelocityGradient_ ), INTENT( IN ) :: L

    ALLOCATE( Constructor_2 )
    Constructor_2 = .Anti. L

 END FUNCTION Constructor_2

!------------------------------------------------------------------------------
!                                                               Constructor1
!------------------------------------------------------------------------------

 FUNCTION Constructor1( )

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1. Empty Velocity Gradient constructor
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    ! Define intent of dummy variables
    TYPE( ContinuumSpin_ ) :: Constructor1

 END FUNCTION Constructor1

!------------------------------------------------------------------------------
!                                                               Constructor2
!------------------------------------------------------------------------------

 FUNCTION Constructor2( L )

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1.  Empty Velocity Gradient constructor
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    USE VelocityGradient_Class

    ! Define intent of dummy varialbes
    TYPE( ContinuumSpin_ ) :: Constructor2
    TYPE( VelocityGradient_ ), INTENT( IN ) :: L

    Constructor2 = .Anti. L

 END FUNCTION Constructor2

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

 END MODULE ContinuumSpin_Class

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

