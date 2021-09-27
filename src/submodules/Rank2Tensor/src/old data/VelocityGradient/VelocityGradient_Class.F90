!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   VelocityGradient_Class.F90
!                   Last Update :   Dec-29-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!   TYPE :: Module
!
!   DESCRIPTION
!       -   Rank2Tensor class is extended to Define VelocityGradient Class
!==============================================================================

 MODULE VelocityGradient_Class
    USE GlobalData
    USE IO
    USE Rank2Tensor_Class
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: VelocityGradient_, VelocityGradient, VelocityGradient_Pointer

!------------------------------------------------------------------------------
!                                                            VelocityGradient_
!------------------------------------------------------------------------------

 TYPE, EXTENDS( Rank2Tensor_ ) :: VelocityGradient_

 END TYPE VelocityGradient_

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    INTERFACE VelocityGradient
        MODULE PROCEDURE Constructor1
    END INTERFACE

    INTERFACE VelocityGradient_Pointer
        MODULE PROCEDURE Constructor_1
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
    CLASS( VelocityGradient_ ), POINTER :: Constructor_1

    ALLOCATE( Constructor_1 )

 END FUNCTION Constructor_1

!------------------------------------------------------------------------------
!                                                               Constructor1
!------------------------------------------------------------------------------

 FUNCTION Constructor1( )

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1. Empty Velocity Gradient constructor
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    ! Define intent of dummy variables
    TYPE( VelocityGradient_ ) :: Constructor1

 END FUNCTION Constructor1

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

 END MODULE VelocityGradient_Class

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

