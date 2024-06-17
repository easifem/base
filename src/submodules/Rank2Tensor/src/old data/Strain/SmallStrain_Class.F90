!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   SmallStrain_Class.F90
!                   Last Update :   Dec-30-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!   TYPE :: Module
!
!   DESCRIPTION
!       -   Rank2Tensor class is extended to Define SmallStrain Class
!==============================================================================

 MODULE SmallStrain_Class
    USE GlobalData
    USE IO
    USE Strain_Class
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: SmallStrain_, SmallStrain, SmallStrain_Pointer

!------------------------------------------------------------------------------
!                                                               SmallStrain_
!------------------------------------------------------------------------------

 TYPE, EXTENDS( Strain_ ) :: SmallStrain_

 END TYPE SmallStrain_

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    INTERFACE SmallStrain
        MODULE PROCEDURE Constructor1
    END INTERFACE

    INTERFACE SmallStrain_Pointer
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
!   1.  Empty Construction
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    ! Define intent of dummy varialbes
    CLASS( SmallStrain_ ), POINTER :: Constructor_1
    ALLOCATE( Constructor_1 )
 END FUNCTION Constructor_1

!------------------------------------------------------------------------------
!                                                               Constructor1
!------------------------------------------------------------------------------

 FUNCTION Constructor1(  )

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1. Empty Construction
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    ! Define intent of dummy variables
    TYPE( SmallStrain_ ) :: Constructor1
 END FUNCTION Constructor1

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

 END MODULE SmallStrain_Class

