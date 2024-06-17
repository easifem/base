!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   Strain_Class.F90
!                   Last Update :   Dec-29-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!   TYPE :: Module
!
!   DESCRIPTION
!       -   Rank2Tensor class is extended to Define Strain Class
!==============================================================================

 MODULE Strain_Class
    USE GlobalData
    USE IO
    USE Rank2Tensor_Class
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: Strain_, Strain, Strain_Pointer

!------------------------------------------------------------------------------
!                                                               Strain_
!------------------------------------------------------------------------------

 TYPE, EXTENDS( Rank2Tensor_ ) :: Strain_

 END TYPE Strain_

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    INTERFACE Strain
        MODULE PROCEDURE Constructor1
    END INTERFACE

    INTERFACE Strain_Pointer
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
!   1.  Empty strain constructor
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


    ! Define intent of dummy varialbes
    CLASS( Strain_ ), POINTER :: Constructor_1

    ALLOCATE( Constructor_1 )

 END FUNCTION Constructor_1

!------------------------------------------------------------------------------
!                                                               Constructor1
!------------------------------------------------------------------------------

 FUNCTION Constructor1( )

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1. Empty strain constructor
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    ! Define intent of dummy variables
    TYPE( Strain_ ) :: Constructor1

 END FUNCTION Constructor1

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

 END MODULE Strain_Class

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

