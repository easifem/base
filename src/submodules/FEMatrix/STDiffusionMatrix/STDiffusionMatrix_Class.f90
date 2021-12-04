!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   STDiffusionMatrix_Class.f90
!                   Last Update :   Nov-20-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!  TYPE :: Module
!
!   DESCRIPTION
!       -   Diffusion matrices for space-time elements
!
!==============================================================================

 MODULE STDiffusionMatrix_Class
    USE GlobalData
    USE IO
    USE STElemShapeData_Class
    USE STShapeData_Class
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: STDiffusionMatrix_, STDiffusionMatrix, &
    STDiffusionMatrix_Pointer

!------------------------------------------------------------------------------
!                                                              STElemShapeData_
!------------------------------------------------------------------------------

 TYPE, EXTENDS( STElemShapeData_ ) :: STDiffusionMatrix_

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1.  Diffusion matrices for the space-time element.
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    CONTAINS
    
        PROCEDURE, PUBLIC, PASS( Obj ) :: &
#include "./MethodNames.part"

 END TYPE STDiffusionMatrix_

!------------------------------------------------------------------------------
!                                                                   INTERFACES
!------------------------------------------------------------------------------

    INTERFACE STDiffusionMatrix_Pointer
        MODULE PROCEDURE Constructor_1, Constructor_2, Constructor_3
    END INTERFACE

    INTERFACE STDiffusionMatrix
        MODULE PROCEDURE Constructor1, Constructor2, Constructor3
    END INTERFACE

!------------------------------------------------------------------------------
!                                                                      CONTAINS
!------------------------------------------------------------------------------

 CONTAINS

#include "./Constructor.part"
#include "./DiffusionMatrix_1.part"
#include "./DiffusionMatrix_2.part"
#include "./DiffusionMatrix_3.part"
#include "./DiffusionMatrix_4.part"
#include "./DiffusionMatrix_5.part"
#include "./DiffusionMatrix_6.part"
#include "./DiffusionMatrix_7.part"
#include "./DiffusionMatrix_8.part"
#include "./DiffusionMatrix_9.part"
#include "./DiffusionMatrix_10.part"
#include "./DiffusionMatrix_11.part"
#include "./DiffusionMatrix_12.part"
#include "./DiffusionMatrix_13.part"
#include "./DiffusionMatrix_14.part"
#include "./DiffusionMatrix_15.part"
#include "./DiffusionMatrix_16.part"
#include "./DiffusionMatrix_17.part"
#include "./DiffusionMatrix_18.part"
#include "./DiffusionMatrix_19.part"
#include "./DiffusionMatrix_20.part"
#include "./DiffusionMatrix_21.part"
#include "./DiffusionMatrix_22.part"
#include "./DiffusionMatrix_23.part"
#include "./DiffusionMatrix_24.part"
#include "./DiffusionMatrix_25.part"
#include "./DiffusionMatrix_26.part"
#include "./DiffusionMatrix_27.part"
#include "./DiffusionMatrix_28.part"
#include "./DiffusionMatrix_29.part"
#include "./DiffusionMatrix_30.part"
#include "./DiffusionMatrix_31.part"

 END MODULE STDiffusionMatrix_Class

