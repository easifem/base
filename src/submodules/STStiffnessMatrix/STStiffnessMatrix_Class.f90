!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   STStiffnessMatrix_Class.f90
!                   Last Update :   Nov-21-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!  TYPE :: Module
!
!   DESCRIPTION
!       -   Stiffness matrices for space-time elements
!
!==============================================================================

 MODULE STStiffnessMatrix_Class
    USE GlobalData
    USE IO
    USE STElemShapeData_Class
    USE STShapeData_Class
    USE ShapeData_Class
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: STStiffnessMatrix_, STStiffnessMatrix_POINTER, STStiffnessMatrix

!------------------------------------------------------------------------------
!                                                              STElemShapeData_
!------------------------------------------------------------------------------

 TYPE, EXTENDS( STElemShapeData_ ) :: STStiffnessMatrix_

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!       1.  Stiffness matrices for the space-time element for &
!           solid mechanics application
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    CONTAINS
    
        PROCEDURE, PUBLIC, PASS :: &
#include "./MethodNames.part"

 END TYPE STStiffnessMatrix_

!------------------------------------------------------------------------------
!                                                                   INTERFACES
!------------------------------------------------------------------------------


    INTERFACE STStiffnessMatrix_POINTER
        MODULE PROCEDURE Constructor_1, Constructor_2, Constructor_3
    END INTERFACE

    INTERFACE STStiffnessMatrix
        MODULE PROCEDURE Constructor1, Constructor2, Constructor3
    END INTERFACE

!------------------------------------------------------------------------------
!                                                                      CONTAINS
!------------------------------------------------------------------------------

 CONTAINS

#include "./Constructor.part"
#include "./StiffnessMatrix_1.part"
#include "./StiffnessMatrix_2.part"
#include "./StiffnessMatrix_3.part"
#include "./StiffnessMatrix_4.part"
#include "./StiffnessMatrix_5.part"
#include "./StiffnessMatrix_6.part"
#include "./StiffnessMatrix_7.part"
#include "./StiffnessMatrix_8.part"
#include "./StiffnessMatrix_9.part"
#include "./StiffnessMatrix_10.part"
#include "./StiffnessMatrix_11.part"
#include "./StiffnessMatrix_12.part"
#include "./StiffnessMatrix_13.part"
#include "./StiffnessMatrix_14.part"

 END MODULE STStiffnessMatrix_Class

