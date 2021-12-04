!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   STMassMatrix_Class.f90
!                   Last Update :   Nov-15-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!  TYPE :: Module
!
!   DESCRIPTION
!       -   STElemShapeData_ Class is extended for computing the Massmatrix.
!==============================================================================

MODULE STMassMatrix_Class
  USE IO
  USE GlobalData
  USE STShapeData_Class
  USE STElemShapeData_Class
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: STMassMatrix_, STMassMatrix, STMassMatrix_Pointer 

  !------------------------------------------------------------------------------
  !                                                               ElemShapeData_
  !------------------------------------------------------------------------------

  TYPE, EXTENDS( STElemShapeData_ ) :: STMassMatrix_

     !.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
     !       1.  This class for computation of mass matrix
     !.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

   CONTAINS
     PROCEDURE, PUBLIC, PASS( Obj ) :: &
#include "./MethodNames.part"
          
          END TYPE STMassMatrix_

     !------------------------------------------------------------------------------
     !                                                                   INTERFACES
     !------------------------------------------------------------------------------

     INTERFACE STMassMatrix_Pointer
        MODULE PROCEDURE Constructor_1, Constructor_2, Constructor_3
     END INTERFACE STMassMatrix_Pointer

     INTERFACE STMassMatrix
        MODULE PROCEDURE Constructor1, Constructor2, Constructor3
     END INTERFACE STMassMatrix

     !------------------------------------------------------------------------------
     !                                                                      CONTAINS
     !------------------------------------------------------------------------------

   CONTAINS

#undef STMat

#include "./Constructor.part"
#include "./MassMatrix_1.part"
#include "./MassMatrix_2.part"
#include "./MassMatrix_3.part"
#include "./MassMatrix_4.part"
#include "./MassMatrix_5.part"
#include "./MassMatrix_6.part"
#include "./MassMatrix_7.part"
#include "./MassMatrix_8.part"
#include "./MassMatrix_9.part"
#include "./MassMatrix_10.part"
#include "./MassMatrix_11.part"
#include "./MassMatrix_12.part"
#include "./MassMatrix_13.part"
#include "./MassMatrix_14.part"
#include "./MassMatrix_15.part"
#include "./MassMatrix_16.part"
#include "./MassMatrix_17.part"
#include "./MassMatrix_18.part"

   END MODULE STMassMatrix_Class

