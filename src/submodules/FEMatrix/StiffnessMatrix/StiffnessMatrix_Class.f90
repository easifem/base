!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   StiffnessMatrix_Class.f90
!                   Last Update :   Mar-08-2018
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!  TYPE :: Module
!
!   DESCRIPTION
!       -   Stiffness matrices for space elements
!
!==============================================================================

MODULE StiffnessMatrix_Class
  USE GlobalData
  USE IO
  USE Utility, ONLY : OUTERPROD
  USE ElemShapeData_Class
  USE ShapeData_Class
  USE ConstitutiveData_Class
  USE MaterialJacobian_Class


  IMPLICIT NONE

  PRIVATE
  PUBLIC :: StiffnessMatrix_, StiffnessMatrix_POINTER, StiffnessMatrix

  !------------------------------------------------------------------------------
  !                                                             StiffnessMatrix_
  !------------------------------------------------------------------------------

  TYPE, EXTENDS( ElemShapeData_ ) :: StiffnessMatrix_

  !.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
  !       1.  Stiffness matrices for the space-time element for &
  !           solid mechanics application
  !.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

  CONTAINS

    PROCEDURE, PUBLIC, PASS( Obj ) :: &
#include "./MethodNames.part"
          
          END TYPE StiffnessMatrix_

  !------------------------------------------------------------------------------
  !                                                                   INTERFACES
  !------------------------------------------------------------------------------


  INTERFACE StiffnessMatrix_POINTER
    MODULE PROCEDURE Constructor_1, Constructor_2
  END INTERFACE StiffnessMatrix_POINTER

  INTERFACE StiffnessMatrix
    MODULE PROCEDURE Constructor1, Constructor2
  END INTERFACE StiffnessMatrix

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

END MODULE StiffnessMatrix_Class

