!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   STFintVector_Class.f90
!                   Last Update :   Jan-04-2018
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!  TYPE :: Module
!
!   DESCRIPTION
!       -   STElemShapeData_ Class is extended for computing the STFintVector.
!==============================================================================

MODULE STFintVector_Class
  USE IO
  USE GlobalData
  USE STElemShapeData_Class
  USE STShapeData_Class
  USE ShapeData_Class
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: STFintVector, STFintVector_, STFintVector_Pointer

  !------------------------------------------------------------------------------
  !                                                                 STFintVector_
  !------------------------------------------------------------------------------

  TYPE, EXTENDS( STElemShapeData_ ) :: STFintVector_

     !.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
     !       1.  This class computes the Space-Time Internal Force vector
     !           for space-time element.
     !.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

   CONTAINS

     PROCEDURE, PUBLIC, PASS :: &
#include "./MethodNames.part"
          
          END TYPE STFintVector_

     !------------------------------------------------------------------------------
     !                                                                   INTERFACES
     !------------------------------------------------------------------------------

     INTERFACE STFintVector
        MODULE PROCEDURE Constructor1, Constructor2, Constructor3
     END INTERFACE STFintVector

     INTERFACE STFintVector_Pointer
        MODULE PROCEDURE Constructor_1, Constructor_2, Constructor_3
     END INTERFACE STFintVector_Pointer

     !------------------------------------------------------------------------------
     !                                                                      CONTAINS
     !------------------------------------------------------------------------------

   CONTAINS

#include "./Constructor.part"
#include "./FintVector_1.part"
#include "./FintVector_2.part"
#include "./FintVector_3.part"
#include "./FintVector_4.part"
#include "./FintVector_5.part"
#include "./FintVector_6.part"
#include "./FintVector_7.part"
#include "./FintVector_8.part"

   END MODULE STFintVector_Class

   !------------------------------------------------------------------------------
   !                                                                      
   !------------------------------------------------------------------------------
