!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   ConvectiveMatrix_Class.f90
!                   Last Update :   Nov-19-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!  TYPE :: Module
!
!   DESCRIPTION
!       -   ElemShapeData_ Class is extended for computing the Convection or
!           Advection matrix.
!==============================================================================

MODULE ConvectiveMatrix_Class
  USE IO
	USE GlobalData
	USE Utility, ONLY : OUTERPROD

  USE ElemShapeData_Class
  USE ShapeData_Class
  
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: ConvectiveMatrix_, ConvectiveMatrix, ConvectiveMatrix_Pointer

!------------------------------------------------------------------------------
!                                                               ElemShapeData_
!------------------------------------------------------------------------------

TYPE, EXTENDS( ElemShapeData_ ) :: ConvectiveMatrix_

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1.  This class for computation of convective matrix
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

  CONTAINS
    PROCEDURE, PUBLIC, PASS( Obj ) :: &
#include "./MethodNames.part"

END TYPE ConvectiveMatrix_

!------------------------------------------------------------------------------
!                                                                  Constructor
!------------------------------------------------------------------------------

  INTERFACE ConvectiveMatrix_Pointer
    MODULE PROCEDURE Constructor_1, Constructor_2
  END INTERFACE

  INTERFACE ConvectiveMatrix
    MODULE PROCEDURE Constructor1, Constructor2
  END INTERFACE

!------------------------------------------------------------------------------
!                                                                      CONTAINS
!------------------------------------------------------------------------------

CONTAINS

#include "./Constructor.part"
#include "./ConvectiveMatrix_1.part"
#include "./ConvectiveMatrix_2.part"
#include "./ConvectiveMatrix_3.part"
#include "./ConvectiveMatrix_4.part"
#include "./ConvectiveMatrix_5.part"
#include "./ConvectiveMatrix_6.part"
#include "./ConvectiveMatrix_7.part"
#include "./ConvectiveMatrix_8.part"
#include "./ConvectiveMatrix_9.part"
#include "./ConvectiveMatrix_10.part"
#include "./ConvectiveMatrix_11.part"
#include "./ConvectiveMatrix_12.part"

END MODULE ConvectiveMatrix_Class
!
