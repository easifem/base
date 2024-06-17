!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   STConvectiveMatrix_Class.f90
!                   Last Update :   Nov-17-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!  TYPE :: Module
!
!   DESCRIPTION
!       -   STElemShapeData_ Class is extended for computing the Convection or
!           Advection matrix.
!
!==============================================================================

!------------------------------------------------------------------------------
!                                                               USE ASSOCIATION
!------------------------------------------------------------------------------

 MODULE STConvectiveMatrix_Class
    USE IO
    USE GlobalData
    USE ShapeData_Class
    USE STShapeData_Class
    USE STElemShapeData_Class
    
    PRIVATE
    PUBLIC :: STConvectiveMatrix_, STConvectiveMatrix_Pointer, &
    STConvectiveMatrix

!------------------------------------------------------------------------------
!                                                         STConvectiveMatrix_
!------------------------------------------------------------------------------

 TYPE, EXTENDS( STElemShapeData_ ) :: STConvectiveMatrix_

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!       1.  -   This class for computation of mass matrix
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    CONTAINS
    PROCEDURE, PUBLIC, PASS :: &
#include "./MethodNames.part"

 END TYPE STConvectiveMatrix_

!------------------------------------------------------------------------------
!                                                                  INTERFACES
!------------------------------------------------------------------------------

    INTERFACE STConvectiveMatrix_Pointer
        MODULE PROCEDURE Constructor_1, Constructor_2, Constructor_3
    END INTERFACE

    INTERFACE STConvectiveMatrix
        MODULE PROCEDURE Constructor1, Constructor2, Constructor3
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
#include "./ConvectiveMatrix_13.part"
#include "./ConvectiveMatrix_14.part"
#include "./ConvectiveMatrix_15.part"
#include "./ConvectiveMatrix_16.part"
#include "./ConvectiveMatrix_17.part"
#include "./ConvectiveMatrix_18.part"
#include "./ConvectiveMatrix_19.part"
#include "./ConvectiveMatrix_20.part"
#include "./ConvectiveMatrix_21.part"
#include "./ConvectiveMatrix_22.part"
#include "./ConvectiveMatrix_23.part"
#include "./ConvectiveMatrix_24.part"
#include "./ConvectiveMatrix_25.part"
#include "./ConvectiveMatrix_26.part"
#include "./ConvectiveMatrix_27.part"
#include "./ConvectiveMatrix_28.part"
#include "./ConvectiveMatrix_29.part"
#include "./ConvectiveMatrix_30.part"
#include "./ConvectiveMatrix_31.part"
#include "./ConvectiveMatrix_32.part"
#include "./ConvectiveMatrix_33.part"
#include "./ConvectiveMatrix_34.part"
#include "./ConvectiveMatrix_35.part"
#include "./ConvectiveMatrix_36.part"
#include "./ConvectiveMatrix_37.part"
#include "./ConvectiveMatrix_38.part"
#include "./ConvectiveMatrix_39.part"

 END MODULE STConvectiveMatrix_Class

