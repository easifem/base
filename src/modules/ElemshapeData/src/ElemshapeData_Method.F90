! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Methods related to [[elemShapeData_]] datatype

MODULE ElemshapeData_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

#include "./ConstructorMethods.inc"
#include "./IOMethods.inc"
#include "./H1LagrangeMethods.inc"
#include "./H1HermitMethods.inc"
#include "./H1SerendipityMethods.inc"
#include "./H1HierarchyMethods.inc"
#include "./H1DivLagrangeMethods.inc"
#include "./H1DivHermitMethods.inc"
#include "./H1DivSerendipityMethods.inc"
#include "./H1DivHierarchyMethods.inc"
#include "./H1CurlLagrangeMethods.inc"
#include "./H1CurlHermitMethods.inc"
#include "./H1CurlSerendipityMethods.inc"
#include "./H1CurlHierarchyMethods.inc"
#include "./DGLagrangeMethods.inc"
#include "./DGHermitMethods.inc"
#include "./DGSerendipityMethods.inc"
#include "./DGHierarchyMethods.inc"
#include "./SetMethods.inc"
#include "./InterpolMethods.inc"
#include "./LocalGradientMethods.inc"
#include "./GradientMethods.inc"
#include "./LocalDivergenceMethods.inc"
#include "./DivergenceMethods.inc"
#include "./ProjectionMethods.inc"
#include "./GetMethods.inc"
#include "./StabilizationParamMethods.inc"

!----------------------------------------------------------------------------
!                                                                   Contains
!----------------------------------------------------------------------------

END MODULE ElemshapeData_Method
