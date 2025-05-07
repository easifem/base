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

MODULE ElemshapeData_Method
USE ElemshapeData_ConstructorMethods
USE ElemshapeData_DivergenceMethods
USE ElemshapeData_GetMethods
USE ElemshapeData_GradientMethods

! USE ElemshapeData_H1Methods
! USE ElemshapeData_HCurlMethods
! USE ElemshapeData_HDivMethods
! USE ElemshapeData_DGMethods

USE ElemshapeData_Lagrange
USE ElemshapeData_Hierarchical
USE ElemshapeData_Orthogonal

USE ElemshapeData_HRGNParamMethods
USE ElemshapeData_HRQIParamMethods
USE ElemshapeData_HminHmaxMethods
USE ElemshapeData_IOMethods
USE ElemshapeData_InterpolMethods
USE ElemshapeData_LocalDivergenceMethods
USE ElemshapeData_LocalGradientMethods
USE ElemshapeData_ProjectionMethods
USE ElemshapeData_SetMethods
USE ElemshapeData_StabilizationParamMethods
USE ElemshapeData_UnitNormalMethods

END MODULE ElemshapeData_Method
