# This program is a part of EASIFEM library Copyright (C) 2020-2021  Vikas
# Sharma, Ph.D
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https: //www.gnu.org/licenses/>
#

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME}
  PRIVATE ${src_path}/ElemshapeData_ConstructorMethods@Methods.F90
          ${src_path}/ElemshapeData_DivergenceMethods@Methods.F90
          ${src_path}/ElemshapeData_GetMethods@Methods.F90
          ${src_path}/ElemshapeData_GradientMethods@Methods.F90
          ${src_path}/Lagrange/ElemshapeData_Lagrange@Methods.F90
          ${src_path}/H1/ElemshapeData_H1Methods@HermitMethods.F90
          ${src_path}/H1/ElemshapeData_H1Methods@HierarchyMethods.F90
          ${src_path}/H1/ElemshapeData_H1Methods@OrthogonalMethods.F90
          ${src_path}/H1/ElemshapeData_H1Methods@SerendipityMethods.F90
          ${src_path}/DG/ElemshapeData_DGMethods@HermitMethods.F90
          ${src_path}/DG/ElemshapeData_DGMethods@HierarchyMethods.F90
          ${src_path}/DG/ElemshapeData_DGMethods@LagrangeMethods.F90
          ${src_path}/DG/ElemshapeData_DGMethods@OrthogonalMethods.F90
          ${src_path}/DG/ElemshapeData_DGMethods@SerendipityMethods.F90
          ${src_path}/HDiv/ElemshapeData_HDivMethods@HermitMethods.F90
          ${src_path}/HDiv/ElemshapeData_HDivMethods@HierarchyMethods.F90
          ${src_path}/HDiv/ElemshapeData_HDivMethods@LagrangeMethods.F90
          ${src_path}/HDiv/ElemshapeData_HDivMethods@OrthogonalMethods.F90
          ${src_path}/HDiv/ElemshapeData_HDivMethods@SerendipityMethods.F90
          ${src_path}/HCurl/ElemshapeData_HCurlMethods@HermitMethods.F90
          ${src_path}/HCurl/ElemshapeData_HCurlMethods@HierarchyMethods.F90
          ${src_path}/HCurl/ElemshapeData_HCurlMethods@LagrangeMethods.F90
          ${src_path}/HCurl/ElemshapeData_HCurlMethods@OrthogonalMethods.F90
          ${src_path}/HCurl/ElemshapeData_HCurlMethods@SerendipityMethods.F90
          ${src_path}/ElemshapeData_HminHmaxMethods@Methods.F90
          ${src_path}/ElemshapeData_HRGNParamMethods@Methods.F90
          ${src_path}/ElemshapeData_HRQIParamMethods@Methods.F90
          ${src_path}/ElemshapeData_InterpolMethods@Methods.F90
          ${src_path}/ElemshapeData_IOMethods@Methods.F90
          ${src_path}/ElemshapeData_LocalDivergenceMethods@Methods.F90
          ${src_path}/ElemshapeData_LocalGradientMethods@Methods.F90
          ${src_path}/ElemshapeData_ProjectionMethods@Methods.F90
          ${src_path}/ElemshapeData_SetMethods@Methods.F90
          ${src_path}/ElemshapeData_StabilizationParamMethods@SUGN3.F90
          ${src_path}/ElemshapeData_StabilizationParamMethods@SUPG.F90
          ${src_path}/ElemshapeData_StabilizationParamMethods@Takizawa2018.F90
          ${src_path}/ElemshapeData_UnitNormalMethods@Methods.F90)
