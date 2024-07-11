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
  PRIVATE ${src_path}/ElemshapeData_Method.F90
          ${src_path}/ElemshapeData_ConstructorMethods.F90
          ${src_path}/ElemshapeData_DivergenceMethods.F90
          ${src_path}/ElemshapeData_GradientMethods.F90
          ${src_path}/ElemshapeData_GetMethods.F90
          ${src_path}/ElemshapeData_H1Methods.F90
          ${src_path}/ElemshapeData_DGMethods.F90
          ${src_path}/ElemshapeData_HDivMethods.F90
          ${src_path}/ElemshapeData_HCurlMethods.F90
          ${src_path}/ElemshapeData_Lagrange.F90
          ${src_path}/ElemshapeData_Hierarchical.F90
          ${src_path}/ElemshapeData_HminHmaxMethods.F90
          ${src_path}/ElemshapeData_HRGNParamMethods.F90
          ${src_path}/ElemshapeData_HRQIParamMethods.F90
          ${src_path}/ElemshapeData_InterpolMethods.F90
          ${src_path}/ElemshapeData_IOMethods.F90
          ${src_path}/ElemshapeData_LocalDivergenceMethods.F90
          ${src_path}/ElemshapeData_LocalGradientMethods.F90
          ${src_path}/ElemshapeData_ProjectionMethods.F90
          ${src_path}/ElemshapeData_SetMethods.F90
          ${src_path}/ElemshapeData_StabilizationParamMethods.F90
          ${src_path}/ElemshapeData_UnitNormalMethods.F90)
