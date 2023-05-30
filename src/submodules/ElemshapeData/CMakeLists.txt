# This program is a part of EASIFEM library
# Copyright (C) 2020-2021  Vikas Sharma, Ph.D
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https: //www.gnu.org/licenses/>
#

SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/ElemshapeData_ConstructorMethods@Methods.F90
  ${src_path}/ElemshapeData_DGHermitMethods@Methods.F90
  ${src_path}/ElemshapeData_DGHierarchyMethods@Methods.F90
  ${src_path}/ElemshapeData_DGLagrangeMethods@Methods.F90
  ${src_path}/ElemshapeData_DGSerendipityMethods@Methods.F90
  ${src_path}/ElemshapeData_DivergenceMethods@Methods.F90
  ${src_path}/ElemshapeData_GetMethods@Methods.F90
  ${src_path}/ElemshapeData_GradientMethods@Methods.F90
  ${src_path}/ElemshapeData_H1CurlHermitMethods@Methods.F90
  ${src_path}/ElemshapeData_H1CurlHierarchyMethods@Methods.F90
  ${src_path}/ElemshapeData_H1CurlLagrangeMethods@Methods.F90
  ${src_path}/ElemshapeData_H1CurlSerendipityMethods@Methods.F90
  ${src_path}/ElemshapeData_H1DivHermitMethods@Methods.F90
  ${src_path}/ElemshapeData_H1DivHierarchyMethods@Methods.F90
  ${src_path}/ElemshapeData_H1DivLagrangeMethods@Methods.F90
  ${src_path}/ElemshapeData_H1DivSerendipityMethods@Methods.F90
  ${src_path}/ElemshapeData_H1HermitMethods@Methods.F90
  ${src_path}/ElemshapeData_H1HierarchyMethods@Methods.F90
  ${src_path}/ElemshapeData_H1LagrangeMethods@Hexahedron.F90
  ${src_path}/ElemshapeData_H1LagrangeMethods@Line.F90
  ${src_path}/ElemshapeData_H1LagrangeMethods@Methods.F90
  ${src_path}/ElemshapeData_H1LagrangeMethods@Prism.F90
  ${src_path}/ElemshapeData_H1LagrangeMethods@Pyramid.F90
  ${src_path}/ElemshapeData_H1LagrangeMethods@Quadrangle.F90
  ${src_path}/ElemshapeData_H1LagrangeMethods@Tetrahedron.F90
  ${src_path}/ElemshapeData_H1LagrangeMethods@Triangle.F90
  ${src_path}/ElemshapeData_H1SerendipityMethods@Methods.F90
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
  ${src_path}/ElemshapeData_UnitNormalMethods@Methods.F90
)