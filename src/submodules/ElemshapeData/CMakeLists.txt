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
  ${src_path}/ElemshapeData_Method@ConstructorMethods.F90
  ${src_path}/ElemshapeData_Method@IOMethods.F90
  ${src_path}/ElemshapeData_Method@GetMethods.F90
  ${src_path}/ElemshapeData_Method@SetMethods.F90
  ${src_path}/ElemshapeData_Method@UnitNormalMethods.F90
  ${src_path}/ElemshapeData_Method@HRGNParamMethods.F90
  ${src_path}/ElemshapeData_Method@HRQIParamMethods.F90
  ${src_path}/ElemshapeData_Method@SUGN3ParamMethods.F90
  ${src_path}/ElemshapeData_Method@SUGN3Param_Takizawa2018Methods.F90
  ${src_path}/ElemshapeData_Method@SUPGParamMethods.F90
  ${src_path}/ElemshapeData_Method@DBEStabParamMethods.F90
  # ${src_path}/ElemshapeData_Method@StabilizationParamMethods.F90
  ${src_path}/ElemshapeData_Method@ProjectionMethods.F90
  ${src_path}/ElemshapeData_Method@InterpolMethods.F90
  ${src_path}/ElemshapeData_Method@LocalGradientMethods.F90
  ${src_path}/ElemshapeData_Method@GradientMethods.F90
  ${src_path}/ElemshapeData_Method@LocalDivergenceMethods.F90
  ${src_path}/ElemshapeData_Method@DivergenceMethods.F90
  ${src_path}/ElemshapeData_Method@H1Lagrange.F90
  ${src_path}/ElemshapeData_Method@H1Lagrange@Line.F90
  ${src_path}/ElemshapeData_Method@H1Lagrange@Triangle.F90
  ${src_path}/ElemshapeData_Method@H1Lagrange@Quadrangle.F90
  ${src_path}/ElemshapeData_Method@H1Lagrange@Tetrahedron.F90
  ${src_path}/ElemshapeData_Method@H1Lagrange@Hexahedron.F90
  ${src_path}/ElemshapeData_Method@H1Lagrange@Prism.F90
  ${src_path}/ElemshapeData_Method@H1Lagrange@Pyramid.F90
  ${src_path}/ElemshapeData_Method@H1Serendipity.F90
  ${src_path}/ElemshapeData_Method@H1Hermit.F90
  ${src_path}/ElemshapeData_Method@H1Hierarchy.F90
  ${src_path}/ElemshapeData_Method@H1DivLagrange.F90
  ${src_path}/ElemshapeData_Method@H1DivSerendipity.F90
  ${src_path}/ElemshapeData_Method@H1DivHermit.F90
  ${src_path}/ElemshapeData_Method@H1DivHierarchy.F90
  ${src_path}/ElemshapeData_Method@H1CurlLagrange.F90
  ${src_path}/ElemshapeData_Method@H1CurlSerendipity.F90
  ${src_path}/ElemshapeData_Method@H1CurlHermit.F90
  ${src_path}/ElemshapeData_Method@H1CurlHierarchy.F90
  ${src_path}/ElemshapeData_Method@DGLagrange.F90
  ${src_path}/ElemshapeData_Method@DGSerendipity.F90
  ${src_path}/ElemshapeData_Method@DGHermit.F90
  ${src_path}/ElemshapeData_Method@DGHierarchy.F90
)
