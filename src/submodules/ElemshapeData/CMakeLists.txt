# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 1/03/2021 this file was created
#-----------------------------------------------------------------------

SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/ElemshapeData_Method@ConstructorMethods.F90
  ${src_path}/ElemshapeData_Method@IOMethods.F90
  ${src_path}/ElemshapeData_Method@GetMethods.F90
  ${src_path}/ElemshapeData_Method@StabilizationParamMethods.F90
  ${src_path}/ElemshapeData_Method@ProjectionMethods.F90
  ${src_path}/ElemshapeData_Method@InterpolMethods.F90
  ${src_path}/ElemshapeData_Method@LocalGradientMethods.F90
  ${src_path}/ElemshapeData_Method@GradientMethods.F90
  ${src_path}/ElemshapeData_Method@LocalDivergenceMethods.F90
  ${src_path}/ElemshapeData_Method@DivergenceMethods.F90
  ${src_path}/ElemshapeData_Method@setMethod.F90
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
