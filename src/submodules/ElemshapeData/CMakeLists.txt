# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 1/03/2021 this file was created
#-----------------------------------------------------------------------

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/ElemshapeData_Method@Constructor.f90
  ${src_path}/ElemshapeData_Method@IO.f90
  ${src_path}/ElemshapeData_Method@getMethod.f90
  ${src_path}/ElemshapeData_Method@setMethod.f90
  ${src_path}/ElemshapeData_Method@H1Lagrange.f90
  ${src_path}/ElemshapeData_Method@H1Lagrange@Line.f90
  ${src_path}/ElemshapeData_Method@H1Lagrange@Triangle.f90
  ${src_path}/ElemshapeData_Method@H1Lagrange@Quadrangle.f90
  ${src_path}/ElemshapeData_Method@H1Lagrange@Tetrahedron.f90
  ${src_path}/ElemshapeData_Method@H1Lagrange@Hexahedron.f90
  ${src_path}/ElemshapeData_Method@H1Lagrange@Prism.f90
  ${src_path}/ElemshapeData_Method@H1Lagrange@Pyramid.f90
)
