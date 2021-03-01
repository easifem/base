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
  # ${src_path}/ElemshapeData_Method@getMethod.f90
  # ${src_path}/ElemshapeData_Method@setMethod.f90
  # ${src_path}/ElemshapeData_Method@LineH1Lagrange.f90
  # ${src_path}/ElemshapeData_Method@QuadrangleH1Lagrange.f90
  # ${src_path}/ElemshapeData_Method@H1Lagrange.f90
  # ${src_path}/ElemshapeData_Method@PrismH1Lagrange.f90
  # ${src_path}/ElemshapeData_Method@TetrahedronH1Lagrange.f90
  # ${src_path}/ElemshapeData_Method@HexahedronH1Lagrange.f90
  # ${src_path}/ElemshapeData_Method@PyramidH1Lagrange.f90
  # ${src_path}/ElemshapeData_Method@TriangleH1Lagrange.f90
)
