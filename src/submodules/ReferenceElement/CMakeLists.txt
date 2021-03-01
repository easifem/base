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
  ${src_path}/ReferenceElement_Method@Constructor.f90
  ${src_path}/ReferenceElement_Method@IO.f90
  ${src_path}/ReferenceElement_Method@PrismMethods.f90
  ${src_path}/ReferenceElement_Method@TetrahedronMethods.f90
  ${src_path}/ReferenceElement_Method@Geometry.f90
  ${src_path}/ReferenceElement_Method@Lagrange.f90
  ${src_path}/ReferenceElement_Method@PyramidMethods.f90
  ${src_path}/ReferenceElement_Method@TriangleMethods.f90
  ${src_path}/ReferenceElement_Method@HexahedronMethods.f90
  ${src_path}/ReferenceElement_Method@LineMethods.f90
  ${src_path}/ReferenceElement_Method@QuadrangleMethods.f90
  ${src_path}/ReferenceElement_Method@VTK.f90
)