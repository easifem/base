# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 2/03/2021 this file was created
# 2 March 2021 new file names were added
#-----------------------------------------------------------------------

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/ReferenceElement_Method@Constructor.f90
  ${src_path}/ReferenceElement_Method@IO.f90
  ${src_path}/ReferenceElement_Method@Geometry.f90
  ${src_path}/ReferenceElement_Method@Line.f90
  ${src_path}/ReferenceElement_Method@Triangle.f90
  ${src_path}/ReferenceElement_Method@Quadrangle.f90
  ${src_path}/ReferenceElement_Method@Tetrahedron.f90
  ${src_path}/ReferenceElement_Method@Prism.f90
  ${src_path}/ReferenceElement_Method@Hexahedron.f90
  ${src_path}/ReferenceElement_Method@Pyramid.f90
  ${src_path}/ReferenceElement_Method@VTK.f90
)