# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 16/02/2021 this file was created
#-----------------------------------------------------------------------

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/ReferenceElement_Method.f90
  ${src_path}/ReferenceLine_Method.f90
  ${src_path}/ReferenceTriangle_Method.f90
  ${src_path}/ReferenceQuadrangle_Method.f90
  ${src_path}/ReferenceTetrahedron_Method.f90
  ${src_path}/ReferenceHexahedron_Method.f90
  ${src_path}/ReferencePrism_Method.f90
  ${src_path}/ReferencePyramid_Method.f90
)