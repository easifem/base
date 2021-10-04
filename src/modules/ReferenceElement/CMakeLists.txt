# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 16/02/2021 this file was created
#-----------------------------------------------------------------------

SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/ReferenceElement_Method.F90
  ${src_path}/ReferencePoint_Method.F90
  ${src_path}/ReferenceLine_Method.F90
  ${src_path}/ReferenceTriangle_Method.F90
  ${src_path}/ReferenceQuadrangle_Method.F90
  ${src_path}/ReferenceTetrahedron_Method.F90
  ${src_path}/ReferenceHexahedron_Method.F90
  ${src_path}/ReferencePrism_Method.F90
  ${src_path}/ReferencePyramid_Method.F90
)