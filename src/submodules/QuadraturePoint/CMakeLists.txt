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
  ${src_path}/QuadraturePoint_Method@IO.F90
  ${src_path}/QuadraturePoint_Method@Constructor.F90
  ${src_path}/QuadraturePoint_Method@GaussLegendre.F90
  ${src_path}/QuadraturePoint_Method@GaussLegendre@Line.F90
  ${src_path}/QuadraturePoint_Method@GaussLegendre@Triangle.F90
  ${src_path}/QuadraturePoint_Method@GaussLegendre@Quadrangle.F90
  ${src_path}/QuadraturePoint_Method@GaussLegendre@Tetrahedron.F90
  ${src_path}/QuadraturePoint_Method@GaussLegendre@Hexahedron.F90
  ${src_path}/QuadraturePoint_Method@GaussLegendre@Prism.F90
  ${src_path}/QuadraturePoint_Method@GaussLegendre@Pyramid.F90
)

