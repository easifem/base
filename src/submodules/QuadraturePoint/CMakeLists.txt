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
  ${src_path}/QuadraturePoint_Method@IO.f90
  ${src_path}/QuadraturePoint_Method@Constructor.f90
  ${src_path}/QuadraturePoint_Method@GaussLegendre.f90
  ${src_path}/QuadraturePoint_Method@GaussLegendre@Line.f90
  ${src_path}/QuadraturePoint_Method@GaussLegendre@Triangle.f90
  ${src_path}/QuadraturePoint_Method@GaussLegendre@Quadrangle.f90
  ${src_path}/QuadraturePoint_Method@GaussLegendre@Tetrahedron.f90
  ${src_path}/QuadraturePoint_Method@GaussLegendre@Hexahedron.f90
  ${src_path}/QuadraturePoint_Method@GaussLegendre@Prism.f90
  ${src_path}/QuadraturePoint_Method@GaussLegendre@Pyramid.f90
)

