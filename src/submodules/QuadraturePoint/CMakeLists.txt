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
  ${src_path}/QuadraturePoint_Method@IOMethods.F90
  ${src_path}/QuadraturePoint_Method@GetMethods.F90
  ${src_path}/QuadraturePoint_Method@ConstructorMethods.F90
  ${src_path}/GaussLegendre/QuadraturePoint_Method@GaussLegendre.F90
  ${src_path}/GaussLegendre/GaussLegendre_Line_Methods.F90
  ${src_path}/GaussLegendre/GaussLegendre_Triangle_Methods.F90
  ${src_path}/GaussLegendre/GaussLegendre_Quadrangle_Methods.F90
  ${src_path}/GaussLegendre/GaussLegendre_Tetrahedron_Methods.F90
  ${src_path}/GaussLegendre/GaussLegendre_Hexahedron_Methods.F90
  ${src_path}/GaussLegendre/GaussLegendre_Prism_Methods.F90
  ${src_path}/GaussLegendre/GaussLegendre_Pyramid_Methods.F90
  ${src_path}/GaussLegendreLobatto/QuadraturePoint_Method@GaussLegendreLobatto.F90
  ${src_path}/GaussLegendreLobatto/GaussLegendreLobatto_Line_Methods.F90
  ${src_path}/GaussLegendreLobatto/GaussLegendreLobatto_Triangle_Methods.F90
  ${src_path}/GaussLegendreLobatto/GaussLegendreLobatto_Quadrangle_Methods.F90
  ${src_path}/GaussLegendreLobatto/GaussLegendreLobatto_Tetrahedron_Methods.F90
  ${src_path}/GaussLegendreLobatto/GaussLegendreLobatto_Hexahedron_Methods.F90
  ${src_path}/GaussLegendreLobatto/GaussLegendreLobatto_Prism_Methods.F90
  ${src_path}/GaussLegendreLobatto/GaussLegendreLobatto_Pyramid_Methods.F90
  ${src_path}/GaussLegendreRadauLeft/QuadraturePoint_Method@GaussLegendreRadauLeft.F90
  ${src_path}/GaussLegendreRadauRight/QuadraturePoint_Method@GaussLegendreRadauRight.F90
)

