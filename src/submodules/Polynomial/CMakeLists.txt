# This program is a part of EASIFEM library Copyright (C) 2020-2021  Vikas
# Sharma, Ph.D
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https: //www.gnu.org/licenses/>
#

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME}
  PRIVATE ${src_path}/LineInterpolationUtility@Methods.F90
          ${src_path}/QuadraturePoint_Triangle_Solin.F90
          ${src_path}/QuadraturePoint_Tetrahedron_Solin.F90
          ${src_path}/TriangleInterpolationUtility@Methods.F90
          ${src_path}/TriangleInterpolationUtility@QuadratureMethods.F90
          ${src_path}/TriangleInterpolationUtility@HeirarchicalBasisMethods.F90
          ${src_path}/TriangleInterpolationUtility@LagrangeBasisMethods.F90
          ${src_path}/TriangleInterpolationUtility@OrthogonalBasisMethods.F90
          ${src_path}/QuadrangleInterpolationUtility@Methods.F90
          ${src_path}/TetrahedronInterpolationUtility@Methods.F90
          ${src_path}/HexahedronInterpolationUtility@Methods.F90
          ${src_path}/PrismInterpolationUtility@Methods.F90
          ${src_path}/PyramidInterpolationUtility@Methods.F90
          ${src_path}/InterpolationUtility@Methods.F90
          ${src_path}/LagrangePolynomialUtility@Methods.F90
          ${src_path}/JacobiPolynomialUtility@Methods.F90
          ${src_path}/UltrasphericalPolynomialUtility@Methods.F90
          ${src_path}/LegendrePolynomialUtility@Methods.F90
          ${src_path}/LobattoPolynomialUtility@Methods.F90
          ${src_path}/UnscaledLobattoPolynomialUtility@Methods.F90
          ${src_path}/Chebyshev1PolynomialUtility@Methods.F90
          ${src_path}/OrthogonalPolynomialUtility@Methods.F90
          ${src_path}/RecursiveNodesUtility@Methods.F90)
