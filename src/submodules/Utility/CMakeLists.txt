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
  ${src_path}/Utility@SortMethods.F90
  ${src_path}/Utility@SwapMethods.F90
  ${src_path}/Utility@ConvertMethods.F90
  ${src_path}/Utility@ReallocateMethods.F90
  ${src_path}/Utility@ProductMethods.F90
  ${src_path}/Utility@GridMethods.F90
  ${src_path}/Utility@FunctionalMethods.F90
  ${src_path}/Utility@OnesMethods.F90
  ${src_path}/Utility@EyeMethods.F90
  ${src_path}/Utility@DiagMethods.F90
  ${src_path}/Utility@AppendMethods.F90
  ${src_path}/Utility@InputMethods.F90
  ${src_path}/Utility@InverseMethods.F90
  ${src_path}/Utility@MatmulMethods.F90
  ${src_path}/Utility@ContractionMethods.F90
  ${src_path}/Utility@AssertMethods.F90
  ${src_path}/Utility@ApproxMethods.F90
  ${src_path}/Utility@HashingMethod.F90
  ${src_path}/Utility@MiscMethods.F90
  ${src_path}/Utility@StringMethods.F90
  ${src_path}/Utility@LineInterpolationPoints.F90
  ${src_path}/Utility@TriangleInterpolationPoints.F90
  ${src_path}/Utility@QuadrangleInterpolationPoints.F90
  ${src_path}/Utility@TetrahedronInterpolationPoints.F90
  ${src_path}/Utility@HexahedronInterpolationPoints.F90
  ${src_path}/Utility@PrismInterpolationPoints.F90
  ${src_path}/Utility@PyramidInterpolationPoints.F90
  ${src_path}/Utility@IntegerMethods.F90
)
