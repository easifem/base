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
  PRIVATE ${src_path}/FEVariable_AdditionMethod@Methods.F90
          ${src_path}/FEVariable_ConstructorMethod@Methods.F90
          ${src_path}/FEVariable_NodalVariableMethod@Methods.F90
          ${src_path}/FEVariable_QuadratureVariableMethod@Methods.F90
          ${src_path}/FEVariable_DivisionMethod@Methods.F90
          ${src_path}/FEVariable_MultiplicationMethod@Methods.F90
          ${src_path}/FEVariable_DotProductMethod@Methods.F90
          ${src_path}/FEVariable_SubtractionMethod@Methods.F90
          ${src_path}/FEVariable_MeanMethod@Methods.F90
          ${src_path}/FEVariable_UnaryMethod@Methods.F90
          ${src_path}/FEVariable_GetMethod@Methods.F90
          ${src_path}/FEVariable_IOMethod@Methods.F90
          ${src_path}/FEVariable_InterpolationMethod@ScalarMethods.F90
          ${src_path}/FEVariable_InterpolationMethod@VectorMethods.F90
          ${src_path}/FEVariable_InterpolationMethod@MatrixMethods.F90
          ${src_path}/FEVariable_InterpolationMethod@Methods.F90)
