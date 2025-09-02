# This program is a part of EASIFEM library Expandable And Scalable
# Infrastructure for Finite Element Methods htttps://www.easifem.com Vikas
# Sharma, Ph.D., vickysharma0812@gmail.com
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
  PRIVATE ${src_path}/FEVariable_Method.F90
          ${src_path}/FEVariable_AdditionMethod.F90
          ${src_path}/FEVariable_SubtractionMethod.F90
          ${src_path}/FEVariable_DivisionMethod.F90
          ${src_path}/FEVariable_MultiplicationMethod.F90
          ${src_path}/FEVariable_DotProductMethod.F90
          ${src_path}/FEVariable_ConstructorMethod.F90
          ${src_path}/FEVariable_QuadratureVariableMethod.F90
          ${src_path}/FEVariable_NodalVariableMethod.F90
          ${src_path}/FEVariable_UnaryMethod.F90
          ${src_path}/FEVariable_GetMethod.F90
          ${src_path}/FEVariable_InterpolationMethod.F90
          ${src_path}/FEVariable_ScalarInterpolationMethod.F90
          ${src_path}/FEVariable_VectorInterpolationMethod.F90
          ${src_path}/FEVariable_MatrixInterpolationMethod.F90
          ${src_path}/FEVariable_IOMethod.F90
          ${src_path}/FEVariable_MeanMethod.F90)

