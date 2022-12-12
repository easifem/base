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
  ${src_path}/MappingUtility@Methods.F90
  ${src_path}/BinomUtility@Methods.F90
  ${src_path}/SortUtility@Methods.F90
  ${src_path}/SwapUtility@Methods.F90
  ${src_path}/ConvertUtility@Methods.F90
  ${src_path}/ReallocateUtility@Methods.F90
  ${src_path}/ProductUtility@Methods.F90
  ${src_path}/ArangeUtility@Methods.F90
  ${src_path}/GridPointUtility@Methods.F90
  ${src_path}/FunctionalFortranUtility@Methods.F90
  ${src_path}/OnesUtility@Methods.F90
  ${src_path}/ZerosUtility@Methods.F90
  ${src_path}/EyeUtility@Methods.F90
  ${src_path}/DiagUtility@Methods.F90
  ${src_path}/AppendUtility@Methods.F90
  ${src_path}/InputUtility@Methods.F90
  ${src_path}/InvUtility@Methods.F90
  ${src_path}/MatmulUtility@Methods.F90
  ${src_path}/ContractionUtility@Methods.F90
  ${src_path}/AssertUtility@Methods.F90
  ${src_path}/ApproxUtility@Methods.F90
  ${src_path}/HashingUtility@Methods.F90
  ${src_path}/MiscUtility@Methods.F90
  ${src_path}/StringUtility@Methods.F90
  ${src_path}/IntegerUtility@Methods.F90
  ${src_path}/PushPopUtility@Methods.F90
  ${src_path}/EigenUtility@Methods.F90
  ${src_path}/SymUtility@Methods.F90
)
