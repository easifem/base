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
  ${src_path}/MappingUtility.F90
  ${src_path}/BinomUtility.F90
  ${src_path}/AppendUtility.F90
  ${src_path}/ApproxUtility.F90
  ${src_path}/AssertUtility.F90
  ${src_path}/FunctionalFortranUtility.F90
  ${src_path}/ArangeUtility.F90
  ${src_path}/GridPointUtility.F90
  ${src_path}/OnesUtility.F90
  ${src_path}/ZerosUtility.F90
  ${src_path}/EyeUtility.F90
  ${src_path}/DiagUtility.F90
  ${src_path}/HashingUtility.F90
  ${src_path}/InputUtility.F90
  ${src_path}/InvUtility.F90
  ${src_path}/MatmulUtility.F90
  ${src_path}/ContractionUtility.F90
  ${src_path}/MiscUtility.F90
  ${src_path}/ProductUtility.F90
  ${src_path}/ReallocateUtility.F90
  ${src_path}/SortUtility.F90
  ${src_path}/StringUtility.F90
  ${src_path}/SwapUtility.F90
  ${src_path}/ConvertUtility.F90
  ${src_path}/IntegerUtility.F90
  ${src_path}/PushPopUtility.F90
  ${src_path}/EigenUtility.F90
  ${src_path}/SymUtility.F90
  ${src_path}/TriagUtility.F90
  ${src_path}/Utility.F90
)