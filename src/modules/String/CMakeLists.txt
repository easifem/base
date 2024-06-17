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
  ${src_path}/String_Class.F90
  ${src_path}/String_Method.F90
)

# set variables used for compile definitions of targets after support check
INCLUDE(CheckFortranSourceRuns)
check_fortran_source_runs(
    "program r16p_support;
        integer, parameter :: r16p = selected_real_kind(33, 4931);
        if(r16p < 0) stop 1;
    end program r16p_support"
    R16P_SUPPORTED
    SRC_EXT f90)
IF(R16P_SUPPORTED)
  SET(r16p_supported "-D_R16P")
ENDIF()

LIST( APPEND TARGET_COMPILE_DEF ${r16p_supported} )