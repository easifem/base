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

IF(USE_SUPERLU)
  SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
  TARGET_SOURCES(
    ${PROJECT_NAME} PRIVATE
    ${src_path}/SuperLU_Types.F90
    ${src_path}/SuperLU_Enums.F90
    ${src_path}/SuperLU_Util_Methods.F90
    ${src_path}/SuperLU_dUtil_Methods.F90
    ${src_path}/SuperLU_dgssv_Methods.F90
    ${src_path}/SuperLU_dgssvx_Methods.F90
    ${src_path}/SuperLU_dgsitrf_Methods.F90
    ${src_path}/SuperLU_dgsisx_Methods.F90
    ${src_path}/SuperLU_dgstrf_Methods.F90
    ${src_path}/SuperLU_dgstrs_Methods.F90
    ${src_path}/SuperLU_dgscon_Methods.F90
    ${src_path}/SuperLU_dgsequ_Methods.F90
    ${src_path}/SuperLU_dlaqgs_Methods.F90
    ${src_path}/SuperLU_dgsrfs_Methods.F90
    ${src_path}/SuperLUInterface.F90
  )
ENDIF()
