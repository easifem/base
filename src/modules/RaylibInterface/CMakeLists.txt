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

IF(USE_RAYLIB)
  SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
  TARGET_SOURCES(
    ${PROJECT_NAME} PRIVATE
    ${src_path}/RaylibTypes.F90
    ${src_path}/RaylibEnums.F90
    ${src_path}/RaylibDrawMethods.F90
    ${src_path}/RaylibGetMethods.F90
    ${src_path}/RaylibSetMethods.F90
    ${src_path}/RaylibImageMethods.F90
    ${src_path}/RaylibUnloadMethods.F90
    ${src_path}/RaylibLoadMethods.F90
    ${src_path}/RaylibIsMethods.F90
    ${src_path}/RaylibCheckMethods.F90
    ${src_path}/RaylibMethods.F90
    ${src_path}/Raylib.F90
    ${src_path}/RaylibCamera.F90
    ${src_path}/RaylibMath.F90
    ${src_path}/RaylibUtil.F90
  )
ENDIF()
