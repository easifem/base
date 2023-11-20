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

#....................................................................
#
#....................................................................

IF( ${PROJECT_NAME} MATCHES "easifemBase" )
  OPTION( USE_LUA OFF )
  IF( USE_LUA )
    LIST( APPEND TARGET_COMPILE_DEF "-DUSE_LUA" )
    FIND_PACKAGE(
      Lua
      REQUIRED
      )

    IF(LUA_FOUND)

      IF(LUA_VERSION_MAJOR LESS 5)
        MESSAGE(ERROR "LUA MAJOR VERSION SHOULD BE 5.4")
      ENDIF()

      IF(LUA_VERSION_MINOR LESS 4)
        MESSAGE(ERROR "LUA  MINOR VERSION SHOULD BE 5.4")
      ENDIF()

      TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LUA_LIBRARIES} )
      TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${LUA_INCLUDE_DIR})

      MESSAGE( STATUS "LUA LIBRARIES :: ${LUA_LIBRARIES}" )
      MESSAGE( STATUS "LUA INCLUDE DIR :: ${LUA_INCLUDE_DIR}" )

    ELSE()
      MESSAGE(ERROR "LUA NOT FOUND")
    ENDIF()

  ELSE()
    MESSAGE( STATUS "NOT USING LUA LIBRARIES" )
  ENDIF()
ENDIF()
