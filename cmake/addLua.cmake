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
      5.4
      EXACT
      )

    IF( NOT LUA_FOUND)
      FIND_PACKAGE(PkgConfig REQUIRED)
      pkg_check_modules(LUA REQUIRED lua)
      FIND_LIBRARY(LUA_LIBRARY NAMES lua lua5.4)
      SET(LUA_LIBRARIES ${LUA_LIBRARY})
      FIND_PATH(LUA_INCLUDE_DIR NAMES lua5.4/lua.h lua5.4/lualib.h lua/lua.h lua/lualib.h)
    ENDIF()

    TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${LUA_LIBRARIES} )
    TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${LUA_INCLUDE_DIR})

    MESSAGE( STATUS "LUA LIBRARIES :: ${LUA_LIBRARIES}" )
    MESSAGE( STATUS "LUA INCLUDE DIR :: ${LUA_INCLUDE_DIR}" )

  ELSE()
    MESSAGE( STATUS "NOT USING LUA LIBRARIES" )
  ENDIF()
ENDIF()
