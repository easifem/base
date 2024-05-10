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

if(${PROJECT_NAME} MATCHES "easifemBase")
  option(USE_LUA OFF)
  if(USE_LUA)
    list(APPEND TARGET_COMPILE_DEF "-DUSE_LUA")
    find_package(Lua 5.4 EXACT)

    if(NOT LUA_FOUND)
      find_package(PkgConfig REQUIRED)
      pkg_check_modules(LUA REQUIRED lua)
      find_library(LUA_LIBRARY NAMES lua lua5.4)
      set(LUA_LIBRARIES ${LUA_LIBRARY})
      find_path(LUA_INCLUDE_DIR NAMES lua5.4/lua.h lua5.4/lualib.h lua/lua.h
                                      lua/lualib.h)
    endif()

    target_link_libraries(${PROJECT_NAME} PUBLIC ${LUA_LIBRARIES})
    target_include_directories(${PROJECT_NAME} PUBLIC ${LUA_INCLUDE_DIR})

    message(STATUS "LUA LIBRARIES :: ${LUA_LIBRARIES}")
    message(STATUS "LUA INCLUDE DIR :: ${LUA_INCLUDE_DIR}")

  else()
    message(STATUS "NOT USING LUA LIBRARIES")
  endif()
endif()
