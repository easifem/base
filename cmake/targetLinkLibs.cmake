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

function(FIND_EASIFEM_DEPENDENCY EXT_PKG_LIST)
  foreach(p ${EXT_PKG_LIST})
    find_package(${p} REQUIRED)
    if(${p}_FOUND)
      message(STATUS "FOUND ${p}")
    else()
      message(ERROR "NOT FOUND ${p}")
    endif()
  endforeach()
endfunction(FIND_EASIFEM_DEPENDENCY)

function(LINK_EASIFEM_DEPENDENCY EXT_PKG_LIST PROJECT_NAME)
  foreach(p ${EXT_PKG_LIST})
    target_link_libraries(${PROJECT_NAME} PUBLIC ${p}::${p})
  endforeach()
endfunction(LINK_EASIFEM_DEPENDENCY)

if(USE_LAPACK95)
  list(APPEND EXT_PKGS LAPACK95)
  list(APPEND TARGET_COMPILE_DEF "-DUSE_LAPACK95")
endif()

list(APPEND EXT_PKGS Sparsekit)
list(APPEND EXT_PKGS toml-f)

find_easifem_dependency("${EXT_PKGS}")
link_easifem_dependency("${EXT_PKGS}" "${PROJECT_NAME}")
