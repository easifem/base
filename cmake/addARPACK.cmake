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
  option(USE_ARPACK OFF)
  if(USE_ARPACK)
    find_package(arpackng REQUIRED)
    if(arpackng_FOUND)
      message(STATUS "FOUND ARPACK-NG")
      list(APPEND TARGET_COMPILE_DEF "-DUSE_ARPACK")
      list(APPEND TARGET_COMPILE_OPT ${arpackng_Fortran_FLAGS})
      target_link_libraries(${PROJECT_NAME} PUBLIC ARPACK::ARPACK)
    else()
      message(ERROR "NOT FOUND ARPACK-NG")
    endif()
  endif()
endif()
