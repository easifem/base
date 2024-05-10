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

if(${PROJECT_NAME} MATCHES "easifemBase")
  option(USE_PLPLOT OFF)
  if(USE_PLPLOT)
    list(APPEND TARGET_COMPILE_DEF "-DUSE_PLPLOT")

    find_package(PkgConfig REQUIRED)
    pkg_check_modules(PLPLOT-FORTRAN REQUIRED plplot-fortran)
    pkg_check_modules(PLPLOT REQUIRED plplot)
    find_library(PLPLOT_LIBRARY NAMES plplot)
    find_library(PLPLOT-FORTRAN_LIBRARY NAMES plplotfortran)
    set(PLPLOT_LIBRARIES ${PLPLOT_LIBRARY} ${PLPLOT-FORTRAN_LIBRARY})
    find_path(PLPLOT_INCLUDE_DIR NAMES plplot/plplot.h)
    find_path(
      PLPLOT_MODULE_DIR
      NAMES plplot.mod
      PATHS ${PLPLOT-FORTRAN_INCLUDE_DIRS})
    include(FindPackageHandleStandardArgs)
    find_package_handle_standard_args(PLPLOT DEFAULT_MSG PLPLOT_LIBRARIES
                                      PLPLOT_INCLUDE_DIR)

    set(PLPLOT_FORTRAN_INCLUDE_DIR "${PLPLOT_MODULE_DIR}")

    target_link_libraries(${PROJECT_NAME} PUBLIC ${PLPLOT_LIBRARIES})
    target_include_directories(
      ${PROJECT_NAME} PUBLIC ${PLPLOT_INCLUDE_DIR}
                             ${PLPLOT_FORTRAN_INCLUDE_DIR})
    message(STATUS "PLPLOT_LIBRARIES : ${PLPLOT_LIBRARIES}")
    message(STATUS "PLPLOT_FORTRAN_LIBRARY : ${PLPLOT_FORTRAN_LIBRARY}")
    message(STATUS "PLPLOT_INCLUDE_DIR : ${PLPLOT_INCLUDE_DIR}")
  else()
    message(STATUS "NOT USING PLPLOT LIBRARIES")
  endif()
endif()
