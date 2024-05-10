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

  option(USE_GTK OFF)

  if(USE_GTK)

    list(APPEND TARGET_COMPILE_DEF "-DUSE_GTK")

    find_package(PkgConfig)
    pkg_check_modules(GTKFORTRAN REQUIRED gtk-4-fortran)
    find_path(
      GTKFORTRAN_MODULE_DIRS
      NAMES gtk.mod
      PATHS ${GTKFORTRAN_INCLUDE_DIRS})
    find_library(GTKFORTRAN_LIBRARY NAMES gtk-4-fortran)

    message(STATUS "GTKFORTRAN_CFLAGS : ${GTKFORTRAN_CFLAGS}")
    message(STATUS "GTKFORTRAN_LIBRARY : ${GTKFORTRAN_LIBRARY}")
    message(STATUS "GTKFORTRAN_LIBRARIES : ${GTKFORTRAN_LIBRARIES}")
    message(STATUS "GTKFORTRAN_LIBRARY_DIRS : ${GTKFORTRAN_LIBRARY_DIRS}")
    message(STATUS "GTKFORTRAN_INCLUDE_DIRS : ${GTKFORTRAN_INCLUDE_DIRS}")
    message(STATUS "GTKFORTRAN_MODULE_DIRS : ${GTKFORTRAN_MODULE_DIRS}")

    target_link_libraries(${PROJECT_NAME} PUBLIC ${GTKFORTRAN_LIBRARY}
                                                 ${GTKFORTRAN_LIBRARIES})
    target_include_directories(${PROJECT_NAME} PUBLIC ${GTKFORTRAN_INCLUDE_DIRS}
                                                      ${GTKFORTRAN_MODULE_DIRS})

  else()

    message(STATUS "NOT USING GTK-Fortran")

  endif()

endif()
