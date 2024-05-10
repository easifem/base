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
  option(USE_OpenMP OFF)
  if(USE_OpenMP)

    if(APPLE)
      if(CMAKE_C_COMPILER_ID MATCHES "Clang" OR CMAKE_C_COMPILER_ID MATCHES
                                                "AppleClang")
        set(OpenMP_C
            "${CMAKE_C_COMPILER}"
            CACHE STRING "" FORCE)
        set(OpenMP_C_FLAGS
            "-fopenmp=libomp -Wno-unused-command-line-argument"
            CACHE STRING "" FORCE)
        set(OpenMP_C_LIB_NAMES
            "libomp" "libgomp" "libiomp5"
            CACHE STRING "" FORCE)
        set(OpenMP_libomp_LIBRARY
            ${OpenMP_C_LIB_NAMES}
            CACHE STRING "" FORCE)
        set(OpenMP_libgomp_LIBRARY
            ${OpenMP_C_LIB_NAMES}
            CACHE STRING "" FORCE)
        set(OpenMP_libiomp5_LIBRARY
            ${OpenMP_C_LIB_NAMES}
            CACHE STRING "" FORCE)

        set(OpenMP_CXX
            "${CMAKE_CXX_COMPILER}"
            CACHE STRING "" FORCE)
        set(OpenMP_CXX_FLAGS
            "-fopenmp=libomp -Wno-unused-command-line-argument"
            CACHE STRING "" FORCE)

        set(OpenMP_CXX_LIB_NAMES
            "libomp" "libgomp" "libiomp5"
            CACHE STRING "" FORCE)
      endif()
    endif()

    find_package(OpenMP REQUIRED)

  endif()

  if(OpenMP_FOUND)
    message(STATUS "FOUND OpenMP")
    message(STATUS "OpenMP_Fortran_LIBRARIES: ${OpenMP_Fortran_LIBRARIES}")
    list(APPEND TARGET_COMPILE_DEF "-DUSE_OpenMP")
    list(APPEND TARGET_COMPILE_OPT ${OpenMP_Fortran_FLAGS})
    # TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${OpenMP_Fortran_LIBRARIES})
    target_link_libraries(${PROJECT_NAME} PUBLIC OpenMP::OpenMP_Fortran)
  else()
    message(ERROR "NOT FOUND OpenMP")
  endif()

endif()
