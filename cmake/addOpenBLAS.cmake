# This program is a part of EASIFEM library Copyright (C) 2020-2021  Vikas
# Sharma, Ph.D
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
  # SET(BLA_VENDOR "OpenBLAS")
  find_package(LAPACK REQUIRED)
  if(LAPACK_FOUND)
    message(STATUS "FOUND LAPACK")
  endif()
  if(BLA_VENDOR MATCHES "MKL")
    message(STATUS "BLA_VENDOR : MKL")
    list(APPEND TARGET_COMPILE_DEF "-DUSE_INTEL_MKL")
  elseif(BLA_VENDOR MATCHES "OpenBLAS")
    message(STATUS "BLA_VENDOR : OpenBLAS")
    list(APPEND TARGET_COMPILE_DEF "-DUSE_OpenBLAS")
  else()
    message(STATUS "BLA_VENDOR : ${BLA_VENDOR}")
    message(STATUS "BLA_VENDOR : System provided")
    list(APPEND TARGET_COMPILE_DEF "-DUSE_NativeBLAS")

    if(APPLE)
      list(APPEND TARGET_COMPILE_DEF "-DUSE_APPLE_NativeBLAS")
    endif()

  endif()

  message(STATUS "BLAS_LIBRARIES: ${BLAS_LIBRARIES}")
  message(STATUS "LAPACK_LIBRARIES: ${LAPACK_LIBRARIES}")

  target_link_libraries(${PROJECT_NAME} PUBLIC ${LAPACK_LIBRARIES}
                                               ${BLAS_LIBRARIES})
endif()
