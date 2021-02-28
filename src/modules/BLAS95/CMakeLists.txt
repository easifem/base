# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 26/02/2021 this file was created
#-----------------------------------------------------------------------

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/EASIFEM_F77_BLAS.f90
  ${src_path}/EASIFEM_BLAS.f90
)

# # FIND PACKAGES BLAS AND LAPACK
# option(USE_BLAS "User blas")
# list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake)

# if(DEFINED ENV{CMAKE_BLAS_ROOT})
#   find_package(LAPACK COMPONENTS OpenBLAS REQUIRED)
# else()
#   if(USE_BLAS)
#     find_package(LAPACK COMPONENTS ${USE_BLAS} REQUIRED)
#   else()
#     find_package(LAPACK COMPONENTS OpenBLAS REQUIRED)
#   endif()
# endif()

# message(STATUS "LAPACK_LIBRARIES = ${LAPACK_LIBRARIES}")
# message(STATUS "BLAS_LIBRARIES = ${BLAS_LIBRARIES}")

# target_link_libraries(
#   ${PROJECT_NAME}
#   PUBLIC
#   ${LAPACK_LIBRARIES}
#   ${BLAS_LIBRARIES}
# )