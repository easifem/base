# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 26/02/2021 this file was created
#-----------------------------------------------------------------------

IF( USE_BLAS95 )
    LIST( APPEND TARGET_COMPILE_DEF "-DUSE_BLAS95" )
    SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
    TARGET_SOURCES(
      ${PROJECT_NAME} PRIVATE
      ${src_path}/EASIFEM_F77_BLAS.F90
      ${src_path}/EASIFEM_BLAS.F90
    )
ENDIF()
