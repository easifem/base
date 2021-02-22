# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 17/02/2021 this file was created
#-----------------------------------------------------------------------

# hdf5 lib
find_package(HDF5 REQUIRED COMPONENTS Fortran HL)
if(HDF5_FOUND)
  cmake_print_variables(HDF5_VERSION HDF5_INCLUDE_DIRS)
else()
  message(ERROR "HDF5 library not found, download it")
  ## todo
  ## fetch the package
endif()

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}interface.f90
  ${src_path}read.f90
  ${src_path}reader.f90
  ${src_path}reader_lt.f90
  ${src_path}string_utils.f90
  ${src_path}write.f90
  ${src_path}writer.f90
  ${src_path}writer_lt.f90
)

target_include_directories(
  ${PROJECT_NAME}
  PUBLIC
  ${HDF5_INCLUDE_DIRS}
)

target_link_libraries(
  ${PROJECT_NAME}
  PUBLIC
  ${HDF5_Fortran_LIBRARIES}
  ${HDF5_Fortran_HL_LIBRARIES}
)
