# This file is a part of easifem-base
# (c) 2021, all right reserved, Dr Vikas Sharma
#
# log
# 16/02/2021 this file was created
#-----------------------------------------------------------------------

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")

target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/Test_Method.f90
)