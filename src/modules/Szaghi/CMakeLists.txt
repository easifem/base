# This file is a part of easifem-base
# (c) 2021, all right reserved, Dr Vikas Sharma
#
# log
# 16/02/2021 this file was created
#-----------------------------------------------------------------------

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")

target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}PENF/penf_global_parameters_variables.f90
  ${src_path}PENF/penf_b_size.f90
  ${src_path}PENF/penf_stringify.f90
  ${src_path}PENF/penf.f90
  ${src_path}BeFoR64/befor64_pack_data_m.f90
  ${src_path}BeFoR64/befor64.f90
  ${src_path}StringiFor/stringifor_string_t.f90
  ${src_path}StringiFor/stringifor.f90
)