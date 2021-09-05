# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 23/02/2021 this file was created
#-----------------------------------------------------------------------

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/Utility@Sort.f90
  ${src_path}/Utility@Swap.f90
  ${src_path}/Utility@Reallocate.f90
  ${src_path}/Utility@Prod.f90
  ${src_path}/Utility@FunctionalFortran.f90
  ${src_path}/Utility@Append.f90
  ${src_path}/Utility@Input.f90
  ${src_path}/Utility@Inverse.f90
  ${src_path}/Utility@MatrixMul.f90
  ${src_path}/Utility@Assert.f90
  ${src_path}/Utility@Approx.f90
  ${src_path}/Utility@HashingMethod.f90
  ${src_path}/Utility@Misc.f90
  ${src_path}/Utility@StringMethods.f90
)