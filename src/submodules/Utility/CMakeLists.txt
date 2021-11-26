# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 23/02/2021 this file was created
#-----------------------------------------------------------------------

SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/Utility@Sort.F90
  ${src_path}/Utility@SwapMethods.F90
  ${src_path}/Utility@ReallocateMethods.F90
  ${src_path}/Utility@ProductMethods.F90
  ${src_path}/Utility@FunctionalFortran.F90
  ${src_path}/Utility@AppendMethods.F90
  ${src_path}/Utility@Input.F90
  ${src_path}/Utility@Inverse.F90
  ${src_path}/Utility@MatrixMul.F90
  ${src_path}/Utility@Assert.F90
  ${src_path}/Utility@ApproxMethods.F90
  ${src_path}/Utility@HashingMethod.F90
  ${src_path}/Utility@Misc.F90
  ${src_path}/Utility@StringMethods.F90
)
