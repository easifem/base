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
  ${src_path}/Utility@SortMethods.F90
  ${src_path}/Utility@SwapMethods.F90
  ${src_path}/Utility@ConvertMethods.F90
  ${src_path}/Utility@ReallocateMethods.F90
  ${src_path}/Utility@ProductMethods.F90
  ${src_path}/Utility@FunctionalMethods.F90
  ${src_path}/Utility@OnesMethods.F90
  ${src_path}/Utility@EyeMethods.F90
  ${src_path}/Utility@DiagMethods.F90
  ${src_path}/Utility@AppendMethods.F90
  ${src_path}/Utility@InputMethods.F90
  ${src_path}/Utility@InverseMethods.F90
  ${src_path}/Utility@MatmulMethods.F90
  ${src_path}/Utility@ContractionMethods.F90
  ${src_path}/Utility@AssertMethods.F90
  ${src_path}/Utility@ApproxMethods.F90
  ${src_path}/Utility@HashingMethod.F90
  ${src_path}/Utility@MiscMethods.F90
  ${src_path}/Utility@StringMethods.F90
)
