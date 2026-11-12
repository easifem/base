# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 21/10/2026 this file was created
#-----------------------------------------------------------------------

SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/MaterialJacobian_Method@ConstructorMethods.F90
  ${src_path}/MaterialJacobian_Method@GetMethods.F90
  ${src_path}/MaterialJacobian_Method@VoigtFormMethods.F90
  ${src_path}/MaterialJacobian_Method@ContractionMethods.F90
  ${src_path}/MaterialJacobian_Method@Rank4Methods.F90
  ${src_path}/MaterialJacobian_Method@IOMethods.F90
)
