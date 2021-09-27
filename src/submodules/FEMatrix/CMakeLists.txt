# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 1/03/2021 this file was created
#-----------------------------------------------------------------------

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/FEMatrix_Method@DiffusionMatrix.F90
  ${src_path}/FEMatrix_Method@MassMatrix.F90
  ${src_path}/FEMatrix_Method@StiffnessMatrix.F90
  ${src_path}/FEMatrix_Method@NitscheMatrix.F90
)
