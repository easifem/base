# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 4/03/2021 this file was created
#-----------------------------------------------------------------------

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/Rank2Tensor_Method@Constructor.F90
  ${src_path}/Rank2Tensor_Method@IO.F90
  ${src_path}/Rank2Tensor_Method@Contraction.F90
  ${src_path}/Rank2Tensor_Method@InvarMethods.F90
  ${src_path}/Rank2Tensor_Method@Operator.F90
  ${src_path}/Rank2Tensor_Method@Pullback.F90
  ${src_path}/Rank2Tensor_Method@PushForward.F90
)
