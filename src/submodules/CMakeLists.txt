# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 17/02/2021 this file was created
#-----------------------------------------------------------------------

#OpenMP put it at top
include(${CMAKE_CURRENT_LIST_DIR}/Utility/CMakeLists.txt)

#OpenMP put it at top
include(${CMAKE_CURRENT_LIST_DIR}/OpenMP/CMakeLists.txt)

#Random
include(${CMAKE_CURRENT_LIST_DIR}/Random/CMakeLists.txt)

#File
include(${CMAKE_CURRENT_LIST_DIR}/File/CMakeLists.txt)

#BoundingBox
include(${CMAKE_CURRENT_LIST_DIR}/BoundingBox/CMakeLists.txt)

#IntVector
include(${CMAKE_CURRENT_LIST_DIR}/IntVector/CMakeLists.txt)

#IndexValue
include(${CMAKE_CURRENT_LIST_DIR}/IndexValue/CMakeLists.txt)

#IterationData
include(${CMAKE_CURRENT_LIST_DIR}/IterationData/CMakeLists.txt)

#KeyValue
include(${CMAKE_CURRENT_LIST_DIR}/KeyValue/CMakeLists.txt)

#Vector3D
include(${CMAKE_CURRENT_LIST_DIR}/Vector3D/CMakeLists.txt)

#BLAS
include(${CMAKE_CURRENT_LIST_DIR}/BLAS/CMakeLists.txt)

#RealVector
include(${CMAKE_CURRENT_LIST_DIR}/RealVector/CMakeLists.txt)

#DOF
include(${CMAKE_CURRENT_LIST_DIR}/DOF/CMakeLists.txt)

#ReferenceElement
include(${CMAKE_CURRENT_LIST_DIR}/ReferenceElement/CMakeLists.txt)

#QuadraturePoint
include(${CMAKE_CURRENT_LIST_DIR}/QuadraturePoint/CMakeLists.txt)

#FEVariable
include(${CMAKE_CURRENT_LIST_DIR}/FEVariable/CMakeLists.txt)

#ElemShapeData
include(${CMAKE_CURRENT_LIST_DIR}/ElemShapeData/CMakeLists.txt)

#FEMatrix
include(${CMAKE_CURRENT_LIST_DIR}/FEMatrix/CMakeLists.txt)

# RealMatrix
include(${CMAKE_CURRENT_LIST_DIR}/RealMatrix/CMakeLists.txt)

# VoigtRank2Tensor
include(${CMAKE_CURRENT_LIST_DIR}/VoigtRank2Tensor/CMakeLists.txt)

# Rank2Tensor
include(${CMAKE_CURRENT_LIST_DIR}/Rank2Tensor/CMakeLists.txt)

# SparseMatrix
include(${CMAKE_CURRENT_LIST_DIR}/SparseMatrix/CMakeLists.txt)

# #BaseMethod
# include(${CMAKE_CURRENT_LIST_DIR}/BaseMethod/CMakeLists.txt)

# #easifemBase
# include(${CMAKE_CURRENT_LIST_DIR}/easifemBase/CMakeLists.txt)
