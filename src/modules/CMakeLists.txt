# This program is a part of EASIFEM library
# Copyright (C) 2020-2021  Vikas Sharma, Ph.D
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https: //www.gnu.org/licenses/>
#

#FFTW
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FFTW/CMakeLists.txt)

#PENF
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/PENF/CMakeLists.txt)

#BeFoR64
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/BeFoR64/CMakeLists.txt)

#String
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/String/CMakeLists.txt)

#FACE
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FACE/CMakeLists.txt)

#FPL
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FPL/CMakeLists.txt)

#System
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/System/CMakeLists.txt)

#BLAS95
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/BLAS95/CMakeLists.txt)

#Test
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Test/CMakeLists.txt)

#GlobalData
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/GlobalData/CMakeLists.txt)

#ARPACK
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/ARPACK/CMakeLists.txt)

#Hashing
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Hashing/CMakeLists.txt)

#Gnuplot
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Gnuplot/CMakeLists.txt)

#CInterface
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/CInterface/CMakeLists.txt)

#SuperLUInterface
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/SuperLUInterface/CMakeLists.txt)

#LISInterface
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/LISInterface/CMakeLists.txt)

#MetisInterface
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/MetisInterface/CMakeLists.txt)

#Display
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Display/CMakeLists.txt)

#MdEncode
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/MdEncode/CMakeLists.txt)

#ErrorHandling
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/ErrorHandling/CMakeLists.txt)


#Utility
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Utility/CMakeLists.txt)

#BaseInterpolation
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/BaseInterpolation/CMakeLists.txt)

#BaseContinuity
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/BaseContinuity/CMakeLists.txt)

#Polynomial
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Polynomial/CMakeLists.txt)

#BaseType
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/BaseType/CMakeLists.txt)

#MultiIndices
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/MultiIndices/CMakeLists.txt)

#OpenMP
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/OpenMP/CMakeLists.txt)

#Random
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Random/CMakeLists.txt)

#BoundingBox
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/BoundingBox/CMakeLists.txt)

#IntVector
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/IntVector/CMakeLists.txt)

#IndexValue
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/IndexValue/CMakeLists.txt)

#IndexValue
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/IterationData/CMakeLists.txt)

#KeyValue
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/KeyValue/CMakeLists.txt)

#Vector3D_
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Vector3D/CMakeLists.txt)

#Lapack
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Lapack/CMakeLists.txt)

#RealVector
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/RealVector/CMakeLists.txt)

#DOF
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/DOF/CMakeLists.txt)

#Geometry
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Geometry/CMakeLists.txt)

#QuadraturePoint
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/QuadraturePoint/CMakeLists.txt)

#FEVariable
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FEVariable/CMakeLists.txt)

#ElemshapeData
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/ElemshapeData/CMakeLists.txt)

#RealMatrix
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/RealMatrix/CMakeLists.txt)

#MassMatrix
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/MassMatrix/CMakeLists.txt)

#STMassMatrix
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/STMassMatrix/CMakeLists.txt)

#DiffusionMatrix
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/DiffusionMatrix/CMakeLists.txt)

#STDiffusionMatrix
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/STDiffusionMatrix/CMakeLists.txt)

#ConvectiveMatrix
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/ConvectiveMatrix/CMakeLists.txt)

#STConvectiveMatrix
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/STConvectiveMatrix/CMakeLists.txt)

#StiffnessMatrix
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/StiffnessMatrix/CMakeLists.txt)

#ElasticNitscheMatrix
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/ElasticNitscheMatrix/CMakeLists.txt)

#FacetMatrix
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FacetMatrix/CMakeLists.txt)

#FEMatrix
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FEMatrix/CMakeLists.txt)

#ForceVector
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/ForceVector/CMakeLists.txt)

#STForceVector
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/STForceVector/CMakeLists.txt)

#FEVector
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FEVector/CMakeLists.txt)

#VoigtRank2Tensor
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/VoigtRank2Tensor/CMakeLists.txt)

#Rank2Tensor
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Rank2Tensor/CMakeLists.txt)

#CSRSparsity
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/CSRSparsity/CMakeLists.txt)

#CSRMatrix
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/CSRMatrix/CMakeLists.txt)

#BaseMethod
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/BaseMethod/CMakeLists.txt)

#easifemBase
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/easifemBase/CMakeLists.txt)
