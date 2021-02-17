# This file is a part of easifem-base
# (c) 2021, all right reserved, Dr Vikas Sharma
#
# log
# 17/02/2021 this file was created
#-----------------------------------------------------------------------

#--------------------- H5Fortran ---------------------
add_subdirectory(H5Fortran)
#----------------------sparsekit----------------------
add_subdirectory(Sparsekit)
#-----------------------Szaghi------------------------
add_subdirectory(Szaghi)

# list(APPEND base_SOURCES
#   AbstractArray_Method.f90 AbstractMatrix_Method.f90 AbstractVector_Method.f90 BaseMethod.f90 BaseType.f90 BaseType@AdditionalMethods.f90 BLASInterface.f90 BoundingBox/BoundingBox_Method.f90 Buffer/Buffer_Method.f90 DispModule/disp_i1mod.f90 DispModule/disp_i2mod.f90 DispModule/disp_i4mod.f90 DispModule/disp_i8mod.f90 DispModule/disp_l1mod.f90 DispModule/disp_r16mod.f90 DispModule/dispmodule.f90 DispModule/dispmodule_util.f90 DOF/DOF_Method.f90 Tensor/Rank2Tensor/dsyevc3.f Tensor/Rank2Tensor/dsyevh3.f Tensor/Rank2Tensor/dsyevq3.f Tensor/Rank2Tensor/dsytrd3.f easifemBase.f90 ElemshapeData/ElemshapeData_Method.f90 ErrorHandling.f90 FEMatrix/FEMatrix_Module.f90 FEVariable/FEVariable_Method.f90 File/File_Method.f90 FoXy/foxy.f90 FoXy/foxy_xml_file.f90 FoXy/foxy_xml_tag.F90 Lapack/GE_Lapack_Method.f90 Geometry/Geometry.f90 GlobalData.f90 IndexValue/IndexValue_Method.f90 Vector/IntVector/IntVector_Method.f90 IO.f90 IterationData/IterationData_Method.f90 KeyValue/KeyValue_Method.f90 Lapack/Lapack_Method.f90 Tensor/Rank2Tensor/matrix_exponential.f90 functional-fortran/mod_functional.f90 functional-fortran/mod_interfaces.f90 Gnuplot/ogpf.f90 DispModule/putstrmodule.f90 QuadraturePoint/QuadraturePoint_Method.f90 Random/Random_Method.f90 Tensor/Rank2Tensor/Rank2Tensor_Method.f90 Matrix/RealMatrix/RealMatrix_Method.f90 Vector/RealVector/RealVector_Method.f90 ReferenceElement/ReferenceElement_Method.f90 SparseMatrix/SparseMatrix_Method.f90 Utility.f90 Tensor/VoigtRank2Tensor/VoigtRank2Tensor_Method.f90 VTKFortran/vtk_fortran.f90 VTKFortran/vtk_fortran_dataarray_encoder.f90 VTKFortran/vtk_fortran_parameters.f90 VTKFortran/vtk_fortran_pvtk_file.f90 VTKFortran/vtk_fortran_vtk_file.f90 VTKFortran/vtk_fortran_vtk_file_xml_writer_abstract.f90 VTKFortran/vtk_fortran_vtk_file_xml_writer_appended.f90 VTKFortran/vtk_fortran_vtk_file_xml_writer_ascii_local.f90 VTKFortran/vtk_fortran_vtk_file_xml_writer_binary_local.f90 VTKFortran/vtk_fortran_vtm_file.F90 )

# add_library(base_obj OBJECT "")
# target_sources(base_obj PRIVATE
#   ${base_SOURCES}
# )
# target_include_directories(
#   base_obj PRIVATE
#   ${EASIFEM_BASE_INCLUDE}
#   ${CMAKE_BINARY_DIR}
#   ${CMAKE_SOURCE_DIR}
# )
# target_compile_options(
#   base_obj
#   PRIVATE
#   ${FORTRAN_FLAGS}
#   "$<$<CONFIG:Debug>:${FORTRAN_FLAGS_DEBUG}>"
#   "$<$<CONFIG:Release>:${FORTRAN_FLAGS_RELEASE}>"
# )
# if(BUILD_SHARED_LIBS)
#   add_library(libeasifembase_module SHARED $<TARGET_OBJECTS:base_obj>)
# else()
#   add_library(libeasifembase_module STATIC $<TARGET_OBJECTS:base_obj>)
# endif()
# target_link_libraries(libeasifembase_module PUBLIC
#     libh5fortran libszaghi libsparsekit)

# install(TARGETS libeasifembase_module
#   DESTINATION ${EASIFEM_BASE_LIB})