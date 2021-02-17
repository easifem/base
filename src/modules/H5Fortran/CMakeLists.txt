list(APPEND H5Fortran_SOURCES
  interface.f90
  read.f90
  reader.f90
  reader_lt.f90
  string_utils.f90
  write.f90
  writer.f90
  writer_lt.f90
)
add_library(h5fortran_obj OBJECT "")
target_sources(h5fortran_obj PRIVATE
  ${H5Fortran_SOURCES}
)
target_include_directories(
  h5fortran_obj PRIVATE
  ${EASIFEM_BASE_INCLUDE}
  ${HDF5_INCLUDE_DIRS}
  ${CMAKE_BINARY_DIR}
  ${CMAKE_SOURCE_DIR}
)
target_compile_options(
  h5fortran_obj
  PRIVATE
  ${FORTRAN_FLAGS}
  "$<$<CONFIG:Debug>:${FORTRAN_FLAGS_DEBUG}>"
  "$<$<CONFIG:Release>:${FORTRAN_FLAGS_RELEASE}>"
)
if(BUILD_SHARED_LIBS)
  add_library(libh5fortran SHARED $<TARGET_OBJECTS:h5fortran_obj>)
  target_link_libraries(libh5fortran PUBLIC
  ${HDF5_Fortran_LIBRARIES}
  ${HDF5_Fortran_HL_LIBRARIES})
else()
  add_library(libh5fortran STATIC $<TARGET_OBJECTS:h5fortran_obj>)
endif()
message(${EASIFEM_BASE_LIB})
install(TARGETS libh5fortran
  DESTINATION ${EASIFEM_BASE_LIB})