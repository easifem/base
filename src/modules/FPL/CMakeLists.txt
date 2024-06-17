#################################################################
# Search F90 files recursively in all subdirs
#################################################################

SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
FILE(GLOB_RECURSE WRAPPER_LIB_SRC  ${src_path}/Wrapper/*.F90)
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${WRAPPER_LIB_SRC}
  ${src_path}/ErrorMessages.F90
  ${src_path}/FPL_utils.F90
  ${src_path}/FPL.F90
  ${src_path}/ParameterEntry.F90
  ${src_path}/ParameterEntryDictionary.F90
  ${src_path}/ParameterList.F90
  ${src_path}/ParameterRootEntry.F90
  )