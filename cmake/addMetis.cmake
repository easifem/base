# METIS LIBRARY
IF( ${PROJECT_NAME} MATCHES "easifemBase" )
  OPTION(USE_METIS ON)
  IF(USE_METIS)
    FIND_LIBRARY(METIS_LIB metis)
    LIST( APPEND TARGET_COMPILE_DEF "-DUSE_METIS" )
    MESSAGE(STATUS "METIS_LIB = ${METIS_LIB}")
  ENDIF()
  TARGET_LINK_LIBRARIES( ${PROJECT_NAME} PUBLIC ${METIS_LIB} )
ENDIF()