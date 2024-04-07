# This program is a part of EASIFEM library Expandable And Scalable
# Infrastructure for Finite Element Methods htttps://www.easifem.com Vikas
# Sharma, Ph.D., vickysharma0812@gmail.com
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https: //www.gnu.org/licenses/>
#

install(
  TARGETS ${PROJECT_NAME} easifemSystemMethodC
  EXPORT ${TARGETS_EXPORT_NAME}
  COMPONENT "${PROJECT_NAME}"
  ARCHIVE DESTINATION ${INSTALL_LIBDIR}
  RUNTIME DESTINATION ${INSTALL_BINDIR}
  LIBRARY DESTINATION ${INSTALL_LIBDIR})

install(
  DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
  DESTINATION "./"
  COMPONENT "${PROJECT_NAME}")

install(
  EXPORT ${TARGETS_EXPORT_NAME}
  FILE "${TARGETS_EXPORT_NAME}.cmake"
  NAMESPACE ${namespace}::
  DESTINATION ${INSTALL_CMAKEDIR}
  COMPONENT "${PROJECT_NAME}")

include(CMakePackageConfigHelpers)

write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY AnyNewerVersion)

configure_package_config_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Config.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
  INSTALL_DESTINATION ${INSTALL_CMAKEDIR}
  PATH_VARS INSTALL_INCLUDEDIR)

install(
  FILES "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
        "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
  DESTINATION ${INSTALL_CMAKEDIR}
  COMPONENT "${PROJECT_NAME}-dev")

# generate the export targets for the build tree EXPORT( EXPORT
# ${TARGETS_EXPORT_NAME} FILE
# "${CMAKE_CURRENT_BINARY_DIR}/cmake/${PROJECT_NAME}Targets.cmake" NAMESPACE
# ${namespace}:: )
