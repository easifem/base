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

# Get all propreties that cmake supports
execute_process(COMMAND cmake --help-property-list
                OUTPUT_VARIABLE CMAKE_PROPERTY_LIST)

# Convert command output into a CMake list
string(REGEX REPLACE ";" "\\\\;" CMAKE_PROPERTY_LIST "${CMAKE_PROPERTY_LIST}")
string(REGEX REPLACE "\n" ";" CMAKE_PROPERTY_LIST "${CMAKE_PROPERTY_LIST}")

function(print_properties)
  message("CMAKE_PROPERTY_LIST = ${CMAKE_PROPERTY_LIST}")
endfunction(print_properties)

function(print_target_properties tgt)

  if(NOT TARGET ${tgt})
    message("There is no target named '${tgt}'")
    return()
  endif()

  foreach(prop ${CMAKE_PROPERTY_LIST})

    string(REPLACE "<CONFIG>" "${CMAKE_BUILD_TYPE}" prop ${prop})
    # Fix
    # https://stackoverflow.com/questions/32197663/how-can-i-remove-the-the-location-property-may-not-be-read-from-target-error-i
    if(prop STREQUAL "LOCATION"
       OR prop MATCHES "^LOCATION_"
       OR prop MATCHES "_LOCATION$")
      continue()
    endif()
    # message ("Checking ${prop}")
    get_property(
      propval
      TARGET ${tgt}
      PROPERTY ${prop}
      SET)
    if(propval)
      get_target_property(propval ${tgt} ${prop})
      message("${tgt} ${prop} = ${propval}")
    endif()
  endforeach(prop)
endfunction(print_target_properties)

