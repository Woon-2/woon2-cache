cmake_minimum_required(VERSION 3.18)

if (NOT DEFINED CMAKE_CONFIGURATION_TYPES)
    set(DEFAULT_CONFIGURATION_TYPES "Debug;Release;RelWithDebInfo;MinSizeRel")

    message(STATUS "Setting configuration types to ${DEFAULT_CONFIGURATION_TYPES}
        as none was specified."
    )
    set(CMAKE_CONFIGURATION_TYPES ${DEFAULT_CONFIGURATION_TYPES} CACHE STRING
        "Write the possible configuration types."
    )
endif()

if(NOT DEFINED CMAKE_BUILD_TYPE)
    set(DEFAULT_BUILD_TYPE "Release")
    
    message(STATUS "Settting build type to ${DEFAULT_BUILD_TYPE}
        as none was specified."
    )
    set(CMAKE_BUILD_TYPE "${DEFAULT_BUILD_TYPE}" CACHE STRING
        "Choose the type of build."
    )
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
        "${CMAKE_CONFIGURATION_TYPES}"
    )
endif()

if (NOT DEFINED CMAKE_INSTALL_PREFIX)
    set(DEFAULT_INSTALL_PATH "${CMAKE_SOURCE_DIR}/install")

    message(STATUS "Setting install path to ${DEFAULT_INSTALL_PATH}
        as none was specified."
    )
    set (CMAKE_INSTALL_PREFIX "${DEFAULT_INSTALL_PATH}" CACHE PATH
        "Set the install path."
    )
endif()

project(project_woon2_cache
    LANGUAGES CXX
    VERSION 2023.11.23
    DESCRIPTION "a lightweight header-only LRU cache"
)

list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")

add_library(woon2cache INTERFACE LRUCache.hpp)

target_compile_features(woon2cache INTERFACE cxx_std_20)

target_include_directories(woon2cache INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}")

add_library(woon2cache::LRUCache ALIAS woon2cache)

if(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    add_subdirectory(test)
endif()