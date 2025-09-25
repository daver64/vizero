cmake_minimum_required(VERSION 3.16)
project(vizero VERSION 1.0.0 LANGUAGES C CXX)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Export compile commands for IDEs
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Platform detection
if(WIN32)
    set(PLATFORM_NAME "windows")
    set(PLUGIN_EXT ".dll")
elseif(UNIX AND NOT APPLE)
    if(CMAKE_SYSTEM_NAME MATCHES "Linux")
        set(PLATFORM_NAME "linux")
    elseif(CMAKE_SYSTEM_NAME MATCHES "FreeBSD")
        set(PLATFORM_NAME "freebsd")
    else()
        set(PLATFORM_NAME "unix")
    endif()
    set(PLUGIN_EXT ".so")
else()
    message(FATAL_ERROR "Unsupported platform")
endif()

# Build type configuration
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug)
endif()

# Output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR})
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Plugin output directory
set(PLUGIN_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/plugins)

# Include directories
include_directories(${CMAKE_SOURCE_DIR}/include)

# Compiler flags
if(MSVC)
    add_compile_options(/W4)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS)
    add_definitions(-DNOMINMAX)
    add_definitions(-DWIN32_LEAN_AND_MEAN)
    add_definitions(-D_CRT_NONSTDC_NO_DEPRECATE)
    
    # Use static runtime library to match Boost
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
else()
    add_compile_options(-Wall -Wextra -pedantic)
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        add_compile_options(-g -O0)
    else()
        add_compile_options(-O3)
    endif()
endif()

# Platform-specific definitions
add_definitions(-DVIZERO_PLATFORM_${PLATFORM_NAME})
add_definitions(-DVIZERO_PLUGIN_EXT="${PLUGIN_EXT}")

# Find packages (only on Unix)
if(UNIX)
    find_package(PkgConfig)
endif()

# SDL2 Configuration
if(DEFINED ENV{SDL2_ROOT})
    set(SDL2_ROOT_DIR $ENV{SDL2_ROOT})
elseif(DEFINED ENV{SDL_ROOT})
    set(SDL2_ROOT_DIR $ENV{SDL_ROOT})
endif()

if(WIN32)
    # Windows SDL2 setup
    if(SDL2_ROOT_DIR)
        set(SDL2_INCLUDE_DIRS ${SDL2_ROOT_DIR}/include)
        if(CMAKE_SIZEOF_VOID_P EQUAL 8)
            set(SDL2_LIBRARIES 
                ${SDL2_ROOT_DIR}/lib/x64/SDL2.lib
                ${SDL2_ROOT_DIR}/lib/x64/SDL2main.lib
                ${SDL2_ROOT_DIR}/lib/x64/SDL2_image.lib
            )
            set(SDL2_DLLS
                ${SDL2_ROOT_DIR}/lib/x64/SDL2.dll
                ${SDL2_ROOT_DIR}/lib/x64/SDL2_image.dll
            )
        else()
            set(SDL2_LIBRARIES 
                ${SDL2_ROOT_DIR}/lib/x86/SDL2.lib
                ${SDL2_ROOT_DIR}/lib/x86/SDL2main.lib
                ${SDL2_ROOT_DIR}/lib/x86/SDL2_image.lib
            )
            set(SDL2_DLLS
                ${SDL2_ROOT_DIR}/lib/x86/SDL2.dll
                ${SDL2_ROOT_DIR}/lib/x86/SDL2_image.lib
            )
        endif()
    else()
        message(FATAL_ERROR "SDL2_ROOT or SDL_ROOT environment variable must be set on Windows")
    endif()
else()
    # Unix SDL2 setup
    if(PKG_CONFIG_FOUND)
        pkg_check_modules(SDL2 REQUIRED sdl2)
        pkg_check_modules(SDL2_IMAGE REQUIRED SDL2_image)
    else()
        find_package(SDL2 REQUIRED)
        find_package(SDL2_image REQUIRED)
        set(SDL2_IMAGE_LIBRARIES SDL2_image)
    endif()
endif()

# GLEW Configuration
if(DEFINED ENV{GLEW_ROOT})
    set(GLEW_ROOT_DIR $ENV{GLEW_ROOT})
endif()

if(WIN32 AND GLEW_ROOT_DIR)
    set(GLEW_INCLUDE_DIRS ${GLEW_ROOT_DIR}/include)
    if(CMAKE_SIZEOF_VOID_P EQUAL 8)
        set(GLEW_LIBRARIES ${GLEW_ROOT_DIR}/lib/Release/x64/glew32.lib)
        set(GLEW_DLLS ${GLEW_ROOT_DIR}/bin/Release/x64/glew32.dll)
    else()
        set(GLEW_LIBRARIES ${GLEW_ROOT_DIR}/lib/Release/Win32/glew32.lib)
        set(GLEW_DLLS ${GLEW_ROOT_DIR}/bin/Release/Win32/glew32.dll)
    endif()
else()
    find_package(GLEW REQUIRED)
endif()

# Boost Configuration
if(WIN32)
    # Windows Boost configuration
    if(DEFINED ENV{BOOST_ROOT})
        set(Boost_ROOT $ENV{BOOST_ROOT})
    endif()

    if(DEFINED ENV{BOOST_LIBRARY_DIR})
        set(Boost_LIBRARY_DIR $ENV{BOOST_LIBRARY_DIR})
    else()
        # Default to stage/lib for typical Boost builds
        if(Boost_ROOT)
            set(Boost_LIBRARY_DIR ${Boost_ROOT}/stage/lib)
        endif()
    endif()

    # Set Boost configuration
    set(Boost_USE_STATIC_LIBS ON)
    set(Boost_USE_MULTITHREADED ON)
    set(Boost_USE_STATIC_RUNTIME OFF)
    set(Boost_NO_SYSTEM_PATHS ON)

    # Debug output
    message(STATUS "Boost_ROOT: ${Boost_ROOT}")
    message(STATUS "Boost_LIBRARY_DIR: ${Boost_LIBRARY_DIR}")

    # Try to find Boost headers first
    find_path(Boost_INCLUDE_DIRS boost/version.hpp PATHS ${Boost_ROOT} ${Boost_ROOT}/boost NO_DEFAULT_PATH)

    if(Boost_INCLUDE_DIRS)
        message(STATUS "Found Boost headers at: ${Boost_INCLUDE_DIRS}")
        
        # Manually find the libraries with the correct names
        find_library(Boost_SYSTEM_LIBRARY 
            NAMES libboost_system-vc143-mt-s-x64-1_87
            PATHS ${Boost_LIBRARY_DIR}
            NO_DEFAULT_PATH
        )
        
        find_library(Boost_FILESYSTEM_LIBRARY 
            NAMES libboost_filesystem-vc143-mt-s-x64-1_87
            PATHS ${Boost_LIBRARY_DIR}
            NO_DEFAULT_PATH
        )
        
        if(Boost_SYSTEM_LIBRARY AND Boost_FILESYSTEM_LIBRARY)
            message(STATUS "Found Boost system library: ${Boost_SYSTEM_LIBRARY}")
            message(STATUS "Found Boost filesystem library: ${Boost_FILESYSTEM_LIBRARY}")
            
            # Create imported targets
            add_library(Boost::system STATIC IMPORTED)
            set_target_properties(Boost::system PROPERTIES
                IMPORTED_LOCATION ${Boost_SYSTEM_LIBRARY}
                INTERFACE_INCLUDE_DIRECTORIES ${Boost_INCLUDE_DIRS}
            )
            
            add_library(Boost::filesystem STATIC IMPORTED)
            set_target_properties(Boost::filesystem PROPERTIES
                IMPORTED_LOCATION ${Boost_FILESYSTEM_LIBRARY}
                INTERFACE_INCLUDE_DIRECTORIES ${Boost_INCLUDE_DIRS}
                INTERFACE_LINK_LIBRARIES Boost::system
            )
            
            # Set variables for compatibility
            set(Boost_FOUND TRUE)
            set(Boost_LIBRARIES ${Boost_SYSTEM_LIBRARY} ${Boost_FILESYSTEM_LIBRARY})
            set(Boost_VERSION "1.87.0")
            
        else()
            message(FATAL_ERROR "Could not find Boost libraries. Found system: ${Boost_SYSTEM_LIBRARY}, filesystem: ${Boost_FILESYSTEM_LIBRARY}")
        endif()
    else()
        message(FATAL_ERROR "Could not find Boost headers")
    endif()
else()
    # Unix Boost configuration - use standard find_package
    find_package(Boost REQUIRED COMPONENTS system filesystem)
    if(Boost_FOUND)
        message(STATUS "Found Boost version: ${Boost_VERSION}")
        message(STATUS "Boost include directories: ${Boost_INCLUDE_DIRS}")
        message(STATUS "Boost libraries: ${Boost_LIBRARIES}")
    endif()
endif()

# OpenGL
find_package(OpenGL REQUIRED)

# Platform-specific libraries
if(WIN32)
    set(PLATFORM_LIBS)
else()
    set(PLATFORM_LIBS dl pthread)
endif()

# Create vizero core library
add_library(vizero_core STATIC
    src/core/application.cpp
    src/core/filewatch_poll.c
    src/core/window.cpp
    src/core/renderer.cpp
    src/core/input_manager.cpp
    src/core/settings.cpp
    src/core/search.cpp
    src/core/colour_theme.cpp
    src/core/session.cpp
    src/text/buffer.cpp
    src/text/cursor.cpp
    src/text/line.cpp
    src/editor/editor_state.cpp
    src/editor/command_parser.cpp
    src/editor/mode_manager.cpp
    src/project/project.cpp
    src/plugin/plugin_manager.cpp
    src/plugin/plugin_interface.cpp
    src/plugin/plugin_registry.cpp
    src/ui/status_bar.cpp
    src/ui/status_bar_render.cpp
    src/ui/editor_window.cpp
    src/ui/editor_window_move_visual_row.c
    src/utils/file_utils.cpp
    src/utils/string_utils.cpp
    src/lsp/lsp_client.cpp
)

target_include_directories(vizero_core PUBLIC
    ${CMAKE_SOURCE_DIR}/include
    ${SDL2_INCLUDE_DIRS}
    ${SDL2_IMAGE_INCLUDE_DIRS}
    ${GLEW_INCLUDE_DIRS}
    ${Boost_INCLUDE_DIRS}
    ${OPENGL_INCLUDE_DIRS}
)

target_link_libraries(vizero_core PUBLIC
    ${SDL2_LIBRARIES}
    ${SDL2_IMAGE_LIBRARIES}
    ${GLEW_LIBRARIES}
    Boost::system
    Boost::filesystem
    ${OPENGL_LIBRARIES}
    ${PLATFORM_LIBS}
)

# Enable position-independent code for static library (required for plugins on Unix)
set_target_properties(vizero_core PROPERTIES POSITION_INDEPENDENT_CODE ON)

# Main executable (console version)
if(WIN32)
    add_executable(vizero
        src/main.cpp
        vizero.rc
    )
else()
    add_executable(vizero
        src/main.cpp
    )
endif()

target_link_libraries(vizero PRIVATE vizero_core)

# Set executable output directory to project root
set_target_properties(vizero PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}
    RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_SOURCE_DIR}
    RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_SOURCE_DIR}
    RUNTIME_OUTPUT_DIRECTORY_RELWITHDEBINFO ${CMAKE_SOURCE_DIR}
    RUNTIME_OUTPUT_DIRECTORY_MINSIZEREL ${CMAKE_SOURCE_DIR}
)

# GUI executable (Windows only - no console window)
if(WIN32)
    add_executable(vizero-gui WIN32
        src/main.cpp
        vizero.rc
    )
    
    target_link_libraries(vizero-gui PRIVATE vizero_core)
    
    # Set executable output directory to project root
    set_target_properties(vizero-gui PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}
        RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_SOURCE_DIR}
        RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_SOURCE_DIR}
        RUNTIME_OUTPUT_DIRECTORY_RELWITHDEBINFO ${CMAKE_SOURCE_DIR}
        RUNTIME_OUTPUT_DIRECTORY_MINSIZEREL ${CMAKE_SOURCE_DIR}
    )
endif()

# Copy DLLs on Windows (for both console and GUI versions)
if(WIN32)
    if(SDL2_DLLS)
        foreach(dll ${SDL2_DLLS})
            add_custom_command(TARGET vizero POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                ${dll} ${CMAKE_SOURCE_DIR}
            )
            add_custom_command(TARGET vizero-gui POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                ${dll} ${CMAKE_SOURCE_DIR}
            )
        endforeach()
    endif()
    
    if(GLEW_DLLS)
        foreach(dll ${GLEW_DLLS})
            add_custom_command(TARGET vizero POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                ${dll} ${CMAKE_SOURCE_DIR}
            )
            add_custom_command(TARGET vizero-gui POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                ${dll} ${CMAKE_SOURCE_DIR}
            )
        endforeach()
    endif()
endif()

# Plugin system
add_subdirectory(plugins)

# Installation
if(WIN32)
    install(TARGETS vizero vizero-gui DESTINATION bin)
else()
    install(TARGETS vizero DESTINATION bin)
endif()
install(DIRECTORY ${CMAKE_SOURCE_DIR}/plugins/ DESTINATION plugins
        FILES_MATCHING PATTERN "*${PLUGIN_EXT}")

# CPack configuration for packaging
set(CPACK_PACKAGE_NAME "Vizero")
set(CPACK_PACKAGE_VENDOR "Vizero Team")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "A vi clone built with SDL2")
set(CPACK_PACKAGE_VERSION ${PROJECT_VERSION})
set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})

if(WIN32)
    set(CPACK_GENERATOR "ZIP;NSIS")
else()
    set(CPACK_GENERATOR "TGZ;DEB")
endif()

include(CPack)

# Tests
option(BUILD_TESTS "Build tests" ON)

if(BUILD_TESTS)
    enable_testing()
    
    # Test executable
    add_executable(vizero_tests
        tests/test_framework.cpp
        tests/test_editor.cpp

    )
    
    target_include_directories(vizero_tests PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/include
        ${CMAKE_CURRENT_SOURCE_DIR}/tests
        ${SDL2_INCLUDE_DIRS}
    )
    
    target_link_libraries(vizero_tests PRIVATE
        vizero_core
    )
    
    # Set output directory for tests
    set_target_properties(vizero_tests PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}
        RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_BINARY_DIR}
        RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}
    )
    
    # Add test to CTest
    add_test(NAME editor_tests COMMAND vizero_tests)
endif()