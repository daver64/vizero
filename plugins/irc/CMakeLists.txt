# IRC Client Plugin CMakeLists.txt

# Find SDL2_ttf
find_package(PkgConfig QUIET)
if(PkgConfig_FOUND)
    pkg_check_modules(SDL2_TTF QUIET SDL2_ttf)
endif()

if(NOT SDL2_TTF_FOUND)
    # Try to find SDL2_ttf manually
    find_path(SDL2_TTF_INCLUDE_DIR SDL_ttf.h
        HINTS
        ${SDL2_ROOT}/include
        $ENV{SDL2_ROOT}/include
        $ENV{SDL2_ROOT}/include/SDL2
        /usr/include/SDL2
        /usr/local/include/SDL2
    )
    
    find_library(SDL2_TTF_LIBRARY
        NAMES SDL2_ttf
        HINTS
        ${SDL2_ROOT}/lib
        $ENV{SDL2_ROOT}/lib
        /usr/lib
        /usr/local/lib
    )
    
    if(SDL2_TTF_INCLUDE_DIR AND SDL2_TTF_LIBRARY)
        set(SDL2_TTF_FOUND TRUE)
        set(SDL2_TTF_INCLUDE_DIRS ${SDL2_TTF_INCLUDE_DIR})
        set(SDL2_TTF_LIBRARIES ${SDL2_TTF_LIBRARY})
    else()
        message(WARNING "SDL2_ttf not found, IRC plugin may not render text properly")
    endif()
endif()

# Create IRC plugin
add_vizero_plugin(irc
    irc_plugin.c
)

# Link SDL2_ttf if found
if(SDL2_TTF_FOUND)
    target_include_directories(irc PRIVATE ${SDL2_TTF_INCLUDE_DIRS})
    target_link_libraries(irc PRIVATE ${SDL2_TTF_LIBRARIES})
    target_compile_definitions(irc PRIVATE HAVE_SDL2_TTF)
    message(STATUS "IRC plugin: Using SDL2_ttf")
else()
    message(STATUS "IRC plugin: Building without SDL2_ttf")
endif()