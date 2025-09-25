# Plugin system CMakeLists.txt

# Macro to create a plugin
macro(add_vizero_plugin plugin_name)
    # Create the plugin library
    add_library(${plugin_name} SHARED ${ARGN})
    
    # Set plugin properties
    set_target_properties(${plugin_name} PROPERTIES
        PREFIX ""
        SUFFIX ${PLUGIN_EXT}
        LIBRARY_OUTPUT_DIRECTORY ${PLUGIN_OUTPUT_DIRECTORY}
        RUNTIME_OUTPUT_DIRECTORY ${PLUGIN_OUTPUT_DIRECTORY}
        LIBRARY_OUTPUT_DIRECTORY_DEBUG ${PLUGIN_OUTPUT_DIRECTORY}
        LIBRARY_OUTPUT_DIRECTORY_RELEASE ${PLUGIN_OUTPUT_DIRECTORY}
        LIBRARY_OUTPUT_DIRECTORY_RELWITHDEBINFO ${PLUGIN_OUTPUT_DIRECTORY}
        LIBRARY_OUTPUT_DIRECTORY_MINSIZEREL ${PLUGIN_OUTPUT_DIRECTORY}
        RUNTIME_OUTPUT_DIRECTORY_DEBUG ${PLUGIN_OUTPUT_DIRECTORY}
        RUNTIME_OUTPUT_DIRECTORY_RELEASE ${PLUGIN_OUTPUT_DIRECTORY}
        RUNTIME_OUTPUT_DIRECTORY_RELWITHDEBINFO ${PLUGIN_OUTPUT_DIRECTORY}
        RUNTIME_OUTPUT_DIRECTORY_MINSIZEREL ${PLUGIN_OUTPUT_DIRECTORY}
    )
    
    # Link against vizero core
    target_link_libraries(${plugin_name} PRIVATE vizero_core)
    
    # Include directories
    target_include_directories(${plugin_name} PRIVATE
        ${CMAKE_SOURCE_DIR}/include
    )
    
    # Platform-specific settings
    if(WIN32)
        # Windows-specific plugin settings
        target_compile_definitions(${plugin_name} PRIVATE VIZERO_PLUGIN_EXPORT)
    else()
        # Unix-specific plugin settings
        set_target_properties(${plugin_name} PROPERTIES
            POSITION_INDEPENDENT_CODE ON
        )
    endif()
endmacro()

# Example plugin
add_vizero_plugin(example_plugin
    example/example_plugin.c
)

# C/C++/Assembly syntax highlighting plugin
add_vizero_plugin(syntax_c
    syntax_c/syntax_c.c
)

# Markdown syntax highlighting plugin
add_vizero_plugin(syntax_markdown
    syntax_markdown/syntax_markdown.c
)

# C# syntax highlighting plugin
add_vizero_plugin(syntax_csharp
    syntax_csharp/syntax_csharp.c
)

# XML syntax highlighting plugin
add_vizero_plugin(syntax_xml
    syntax_xml/syntax_xml.c
)

# Python syntax highlighting plugin
add_vizero_plugin(syntax_python
    syntax_python/syntax_python.c
)

# Common Lisp syntax highlighting plugin  
add_vizero_plugin(syntax_lisp
    syntax_lisp/syntax_lisp.c
)

# File browser plugin
add_vizero_plugin(file_browser
    file_browser/file_browser.c
)

# clangd LSP plugin
add_subdirectory(clangd)

