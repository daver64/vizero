# GitHub LLM Plugin CMakeLists.txt

# Only build if CURL is available
if(CURL_FOUND)
    # Create the plugin
    add_library(github_llm SHARED github_llm_plugin.c)
    
    # Set plugin properties
    set_target_properties(github_llm PROPERTIES
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
    
    # Include directories
    target_include_directories(github_llm PRIVATE
        ${CMAKE_SOURCE_DIR}/include
        ${CURL_INCLUDE_DIRS}
    )
    
    # Link libraries
    target_link_libraries(github_llm
        vizero_core
        ${CURL_LIBRARIES}
    )
    
    # Platform-specific settings
    if(WIN32)
        # Copy CURL DLL to plugin directory for runtime
        if(CURL_DLL)
            add_custom_command(TARGET github_llm POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    "${CURL_DLL}"
                    "${PLUGIN_OUTPUT_DIRECTORY}/"
                COMMENT "Copying CURL DLL to plugin directory"
            )
        endif()
        
        # Add Windows-specific definitions
        target_compile_definitions(github_llm PRIVATE
            VIZERO_PLUGIN_EXPORT
            CURL_STATICLIB=1
        )
        
        # Link Windows libraries that curl might need
        target_link_libraries(github_llm
            ws2_32
            wldap32
            crypt32
            normaliz
        )
    else()
        # Unix-specific settings
        target_compile_definitions(github_llm PRIVATE
            VIZERO_PLUGIN_EXPORT
        )
    endif()
    
    # Compiler-specific settings
    if(MSVC)
        target_compile_options(github_llm PRIVATE /W4)
    else()
        target_compile_options(github_llm PRIVATE -Wall -Wextra -pedantic)
    endif()
    
    message(STATUS "GitHub LLM plugin: Enabled with CURL support")
    
else()
    message(WARNING "GitHub LLM plugin: Disabled - CURL not found")
endif()