cmake_minimum_required(VERSION 3.14)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_INTERPROCEDURAL_OPTIMIZATION ON)
set(CMAKE_CXX_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN ON)

#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-omit-frame-pointer -fsanitize=address")
#set(CMAKE_LINKER_FLAGS "${CMAKE_LINKER_FLAGS} -fno-omit-frame-pointer -fsanitize=address")

add_library(ambercpp SHARED llvmasm.cpp)
target_compile_definitions(ambercpp PRIVATE AMBERCPP_EXPORTS=1)

find_package(LLVM REQUIRED CONFIG)
llvm_map_components_to_libnames(LLVM_LIBS
  amdgpuasmparser amdgpudesc amdgpuutils amdgpuinfo
  core remarks support mc mcparser)
target_link_libraries(ambercpp PRIVATE ${LLVM_LIBS})
target_link_options(ambercpp PRIVATE "-Wl,--exclude-libs,ALL")
