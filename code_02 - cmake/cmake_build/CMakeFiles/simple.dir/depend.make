# CMAKE generated file: DO NOT EDIT!
# Generated by "MinGW Makefiles" Generator, CMake Version 3.27

# Note that incremental build could trigger a call to cmake_copy_f90_mod on each re-build
CMakeFiles/simple.dir/main.f90.obj: CMakeFiles/simple.dir/module_example_mod.mod.stamp
CMakeFiles/simple.dir/module_example.f90.obj.provides.build: CMakeFiles/simple.dir/module_example_mod.mod.stamp
CMakeFiles/simple.dir/module_example_mod.mod.stamp: CMakeFiles/simple.dir/module_example.f90.obj
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod module_example_mod.mod CMakeFiles\simple.dir\module_example_mod.mod.stamp GNU
CMakeFiles/simple.dir/module_example.f90.obj.provides.build:
	$(CMAKE_COMMAND) -E touch CMakeFiles/simple.dir/module_example.f90.obj.provides.build
CMakeFiles/simple.dir/build: CMakeFiles/simple.dir/module_example.f90.obj.provides.build