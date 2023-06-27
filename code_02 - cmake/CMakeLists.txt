cmake_minimum_required(VERSION 3.5)
project(simple)
enable_language(Fortran)

add_executable(simple main.f90 module_example.f90)