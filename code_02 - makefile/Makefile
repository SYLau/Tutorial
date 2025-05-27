## These lines will first compile the f90 files into individual .o files (-c flag is used for compilation and generate .o files)
## and then link them to produce an a.out (-o flag is used to specify the output name. Optional)
main: module_example.f90 main.f90
	gfortran -c module_example.f90 main.f90
	gfortran module_example.o main.o -o a.exe

# These lines are used to clean everything produced by fortran
# To use it, type in the terminal "make clean" or "make -f Makefle clean"
clean:
	rm -r *.o
	rm -r *.mod
	rm -r *.out