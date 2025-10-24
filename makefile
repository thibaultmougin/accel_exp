noomp : 
	gfortran -c m_timer.f90 accel_fortran.f90
	gfortran -o3 accel_fortran.o m_timer.o -o accel_fortran_noomp.out
	rm *.o

omp :
	gfortran -c m_timer.f90 accel_fortran.f90
	gfortran -o3 -fopenmp accel_fortran.o m_timer.o -o accel_fortran_omp.out
	rm *.o
