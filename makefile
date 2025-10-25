noomp : 
	gfortran -c m_timer.f90 accel_fortran.f90
	gfortran -o3 accel_fortran.o m_timer.o -o accel_fortran_noomp.out
	rm *.o

omp :
	gfortran -c m_timer.f90 accel_fortran.f90 -fopenmp
	gfortran -o3 m_timer.o accel_fortran.o -o accel_fortran_omp.out -fopenmp
	rm *.o
