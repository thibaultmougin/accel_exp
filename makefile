o2 : 
	gfortran -c m_timer.f90 accel_fortran.f90 cumul.f90 egamat.f90 nulmat.f90 
	gfortran -O2 accel_fortran.o m_timer.o cumul.o egamat.o nulmat.o -o accel_fortran.out
	rm *.o
o3 : 
	gfortran -c m_timer.f90 accel_fortran.f90 cumul.f90 egamat.f90 nulmat.f90
	gfortran -O3 accel_fortran.o m_timer.o cumul.o egamat.o nulmat.o -o accel_fortran.out
	rm *.o

o3_omp :
	gfortran -c m_timer.f90 accel_fortran.f90 cumul.f90 egamat.f90 nulmat.f90
	gfortran -O3 accel_fortran.o m_timer.o cumul.o egamat.o nulmat.o -o accel_fortran.out -fopenmp
	rm *.o

o5 :
	gfortran -c m_timer.f90 accel_fortran.f90 cumul.f90 egamat.f90 nulmat.f90
	gfortran -O5 accel_fortran.o m_timer.o cumul.o egamat.o nulmat.o -o accel_fortran.out
	rm *.o

o5_omp :
	gfortran -c m_timer.f90 accel_fortran.f90 cumul.f90 egamat.f90 nulmat.f90
	gfortran -O5 accel_fortran.o m_timer.o cumul.o egamat.o nulmat.o -o accel_fortran.out -fopenmp
	rm *.o

fast :
	gfortran -c m_timer.f90 accel_fortran.f90 cumul.f90 egamat.f90 nulmat.f90
	gfortran -Ofast accel_fortran.o m_timer.o cumul.o egamat.o nulmat.o -o accel_fortran.out 
	rm *.o

fast_o5 :
	gfortran -c m_timer.f90 accel_fortran.f90 cumul.f90 egamat.f90 nulmat.f90
	gfortran -Ofast -O5 accel_fortran.o m_timer.o cumul.o egamat.o nulmat.o -o accel_fortran.out 
	rm *.o
