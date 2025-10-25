program testit
use m_timer,only : timer, say_hello
use OMP_LIB
implicit none

real, allocatable :: array1(:,:,:),  array2(:,:,:),  array4(:,:,:)
real, allocatable :: array3(:)
integer,parameter           :: nx = 20
integer,parameter           :: ny = 20
integer,parameter           :: nz = 100
type(timer)                 :: clock
integer                     :: i,j,k
character(len=*),parameter  :: all='(*(g0,1x))'

clock=timer()

print all, 'Initialisation à 0 via boucle sur i,j,k'
print all, ''

allocate(array1(nx,ny,nz))
allocate(array2(nx,ny,nz))
allocate(array3(nx*ny*nz))
allocate(array4(nx,ny,nz))

array1 = 1.
array2 = 1.
array4 = 1.

call clock%tic()

!$OMP PARALLEL DO
do i = 1,nx
    do j = 1,ny
        do k = 1,nz
            array4(i,j,k) = 0.
        enddo
    enddo
enddo

call clock%toc()

call clock%print()

print all, ''
print all, 'Initialisation à 0 via A = 0.'
print all, ''

array1 = 1.
array2 = 1.
array4 = 1.

call clock%tic()

!$OMP PARALLEL
array4 = 0.
!$OMP END PARALLEL

call clock%toc()

call clock%print()

print all, ''
print all, 'A = 2*B + C via boucle sur i,j,k'
print all, ''

array1 = 1.
array2 = 1.
array4 = 1.

call clock%tic()

!$OMP PARALLEL DO
do i = 1,nx
    do j = 1,ny
        do k = 1,nz
            array4(i,j,k) = 2*array2(i,j,k) + array1(i,j,k)
        enddo
    enddo
enddo

call clock%toc()

call clock%print()


print all, ''
print all, 'A = 2*B + C'
print all, ''

array1 = 1.
array2 = 1.
array4 = 1.

call clock%tic()

!$OMP PARALLEL
array4 = 2*array2 + array1
!$OMP END PARALLEL

call clock%toc()

call clock%print()


end program testit