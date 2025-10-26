program testit
use m_timer,only : timer
use OMP_LIB
implicit none

real, allocatable :: array1(:,:,:),  array2(:,:,:),  array4(:,:,:)
real, allocatable :: array3(:,:), array5(:,:), array6(:,:)
real, allocatable :: array1d1(:), array1d2(:), array1d3(:)
integer,parameter           :: nx = 20
integer,parameter           :: ny = 20
integer,parameter           :: nz = 100
type(timer)                 :: clock
integer                     :: i,j,k
character(len=*),parameter  :: all='(*(g0,1x))'

clock=timer()

allocate(array1(nx,ny,nz))
allocate(array2(nx,ny,nz))
allocate(array4(nx,ny,nz))
allocate(array3(nx,ny))
allocate(array5(nx,ny))
allocate(array6(nx,ny))
allocate(array1d1(nx*ny*nz))
allocate(array1d2(nx*ny*nz))
allocate(array1d3(nx*ny*nz))

print all, 'Initialisation à 0 via boucle sur i,j,k'
print all, ''

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

print all, ''
print all, 'En 1D : A = 2*B + C via fonction cumul'
print all, ''

array1d1 = 1.
array1d2 = 1.
array1d3 = 1.

call clock%tic()

call cumul(array1d1, 2., array1d2, array1d3, nx*ny*nz)

call clock%toc()

call clock%print()

print all, ''
print all, 'En 1D : A = 2*B + C via boucle'
print all, ''

array1d1 = 1.
array1d2 = 1.
array1d3 = 1.

call clock%tic()

!$OMP PARALLEL DO
do i = 1,nx*ny*nz
    array1d1(i) = 2*array1d2(i) + array1d3(i)
enddo

call clock%toc()

call clock%print()

print all, ''
print all, 'En 1D : A = 2*B + C'
print all, ''

array1d1 = 1.
array1d2 = 1.
array1d3 = 1.

call clock%tic()

!$OMP PARALLEL
array1d1 = 2*array1d2 + array1d3(i)
!$OMP END PARALLEL

call clock%toc()

call clock%print()

print all, ''
print all, 'Produit de matrices via boucle'
print all, ''

array3 = 2.
array5 = 3.

call clock%tic()

!$OMP PARALLEL DO
do i = 1, nx   
    do j = 1, ny
        array6(i, j) = sum(array3(i,:)*array5(:,j))
    enddo
enddo

call clock%toc()

call clock%print()


print all, ''
print all, 'Produit de matrices via matmul'
print all, ''

array3 = 2.
array5 = 3.
array6 = 0.

call clock%tic()

!$OMP PARALLEL
array6 = matmul(array3, array5)
!$OMP END PARALLEL

call clock%toc()

call clock%print()


end program testit