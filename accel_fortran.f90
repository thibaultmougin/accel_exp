program testit
use m_timer,only : timer, say_hello
implicit none


real :: a, b, c
real, allocatable :: array1(:,:,:),  array2(:,:,:),  array4(:,:,:)
real, allocatable :: array3(:)
integer,parameter           :: n = 500
type(timer)                 :: clock
integer                     :: i,j,k
character(len=*),parameter  :: all='(*(g0,1x))'

clock=timer()

print all, 'test 1'
print all, ''

allocate(array1(n,n,n))
allocate(array2(n,n,n))
allocate(array3(n*n*n))
allocate(array4(n,n,n))

array1 = 1.
array2 = 1.
array4 = 1.

a = 3.

call clock%tic()

!$OMP PARALLEL DO
do i = 1,n
    do j = 1,n
        do k = 1,n
            array4(i,j,k) = 2.5*array1(i,j,k) + array2(i,j,k)
        enddo
    enddo
enddo
call clock%toc()
print*, array4(2,2,2), array4(4,2,5), array4(20,2,1)


call clock%print()

print all, ''
print all, 'test 2'
print all, ''

array1 = 1.
array2 = 1.
array4 = 1.

call clock%tic()

array4 = 2.5*array1+array2

call clock%toc()
print*, array4(2,2,2), array4(4,2,5), array4(20,2,1)

call clock%print()


end program testit