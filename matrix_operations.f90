module arrayModule

contains

function matrixVectorProd(matrix, vec, numRows, numCols)
implicit none
integer::numRows,numCols
real::matrix(numRows,numCols), vec(numCols)
real::matrixVectorProd(numRows)
real, allocatable::matAux(:,:)
real::matIdAux, tempVal

real::tempSum
integer::i,j

    do i=1, numRows
        tempSum = 0
        do j=1, numCols
            tempSum = tempSum + matrix(i,j)*vec(j)
        end do
        matrixVectorProd(i) = tempSum 
    end do 

print*, "matrixVector"
call displayMatrixContent(matrixVectorProd, numRows, 1)
end function

end module 

program withModule
use arrayModule
implicit none

integer, parameter :: numElements=5
real :: vecA(numElements), matA(numElements,numElements), matPerm(numElements, numElements), matPermT(numElements, numElements), matProd(numElements, numElements)
real, allocatable::vecD(:)
real::sumElements, euclideanNorm
integer::numRows, numCols
real, allocatable::matId(:,:)
real::matIdAux, temp, matrixTimesVector

integer::i, j, k, scalarInt
real::scalar=2.0, sumVal, tempSum, vecInit

! Initialization
vecA=(/3.1,4.2,5.3,1.4,2.5/);

! Compute the scalar product of vecA by 2.0
allocate(vecD(numElements))
do i=1, numElements
    vecD(i) = scalar*vecA(i)
end do

print*, "Product of vecA by 2.0:"
call displayVectorContent(vecD, numElements)
print*, " "

! Compute the sum of the elements of vecA
sumVal = sumElements(vecA, numElements)
print*, "Sum of elements of vecA =", sumVal
print*, " "

! Compute the euclidean norm of vecA
sumVal = euclideanNorm(vecA, numElements)
print*, "Euclidean norm of vecA =", sumVal

! Identity matrix
numRows = 5
numCols = 5
scalarInt = 2
allocate(matId(numRows,numCols))
matId = 0.0
do i = 1, numRows
    do j = 1, numCols
        if( i == j )then
            matId(i,j) = 1.0
        end if
    end do
end do

print*, "Identity matrix matId"
call displayMatrixContent(matId, numRows, numCols)

! Compute the product of matId by 2.0
temp = matIdAux(matId, numRows, numCols, scalarInt)
print*, " "

! Compute the product of matId and vecA
do i=1, numElements
    tempSum = 0
    do j=1, numElements
        tempSum = tempSum + matId(i,j)*vecA(j)
    end do
    vecD(i) = tempSum 
end do

print*, "Product of matId and vecA:"
call displayMatrixContent(vecD, numRows, 1)
print*, " "

! Further matrix operations (perm, transpose, etc.) go here ...

end program withModule

subroutine displayVectorContent(vec, n)
implicit none
integer:: n
real :: vec(n)
integer::i
    do i=1, n
        print*, vec(i)
    end do
end subroutine

real function sumElements(vec, n)
implicit none
integer:: n
real :: vec(n)
real::sumVal
integer::i

sumElements = 0
    do i=1, n
        sumElements = sumElements+vec(i)
    end do
end function

real function euclideanNorm(vec, n)
implicit none
integer:: n
real :: vec(n)
real::norm
integer::i

euclideanNorm = 0
    do i=1, n
        euclideanNorm = euclideanNorm+vec(i)**2
    end do
    euclideanNorm = sqrt(euclideanNorm)
end function

subroutine displayMatrixContent(mat, numRows, numCols)
integer ::numRows, numCols
real :: mat(numRows, numCols)
integer:: i, j

    DO i=1, numRows
        DO j=1, numCols
            write(*, '(f5.2)', advance='NO' ) mat(i, j)			
        END DO
        print*, ' '	
    END DO
end subroutine

real function matIdAux(mat, numRows, numCols, scalarInt)
implicit none
integer::numRows, numCols, scalarInt
real::mat(numRows,numCols)
real::outputMat(numRows,numCols)
integer::i, j

outputMat = 0.0
do i = 1, numRows
    do j = 1, numCols
        outputMat(i,j) = mat(i,j)*scalarInt
    enddo
enddo

call displayMatrixContent(outputMat, numRows, numCols)
end function
