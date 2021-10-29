module arrayMod

contains

function matVet(mAT, vet, nL, nC)	
implicit none
integer::nL,nC
real::mAT(nL,nC), vet(nC)
real::matVet(nL)

real, allocatable::mA2(:,:)
real::mId2, temp

real::tmp
integer::i,j

		do i=1, nL
			tmp = 0
			do j=1, nC
				tmp = tmp + mAT(i,j)*vet(j)
			end do
			matVet(i) = tmp 
		end do 

print*, "matvet"
call mostrarConteudoM(matVet, nL, 1)
end function	

end module 

program comModule
use arrayMod
implicit none
integer, parameter :: numElem=5
real :: vA(numElem), mA(numElem,numElem), mPerm(numElem, numElem), mPermT(numElem, numElem), mProd(numElem, numElem)
real, allocatable::vD(:)
real::somaElement, normaEuclidiana
integer::nL, nC
real, allocatable::mId(:,:)
real::mId2, temp, matrizXvetor

integer::i, j, k, alphaI
real::alpha=2.0, soma, tmp, vO

!##########################################################################

 vA=(/3.1,4.2,5.3,1.4,2.5/);

! print*,  "valores vA"
		do i=1, numElem
!			print*, vA(i)
		end do
	
!3.1-##########################################################################

allocate(vD(numElem))

		do i=1, numElem
			vD(i) = alpha*vA(i)
		end do

 print*,  "3.1 Calcular o  produto do vetor vA por  2.0:"
call mostrarConteudo(vD, numElem)
print*, " "

!3.2-##########################################################################

soma = 0
		do i=1, numElem
			soma = soma+vA(i)
		end do
!print*, "valor da soma=", soma

soma = 0

soma = somaElement(vA, numElem)
print*, "3.2 Calcular a soma dos elementos do vetor vA=", soma
print*, " "

!3.3-##########################################################################

soma = 0
		do i=1, numElem
			soma = soma+vA(i)**2
		end do

soma = sqrt(soma)
			
!print*, "valor da norma euclidiana vA=", soma		

soma = normaEuclidiana(vA, numElem)
			
print*, "3.3. Calcular a norma euclidiana do vetor vA=", soma		

!mId-##########################################################################

nL = 5
nC = 5
alphaI = 2

allocate(mId(nL,nC))
mId = 0.0

		do i = 1, nL
			do j = 1, nC
				if( i == j )then
					mId( i,j) = 1.0
				end if
			end do
		end do

print*," "
print*, "Matriz Identidade mId"
call mostrarConteudoM(mId, nL, nC)

!3.4-##########################################################################

print*, " "
print*, '3.4.	Calcular o produto da matriz mId por 2.0:'
temp = mId2(mId, nL, nC, alphaI)
print*, " "

!3.5-##########################################################################

alpha = 0
!mId = 0.0

		do i=1, numElem
			tmp = 0
			do j=1, numElem
				tmp = tmp + mId(i,j)*vA(j)
			end do
			vD(i) = tmp 
		end do 

print*, "3.5. calcular o produto da matriz mId pelo vetor vA:"
call mostrarConteudoM(vD, nL, 1)
print*, " "

!3.6-##########################################################################

mPerm = 0
i=1
mPerm(i,4)=1;i=i+1
mPerm(i,5)=1;i=i+1
mPerm(i,1)=1;i=i+1
mPerm(i,2)=1;i=i+1
mPerm(i,3)=1;

print*,"Matriz Permutação para ordenar vetor vA em ordem crescente:"
		do i=1, numElem
			do j=1, numElem
				write(*, '(f8.2)', ADVANCE='NO') mPerm(i,j)
			end do
			print*
		end do 


		do i=1, numElem
			tmp = 0
			do j=1, numElem
				tmp = tmp + mPerm(i,j)*vA(j)
			end do
			vD(i) = tmp 
		end do 
print*, " "		
print*,"3.6. Vetor vA em ordem crescente"
call mostrarConteudoM(vD, numElem, 1)
print*, " "		

!3.7-##########################################################################


print*,"3.7. Matriz Transposta da Matriz Permutação:"
tmp = 0
		do i=1, numElem
			do j=1, numElem
			     mPermT(i,j) = mPerm(j,i)
				!write(*, '(f8.2)', ADVANCE='NO') mPermT(j,i)
				!tmp = tmp + mPermT(j,i)	
			end do
				!vD = tmp
			!print*
		end do 

call mostrarConteudoM(mPermT, numElem, numElem)

print*, " "
print*,"3.8 Matriz Permutação x Matriz Transposta"	
!mProd = matmul(mPerm, transpose(mPerm))

!call mostrarConteudoM(mProd, numElem, numElem)


	do  i = 1, numElem !faz produto com a matriz quadrada B	
		do  j = 1, numElem
			tmp = 0.0 ! Inicia valor da posição da matriz
			do k = 1, numElem
				tmp = tmp+mPerm(i, k)*mPermT(k, j) !Adiciona contribuições
			end do
			mProd(i, j) = tmp ! Guarda valor na posição da matriz
		end do 
 end do 

call mostrarConteudoM(mProd, numElem, numElem)

!3.8-##########################################################################

!print*, " "
!print*,"3.8 Matriz Permutação x Matriz Transposta"
		do i=1, numElem
			tmp = 0	
			do j=1, numElem
				tmp = tmp + mPerm(i,j)*mPermT(j,i)
			end do
			vD(i) = tmp 
		end do 

!call mostrarConteudoM(vD, numElem, numElem)


end program comModule

subroutine mostrarConteudo(vet, n)
implicit none


integer:: n
real :: vet(n)

integer::i
		do i=1, n
			print*, vet(i)
		end do
end subroutine

real function somaElement(vet, n)
implicit none
integer:: n
real :: vet(n)

real::s
integer::i

somaElement = 0
		do i=1, n
			somaElement = somaElement+vet(i)
		end do

end function

real function normaEuclidiana(vet, n)
implicit none
integer:: n
real :: vet(n)

real::s
integer::i

normaEuclidiana = 0
		do i=1, n
			normaEuclidiana = normaEuclidiana+vet(i)**2
		end do
		normaEuclidiana = sqrt(normaEuclidiana)

end function

subroutine mostrarConteudoM(MI, NL, NC)
integer ::NL, NC
real :: MI(NL, NC)

integer:: i, j

	DO i=1, NL
		DO j=1, NC
			write(*, '(f5.2)', advance='NO' ) MI(i, j)			
		END DO
		print*, ' '	
	END DO


end subroutine

!###################################
!3.4.	Calcular o produto da matriz mId por 2.0 e escrever o resultado na tela usando a subrotina mostrarConteudoM
real function mId2(MI, NL, NC, alphaI)
implicit none

integer::NL, NC, alphaI
real::MI(NL,NC)

real::mout(NL,NC)
integer::i, j

mout = 0.0
do i = 1, NL
	do j = 1, NC
		mout(i,j) = MI(i,j)*alphaI
		!print*, mout(i,j)
	enddo
enddo

call mostrarConteudoM(mout, NL, NC)

end function

