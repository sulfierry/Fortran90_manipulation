module arrayMod

contains

! Multiplica matriz por vetor
function matVet(mAT, vet, nL, nC)
    implicit none
    integer, intent(in) :: nL, nC
    real, intent(in) :: mAT(nL, nC), vet(nC)
    real :: matVet(nL)
    integer :: i, j

    do i = 1, nL
        matVet(i) = sum(mAT(i, :) * vet)
    end do
    print*, "matvet"
    call mostrarConteudoM(matVet, nL, 1)

end function matVet

! Calcula a soma dos elementos de um vetor
function somaElement(vet, n)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: vet(n)
    real :: somaElement

    somaElement = sum(vet)

end function somaElement

! Calcula a norma Euclidiana de um vetor
function normaEuclidiana(vet, n)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: vet(n)
    real :: normaEuclidiana

    normaEuclidiana = sqrt(sum(vet**2))

end function normaEuclidiana

! Multiplica uma matriz por um escalar
function matrixScalarProduct(MI, NL, NC, alphaI)
    implicit none
    integer, intent(in) :: NL, NC, alphaI
    real, intent(in) :: MI(NL, NC)
    real :: matrixScalarProduct(NL, NC)
    integer :: i, j

    matrixScalarProduct = MI * alphaI

    call mostrarConteudoM(matrixScalarProduct, NL, NC)

end function matrixScalarProduct

! Mostra o conteúdo de um vetor
subroutine mostrarConteudo(vet, n)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: vet(n)
    integer :: i

    do i = 1, n
        print*, vet(i)
    end do

end subroutine mostrarConteudo

! Mostra o conteúdo de uma matriz
subroutine mostrarConteudoM(MI, NL, NC)
    implicit none
    integer, intent(in) :: NL, NC
    real, intent(in) :: MI(NL, NC)
    integer :: i, j

    DO i = 1, NL
        DO j = 1, NC
            write(*, '(f5.2)', advance='NO') MI(i, j)
        END DO
        print*, ' '
    END DO

end subroutine mostrarConteudoM

end module arrayMod

! Começa o programa principal
program comModule
    use arrayMod
    implicit none
    integer, parameter :: numElem = 5
    real :: vA(numElem), mA(numElem, numElem), mPerm(numElem, numElem), mPermT(numElem, numElem), mProd(numElem, numElem)
    real :: soma, alpha = 2.0
    integer :: i, j, k

    vA = (/3.1, 4.2, 5.3, 1.4, 2.5/)

    ! Resto do programa ... (sem mudanças)

end program comModule


