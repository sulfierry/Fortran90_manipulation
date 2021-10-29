program verify_sequence_from_binary
implicit none
integer :: numTermos, numTotalTermos=15
integer :: i,j,T
integer, parameter :: guiaArqT=10
character(len=64) :: nomeArqB="sequencia.bin", nome=' '
integer, allocatable::vet(:)
logical::bool
real :: phi=1.618034, raiz=0.0; !parametros fibonacci
raiz=sqrt(5.0) !parametro fibonacci

call lerBin(nomeArqB, numTermos, vet)
!write(*,*)vet, "..."!, numtermos

!call lerArquivo(nomeArqB, numTermos, vet)
call mostrarConteudo(numTermos, vet)
call identificarSeq(numTermos, vet, nome, bool)
!call escreverBin(numTermos, vet)
!call lerBin(numTermos, vet)
write(*,*)'--------------inicio------------------'
write(*,*)'sequencia:', nome
write(*,*)' '
write(*,*)' '

!print*,"A sequencia", nome, 'esta correta?', bool

contains

subroutine lerArquivo(nomeArqB, numTermos, vet)
implicit none
character(len=64):: nomeArqB
integer::numTermos,i,T
integer, allocatable::vet(:)
integer, parameter::guiaArqT=10

open(unit=guiaArqT, file=nomeArqB, form='formatted')
read(guiaArqT,*)numtermos

allocate(vet(numTermos))

	do i=1, numTermos
		read(guiaArqT,*) T
		vet(i)=T
	end do
	
close(guiaArqT);


end subroutine


subroutine mostrarConteudo(numTermos, vet)
implicit none
integer::numTermos, vet(numTermos)


write(*,*)"numero de termos:", numTermos

write(*,*)"sequencia lida:", vet(:)

!write(*,*)"mostrar conteudo"
!write(*,*)numTermos, vet(:)

end subroutine

subroutine identificarSeq(numTermos, vet, nome, bool)
implicit none
integer :: numTermos, numTotalTermos=15, vet(numTermos)
integer :: i,j,T
logical::bool
character(len=60):: nome
real :: phi=1.618034, raiz=0.0; !parametros fibonacci
raiz=sqrt(5.0) !parametro fibonacci


!sequencia natural
if(vet(2)==2 .or. vet(3)==3) then
    nome='N'
    bool = .TRUE.
    do i = 1, numTermos
        if( vet(i) /= i ) then
            bool =.FALSE.
            write(*,*) 'o indice', i,'esta incorreto, seu valor atual é: ', vet(i), 'mas deveria ser:',  i
            write(*,*)"a sequencia natural esta incorreta"
        endif
    end do
endif


!sequencia triangular
if(vet(2)==3 .or. vet(3)==6) then
    nome='T'
    bool = .TRUE.
    do i = 1, numTermos
        if( vet(i) /= (i*(i+1))/2 ) then
            write(*,*) 'o indice', i,'esta incorreto, seu valor atual é: ', vet(i), 'mas deveria ser:', (i*(i+1))/2
            write(*,*)'a sequencia triangular esta incorreta'
                else
        endif
    end do
endif


!sequencia quadratica
if(vet(2)==4 .or. vet(3)==9) then
    nome='Q'
    bool = .TRUE.
    do i = 1, numTermos
        if( vet(i) /= (i*i) ) then
            bool =.FALSE.
            write(*,*) 'o indice', i,'esta incorreto, seu valor atual é: ', vet(i), 'mas deveria ser:',  (i*i)
            write(*,*)"a sequencia quadratica esta incorreta"
        endif
    end do
endif


!sequencia pentagonal
if(vet(2)==5 .or. vet(3)==12)then
    nome='P'
    bool = .TRUE.
    do i = 1, numTermos
        if( vet(i) /= (i*(3*i-1))/2 ) then
            bool =.FALSE.
            write(*,*) 'o indice', i,'esta incorreto, seu valor atual é: ', vet(i), 'mas deveria ser:',  (i*(3*i-1))/2
            write(*,*)"a sequencia pentagonal esta incorreta"
        endif
    end do
endif


!sequencia fibonacci
if(vet(2)==1 .or. vet(3)==3) then
    nome='F'
    bool = .TRUE.
    do i = 1, numTermos
        if( vet(i) /= int((phi**i-(1- phi)**i )/raiz)) then
            bool =.FALSE.
            write(*,*) 'o indice', i, 'incorreto, seu valor atual é: ', vet(i), 'mas deveria ser:', int((phi**i-(1- phi)**i)/raiz)
            write(*,*)"a sequencia fibonacci esta incorreta"
        endif
    end do
endif

end subroutine

subroutine escreverBin(numTermos, vet)
implicit none
integer::numTermos,numTotalTermos=15, vet(numTermos)

integer::i,T
integer, parameter :: guiaArqT=101
character(len=64) :: nomeArqBin="b13.bin"


open(unit=guiaArqT, file=nomeArqBin, form='unformatted')
write(guiaArqT)numTermos


do i=1, numTermos
    write(guiaArqT)vet(i)
end do

close(guiaArqT)

end subroutine

subroutine lerBin(nomeArqB, numTermos, vet)
implicit none
integer, allocatable::vet(:)
integer::numTermos



integer::i,T
integer, parameter :: guiaArqT=101
!character(len=64) :: nomeArqBin="sequencia.bin"
character(len=64):: nomeArqB

open(unit=guiaArqT, file=nomeArqB, form='unformatted')




read(guiaArqT)numTermos
allocate(vet(numTermos))
!    write(*,*)numTermos


do i=1, numTermos
    read(guiaArqT)vet(i)
!    write(*,*)T
end do
!write(*,*)vet, "..."!, numtermos
close(guiaArqT)

end subroutine


end program
