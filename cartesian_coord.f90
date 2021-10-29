module modCartesiano2D
implicit none

type::coord2D

real::x,y

end type coord2D

contains

real function calcDistancia(pA_,pB_)
!use modCartesiano2D
implicit none
real:: x, y
type(Coord2D)::pA_, pB_

x = pA_%x
y = pB_%y

calcDistancia = sqrt((pA_%x - pB_%x)**2 + (pA_%y - pB_%y)**2)

end function

subroutine atribuirConteudo(pA_, x_, y_)
!use modCartesiano2D
implicit none
real::x_, y_
type(Coord2D)::pA_

pA_%x  = x_
pA_%y  = y_

end subroutine

subroutine mostrarConteudo(R_)
!use modCartesiano2D
implicit none

type(Coord2D)::R_

print*, R_

end subroutine

subroutine transladar(P_,x_,y_)
!use modCartesiano2D
implicit none
real::x_, y_
type(Coord2D)::P_

P_%x = (p_%x + x_)
P_%y = (p_%y + y_)

end subroutine

end module

program expTipoDerivado
use modCartesiano2D
implicit none
type(Coord2D) :: origem, pA, pB !3 variaveis do novo tipo 
!real :: dAB, dBA, calcDistancia
real :: dAB
real :: dBA

	origem%x= 0.0; origem%y=0.0
 call atribuirConteudo (pA, 3.0, 3.0); 

 print*, " Coordenadas da origem "
 call mostrarConteudo (origem);

 print*, " Coordenadas do ponto A "
 call mostrarConteudo (pA);     
 pB = pA;

 print*, " Coordenadas do ponto B "
 call mostrarConteudo (pB);
 call transladar      (pB,3.0,4.0);

 print*, " Coordenadas do ponto B transladado "
 call mostrarConteudo (pB)

 dAB = calcDistancia(pA,pB)
 print*, " distancia entre A e B =", dAB

 dBA = calcDistancia(pB,pA)
 print*, " distancia entre B e A =", dBA

end program expTipoDerivado