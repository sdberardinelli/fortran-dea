! GLPK_subroutine_DEA-loop-DMUs-droprows
!********************************************************************
subroutine LP_CLP(nr,nc,nDMU,nY,nX,objtype,objvals,rest,rhs,A,obj,soln,runtime,runtimes,EFFscores)
!********************************************************************
use clp
use clp_constants
use iso_c_binding
implicit none
type(c_ptr) :: dea
type(c_ptr) :: dea2
type(c_ptr) :: qdea
!********************************************************************
!input variables
integer     :: nr,nc
integer     :: nDMU,nY,nX
character*3 :: objtype,rest(nr)
real*8      :: objvals(nc),rhs(nr)
real*8      :: A(nr,nc)
!********************************************************************
!output variables
real*8      :: obj
real*8      :: soln(nc)
real*8      :: runtime
real*8      :: runtimes(10)
!DEA and QDEA outputs
real*8      :: EFFscores(nDMU,3)
!********************************************************************
!DEA  variables
character*3 :: restD(nDMU+1),restD2(nDMU+1)
real*8      :: objvalsD(nY+nX)
real*8      :: rhsD(nDMU+1),rhsD2(nDMU+1)
real*8      :: AD(nDMU+1,nY+nX),AD2(nDMU+1,nY+nX)
real*8      :: objD,objD2
real*8      :: solnD(nY+nX)
!DEA internal viables
integer     :: nYX,nDMUp1,nconst
integer     :: nrD,ncD
integer     :: nrD2,ncD2
integer     :: irD2
integer     :: jDMU
integer     :: itmp,itmp1,itmp2,itmp3
integer     :: jtmp,jtmp1,jtmp2,jtmp3
integer     :: dstart,dend
integer     :: ijconrowD(nX),ijconrowQ(nX)
integer     :: dcount
real*8      :: eps=10.0**(-10.0)
real*8      :: dmult(nDMU),dvals(nDMU)
!********************************************************************
!internal variables
integer     :: nsims

integer :: ret
integer, dimension(0:(nr*nc))        :: ia,ja
integer, dimension(0:((nDMU+1)*(nY+nX))) :: iaD,jaD
integer, dimension(0:((nDMU+1)*(nY+nX))) :: iaD2,jaD2

integer i,j,k
integer kD,kQ,kD2
real(kind=8), dimension(0:(nr*nc))        :: ar
real(kind=8), dimension(0:((nDMU+1)*(nY+nX))) :: arD
real(kind=8), dimension(0:((nDMU+1)*(nY+nX))) :: arD2

real(8), dimension(0:4) :: obj_coeff
real(8), dimension(0:4) :: obj_coeff_temp
real(8), dimension(0:4) :: row_coeff
real(8), dimension(0:4) :: row_coeff_temp

real(kind=8) :: z,z8

integer ijrowD(nX),ijrow(nX)
integer jcount
!********************************************************************
real*8  :: t0,t1,t2,t3,t4,t6,t7,t8,t9,t10
!********************************************************************
!********************************************************************

    write (*,*) Clp_VersionMajor()
    write (*,*) Clp_VersionMinor()
    write (*,*) Clp_VersionRelease()

end subroutine LP_CLP
!********************************************************************
