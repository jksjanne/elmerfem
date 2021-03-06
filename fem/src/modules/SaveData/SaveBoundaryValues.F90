!/*****************************************************************************/
! *
! *  Elmer, A Finite Element Software for Multiphysical Problems
! *
! *  Copyright 1st April 1995 - , CSC - IT Center for Science Ltd., Finland
! * 
! *  This program is free software; you can redistribute it and/or
! *  modify it under the terms of the GNU General Public License
! *  as published by the Free Software Foundation; either version 2
! *  of the License, or (at your option) any later version.
! * 
! *  This program is distributed in the hope that it will be useful,
! *  but WITHOUT ANY WARRANTY; without even the implied warranty of
! *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! *  GNU General Public License for more details.
! *
! *  You should have received a copy of the GNU General Public License
! *  along with this program (in file fem/GPL-2); if not, write to the 
! *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
! *  Boston, MA 02110-1301, USA.
! *
! *****************************************************************************/
!
!/******************************************************************************
! *
! *  Subroutine for saving scalar data to files
! *
! ******************************************************************************
! *
! *  Authors: Peter Råback
! *  Email:   Peter.Raback@csc.fi
! *  Web:     http://www.csc.fi/elmer
! *  Address: CSC - IT Center for Science Ltd.
! *           Keilaranta 14
! *           02101 Espoo, Finland 
! *
! *  Original Date: 20 Nov 2001
! *
! *****************************************************************************/

!> \ingroup Solvers
!> \{


!------------------------------------------------------------------------------
!> Routine for saving boundary values as fields.
!------------------------------------------------------------------------------
SUBROUTINE SaveBoundaryValues( Model,Solver,dt,TransientSimulation )
  USE DefUtils
  USE Types
  USE Lists
  USE Integration
  USE ElementDescription
  USE SolverUtils

  IMPLICIT NONE
! Types
!------------------------------------------------------------------------------
  TYPE(Solver_t), TARGET :: Solver
  TYPE(Model_t) :: Model
  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
  TYPE(Solver_t), POINTER :: PointerToSolver
  TYPE(Mesh_t), POINTER :: Mesh
  TYPE(Element_t),POINTER :: CurrentElement
  TYPE(ValueList_t), POINTER :: ValueList, Params
  TYPE(Variable_t), POINTER :: Var
  INTEGER :: NoParams, DIM, ParamNo, istat, LocalNodes, &
       n, j, i, elementNumber
  INTEGER, POINTER :: NodeIndexes(:), FieldPerm(:)=>NULL()
  REAL(KIND=dp), POINTER :: Field(:)
  REAL(KIND=dp), ALLOCATABLE :: LocalParam(:)
  CHARACTER(LEN=MAX_NAME_LEN) ::  ParamName(99), Name
  LOGICAL :: GotCoeff, GotIt, GotOper, GotVar, ExactCoordinates
  LOGICAL, ALLOCATABLE :: Valid(:)

  SAVE DIM, LocalNodes, ParamName, NoParams


  CALL Info('SaveBoundaryValues','Creating selected boundary values as fields')

  !------------------------------------------------
  ! Set some pointers and other initilization stuff
  !------------------------------------------------
  Params => GetSolverParams()
  PointerToSolver => Solver
  Mesh => Solver % Mesh
  
  DIM = CoordinateSystemDimension()
  LocalNodes = Model % NumberOfNodes

  n = Mesh % MaxElementNodes
  ALLOCATE( LocalParam(n), STAT=istat )
  IF( istat /= 0 ) CALL Fatal('SaveBoundaryValues','Memory allocation error 1') 

  ! Find out how many variables should we saved
  NoParams = 0
  GotVar = .TRUE.
  
  DO WHILE(GotVar)  
    NoParams = NoParams + 1
    WRITE (Name,'(A,I0)') 'Parameter ',NoParams
    ParamName(NoParams) = ListGetString( Params, TRIM(Name), GotVar )
  END DO
  NoParams = NoParams-1

  IF( NoParams == 0) THEN
    CALL WARN( 'SaveBoundaryValues', 'No parameters found: No fields will be created')       
    RETURN
  END IF
     
  ALLOCATE(Valid(NoParams)) !Prevent seg faults when missing
  Valid = .TRUE.
  
  !------------------
  ! Add new Variables
  ! -----------------
  DO ParamNo = 1, NoParams
    Var => VariableGet( Model % Variables, TRIM(ParamName(ParamNo)), .TRUE.)     
    IF(ASSOCIATED( Var ) ) CYCLE

    ALLOCATE(FieldPerm(LocalNodes),STAT=istat)
    IF( istat /= 0 ) CALL Fatal('SaveBoundaryValues','Memory allocation error 2') 
    
    FieldPerm = 0
    
    DO elementNumber=Mesh % NumberOfBulkElements+1, &
        Mesh % NumberOfBulkElements + Mesh % NumberOfBoundaryElements
      
      CurrentElement => Mesh % Elements(elementNumber)
      !------------------------------------------------------------------
      ! do nothing, if we are dealing with a halo-element in parallel run
      !------------------------------------------------------------------
      IF (CurrentElement % PartIndex /= Parenv % mype) CYCLE
      
      n = GetElementNOFNodes(CurrentElement)
      NodeIndexes => CurrentElement % NodeIndexes           
      Model % CurrentElement => CurrentElement
      
      ValueList => GetBC(CurrentElement)
      IF( ListCheckPresent(ValueList, TRIM(ParamName(ParamNo))) ) THEN
        FieldPerm( NodeIndexes ) = 1
      END IF
    END DO
    
    j = 0 
    DO i=1,LocalNodes
      IF( FieldPerm(i) > 0 ) THEN
        j = j + 1
        FieldPerm(i) = j
      END IF
    END DO
    
    IF( j == 0 ) THEN
      CALL Warn('SaveBoundaryValues',&
          'Parameter '//TRIM(ParamName(ParamNo))//' not present in any material')
      Valid(ParamNo) = .FALSE.
    ELSE
      WRITE( Message,'(A,I0,A)') 'Parameter > '//TRIM(ParamName(ParamNo))&
          //' < defined with ',j,' dofs'
      CALL Info('SaveBoundaryValues',Message)
      
      ALLOCATE(Field(j),STAT=istat)
      IF( istat /= 0 ) CALL Fatal('SaveBoundaryValues','Memory allocation error 3') 
      Field = 0.0_dp
      
      CALL VariableAdd( Mesh % Variables, Mesh, PointerToSolver, &
          TRIM(ParamName(ParamNo)), 1, Field, FieldPerm )         
      
      NULLIFY( Field )
    END IF
    
    NULLIFY( FieldPerm )
  END DO   

  !-------------------------------------------------------
  ! Loop all parameters to be exported and update their values.
  ! NoParams is set the 1st time the suboutine is visited. 
  !--------------------------------------------------------
  DO ParamNo=1,NoParams 
    IF(.NOT. Valid(ParamNo)) CYCLE !prevents segfaults...
    
    Var => VariableGet( Model % Variables, TRIM(ParamName(ParamNo)), .TRUE.)     
    Field => Var % Values
    FieldPerm => Var % Perm
    
    DO elementNumber=Mesh % NumberOfBulkElements+1, &
        Mesh % NumberOfBulkElements + Mesh % NumberOfBoundaryElements
      
      CurrentElement => Mesh % Elements(elementNumber)
      IF (CurrentElement % PartIndex /= Parenv % mype) CYCLE
      
      n = GetElementNOFNodes(CurrentElement)
      NodeIndexes => CurrentElement % NodeIndexes           
      
      IF( ASSOCIATED( FieldPerm ) ) THEN
        IF( .NOT. ALL(FieldPerm(NodeIndexes) > 0) ) CYCLE
      END IF
      
      Model % CurrentElement => CurrentElement
      
      ValueList => GetBC(CurrentElement)
      LocalParam(1:n) = ListGetReal(ValueList, TRIM(ParamName(ParamNo)), &
          n, NodeIndexes, GotIt)
      IF(.NOT. GotIt) CYCLE
      
      IF( ASSOCIATED( FieldPerm ) ) THEN
        Field(FieldPerm(NodeIndexes(1:n))) = LocalParam(1:n)      
      ELSE
        Field(NodeIndexes(1:n)) = LocalParam(1:n)      
      END IF
      
    END DO
  END DO

  DEALLOCATE(LocalParam, Valid)

END SUBROUTINE SaveBoundaryValues

!> \}
