!------------------------------------------------------------------------------
!> Interpolates fields from one mesh toanother. 
!------------------------------------------------------------------------------
SUBROUTINE Mesh2MeshSolver( Model,Solver,dt,TransientSimulation )
!------------------------------------------------------------------------------
  USE DefUtils

  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t) :: Solver
  TYPE(Model_t) :: Model

  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
  TYPE(Mesh_t), POINTER :: Mesh, ThisMesh, TargetMesh
  TYPE(ValueList_t), POINTER :: Params
  TYPE(Variable_t), POINTER :: Var
  CHARACTER(LEN=MAX_NAME_LEN) :: Name, VarName
  INTEGER :: i
  LOGICAL :: Found
  
  CALL Info('Mesh2MeshSolver','Mapping result between meshes')

  
  ThisMesh => Getmesh()
  Params => GetSolverParams()

  TargetMesh => NULL()

  i = ListGetInteger( Params,'Target Mesh Solver Index',Found ) 
  IF( Found ) THEN
    ! Target mesh solver is explicitely given
    TargetMesh => CurrentModel % Solvers(i) % Mesh
  ELSE  
    ! Otherwise use the 1st mesh that is not this old data mesh
    Mesh => CurrentModel % Meshes      
    DO WHILE( ASSOCIATED(Mesh) ) 
      IF( .NOT. ASSOCIATED( Mesh, ThisMesh ) ) THEN
        TargetMesh => Mesh
        EXIT
      END IF
    END DO
  END IF

  IF(ASSOCIATED( TargetMesh ) ) THEN
    CALL Info('Mesh2MeshSolver','Target mesh name is: '//TRIM(Mesh % Name),Level=7)
  ELSE
    CALL Fatal('Mesh2MeshSolver','Could not find a target mesh!')
  END IF

  
  CALL SetCurrentMesh( CurrentModel, TargetMesh )
  DO i = 1,100    
    WRITE (Name,'(A,I0)') 'Variable ',i
    VarName = GetString( Params, Name, Found )
    IF(.NOT. Found ) EXIT
    
    ! Use namespace such that in principle each variable could have different set of
    ! interpolation rules attached to them. 
    CALL ListPushNameSpace('var'//TRIM(I2S(i)))

    ! Here we might invalidate the variable in the primary mesh so that it really needs to be interpolated
    Var => VariableGet( TargetMesh % Variables, VarName, ThisOnly = .TRUE. )
    IF( ASSOCIATED( Var ) ) Var % Valid = .FALSE.
    
    ! Try to find the variable in target mesh, this includes MeshToMesh interpolation by default
    Var => VariableGet( TargetMesh % Variables, VarName )
    IF(.NOT. ASSOCIATED( Var ) ) THEN
      CALL Warn('Mesh2MeshSolver','Could not find variable '//TRIM(VarName)//' in part '//TRIM(I2S(ParEnv % MyPe)))
    ELSE
      PRINT *,'Variable range: ',i,parenv % mype, MINVAL(Var % values), MAXVAL(Var % values)
    END IF
    
    CALL ListPopNamespace()
  END DO
  CALL SetCurrentMesh( CurrentModel, ThisMesh )
  
  CALL Info('Mesh2MeshSolver','Succesfully interpolated '//TRIM(I2S(i-1))//' variables',Level=7)

  
!------------------------------------------------------------------------------
END SUBROUTINE Mesh2MeshSolver
!------------------------------------------------------------------------------
