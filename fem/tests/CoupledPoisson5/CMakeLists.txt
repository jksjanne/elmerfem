INCLUDE(test_macros)
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR}/fem/src)

CONFIGURE_FILE( case.sif case.sif COPYONLY)
ADD_ELMERTEST_MODULE(CoupledPoisson5 PoissonAssembly PoissonAssembly.f90)

file(COPY ELMERSOLVER_STARTINFO PoissonAssembly.f90 angle.grd DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/")

ADD_ELMER_TEST(CoupledPoisson5)
ADD_ELMER_LABEL(CoupledPoisson5 quick)
