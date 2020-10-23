

####### Compiler, tools and options

# CC            = gcc
CXX           = mpicxx
CXXFLAGS      = -c -O3 -Wall 

####### Compile
all: LakeComo

LakeComo: mainP_lakecomo.o model_lakecomo.o param_function.o ncRBF.o rbf.o catchment.o lake.o lakecomo.o borgmm.o utils.o mt19937ar.o
	$(CXX) mainP_lakecomo.o model_lakecomo.o param_function.o ncRBF.o rbf.o catchment.o lake.o lakecomo.o borgmm.o utils.o mt19937ar.o -o LakeComo

mainP_lakecomo.o: mainP_lakecomo.cpp ../src/model_lakecomo.h \
	../mm-borg-moea/borgmm.h
	$(CXX) $(CXXFLAGS) mainP_lakecomo.cpp

model_lakecomo.o: ../src/model_lakecomo.cpp ../src/lake.h \
	../src/lakecomo.h \
	../src/catchment.h \
	../src/param_function.h \
	../src/ncRBF.h \
	../src/rbf.h 
	$(CXX) $(CXXFLAGS) ../src/model_lakecomo.cpp

ncRBF.o: ../src/ncRBF.cpp ../src/ncRBF.h \
	../src/param_function.h
	$(CXX) $(CXXFLAGS) ../src/ncRBF.cpp

rbf.o: ../src/rbf.cpp ../src/rbf.h \
	../src/param_function.h
	$(CXX) $(CXXFLAGS) ../src/rbf.cpp

param_function.o: ../src/param_function.cpp ../src/param_function.h
	$(CXX) $(CXXFLAGS) ../src/param_function.cpp

catchment.o: ../src/catchment.cpp ../src/catchment.h 
	$(CXX) $(CXXFLAGS) ../src/catchment.cpp

lakecomo.o: ../src/lakecomo.cpp ../src/lakecomo.h \
	../src/lake.h
	$(CXX) $(CXXFLAGS) ../src/lakecomo.cpp

lake.o: ../src/lake.cpp ../src/lake.h 
	$(CXX) $(CXXFLAGS) ../src/lake.cpp

borgmm.o: ../mm-borg-moea/borgmm.c ../mm-borg-moea/borgmm.h \
	../mm-borg-moea/borg.h
	$(CXX) $(CXXFLAGS) ../mm-borg-moea/borgmm.c

mt19937ar.o: ../mm-borg-moea/mt19937ar.c ../mm-borg-moea/mt19937ar.h
	$(CXX) $(CXXFLAGS) ../mm-borg-moea/mt19937ar.c

utils.o: ../src/utils.cpp ../src/utils.h
	$(CXX) $(CXXFLAGS) ../src/utils.cpp

clean:
	rm -rf *.o 
	rm LakeComo

