CC = gcc
MPICC = mpicc
CFLAGS = -O3
LDFLAGS = -Wl,-R,\.
LIBS = -lm
UNAME_S = $(shell uname -s)

ifneq (, $(findstring SunOS, $(UNAME_S)))
	LIBS += -lnsl -lsocket -lresolv
endif

compile:
	$(CC) $(CFLAGS) $(LDFLAGS) -o dtlz2_serial.exe dtlz2_serial.c -L. -lborg $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o dtlz2_advanced.exe dtlz2_advanced.c -L. -lborg $(LIBS)
	$(MPICC) $(CFLAGS) $(LDFLAGS) -o dtlz2_ms.exe dtlz2_ms.c -L. -lborgms $(LIBS)
	$(MPICC) -fopenmp $(CFLAGS) $(LDFLAGS) -o TestmainParallelmm_worker.exe TestmainParallelmm_worker.cpp -L. -lborgmm -lstdc++ $(LIBS)
	$(MPICC) -fopenmp $(CFLAGS) $(LDFLAGS) -o mainParallelmm_worker.exe mainParallelmm_worker.cpp -L. -lborgmm -lstdc++ $(LIBS)
	$(MPICC) $(CFLAGS) $(LDFLAGS) -o dtlz2_mm.exe dtlz2_mm.c -L. -lborgmm $(LIBS)
.PHONY: compile

