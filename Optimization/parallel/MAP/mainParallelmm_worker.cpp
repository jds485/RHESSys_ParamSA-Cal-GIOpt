//============================================================================
// Name        : mainParallelmm.cpp
// Author      : JaredDSmith
// Version     : 0.1
// Copyright   : 2020
//============================================================================

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <fstream>
#include "borgmm.h"

using namespace std;

// Decision Variables: Up, Mid, Downslope proportion in 2 hillslopes
int nvars = 6;
// Objectives: flooding, low flows, area
int nobjs = 3;

void model_wrapper(double *vars, double *objs, double *constr, int *evals, int *isle, int *worker){
    /*Loop over the parameterizations
    //#pragma omp parallel for 
    for (int i = 1; i < 20; i++){*/
    
    // When using one parameter set 
    unsigned int i = 1;
    
    // write decision variables to a txt file
    unsigned int q;
    unsigned int N = *evals;
    unsigned int inum = *isle;
  	char f10[256];
    sprintf(f10, "n%u_i%u_m%u.txt", N, i, inum);
    FILE *fv = fopen(f10, "w");
    for(q = 0; q<nvars; q++){
      if (q != (nvars-1)){
        fprintf(fv, "%f\t", vars[q]);
      }else{
        fprintf(fv, "%f\n", vars[q]);
      }
    }
    fclose(fv);
    
    // run RHESSys with pre- and post-processing shell script
    char command[256];
    char fc[256];
    sprintf(fc, "n%u_i%u_m%u_out.txt", N, i, inum);
    unsigned int work = *worker;
    sprintf(command, "sh /scratch/jdq6nn/Bais910Hill30mOptimizeGI_MAP/RunArray_NFT_GI_worker.sh %u %u %u %u &>> %s", N, i, inum, work, fc);
    std::system(command);
    
    // Move output from system command to storage folder
    char fdir[256];
    sprintf(fdir, "/scratch/jdq6nn/Bais910Hill30mOptimizeGI_MAP/RHESSysRuns/output/%s", fc);
    if(std::rename(fc, fdir) != 0) {
      fputs("Error moving system output file", stderr);
    }
    
    // read in objectives file and assign to objs
    FILE* oFile;
    double buffer[nobjs];
    int result = 0;
    // Open the objectives file
    char of10[256];
    sprintf(of10, "Run%u_P%u_M%u_Objs.txt", N, i, inum);
    oFile = fopen(of10, "r");
    if (oFile==NULL){
      fputs ("Error opening objectives file",stderr); exit (1);
    }
    // copy the file into the buffer:
    for (unsigned int g = 0; g<nobjs; g++){
      result += fscanf(oFile, "%lf\n", &buffer[g]);
    }
    if (result != nobjs){
      fputs("Error reading objectives file", stderr); exit (3);
    }
    fclose(oFile);
    //Delete the objectives file from this directory
    if(remove(of10) != 0){
      fputs("Error deleting objectives file", stderr);
    }
    // assign objectives
    for(int k = 0; k<nobjs; k++){
        objs[k] = buffer[k];
    }
}

// Baisman Hillslope 9+10 GI Optimization Problem
int main(int argc, char* argv[]) {
    // Debug mode will slow computation time. .exe size will increase.
    // BORG_Debug_on();
    // set random seed
    unsigned int seed = 8;
    srand(seed);
    
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Changes for MPI version start here
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    BORG_Algorithm_ms_startup(&argc, &argv);
    // Number of masters
    BORG_Algorithm_ms_islands(2);
    // Max time
    BORG_Algorithm_ms_max_time(71.4);
    
    //Enable global Latin hypercube initialization to ensure each island
    // gets a well sampled distribution of solutions.
    BORG_Algorithm_ms_initialization(INITIALIZATION_LATIN_GLOBAL);
    
    // Print archive every NFEi evaluations
    BORG_Algorithm_output_frequency(200);
    
    // Define the problem with decisions, objectives, constraints, and the evaluation function
    BORG_Problem problem = BORG_Problem_create(nvars, nobjs, 0, model_wrapper);
    
    // Set decision variable bounds
    for(int i=0; i<nvars; i++){
        // Order is GI proportion Hill 9 Downslope, Midslope, Upslope. Then same for Hillslope 10.
        BORG_Problem_set_bounds(problem, i, 0.0, 1.0);
    }
    
    // Set epsilon tolerance for each objective
    // Order is flooding (cfs/cfs), low flows (cfs/cfs), area (number of trees)
    BORG_Problem_set_epsilon(problem, 0, 0.015);
    BORG_Problem_set_epsilon(problem, 1, 0.015);
    BORG_Problem_set_epsilon(problem, 2, 1.0);
    
    // Generate output and runtime files for this seed
    char outputFilename[256];
    char runtime[256];
    FILE* outputFile = NULL;
    sprintf(outputFilename, "/scratch/jdq6nn/Bais910Hill30mOptimizeGI_MAP/RHESSysRuns/output/GI_S%d.set", seed); // output path (make sure this exists)
    sprintf(runtime, "/scratch/jdq6nn/Bais910Hill30mOptimizeGI_MAP/RHESSysRuns/output/GI_S%d_M%%d.runtime", seed); // runtime path (make sure this exists)
    
    BORG_Algorithm_output_runtime(runtime);
    
    // Use a different random seed on each processor
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    BORG_Random_seed(37*seed*(rank+1));
    
    // Run the optimization
    BORG_Archive result = BORG_Algorithm_ms_run(problem);
    
    // If this is the master node, print out the final archive
    if (result != NULL) {
        outputFile = fopen(outputFilename, "w");
        if (!outputFile) {
            BORG_Debug("Unable to open final output file\n");
        }
        BORG_Archive_print(result, outputFile);
        BORG_Archive_destroy(result);
        fclose(outputFile);
    }
    
    // Shutdown, destroy, and return status
    BORG_Algorithm_ms_shutdown();
    BORG_Problem_destroy(problem);
    
    return EXIT_SUCCESS;
}