#pragma once 

extern "C" void Isosurface( float values[], int size, int dimx, int dimy, int dimz, int visstep, float isolevel);
extern "C" void SlicePlane( float values[], int size, int dimx, int dimy, int dimz, int visstep );
extern "C" void RayCasting( float values[], int size, int dimx, int dimy, int dimz, int visstep );
