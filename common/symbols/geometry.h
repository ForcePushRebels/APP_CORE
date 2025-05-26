#ifndef __GEOMETRY_H__
#define __GEOMETRY_H__

#include <stddef.h>

typedef struct {
	int x;
	int y;
} Point;

typedef struct {
	Point *points; // Dynamic array of points
	size_t count;        // Number of points in the path
} Path;

typedef struct {
	Point point; // Position in 2D space
	float angle; // Angle de rotation en radians
} Position;

#endif /* __GEOMETRY_H__ */