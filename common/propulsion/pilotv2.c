////////////////////////////////////////////////////////////
// Module: pilot
// Description: Pilotage des moteurs (machine à états + POSIX mqueues)
// Date    : 17/11/2025
////////////////////////////////////////////////////////////

#include "pilotv2.h"
#include "robotConfiguration.h" // Pour WHEEL_DIAMETER_CM
#include "xLog.h"
#include <errno.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h> // pour sleep, etc.