#ifndef HaskinoConfigH
#define HaskinoConfigH

#define MESSAGE_MAX_SIZE    256
#define MAX_REFS            32
#define BIND_SPACING        5
#define DEFAULT_BIND_COUNT  10
#define MAX_BLOCK_LEVELS    5
#define NUM_SEMAPHORES      5
#define MAX_INTERRUPTS      6 

#define MAX_FIRM_SERVOS     4
#define MAX_FIRM_STEPPERS   4

#define INCLUDE_DIG_CMDS
#define INCLUDE_ALG_CMDS
#undef  INCLUDE_I2C_CMDS
#undef  INCLUDE_ONEW_CMDS
#undef  INCLUDE_SRVO_CMDS
#undef  INCLUDE_STEP_CMDS
#undef  INCLUDE_SPI_CMDS
#define INCLUDE_SCHED_CMDS

//#define DEBUG
#endif /* HaskinoConfigH */
