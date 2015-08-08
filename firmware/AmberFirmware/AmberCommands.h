#ifndef AmberCommandsH
#define AmberCommandsH

#define CMD_TYPE_MASK           0xF0
#define CMD_SUBTYPE_MASK        0x0F

// Board Control commands (0x0n)
#define BC_CMD_MASK             0x00
#define BC_CMD_REQUEST_VERSION  0x00
#define BC_CMD_SET_PIN_MODE     0x01
#define BC_CMD_REQUEST_MICROS   0x02
#define BC_CMD_REQUEST_MILLIS   0x03
#define BC_CMD_DELAY_MILLIS     0x04
#define BC_CMD_DELAY_MICROS     0x05
#define BC_CMD_SYSTEM_RESET     0x06

// Board Control responses (0x0m)
#define BC_RESP_VERSION         0x08
#define BC_RESP_MICROS          0x09
#define BC_RESP_MILLIS          0x0A
#define BC_RESP_STRING          0x0B

// Digital commands (0x1n)
#define DIG_CMD_MASK            0x10
#define DIG_CMD_READ_PIN        0x10
#define DIG_CMD_WRITE_PIN       0x11

// Digital responses (0x1m)
#define DIG_RESP_READ_PIN       0x18

// Analog commands (0x2n)
#define ALG_CMD_MASK            0x20
#define ALG_CMD_READ_PIN        0x20
#define ALG_CMD_WRITE_PIN       0x21

// Analog responses (0x2m)
#define ALG_RESP_READ_PIN       0x28

// I2C commands (0x3n)
#define I2C_CMD_MASK            0x30
#define I2C_CMD_READ            0x30
#define I2C_CMD_WRITE           0x31

// I2C responses (0x3m)
#define I2C_RESP_READ           0x38

// One Wire commands (0x4n)
#define ONEW_CMD_MASK           0x40

// One Wire responses (0x4m)

// Servo commands (0x5n)
#define SRVO_CMD_MASK           0x50

// Servo responses (0x5m)

// Stepper commands (0x6n)
#define STEP_CMD_MASK           0x60

// Stepper responses (0x6m)

// Scheduler commands (0x7n)
#define SCHED_CMD_MASK          0x70
#define SCHED_CMD_CREATE_TASK   0x70
#define SCHED_CMD_DELETE_TASK   0x71
#define SCHED_CMD_ADD_TO_TASK   0x72
#define SCHED_CMD_DELAY_TASK    0x73
#define SCHED_CMD_SCHED_TASK    0x74
#define SCHED_CMD_QUERY         0x75
#define SCHED_CMD_QUERY_ALL     0x76

// Scheduler responses (0x7m)
#define SCHED_RESP_QUERY        0x78
#define SCHED_RESP_QUERY_ALL    0x79

#endif /* AmberCommandsH */

