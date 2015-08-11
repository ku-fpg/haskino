#ifndef AmberCommandsH
#define AmberCommandsH

// Note:  None of the CMD_TYPE's should be 0x70, so that there
// is no possibility of sending an HDLC frame or escape as
// a command and requiring to send an escape.

#define CMD_TYPE_MASK           0xF0
#define CMD_SUBTYPE_MASK        0x0F

// Board Control commands
#define BC_CMD_TYPE             0x00
#define BC_CMD_SET_PIN_MODE     (BC_CMD_TYPE | 0x0)
#define BC_CMD_DELAY_MILLIS     (BC_CMD_TYPE | 0x1)
#define BC_CMD_DELAY_MICROS     (BC_CMD_TYPE | 0x2)
#define BC_CMD_SYSTEM_RESET     (BC_CMD_TYPE | 0x3)

// Board Control responses

// Board Status commands
#define BS_CMD_TYPE             0x10
#define BS_CMD_REQUEST_VERSION  (BS_CMD_TYPE | 0x1)
#define BS_CMD_REQUEST_TYPE     (BS_CMD_TYPE | 0x2)
#define BS_CMD_REQUEST_MICROS   (BS_CMD_TYPE | 0x3)
#define BS_CMD_REQUEST_MILLIS   (BS_CMD_TYPE | 0x4)

// Board Status responses
#define BS_RESP_VERSION         (BS_CMD_TYPE | 0x8)
#define BS_RESP_TYPE            (BS_CMD_TYPE | 0x9)
#define BS_RESP_MICROS          (BS_CMD_TYPE | 0xA)
#define BS_RESP_MILLIS          (BS_CMD_TYPE | 0xB)
#define BS_RESP_STRING          (BS_CMD_TYPE | 0xC)

// Digital commands
#define DIG_CMD_TYPE            0x20
#define DIG_CMD_READ_PIN        (DIG_CMD_TYPE | 0x0)
#define DIG_CMD_WRITE_PIN       (DIG_CMD_TYPE | 0x1)

// Digital responses
#define DIG_RESP_READ_PIN       (DIG_CMD_TYPE | 0x8)

// Analog commands
#define ALG_CMD_TYPE            0x30
#define ALG_CMD_READ_PIN        (ALG_CMD_TYPE | 0x0)
#define ALG_CMD_WRITE_PIN       (ALG_CMD_TYPE | 0x1)

// Analog responses
#define ALG_RESP_READ_PIN       (ALG_CMD_TYPE | 0x8)

// I2C commands
#define I2C_CMD_TYPE            0x40
#define I2C_CMD_READ            (I2C_CMD_TYPE | 0x0)
#define I2C_CMD_READ_REG        (I2C_CMD_TYPE | 0x1)
#define I2C_CMD_WRITE           (I2C_CMD_TYPE | 0x2)

// I2C responses
#define I2C_RESP_READ           (I2C_CMD_TYPE | 0x8)

// One Wire commands
#define ONEW_CMD_TYPE           0x50

// One Wire responses

// Servo commands
#define SRVO_CMD_TYPE           0x60

// Servo responses

// Stepper commands
#define STEP_CMD_TYPE           0x80

// Stepper responses

// Scheduler commands
#define SCHED_CMD_TYPE          0x90
#define SCHED_CMD_CREATE_TASK   (SCHED_CMD_TYPE | 0x0)
#define SCHED_CMD_DELETE_TASK   (SCHED_CMD_TYPE | 0x1)
#define SCHED_CMD_ADD_TO_TASK   (SCHED_CMD_TYPE | 0x2)
#define SCHED_CMD_DELAY_TASK    (SCHED_CMD_TYPE | 0x3)
#define SCHED_CMD_SCHED_TASK    (SCHED_CMD_TYPE | 0x4)
#define SCHED_CMD_QUERY         (SCHED_CMD_TYPE | 0x5)
#define SCHED_CMD_QUERY_ALL     (SCHED_CMD_TYPE | 0x6)
#define SCHED_CMD_RESET         (SCHED_CMD_TYPE | 0x7)

// Scheduler responses
#define SCHED_RESP_QUERY        (SCHED_CMD_TYPE | 0x8)
#define SCHED_RESP_QUERY_ALL    (SCHED_CMD_TYPE | 0x9)

#endif /* AmberCommandsH */

