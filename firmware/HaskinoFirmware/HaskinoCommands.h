#ifndef HaskinoCommandsH
#define HaskinoCommandsH

// Note:  None of the CMD_TYPE's should be 0x7x, so that there
// is no possibility of sending an HDLC frame or escape as
// a command and causing an extra escape to be sent.
// Also, not using 0x0x as a command type to avoid sending an
// all zero message.

#define CMD_TYPE_MASK           0xF0
#define CMD_SUBTYPE_MASK        0x0F

// Board Control commands
#define BC_CMD_TYPE             0x10
#define BC_CMD_SET_PIN_MODE     (BC_CMD_TYPE | 0x0)
#define BC_CMD_DELAY_MILLIS     (BC_CMD_TYPE | 0x1)
#define BC_CMD_DELAY_MICROS     (BC_CMD_TYPE | 0x2)
#define BC_CMD_SYSTEM_RESET     (BC_CMD_TYPE | 0x3)
#define BC_CMD_SET_PIN_MODE_E   (BC_CMD_TYPE | 0x4)
#define BC_CMD_DELAY_MILLIS_E   (BC_CMD_TYPE | 0x5)
#define BC_CMD_DELAY_MICROS_E   (BC_CMD_TYPE | 0x6)
#define BC_CMD_WHILE            (BC_CMD_TYPE | 0x7)
#define BC_CMD_IF_THEN_ELSE     (BC_CMD_TYPE | 0x8)

// Board Control responses
#define BC_RESP_DELAY           (BC_CMD_TYPE | 0x9)

// Board Status commands
#define BS_CMD_TYPE             0x20
#define BS_CMD_REQUEST_VERSION  (BS_CMD_TYPE | 0x0)
#define BS_CMD_REQUEST_TYPE     (BS_CMD_TYPE | 0x1)
#define BS_CMD_REQUEST_MICROS   (BS_CMD_TYPE | 0x2)
#define BS_CMD_REQUEST_MILLIS   (BS_CMD_TYPE | 0x3)

// Board Status responses
#define BS_RESP_VERSION         (BS_CMD_TYPE | 0x8)
#define BS_RESP_TYPE            (BS_CMD_TYPE | 0x9)
#define BS_RESP_MICROS          (BS_CMD_TYPE | 0xA)
#define BS_RESP_MILLIS          (BS_CMD_TYPE | 0xB)
#define BS_RESP_STRING          (BS_CMD_TYPE | 0xC)

// Digital commands
#define DIG_CMD_TYPE            0x30
#define DIG_CMD_READ_PIN        (DIG_CMD_TYPE | 0x0)
#define DIG_CMD_WRITE_PIN       (DIG_CMD_TYPE | 0x1)
#define DIG_CMD_READ_PIN_E      (DIG_CMD_TYPE | 0x2)
#define DIG_CMD_WRITE_PIN_E     (DIG_CMD_TYPE | 0x3)

// Digital responses
#define DIG_RESP_READ_PIN       (DIG_CMD_TYPE | 0x8)

// Analog commands
#define ALG_CMD_TYPE            0x40
#define ALG_CMD_READ_PIN        (ALG_CMD_TYPE | 0x0)
#define ALG_CMD_WRITE_PIN       (ALG_CMD_TYPE | 0x1)
#define ALG_CMD_TONE_PIN        (ALG_CMD_TYPE | 0x2)
#define ALG_CMD_NOTONE_PIN      (ALG_CMD_TYPE | 0x3)
#define ALG_CMD_READ_PIN_E      (ALG_CMD_TYPE | 0x4)
#define ALG_CMD_WRITE_PIN_E     (ALG_CMD_TYPE | 0x5)
#define ALG_CMD_TONE_PIN_E      (ALG_CMD_TYPE | 0x6)
#define ALG_CMD_NOTONE_PIN_E    (ALG_CMD_TYPE | 0x7)

// Analog responses
#define ALG_RESP_READ_PIN       (ALG_CMD_TYPE | 0x8)

// I2C commands
#define I2C_CMD_TYPE            0x50
#define I2C_CMD_CONFIG          (I2C_CMD_TYPE | 0x0)
#define I2C_CMD_READ            (I2C_CMD_TYPE | 0x1)
#define I2C_CMD_WRITE           (I2C_CMD_TYPE | 0x2)
#define I2C_CMD_READ_E          (I2C_CMD_TYPE | 0x3)
#define I2C_CMD_WRITE_E         (I2C_CMD_TYPE | 0x4)

// I2C responses
#define I2C_RESP_READ           (I2C_CMD_TYPE | 0x8)

// One Wire commands
#define ONEW_CMD_TYPE           0x60

// One Wire responses

// Servo commands
#define SRVO_CMD_TYPE           0x80

// Servo responses

// Stepper commands
#define STEP_CMD_TYPE           0x90

// Stepper responses

// Scheduler commands
#define SCHED_CMD_TYPE          0xA0
#define SCHED_CMD_CREATE_TASK   (SCHED_CMD_TYPE | 0x0)
#define SCHED_CMD_DELETE_TASK   (SCHED_CMD_TYPE | 0x1)
#define SCHED_CMD_ADD_TO_TASK   (SCHED_CMD_TYPE | 0x2)
#define SCHED_CMD_SCHED_TASK    (SCHED_CMD_TYPE | 0x3)
#define SCHED_CMD_QUERY         (SCHED_CMD_TYPE | 0x4)
#define SCHED_CMD_QUERY_ALL     (SCHED_CMD_TYPE | 0x5)
#define SCHED_CMD_RESET         (SCHED_CMD_TYPE | 0x6)
#define SCHED_CMD_CREATE_TASK_E (SCHED_CMD_TYPE | 0x7)
#define SCHED_CMD_DELETE_TASK_E (SCHED_CMD_TYPE | 0x8)
#define SCHED_CMD_ADD_TO_TASK_E (SCHED_CMD_TYPE | 0x9)
#define SCHED_CMD_SCHED_TASK_E  (SCHED_CMD_TYPE | 0xA)
#define SCHED_CMD_QUERY_E       (SCHED_CMD_TYPE | 0xB)
#define SCHED_CMD_BOOT_TASK_E   (SCHED_CMD_TYPE | 0xC)

// Scheduler responses
#define SCHED_RESP_QUERY        (SCHED_CMD_TYPE | 0xD)
#define SCHED_RESP_QUERY_ALL    (SCHED_CMD_TYPE | 0xE)

// Reference commands
#define REF_CMD_TYPE            0xB0

#define REF_CMD_NEW             (REF_CMD_TYPE | 0x0)
#define REF_CMD_READ            (REF_CMD_TYPE | 0x1)
#define REF_CMD_WRITE           (REF_CMD_TYPE | 0x2)

// Reference  response
#define REF_RESP_NEW            (REF_CMD_TYPE | 0x8)
#define REF_RESP_READ           (REF_CMD_TYPE | 0x9)

// Expression Eval commands
#define EXP_CMD_TYPE            0xC0

#define EXP_CMD_EVAL            (EXP_CMD_TYPE | 0x0)

// Reference  response
#define EXP_RESP_EVAL           (EXP_CMD_TYPE | 0x8)

#endif /* HaskinoCommandsH */

