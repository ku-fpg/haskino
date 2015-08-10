// Arduino Duemilanove, Diecimila, and NG
#if defined(__AVR_ATmega168__) || defined(__AVR_ATmega328P__)
#if defined(NUM_ANALOG_INPUTS) && NUM_ANALOG_INPUTS == 6
#define TOTAL_ANALOG_PINS       6
#define TOTAL_PINS              20 // 14 digital + 6 analog
#else
#define TOTAL_ANALOG_PINS       8
#define TOTAL_PINS              22 // 14 digital + 8 analog
#endif

// old Arduinos
#elif defined(__AVR_ATmega8__)
stuff!!!!
// Arduino Mega
#elif defined(__AVR_ATmega1280__) || defined(__AVR_ATmega2560__)

// Arduino DUE
#elif defined(__SAM3X8E__)

// Leonardo
#elif defined(__AVR_ATmega32U4__)

// Intel Galileo Board
#elif defined(ARDUINO_LINUX)

// Sanguino
#elif defined(__AVR_ATmega644P__) || defined(__AVR_ATmega644__)

// Illuminato
#elif defined(__AVR_ATmega645__)

// anything else
#else
#error "Please edit Boards.h with a hardware abstraction for this board"
#endif
