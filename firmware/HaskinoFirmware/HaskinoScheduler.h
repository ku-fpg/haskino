#ifndef HaskinoSchedulerH
#define HaskinoSchedulerH

bool parseSchedulerMessage(int size, const byte *msg, byte *local);
void schedulerBootTask();
void schedulerRunTasks();
bool isRunningTask();
void delayRunningTask(unsigned long ms);

#endif /* HaskinoSchedulerH */
