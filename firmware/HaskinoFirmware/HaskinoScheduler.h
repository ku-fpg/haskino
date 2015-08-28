#ifndef HaskinoSchedulerH
#define HaskinoSchedulerH

bool parseSchedulerMessage(int size, const byte *msg, byte *local);
void schedulerRunTasks();
bool isRunningTask();
void delayRunningTask(unsigned long ms);

#endif /* HaskinoSchedulerH */
