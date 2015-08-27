#ifndef HaskinoSchedulerH
#define HaskinoSchedulerH

bool parseSchedulerMessage(int size, byte *msg);
void schedulerRunTasks();
bool isRunningTask();
void delayRunningTask(unsigned long ms);

#endif /* HaskinoSchedulerH */
