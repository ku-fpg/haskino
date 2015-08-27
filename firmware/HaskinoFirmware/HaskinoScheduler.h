#ifndef AmberSchedulerH
#define AmberSchedulerH

bool parseSchedulerMessage(int size, byte *msg);
void schedulerRunTasks();
bool isRunningTask();
void delayRunningTask(unsigned long ms);

#endif /* AmberSchedulerH */
