See [this package's page on hackage](http://hackage.haskell.org/package/alarmclock).

Device for running an action at (i.e. shortly after) a certain time, which
can be used to implement things like time-based cache expiry.

This implementation avoids the use of polling and leans on Haskell's scheduler
to achieve low-latency without lots of computational overhead.

The alarm can be set multiple times, and in this case the alarm will go off at
the earliest requested time. If the alarm is set in the past, the action will
run immediately. When the action runs, it clears all future alarms; the action
can itself return the time at which it should run again.

To perform time-based cache expiry, create an 'AlarmClock' whose action flushes
any stale entries from the cache and then calls `setAlarm` for the next time
that an entry will expire (if there are any). When expiring entries are added
to the cache, call 'setAlarm' to ensure that they will expire in a timely
fashion.
