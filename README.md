process-status
==============

Emacs buffer mode for displaying information from /proc.

`M-x mc-proc RET` shows a graph of processes with parent/child
relationships.  For example:

```
      1   systemd
    236    |_ systemd-journal
    248    |_ systemd-udevd
    669    |_ repowerd
    670    |_ avahi-daemon
    704    |   \_ avahi-daemon
   1163    |_ lightdm
   1215    |   |_ Xorg
   1326    |   \_ lightdm
   1353    |       \_ run-systemd-ses
   1461    |           |_ unclutter
   1494    |           |_ ssh-agent
   1645    |           \_ systemctl
...
```

Press RET on any line to see details about that process.
For example:

```
pid      1645
ppid     1353
argv     systemctl --user start --wait ubuntu-session.target
comm     systemctl
cwd      /home/michael
exe      /bin/systemctl
state    Sleeping in an interruptible wait (S)
started  31 hours ago at 2017-06-30 09:58:25
session  1353
pgrp     1353
faults   major 0, minor 186, children 0/0
priority 20
nice     0
threads  1
itreal   0
rss      403 of unlimited (32 bits)
environ  CLUTTER_IM_MODULE=xim
         COMPIZ_CONFIG_PROFILE=ubuntu
         DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus
         DEFAULTS_PATH=/usr/share/gconf/ubuntu.default.path
...
```

g to refresh.

To send a signal to processes, mark the processes with k (KILL), t (TERM),
h (HUP) or \ (QUIT), then press x to send the signal(s).

n and p move to next and previous lines and display the process details.
