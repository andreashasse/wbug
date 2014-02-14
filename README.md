# Wbug
An terminal debugger for Erlang.
This is a prototyp, all help, pull requests and bug reports are welcome.

[![Build Status](https://secure.travis-ci.org/anha0825/wbug.png)](http://travis-ci.org/anha0825/wbug)

## Aims
* Ease to use over nice interface
* Not for production use ... Use redbug for that.

## Don't
* Wbug doesn't work so nicely if you have a second process triggering a breakpoint while you'r debugging.
* Don't use this on a critical system
 * Slow
 * Code reloads

## How does it work
This projects used the module int.erl. This is the same interface as the graphical erlang debugger runs.
Modules with breakpoints are evaluated in a special interperter (much slower).
When a breakpoints is triggers a meta processes is spawned.
This is the processes that the this projects is communicating with (through the int module).

## Setup
rebar compile

## Demo
run/look at wbug:manual(), to see a little example.

## TODO
* Another process hits a breakpoint while you're debugging -> some kind of queue.
 * Choose process to debug from queue.
* Stop command that works. (Wbug currenlty uses 5 of the 13 commands that can be sent to the meta process).
* Currently only 2 unit tests that are covering parts of one module.
* Look at other debuggers to find nice features.
* More modes
 * (timeout + stepping is a bad combo ... maybe some autostep that writes the terminal output to file).
 * Other types from breakpoints (enter function, conditional break).