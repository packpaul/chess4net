last changed: 2010-04-28

SETUP:
1) Make a copy of IMEClient1\ and rename it to IMEClient2
2) Download the latest version of Chess4Net plugin for Miranda from http://chess4net.ru
3) Extract archive to IMEClient1\Plugins and IMEClient2\Plugins

TESTING:
1) Run IMEServer.exe, IMEClient1\IMEClient.exe and IMEClient2\IMEClient.exe
2) connect with the clients to server, use different handle IDs and handle names
3) Start Chess4Net plugin from each of the clients
4) Report any found bugs to packpaul@mail.ru in the following format:

BUG <bug number>

Version:
  <affected version>

Description:
  <description>

Steps:
  <reproduction steps>

Error output:
<error output>

Needed output:
<needed output>


for example:


BUG 1

Version:
  2007.2

Description:
  Time incrementation doesn't work

Steps:
  1) Start Chess4Net session
  2) Setup timing 5:5
  3) Start game make a move on client 1

Error output:
  Timing for white increments by 5 sec only on client 1 showing 5:05. On client 2 it remains 5:00

Needed output:
  Timing for white on client 1 and client 2 is 5:05