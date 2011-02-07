RIFEC: Receive Images From Eye-Fi Cards
=======================================

rifec.pl is a standalone program that receives images from Eye-Fi
cards and stores them to disk.

It works great with my Pro X2 card running firmware 4.2172 (4.2139
worked fine as well).  I have not tested other versions, but feedback
from people who have is welcome.

It runs mainly on Linux, but making it run on other Unix variants
should be trivial.  It is written in Perl, uses some CPAN modules, and
is GPL.  See the file INSTALL.txt for more information on installation
and dependencies.

This is third-party software; it is not developed nor supported by
Eye-Fi.  Because of this, there will be limitations in what it can do
compared to the official Eye-Fi software. See the file
TROUBLESHOOTING.txt for more information and help on troubleshooting.


Resources and other implementations
===================================

The hardware: http://www.eye.fi/

This is not the first implementation of such a server.  Some of the
others are:

* https://github.com/tachang/EyeFiServer (Python)

* https://github.com/kenkeiter/ryfi (Ruby)

* https://github.com/hacker/iii (C++)

* http://randomtechmakings.blogspot.com/2009/01/i-bought-eye-fi-sd-card-few-weeks-ago.html (Perl)

The Eye-Fi forum thread at
http://forums.eye.fi/viewtopic.php?f=4&t=270 contains some very useful
information for everyone interested in running or making their own
server.


Todo
====

* Consider checking the card firmware version in the HTTP header, and
  warn (or die?) if it is a new or unknown version.

* Limit the number of forked threads, to make it just a little less
  DoS'able

Things I'm still wondering about
================================

* What is the filesignature field in the SOAP envelope of the file
  upload?  Something we can check?

* What does the 'flags' field in the GetPhotoStatus request mean?


Ideas
=====

* More control over destination directories:

  - Based on file type (jpegs /go/here, raw files /go/there, etc.)

  - Based on EXIF data, so if you have multiple cameras and multiple
    cards the images from camera A would end up in the same place
    regardless of which card you put in it.  (Would be more
    interesting if I had multiple Eye-Fi cards.)

  The biggest problem with these features is the configuration part:
  How to make this configurable in a robust, easy to understand, and
  easy to debug way?

* Hooks for running custom scripts on successful upload.  Would
  probably solve the previous problem as well, but has pretty much the
  same problems, too.

* Change the usage of HTTP::Daemon::get_request() so that the Upload
  operation is sent to our code before the entire HTTP operation is
  complete: Would allow better progress notification ("upload in
  progress, 10% received") and probably make (network) debugging
  easier, but is a bit more complex on the code side.


Things I've decided not to worry about for now
==============================================

* Some kind of persistent upload counter not just between handlers in
  the same session, but between sessions?  Does it make any difference
  at all to the card?  
  => It seems not, a lot of the other implementations have hard coded
  it to 1.

* Should I just echo back the transfermode I get from the card
  regardless of what it is, or are there some transfermodes I should
  reject?
  => The transfermode field seems to be a bitmask; I'll start by only
  accepting the bits I (think I) know what means, and add more if/when
  necessary.

* What does the 'offset' field in the GetPhotoStatus reply mean?
  => Reading the Card log, my guess is that this field is used when
  the server has received a partial upload, ie. half a file.  It can
  then use this field to say how much of the file is already received.
  So leaving this as 0 should be pretty safe - we don't support
  partial uploads at all anyway (only complete requests come through
  the HTTP library; this can be worked around, but I'm not sure if
  it's worth it.)
