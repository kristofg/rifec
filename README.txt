RIFEC: Receive Images From Eye-Fi Cards
=======================================

rifec.pl is a standalone Linux server script that receives images from
Eye-Fi cards and stores them to disk.

It is written in Perl, uses a few CPAN modules, and is GPL.  It works
great with my Pro X2 card running firmware 4.2139.  I have not tested
other versions, but feedback from people who have is welcome.

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
http://forums.eye.fi/viewtopic.php?f=4&t=270 also contains some very
useful information.

Todo
====

* Consider checking the card firmware version in the HTTP header, and
  warn (or die) if it is a new or unknown version.

Things I'm still wondering about
================================

* What is the filesignature field in the SOAP envelope of the file
  upload?  Something we can check?

* What does the 'flags' field in the GetPhotoStatus request mean?

* What does the 'offset' field in the GetPhotoStatus reply mean?

Ideas
=====

* Destination subdirectories not just based on date but on camera
  (fetched from EXIF info)?  At lot more useful if you move your card
  around - or have a pool of cards and a pool of cameras; would be
  more interesting if I had several Eye-Fi cards. :)

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
