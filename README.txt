Eye-Fi Receiver
===============

Eye-Fi Receiver is a standalone server designed to receive files from
Eye-Fi cards and store them to disk as reliably as possible.  That's
it.  It is written in Perl, and has some module dependencies, and is
GPL.

Resources and other implementations:
====================================

This is not the first implementation of such a server.  Some of the
others are:

* https://github.com/tachang/EyeFiServer (Python)

* https://github.com/kenkeiter/ryfi (Ruby)

* https://github.com/hacker/iii (C++)

* http://randomtechmakings.blogspot.com/2009/01/i-bought-eye-fi-sd-card-few-weeks-ago.html (Perl)

The Eye-Fi forum thread at
http://forums.eye.fi/viewtopic.php?f=4&t=270 also contains some very
useful information.

Todo:
=====

* More atomic file-writing, making it impossible for even simultaneous
  uploads of the same file (name) to clobber each other

* Check the card firmware version in the HTTP header, and warn (or
  die) if it is a new or unknown version.

Things I'm still wondering about:
=================================

* What is the filesignature field in the SOAP envelope of the file
  upload?  Something we can check?

Ideas (not quite on the todo list, at least yet):
=================================================

* Destination subdirectories not just based on date but on camera
  (fetched from EXIF info)?  At lot more useful if you move your card
  around - or have a pool of cards and a pool of cameras

Things I've decided not to worry about for now:
===============================================

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
