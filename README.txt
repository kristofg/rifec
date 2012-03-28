RIFEC: Receive Images From Eye-Fi Cards
=======================================

rifec.pl is a standalone program that receives images from Eye-Fi
cards and stores them to disk.

It works great with my Pro X2 card running firmware 5.0001 (all the
4.x versions I have tried worked fine as well).  Feedback from people
who have tested other cards and versions is welcome.

I have only tested it on Linux, but making it run on other Unix
variants should be trivial.  It is written in Perl 5, uses some CPAN
modules, and is GPL.  See the file INSTALL.txt for more information on
installation and dependencies.

This is third-party software; it is not developed nor supported by
Eye-Fi.  Because of this, there will be limitations in what it can do
compared to the official Eye-Fi software.

Please see the file TROUBLESHOOTING.txt if you have problems.

What can it do?
===============

The primary use case is to have the images saved directly to the disk
of a Linux computer on the same WLAN as the camera and card.

* It supports multiple cards.  The destination directory can be
  configured per card or as a shared setting.

* It supports date-based destination directories, based on the clock
  on the receiving computer at the time of transfer.

* It supports different destination directories for different file
  types coming from the same card, so you can store jpeg files in one
  directory, raw files in another, and videos in a third.

What can it NOT do?
===================

See also the "Todo" and "Ideas" sections further down. The main reason
for not supporting these things are lack of time and need. If someone
has time and need, patches are welcome!

* The geotagging feature is currently not supported.

* Running programs (hooks) to manipulate the images after they are
  saved is currently not supported.

* Date-based destination directories based on the image (EXIF) date is
  currently not supported (note that date-based destination
  directories based on the time of transfer works fine.)


Resources and other implementations
===================================

The hardware: http://www.eye.fi/

This is not the first implementation of such a server.  Some of the
others are:

* https://github.com/tachang/EyeFiServer (Python)

* https://launchpad.net/eyefi (Python; seems to be the most actively
  maintained Python variant)

* https://github.com/kenkeiter/ryfi (Ruby)

* https://github.com/hacker/iii (C++)

* http://randomtechmakings.blogspot.com/2009/01/i-bought-eye-fi-sd-card-few-weeks-ago.html (Perl)

* http://code.google.com/p/eyefiserver/ (Python - based on
  https://github.com/tachang/EyeFiServer)

The Eye-Fi forum thread at
http://forums.eye.fi/viewtopic.php?f=4&t=270 contains some very useful
information for everyone interested in running or making their own
server.


Todo
====

* Tests. It's amazing how fast a small program becomes big enough to
  allow bugs to hide in the corners where people rarely go.

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
