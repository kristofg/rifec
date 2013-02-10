How to build and install rifec.pl on a Synology NAS
===================================================

NO WARRANTY: THIS SOFTWARE IS INSTALLED AND USED AT YOUR OWN RISK.  It
works for me on my DS212+, but I cannot guarantee it for other people
on other devices.

To build an SPK package installable on Synology DSM:

  make spk

Should produce a file called "rifec-VERSION.spk", which can be
installed through the package center on DSM4.

Note that this SPK does not contain a config file, so you will have to
log in to the Synology device and create one by hand.  Once that is
done, the config file should be preserved over package updates
