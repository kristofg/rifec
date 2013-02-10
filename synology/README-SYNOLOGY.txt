How to build and install rifec.pl on a Synology NAS
===================================================

NO WARRANTY: THIS SOFTWARE IS INSTALLED AND USED AT YOUR OWN RISK.

This package works great for me on my DS212+ running DSM 4.1.  I hope
it will be useful for others as well, but I cannot guarantee it will
work for other people on other devices.

To build an SPK package installable on Synology DSM, cd to the
'synology/' subdirectory (the one this README file is in) and run the
command 'make'.  This should produce a file called
"rifec-VERSIONSTRING.spk", which can be installed through the package
center on DSM 4.

This build process requires an Linux/Unix computer with a basic set of
Perl development packages installed.

Note that this SPK does not contain a config file, so you will have to
log in to the Synology device and create one by hand.  It should be
called 'synology.config' and be put in the directory
'/var/packages/rifec/target/etc/'.  See the documented example config
file included in this repository for help on making a config file.
Remember to use the local paths on the Synology device, not the
exported share names.

Once a config file is in place, the package script should make sure it
is preserved across package updates automatically.
