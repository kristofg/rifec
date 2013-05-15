RIFECVERSION=0.9
GITOUTPUT=$(shell git describe --always --tags --dirty 2> /dev/null)
GITVERSION=$(shell if [ -n "$(GITOUTPUT)" ]; then echo "$(GITOUTPUT)"; else echo "nogit"; fi)

# This is put into the INFO file and SPK file name
VERSION=$(RIFECVERSION)-$(GITVERSION)

TARGETDIR=$(CURDIR)/installtarget
WGET=wget -nv
TARBALL=$(CURDIR)/spk/package.tgz
SPKFILE=$(CURDIR)/rifec-$(VERSION).spk

# Make the files end up in semi-sane places
MAKEMAKERARGS=PREFIX=$(TARGETDIR) LIB=$(TARGETDIR)/share/perl5
MODULEBUILDERARGS=--install_base=$(TARGETDIR) --install_path lib=$(TARGETDIR)/share/perl5

# List::MoreUtils
LIST-MOREUTILS=List-MoreUtils-0.33
LIST-MOREUTILS-URL=http://search.cpan.org/CPAN/authors/id/A/AD/ADAMK/$(LIST-MOREUTILS).tar.gz

# Config::IniFiles
CONFIG-INIFILES=Config-IniFiles-2.80
CONFIG-INIFILES-URL=http://search.cpan.org/CPAN/authors/id/S/SH/SHLOMIF/$(CONFIG-INIFILES).tar.gz

# Proc::Daemon
PROC-DAEMON=Proc-Daemon-0.14
PROC-DAEMON-URL=http://search.cpan.org/CPAN/authors/id/D/DE/DETI/Proc/$(PROC-DAEMON).tar.gz

# Module::Runtime (needed by Params::Validate via Module::Implementation)
MODULE-RUNTIME=Module-Runtime-0.013
MODULE-RUNTIME-URL=http://search.cpan.org/CPAN/authors/id/Z/ZE/ZEFRAM/$(MODULE-RUNTIME).tar.gz

# Try::Tiny (needed by Params::Validate via Module::Implementation)
TRY-TINY=Try-Tiny-0.11
TRY-TINY-URL=http://search.cpan.org/CPAN/authors/id/D/DO/DOY/$(TRY-TINY).tar.gz

# Module::Implementation (needed by Params::Validate)
MODULE-IMPLEMENTATION=Module-Implementation-0.06
MODULE-IMPLEMENTATION-URL=http://search.cpan.org/CPAN/authors/id/D/DR/DROLSKY/$(MODULE-IMPLEMENTATION).tar.gz

# Params::Validate
PARAMS-VALIDATE=Params-Validate-1.06
PARAMS-VALIDATE-URL=http://search.cpan.org/CPAN/authors/id/D/DR/DROLSKY/$(PARAMS-VALIDATE).tar.gz

# XML::NamespaceSupport
XML-NAMESPACESUPPORT=XML-NamespaceSupport-1.11
XML-NAMESPACESUPPORT-URL=http://search.cpan.org/CPAN/authors/id/P/PE/PERIGRIN/$(XML-NAMESPACESUPPORT).tar.gz

# XML::SAX::Base
XML-SAX-BASE=XML-SAX-Base-1.08
XML-SAX-BASE-URL=http://search.cpan.org/CPAN/authors/id/G/GR/GRANTM/$(XML-SAX-BASE).tar.gz

# XML::SAX
XML-SAX=XML-SAX-0.99
XML-SAX-URL=http://search.cpan.org/CPAN/authors/id/G/GR/GRANTM/$(XML-SAX).tar.gz

# XML::Simple
XML-SIMPLE=XML-Simple-2.20
XML-SIMPLE-URL=http://search.cpan.org/CPAN/authors/id/G/GR/GRANTM/$(XML-SIMPLE).tar.gz

##
.PHONY: all
all: spk

.PHONY: spk
spk: metafiles tarball
	cd $(CURDIR)/spk && tar -cf $(SPKFILE) *

.PHONY: tarball
tarball: perlmodules scriptfiles
	cd $(TARGETDIR) && tar -czf $(TARBALL) *

.PHONY: scriptfiles
scriptfiles:
	mkdir -p $(TARGETDIR)/bin $(TARGETDIR)/etc $(TARGETDIR)/logs
	cp ../rifec.pl $(TARGETDIR)/bin/
	cp ../rifec.config $(TARGETDIR)/etc/example.config

.PHONY: metafiles
metafiles: tarball
	$(eval PACKAGEMD5 := $(shell md5sum $(TARBALL) | awk '{print $$1}'))
	perl -pe 's/%%VERSION%%/$(VERSION)/g; s/%%CHECKSUM%%/$(PACKAGEMD5)/g;' < INFO.template > $(CURDIR)/spk/INFO

##
.PHONY: perlmodules
perlmodules: list-moreutils config-inifiles proc-daemon module-runtime try-tiny module-implementation params-validate xml-namespacesupport xml-sax-base xml-sax xml-simple

.PHONY: clean
clean:
	rm -rf $(LIST-MOREUTILS)
	rm -rf $(CONFIG-INIFILES)
	rm -rf $(PROC-DAEMON)
	rm -rf $(MODULE-RUNTIME)
	rm -rf $(TRY-TINY)
	rm -rf $(MODULE-IMPLEMENTATION)
	rm -rf $(PARAMS-VALIDATE)
	rm -rf $(XML-NAMESPACESUPPORT)
	rm -rf $(XML-SAX-BASE)
	rm -rf $(XML-SAX)
	rm -rf $(XML-SIMPLE)

.PHONY: distclean
distclean: clean
	rm -rf $(LIST-MOREUTILS).tar.gz
	rm -rf $(CONFIG-INIFILES).tar.gz
	rm -rf $(PROC-DAEMON).tar.gz
	rm -rf $(MODULE-RUNTIME).tar.gz
	rm -rf $(TRY-TINY).tar.gz
	rm -rf $(MODULE-IMPLEMENTATION).tar.gz
	rm -rf $(PARAMS-VALIDATE).tar.gz
	rm -rf $(XML-NAMESPACESUPPORT).tar.gz
	rm -rf $(XML-SAX-BASE).tar.gz
	rm -rf $(XML-SAX).tar.gz
	rm -rf $(XML-SIMPLE).tar.gz

.PHONY: targetclean
targetclean: distclean
	rm -rf $(TARGETDIR)
	rm -rf $(TARBALL)
	rm -rf $(SPKFILE)
	rm -f $(CURDIR)/spk/INFO

##
$(LIST-MOREUTILS).tar.gz:
	$(WGET) $(LIST-MOREUTILS-URL)

.PHONY: list-moreutils
list-moreutils: $(LIST-MOREUTILS).tar.gz
	tar xzf $(LIST-MOREUTILS).tar.gz
	cd $(LIST-MOREUTILS) && \
		perl Makefile.PL $(MAKEMAKERARGS) && \
		make && \
		make install

##
$(CONFIG-INIFILES).tar.gz:
	$(WGET) $(CONFIG-INIFILES-URL)

.PHONY: config-inifiles
config-inifiles: $(CONFIG-INIFILES).tar.gz
	tar xzf $(CONFIG-INIFILES).tar.gz
	cd $(CONFIG-INIFILES) && \
		perl Build.PL $(MODULEBUILDERARGS) && \
		./Build && \
		./Build install

##
$(PROC-DAEMON).tar.gz:
	$(WGET) $(PROC-DAEMON-URL)

.PHONY: proc-daemon
proc-daemon: $(PROC-DAEMON).tar.gz
	tar xzf $(PROC-DAEMON).tar.gz
	cd $(PROC-DAEMON) && \
		perl Makefile.PL $(MAKEMAKERARGS) && \
		make && \
		make install

##
$(MODULE-RUNTIME).tar.gz:
	$(WGET) $(MODULE-RUNTIME-URL)

.PHONY: module-runtime
module-runtime:	$(MODULE-RUNTIME).tar.gz
	tar xzf $(MODULE-RUNTIME).tar.gz
	cd $(MODULE-RUNTIME) && \
		perl Build.PL $(MODULEBUILDERARGS) && \
		./Build && \
		./Build install

##
$(TRY-TINY).tar.gz:
	$(WGET) $(TRY-TINY-URL)

.PHONY: try-tiny
try-tiny: $(TRY-TINY).tar.gz
	tar xzf $(TRY-TINY).tar.gz
	cd $(TRY-TINY) && \
		perl Makefile.PL $(MAKEMAKERARGS) && \
		make && \
		make install

##
$(MODULE-IMPLEMENTATION).tar.gz:
	$(WGET) $(MODULE-IMPLEMENTATION-URL)

.PHONY: module-implementation
module-implementation: $(MODULE-IMPLEMENTATION).tar.gz
	tar xzf $(MODULE-IMPLEMENTATION).tar.gz
	cd $(MODULE-IMPLEMENTATION) && \
		perl Makefile.PL $(MAKEMAKERARGS) && \
		make && \
		make install

##
$(PARAMS-VALIDATE).tar.gz:
	$(WGET) $(PARAMS-VALIDATE-URL)

.PHONY: params-validate
params-validate: $(PARAMS-VALIDATE).tar.gz
	tar xzf $(PARAMS-VALIDATE).tar.gz
	cd $(PARAMS-VALIDATE) && \
		perl Build.PL $(MODULEBUILDERARGS) --pp && \
		./Build && \
		./Build install

##
$(XML-NAMESPACESUPPORT).tar.gz:
	$(WGET) $(XML-NAMESPACESUPPORT-URL)

.PHONY: xml-namespacesupport
xml-namespacesupport: $(XML-NAMESPACESUPPORT).tar.gz
	tar xzf $(XML-NAMESPACESUPPORT).tar.gz
	cd $(XML-NAMESPACESUPPORT) && \
		perl Makefile.PL $(MAKEMAKERARGS) && \
		make && \
		make install

##
$(XML-SAX-BASE).tar.gz:
	$(WGET) $(XML-SAX-BASE-URL)

.PHONY: xml-sax-base
xml-sax-base: $(XML-SAX-BASE).tar.gz
	tar xzf $(XML-SAX-BASE).tar.gz
	cd $(XML-SAX-BASE) && \
		perl Makefile.PL $(MAKEMAKERARGS) && \
		make && \
		make install

##
$(XML-SAX).tar.gz:
	$(WGET) $(XML-SAX-URL)


# See also http://perl-xml.sourceforge.net/faq/#parserdetails.ini
.PHONY: xml-sax
xml-sax: $(XML-SAX).tar.gz
	tar xzf $(XML-SAX).tar.gz
	cd $(XML-SAX) && \
		export PERL5LIB="$(TARGETDIR)/share/perl5:$(TARGETDIR)/lib/perl5:$(PERL5LIB)" && \
		echo "n" | perl Makefile.PL $(MAKEMAKERARGS) && \
		make && \
		make install && \
		perl -MXML::SAX  -e "XML::SAX->add_parser(q(XML::SAX::PurePerl))->save_parsers()"

##
$(XML-SIMPLE).tar.gz:
	$(WGET) $(XML-SIMPLE-URL)

.PHONY: xml-simple
xml-simple: $(XML-SIMPLE).tar.gz
	tar xzf $(XML-SIMPLE).tar.gz
	cd $(XML-SIMPLE) && \
		export PERL5LIB="$(TARGETDIR)/share/perl5:$(TARGETDIR)/lib/perl5:$(PERL5LIB)" && \
		perl Makefile.PL $(MAKEMAKERARGS) && \
		make && \
		make install
