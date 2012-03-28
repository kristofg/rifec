TARGETDIR=/mnt/synoperl

#
CONFIG-INIFILES=Config-IniFiles-2.68
CONFIG-INIFILES-URL=http://search.cpan.org/CPAN/authors/id/S/SH/SHLOMIF/$(CONFIG-INIFILES).tar.gz

#
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
XML-SIMPLE=XML-Simple-2.18
XML-SIMPLE-URL=http://search.cpan.org/CPAN/authors/id/G/GR/GRANTM/$(XML-SIMPLE).tar.gz


##
.PHONY: all
all: config-inifiles proc-daemon module-runtime try-tiny module-implementation params-validate xml-namespacesupport xml-sax-base xml-sax xml-simple

download: $(CONFIG-INIFILES).tar.gz $(PROC-DAEMON).tar.gz $(PARAMS-VALIDATE).tar.gz $(XML-SIMPLE).tar.gz $(XML-SAX).tar.gz

.PHONY: clean
clean:
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
targetclean:
	rm -rf $(TARGETDIR)

##
$(CONFIG-INIFILES).tar.gz:
	wget $(CONFIG-INIFILES-URL)

.PHONY: config-inifiles
config-inifiles: $(CONFIG-INIFILES).tar.gz
	tar xzf $(CONFIG-INIFILES).tar.gz
	cd $(CONFIG-INIFILES) && \
		perl Build.PL --install_base=$(TARGETDIR) && \
		./Build && \
		./Build install

##
$(PROC-DAEMON).tar.gz:
	wget $(PROC-DAEMON-URL)

.PHONY: proc-daemon
proc-daemon: $(PROC-DAEMON).tar.gz
	tar xzf $(PROC-DAEMON).tar.gz
	cd $(PROC-DAEMON) && \
		perl Makefile.PL PREFIX=$(TARGETDIR) && \
		make && \
		make install

##
$(MODULE-RUNTIME).tar.gz:
	wget $(MODULE-RUNTIME-URL)

.PHONY: module-runtime
module-runtime:	$(MODULE-RUNTIME).tar.gz
	tar xzf $(MODULE-RUNTIME).tar.gz
	cd $(MODULE-RUNTIME) && \
		perl Build.PL --install_base=$(TARGETDIR) && \
		./Build && \
		./Build install

##
$(TRY-TINY).tar.gz:
	wget $(TRY-TINY-URL)

.PHONY: try-tiny
try-tiny: $(TRY-TINY).tar.gz
	tar xzf $(TRY-TINY).tar.gz
	cd $(TRY-TINY) && \
		perl Makefile.PL PREFIX=$(TARGETDIR) && \
		make && \
		make install

##
$(MODULE-IMPLEMENTATION).tar.gz:
	wget $(MODULE-IMPLEMENTATION-URL)

.PHONY: module-implementation
module-implementation: $(MODULE-IMPLEMENTATION).tar.gz
	tar xzf $(MODULE-IMPLEMENTATION).tar.gz
	cd $(MODULE-IMPLEMENTATION) && \
		perl Makefile.PL PREFIX=$(TARGETDIR) && \
		make && \
		make install

##
$(PARAMS-VALIDATE).tar.gz:
	wget $(PARAMS-VALIDATE-URL)

.PHONY: params-validate
params-validate: $(PARAMS-VALIDATE).tar.gz
	tar xzf $(PARAMS-VALIDATE).tar.gz
	cd $(PARAMS-VALIDATE) && \
		perl Build.PL --install_base=$(TARGETDIR) --pp && \
		./Build && \
		./Build install

##
$(XML-NAMESPACESUPPORT).tar.gz:
	wget $(XML-NAMESPACESUPPORT-URL)

.PHONY: xml-namespacesupport
xml-namespacesupport: $(XML-NAMESPACESUPPORT).tar.gz
	tar xzf $(XML-NAMESPACESUPPORT).tar.gz
	cd $(XML-NAMESPACESUPPORT) && \
		perl Makefile.PL PREFIX=$(TARGETDIR) && \
		make && \
		make install

##
$(XML-SAX-BASE).tar.gz:
	wget $(XML-SAX-BASE-URL)

.PHONY: xml-sax-base
xml-sax-base: $(XML-SAX-BASE).tar.gz
	tar xzf $(XML-SAX-BASE).tar.gz
	cd $(XML-SAX-BASE) && \
		perl Makefile.PL PREFIX=$(TARGETDIR) && \
		make && \
		make install

##
$(XML-SAX).tar.gz:
	wget $(XML-SAX-URL)


# See also http://perl-xml.sourceforge.net/faq/#parserdetails.ini
.PHONY: xml-sax
xml-sax: $(XML-SAX).tar.gz
	tar xzf $(XML-SAX).tar.gz
	cd $(XML-SAX) && \
		perl Makefile.PL PREFIX=$(TARGETDIR) && \
		make && \
		make install && \
		perl -I$(TARGETDIR)/share/perl5 -MXML::SAX  -e "XML::SAX->add_parser(q(XML::SAX::PurePerl))->save_parsers()"

##
$(XML-SIMPLE).tar.gz:
	wget $(XML-SIMPLE-URL)

.PHONY: xml-simple
xml-simple: $(XML-SIMPLE).tar.gz
	tar xzf $(XML-SIMPLE).tar.gz
	cd $(XML-SIMPLE) && \
		perl Makefile.PL PREFIX=$(TARGETDIR) && \
		make && \
		make install
