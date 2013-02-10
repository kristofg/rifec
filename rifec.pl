#! /usr/bin/perl
#
# RIFEC.pl: Receive Images From Eye-Fi Cards
# Copyright (C) 2011 Kristoffer Gleditsch, https://github.com/kristofg
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.
#

# There are two global objects, ::Config and ::Log, each accessed
# through a global variable. At the bottom of the script, there is a
# plain sub implementing the listen/fork loop on the socket. For each
# incoming connection, it instantiates a ::Handler object. All
# incoming requests on the same connection is sent there. The
# ::Session and ::File objects are helper objects used internally by
# the ::Handler.

require 5.014;

use strict;
use warnings;

my $log;
my $config;

# Config parsing and handling (accessed through the $config object)
package RIFEC::Config {
    use Carp qw(confess);
    use Config::IniFiles;
    use Cwd qw();
    use Data::Dumper;
    use Params::Validate;
    use POSIX qw();

    our $card_section_re     = qr/\A card \s+ (.*?)\z/xi;
    our $filetype_section_re = qr/\A filetype \s+ (.*?)\z/xi;
    our $from_section_re     = qr/\A from \s+ (.*?) \s+ filetype \s+ (.*?)\z/xi;

    sub new {
        validate_pos(@_, 1, 0);
        my ($class, $file) = @_;

        my $self = {};
        bless($self, $class);

        $self->{'_file'}    = $file if @_ > 1;
        $self->{'_cfg'}     = $self->_build_config();
        $self->{'_counter'} = 0;

        return $self;
    }

    sub _file {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->{'_file'};
    }

    sub _cfg {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->{'_cfg'};
    }

    sub counter {
        validate_pos(@_, 1);
        my ($self) = @_;
	return $self->{'_counter'};
    }

    sub bump_counter {
        validate_pos(@_, 1);
        my ($self) = @_;
        return ++$self->{'_counter'};
    }

    sub normalize_mac {
        validate_pos(@_, 1, 1);
        my ($self, $in) = @_;

	my $ret = lc $in;
	$ret =~ s/[-: ]//gx;
        confess "'$in' doesn't look like a MAC address"
            unless $ret =~ m|\A [a-z0-9]{12} \z|xi;

	return $ret;
    }

    sub _read_inifile {
        validate_pos(@_, 1);
        my ($self) = @_;

	# Fall back to default if undef or empty:
	$self->{'file'} = "rifec.config" unless $self->_file;

	# Absolute-ize, so that error messages output a full path:
	$self->{'file'} = Cwd::abs_path($self->_file);

	confess sprintf("Config file '%s' does not exist", $self->_file)
	    unless -e $self->_file;
	confess sprintf("Config file '%s' is not readable", $self->_file)
	    unless -r $self->_file;
	confess sprintf("Config file '%s' is empty", $self->_file)
	    unless -s $self->_file;

        my %cfg;
        tie(%cfg, 'Config::IniFiles', (-file       => $self->_file,
                                       -nocase     => 1,
                                       -allowempty => 0));
	confess "Unable to read config file: ", Dumper(@Config::IniFiles::errors)
	    unless %cfg;
	return \%cfg;
    }

    sub _validate_inifile {
        validate_pos(@_, 1, { type => Params::Validate::HASHREF });
        my ($self, $ini) = @_;

        my %paramspec_of = (
            $card_section_re => {
                'macaddress' => 1,
                'uploadkey'  => 1,
                'folder'     => 0,
                'subfolder'  => 0,
            },
            $from_section_re => {
                'folder'    => 0,
                'subfolder' => 0,
            },
            $filetype_section_re => {
                'folder'    => 0,
                'subfolder' => 0,
            },
            qr/\A main \z/xi => {
                'logfile'             => 1,
                'loglevel'            => 1,
                'port'                => 1,
                'sockettimeout'       => 1,
                'subfoldertimesource' => 0,
                'tarcommand'          => 0,
                'folder'              => 0,
                'subfolder'           => 0,
            },
            );

        foreach my $key (keys %$ini) {
            # Find the paramspec for this type of section:
            my @match = grep { $key =~ $_ } keys %paramspec_of;
            # Every section should match one and only one paramspec:
            if (scalar(@match) != 1) {
                confess sprintf("Unrecognized or malformed config section name '%s'",
                                $key);
            }

            my @inifields = %{$ini->{$key}};
            validate(@inifields, $paramspec_of{$match[0]});
        }
    }

    sub _build_config {
        validate_pos(@_, 1);
        my ($self) = @_;

        my $filecfg = $self->_read_inifile();
        $self->_validate_inifile($filecfg);
        my $c = {};

        # Main section:
        foreach my $s (keys %{ $filecfg->{'main'} }) {
            $c->{'main'}->{$s} = $filecfg->{'main'}->{$s};
        }

        # top-level filetype sections:
        foreach my $ft_section (grep { $_ =~ $filetype_section_re } keys %$filecfg) {
            $ft_section =~ $filetype_section_re
                || confess "Unexpected regexp mismatch";
            my $type = $1;
            foreach my $f (keys %{ $filecfg->{$ft_section} }) {
                $c->{'filetypes'}->{$type}->{$f} = $filecfg->{$ft_section}->{$f};
            }
        }

        # card sections:
        foreach my $card_section (grep { $_ =~ $card_section_re } keys %$filecfg) {
            $card_section =~ $card_section_re
                || confess "Unexpected regexp mismatch";
            my $cardname = $1;
            foreach my $s (keys %{ $filecfg->{$card_section}}) {
                my $value = $filecfg->{$card_section}->{$s};
                if ($s eq 'macaddress') {
                    $value = $self->normalize_mac($value);
                }
                $c->{'cards'}->{$cardname}->{$s} = $value;
            }
            # We are messing around with card names here, but the
            # actual config lookups will be by MAC:
            my $mac = $c->{'cards'}->{$cardname}->{'macaddress'};
            $c->{'macs'}->{$mac} = $c->{'cards'}->{$cardname};
            # Add the name to the hash, so the people who've only got
            # the MAC can get at it:
            $c->{'cards'}->{$cardname}->{'name'} = $cardname;
        }

        # from X filetype Y sections:
        foreach my $from_section (grep { $_ =~ $from_section_re } keys %$filecfg) {
            $from_section =~ $from_section_re
                || confess "Unexptected regexp mismatch";
            my $card = $1;
            my $type = $2;

            confess sprintf("section '%s' doesn't match any card names!",
                            $from_section)
                unless exists $c->{'cards'}->{$card};

            foreach my $s (keys %{ $filecfg->{$from_section} }) {
                $c->{'cards'}->{$card}->{'filetypes'}->{$type}->{$s} =
                    $filecfg->{$from_section}->{$s};
            }
        }

        #print STDERR "Config hash dump: ", Dumper($c), "\n";
        return $c;
    }

    sub say_hello {
        validate_pos(@_, 1);
        my ($self) = @_;

        $log->info("Config file '%s', %d card(s) configured:",
                   Cwd::abs_path($self->_file),
                   scalar keys %{ $self->_cfg->{'cards'} });

        foreach my $card (keys %{ $self->_cfg->{'cards'} }) {
            $log->info("    Card '%s' (%s)",
                       $card,
                       $self->_cfg->{'cards'}->{$card}->{'macaddress'});
        }
    }

    sub doiknow {
        validate_pos(@_, 1, 1);
        my ($self, $mac) = @_;

        $mac = $self->normalize_mac($mac);

        confess sprintf("Sorry, I don't know the card with MAC '%s'", $mac)
            unless exists $self->_cfg->{'macs'}->{$mac};

        return 1;
    }

    sub check_writeable_dir {
        validate_pos(@_, 1, 1);
        my ($self, $dir) = @_;

	confess sprintf("Destination directory '%s' not found", $dir)
	    unless -e $dir;
	confess sprintf("Destination directory '%s' not a directory", $dir)
	    unless -d $dir;
	confess sprintf("Destination directory '%s' not writeable", $dir)
	    unless -w $dir;
    }

    # Actual config accessors.
    sub logfile {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->_cfg->{'main'}->{'logfile'};
    }

    sub loglevel {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->_cfg->{'main'}->{'loglevel'};
    }

    sub port {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->_cfg->{'main'}->{'port'} || 59278;
    }

    sub sockettimeout {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->_cfg->{'main'}->{'sockettimeout'} || 600;
    }

    sub tarcommand {
        validate_pos(@_, 1);
        my ($self) = @_;

        my $tc = $self->_cfg->{'main'}->{'tarcommand'} || "/bin/tar";

        # Verify that the tar command looks sane:
        my $help = "Please tell me where tar is by adding 'TarCommand=/path/to/tar' to the main section of your config file";
        confess "'$tc' not found: $help"
            unless -e $tc;
        confess "'$tc' is not executable: $help"
            unless -x $tc;

        return $tc;
    }

    sub subfoldertimesource {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->_cfg->{'main'}->{'subfoldertimesource'} || "local";
    }

    # Per-card settings:
    sub _mac_setting {
        validate_pos(@_, 1, 1, 1);
        my ($self, $mac, $setting) = @_;

        $self->doiknow($mac);
        $mac = $self->normalize_mac($mac);

        confess sprintf("Error looking up setting '%s' for card '%s'",
                        $setting, $mac)
            unless exists $self->_cfg->{'macs'}->{$mac}->{$setting};

        return $self->_cfg->{'macs'}->{$mac}->{$setting};
    }

    sub cardname {
        validate_pos(@_, 1, 1);
        my ($self, $mac) = @_;
        return $self->_mac_setting($mac, 'name');
    }

    sub uploadkey {
        validate_pos(@_, 1, 1);
        my ($self, $mac) = @_;
        return $self->_mac_setting($mac, 'uploadkey');
    }

    sub _foldersetting {
        validate_pos(@_, 1, 1, 1, 1);
        my ($self, $setting, $mac, $filename) = @_;

        $self->doiknow($mac);
        $mac = $self->normalize_mac($mac);

        my $cfg = $self->_cfg;

        if (defined($filename) && $filename) {
            $filename =~ / \. ([^ \.]*?) \z/xi
                || confess "Unable to extract file type from name '$filename'";
            my $type = lc $1;

            if (exists $cfg->{'macs'}->{$mac}->{'filetypes'}->{$type} &&
                exists $cfg->{'macs'}->{$mac}->{'filetypes'}->{$type}->{$setting})
            {
                return $cfg->{'macs'}->{$mac}->{'filetypes'}->{$type}->{$setting};
            }

            if (exists $cfg->{'filetypes'}->{$type} &&
                exists $cfg->{'filetypes'}->{$type}->{$setting})
            {
                return $cfg->{'filetypes'}->{$type}->{$setting};
            }
        }

        if (exists $cfg->{'macs'}->{$mac}->{$setting}) {
            return $cfg->{'macs'}->{$mac}->{$setting};
        }

        if (exists $cfg->{'main'}->{$setting}) {
            return $cfg->{'main'}->{$setting}
        }

        return;
    }

    sub folder {
        validate_pos(@_, 1, 1, 0);
        my ($self, $mac, $filename) = @_;

        my $f =  $self->_foldersetting('folder', $mac, $filename || '');

        confess sprintf("Error looking up folder for card '%s' file '%s'",
                        $self->_cfg->{'macs'}->{$mac}->{'name'},
                        $filename || '')
            unless defined($f) && $f;

        $self->check_writeable_dir($f);
        return $f;
    }

    sub subfolder {
        validate_pos(@_, 1, 1, 0);
        my ($self, $mac, $filename) = @_;
        return $self->_foldersetting('subfolder', $mac, $filename || '');
    }

    1;
}

# Logging, accessed through the $log object:
package RIFEC::Log {
    use Carp qw(confess);
    use Data::Dumper;
    use HTTP::Date qw();
    use IO::File;
    use Params::Validate;

    sub new {
        validate_pos(@_, 1, 0);
        my ($class, $file) = @_;

        my $self = {};
        bless($self, $class);

        $self->{'_fh'} = \*STDOUT;
        $self->open();

        return $self;
    }

    sub _fh {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->{'_fh'};
    }

    sub DESTROY {
        my ($self) = @_;

        if (defined $self->_fh) {
            $self->_fh->close()
                or confess "Unable to close log filehandle while exiting: $!";
        }
    }

    sub loglevel {
        return {
            'off'     => 100,
            'warning' => 4,
            'info'    => 3,
            'debug'   => 2,
            'trace'   => 1,
        };
    };

    sub open {
        validate_pos(@_, 1);
        my ($self) = @_;

	if (my $lf = $config->logfile()) {
            $self->_fh->close()
                or confess "Unable to close logfile while exiting: $!";

	    my $fh = IO::File->new($lf, O_WRONLY|O_APPEND|O_CREAT)
		or confess "Unable to open logfile '$lf' for writing: $!";
	    $self->{'_fh'} = $fh;
	}
    }

    sub _get_preamble {
        validate_pos(@_, 1, 1);
        my ($self, $ll) = @_;

	return sprintf("%s %7u %7s ",
		       HTTP::Date::time2isoz(),
		       $$,
		       uc $ll);
    }

    sub _print_if {
        validate_pos(@_, 1, 1, (0) x (@_ - 2));
        my ($self, $loglevel, @str) = @_;

	# Sanity check.
	foreach my $ll ($loglevel, $config->loglevel) {
	    confess sprintf("Invalid loglevel '%s'", $ll)
		unless $self->loglevel->{lc $ll};
	}

	if ($self->loglevel->{lc $loglevel} >= $self->loglevel->{lc $config->loglevel})
	{
	    my $out = $self->_get_preamble($loglevel);

	    # figure out if it's a (s)printf or not:
	    if (scalar(@str) == 1) {
		$out .= $str[0];
	    }
	    elsif (scalar(@str) > 1) {
		$out .= sprintf shift(@str), @str;
	    }
	    else {
		confess "If you log, please send content too.";
	    }
	    # \n-terminate if needed
	    $out .= ($out =~ / \n \z/x) ? "" : "\n";

	    print { $self->_fh } $out;
	    $self->_fh->flush();
	}
    }

    sub trace {
        my ($self, @str) = @_;
	$self->_print_if('trace', @str);
    }

    sub debug {
        my ($self, @str) = @_;
	$self->_print_if('debug', @str);
    }

    sub info {
        my ($self, @str) = @_;
	$self->_print_if('info', @str);
    }

    sub warning {
        my ($self, @str) = @_;
	$self->_print_if('warning', @str);
    }

    1;
}

package RIFEC::Session {
    use Carp qw(confess);
    use Data::Dumper;
    use Digest::MD5 qw(md5_hex);
    use IO::File;
    use Params::Validate;

    sub new {
        my $class = shift;
        my %p = validate(@_, { 'card'                  => 1,
                               'transfermode'          => 1,
                               'transfermodetimestamp' => 1,
                               'card_nonce'            => 1, });

        my $self = {};
        bless($self, $class);

        foreach my $key (keys %p) {
            $self->{$key} = $p{$key};
        }
        $self->{'server_nonce'} = $self->_generate_s_nonce();
        $self->{'is_authenticated'} = undef;

        $self->_check_params();

        return $self;
    }

    sub card {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->{'card'};
    }

    sub transfermode {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->{'transfermode'};
    }

    sub transfermodetimestamp {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->{'transfermodetimestamp'};
    }

    sub card_nonce {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->{'card_nonce'};
    }

    sub server_nonce {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->{'server_nonce'};
    }

    sub authenticated {
        validate_pos(@_, 1, 0);
        my ($self, $set) = @_;

        $self->{'is_authenticated'} = $set if @_ > 1;
        return $self->{'is_authenticated'};
    }

    # We used to use the Crypt::Random library for this, but it is not
    # available through the package manager of all Linux
    # distributions, and was just a wrapper around reading the data
    # from /dev/urandom anyway.
    sub getrandom {
        validate_pos(@_, 1, 1);
        my ($self, $bytes) = @_;

	my $random_file = "/dev/urandom";
	my $output;
	my $has_read = 0;

	my $fh = IO::File->new($random_file, O_RDONLY)
	    or confess "Unable to open '$random_file' for reading random data: $!";

	while ($has_read < $bytes) {
	    my $o;
	    my $read_status = $fh->read($o, $bytes - $has_read);

	    if (!defined $read_status) {
		confess "Error while reading random data from '$random_file': $!";
	    }
	    elsif ($read_status == 0) {
		confess "Reached EOF while reading random data from '$random_file'";
	    }
	    $output .= $o;
	    $has_read += $read_status;
	}

	$fh->close()
	    or confess "Unable to close '$random_file': $!";
	return $output;
    }

    sub _generate_s_nonce {
        my ($self) = @_;
	my $octets = $self->getrandom(16);
	return unpack("H*", $octets);
    }

    sub _check_params {
        my ($self) = @_;

	# Only accept cards that have a config section
	$config->doiknow($self->card);

	# Only accept transfer modes that we recognize.  My card sends
	# 546 (0x222), and from basic testing twiddling switches in
	# the config GUI, it looks like this number is a bitmask:
	#
	# 0 1 0 0 0 1 0 0 0 1 0 = 546
	#                   1   =   2 : Transfer photos to computer
	#           1           =  32 : Transfer videos to computer
	#   1                   = 512 : Transfer RAW files to computer
	#
	# I'll go with that theory for now: Either way this approach
	# means all the modes I have seen will work, and the ones I
	# haven't won't.
	my $known_txmodes = 1<<1 | 1<<5 | 1<<9;

	if ($self->transfermode & ~$known_txmodes) {
	    confess sprintf("Unsupported transfermode '%s' from card '%s' (%s)," .
                            " See TROUBLESHOOTING.txt for info about what this means",
                            $self->transfermode,
                            $config->cardname($self->card),
                            $self->card);
	}
    }

    sub server_credential {
        my ($self) = @_;
	return md5_hex(pack("H*", $self->card),
		       pack("H*", $self->card_nonce),
		       pack("H*", $config->uploadkey($self->card)));
    }

    sub card_credential {
        my ($self) = @_;
	return md5_hex(pack("H*", $self->card),
		       pack("H*", $config->uploadkey($self->card)),
		       pack("H*", $self->server_nonce));
    }
}

package RIFEC::File {
    use Carp qw(confess);
    use Data::Dumper;
    use Digest::MD5 qw();
    use File::Spec;
    use File::Temp qw();
    use IO::File;
    use Params::Validate;
    use POSIX qw();

    # If your camera produces files containing other characters than I
    # have thought of here, you may have to change this regexp.  (All
    # places checking the file name should use this, so it should be
    # sufficient to update this one place).
    our $filename_regexp = qr|\A [ a-z0-9._-]* \z|xi;

    sub new {
        my $class = shift;
        my %p = validate(@_, { 'session'       => { isa => 'RIFEC::Session' },
                               'tarfilename'   => 1,
                               'size'          => 1,
                               'encryption'    => 0,
                               'filesignature' => 1, });

        my $self = {};
        bless($self, $class);

        foreach my $key (keys %p) {
            $self->{$key} = $p{$key};
        }
        return $self;
    }

    # RO parameters:
    sub session {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->{'session'};
    }

    sub tarfilename {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->{'tarfilename'};
    }

    sub size {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->{'size'};
    }

    sub encryption {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->{'encryption'};
    }

    sub filesignature {
        validate_pos(@_, 1);
        my ($self) = @_;
        return $self->{'filesignature'};
    }

    # RW parameters:
    sub integritydigest {
        validate_pos(@_, 1, 0);
        my ($self, $set) = @_;

        $self->{'integritydigest'} = $set if @_ > 1;
        return $self->{'integritydigest'};
    }

    sub calculated_digest {
        validate_pos(@_, 1, 0);
        my ($self, $set) = @_;

        $self->{'calculated_digest'} = $set if @_ > 1;
        return $self->{'calculated_digest'};
    }

    sub tarfile {
        validate_pos(@_, 1, 0);
        my ($self, $set) = @_;

        $self->{'tarfile'} = $set if @_ > 1;
        return $self->{'tarfile'};
    }

    sub stored_file {
        validate_pos(@_, 1, 0);
        my ($self, $set) = @_;

        $self->{'stored_file'} = $set if @_ > 1;
        return $self->{'stored_file'};
    }

    # See http://forums.eye.fi/viewtopic.php?f=4&t=270#p3874
    sub _calculate_tcp_checksum {
	my ($self, $block) = @_;
	my $val = 0;

	map { $val += $_ } unpack "S*", $block;

        while (my $rest = $val >> 16) {
	    $val = ($val & 0xFFFF) + $rest;
	}
        return pack("S", ~$val);
    }

    # Calculating this while receiving the file, ie. in
    # Handler::read_socket, would be much faster.  However, by doing
    # it afterwards we get a sanity check that read_part/read_socket
    # didn't mess up our data in transit.  Belt and braces.
    sub _calculate_integritydigest {
        my ($self) = @_;

        my $file = $self->tarfile;
	$log->debug("Calculating integrity digest of '%s'", $file);

        my $blocksize = 512;

	# Make sure it is 512-block aligned (it should be)
        my $length = (stat($file))[7];
	confess "Tar file not 512-byte aligned!"
	    if $length % $blocksize;

	my $md5 = Digest::MD5->new();
	my $fh = IO::File->new($file, O_RDONLY)
	    or confess "Unable to open '$file': $!";

	my $i = 0;
	while ($i < $length) {
            my $block;
	    my $read_status = $fh->read($block, $blocksize);

            confess "Error while reading from '$file': $!"
                unless defined $read_status;

            confess "Reached EOF while reading from '$file'"
                if $read_status == 0;

            confess "Unexpected read length [$read_status] from '$file'"
                unless $read_status == $blocksize;

	    $md5->add( $self->_calculate_tcp_checksum($block) );
	    $i += $blocksize;
	}

	$fh->close()
	    or confess "Unable to close '$file': $!";

	$md5->add(pack("H*", $config->uploadkey($self->session->card)));
	my $md5sum = $md5->hexdigest();

	$log->debug("...done: %s", uc $md5sum);
        $self->calculated_digest(uc $md5sum);
	return uc $md5sum;
    }

    sub receiver_filehandle {
        validate_pos(@_, 1, 1);
        my ($self, $inner_filename) = @_;

	my $folder = $config->folder($self->session->card);
        $config->check_writeable_dir($folder);

	my $tfh = File::Temp->new(
	    TEMPLATE => sprintf(".rifec-receiving-%d--%s--XXXXXXXX",
                                $$,
                                $inner_filename),
	    DIR      => $folder,
	    UNLINK   => 0);
	my $tfn = $tfh->filename;

	$self->tarfile($tfn); # Remember where we put it
	$log->debug("Receiver file: '%s' ('%s')", $tfn, $self->tarfilename);
        return $tfh;
    }

    sub check {
        validate_pos(@_, 1);
        my ($self) = @_;

	# First that the size matches what the card said:
	my $stat_size = (stat($self->tarfile))[7];
	if ($stat_size == $self->size)
	{
	    $log->debug("File '%s' is %d bytes long, as expected",
			$self->tarfile,
			$self->size);
	}
	else {
	    $log->warning("File '%s' is %d bytes long, should have been %d",
                          $self->tarfile,
                          $stat_size,
                          $self->size);
	    return;
	}

	# Then check the integrity digest field:
        $self->_calculate_integritydigest();

	if (uc($self->calculated_digest()) eq uc($self->integritydigest()))
	{
	    $log->debug("Integritydigest OK: [%s]", uc $self->integritydigest());
	}
	else {
	    $log->warning("Integritydigests does not match!");
	    $log->warning(" Calculated: [%s]\n   Received: [%s]",
                          uc $self->calculated_digest(),
                          uc $self->integritydigest());
	    return;
	}
	return 1;
    }

    sub _subfolder {
        validate_pos(@_, 1, 1, 1);
        my ($self, $tmpimage, $imagefilename) = @_;

        my $card = $self->session->card;

        my $topfolder = $config->folder($card, $imagefilename);
        my $subfolder;
        my $subtemplate = $config->subfolder($card, $imagefilename);

        # We can create sub folders, but the top folders should be there:
        $config->check_writeable_dir($topfolder);

        if (!$subtemplate || $subtemplate =~ /\A \s* \z/mxi) {
            $log->debug("Card '%s' file '%s': Root folder '%s', empty/blank subfolder",
                        $config->cardname($card),
                        $imagefilename,
                        $topfolder);
            return $topfolder;
        }

        if ($config->subfoldertimesource() ne 'local') {
            confess "Sorry, other time sources than 'local' is not implemented yet";
        }
        $subfolder = POSIX::strftime($subtemplate, localtime());

        my $destination = File::Spec->catfile($topfolder, $subfolder);
        $log->debug("Destination subdirectory: '%s' -> '%s', full path: '%s'",
                    $subtemplate,
                    $subfolder,
                    $destination);

        # Next, create it.  Note that the subfolder may be
        # $topfolder/X/Y/Z, not just $topfolder/X, so mkdir one level
        # at a time:
        my @path_elements = File::Spec->splitdir($subfolder);
        my $makedir = $topfolder;
        foreach my $pe (@path_elements) {
            $makedir = File::Spec->catfile($makedir, $pe);
            if (! -e $makedir) {
                $log->trace("mkdir '$makedir'");
                mkdir $makedir
                    or confess "Unable to mkdir '$makedir': $!";
            }
        }

        # Final sanity check:
        $config->check_writeable_dir($destination);
        return $destination;
    }

    # We never want to overwrite existing files, but we don't want the
    # card to be stuck with files because we're not able to write them
    # to disk either.  So we receive it, but add .1 (or .2, or .n+1)
    # to the filename.  The user will have to sort out duplicates
    # afterwards.
    #
    # (We *assume* that if the link() fails, it is because the
    # filename already exists.)
    sub _link_file {
        validate_pos(@_, 1, 1, 1);
        my ($self, $tempfile, $destination_name) = @_;

	my $tries = 0;
	my $max   = 100;
	my $dst   = $destination_name;
	my $done  = undef;

	while (!$done && $tries < $max) {
	    if (link $tempfile, $dst) {
		$done = $dst;
		$log->debug("'%s' created OK (linked from '%s')", $dst, $tempfile);
	    }
	    else {
		my $prev_dst = $dst;
		$dst = sprintf("%s.%d", $destination_name, ++$tries);
		$log->warning("'%s' already exists, trying '%s'", $prev_dst, $dst);
	    }
	}

	confess sprintf("Unable to write '%s': Destination files already there!",
                        $dst)
	    unless $done;

	return $dst;
    }

    sub _extract_tarfile {
        validate_pos(@_, 1);
        my ($self) = @_;

        my $tar_cmd  = $config->tarcommand;
        my $tar_file = $self->tarfile;

        my @files = `$tar_cmd -tf $tar_file`;
        # Remove leading and trailing whitespace from each element:
        foreach my $f (@files) {
            $f =~ s/\A \s*//xg;
            $f =~ s/\s* \z//xg;
        }
        $log->debug("Files in tarball on disk: %s", join(", ", @files));

	confess sprintf("I don't know how to handle tarballs with >1 files! (%s)",
                        join(", ", @files))
	    if scalar(@files) > 1;

	my $fn = shift @files;
	confess sprintf("Illegal name of file inside tarball: '%s'", $fn)
	    unless $fn =~ $RIFEC::File::filename_regexp;

	my $tfh = File::Temp->new(
	    TEMPLATE => sprintf(".rifec-untarred-%d--%s--XXXXXXXX", $$, $fn),
	    DIR      => $config->folder( $self->session->card ),
	    UNLINK   => 0);
	my $tfn = $tfh->filename;

	$log->debug("Writing target file '%s' to tempfile '%s'", $fn, $tfn);
        # tar -xOf /tmp/movfile.tar DSC_1720.MOV > fnordmovie.mov
        my $extract_command = "$tar_cmd -xOf $tar_file $fn > $tfn";
        $log->trace("Extract command: '$extract_command'");
        {
            # unset SIGCHLD handler, since system() expects to wait on
            # its children:
            local $SIG{CHLD} = '';
            my $status = system($extract_command);
            confess "Failed to run '$tar_cmd': $!\n"
                if $status == -1;
            confess "Failure exit status from '$tar_cmd': " . $status >> 8
                if $status != 0;
        }
	$tfh->flush() or confess "Unable to flush '$tfn': $!";
	$tfh->sync()  or confess "Unable to sync '$tfn': $!";
	$tfh->close() or confess "Unable to close '$tfn': $!";

	# Return the filename of the file in the tarball plus the
	# tempfile this file is currently stored in:
	return ($fn, $tfn);
    }

    sub extract {
        validate_pos(@_, 1);
        my ($self) = @_;

	# Extract the tar file and save the contents to a temp file:
	my ($filename, $tmpfilename) = $self->_extract_tarfile();

        # Create the destination sub folder if necessary:
        my $dest_folder = $self->_subfolder($tmpfilename, $filename);

	# Do the hard linking from the final file name to the temp
	my $full_filename = File::Spec->catfile($dest_folder, $filename);

	my $outfile = $self->_link_file($tmpfilename, $full_filename);

	$log->warning("Destination file '%s' saved as '%s' to avoid collision",
                      $full_filename, $outfile)
	    unless $outfile eq $full_filename;

	$self->stored_file($outfile); # Remember where we put it

	$log->debug("Removing tar file '%s'", $self->tarfile());
        unlink $self->tarfile
            or confess sprintf("Unable to unlink tarfile '%s': $!",
                               $self->tarfile);

	$log->debug("Removing temp file '%s'", $tmpfilename);
        unlink $tmpfilename
            or confess "Unable to unlink tempfile '$tmpfilename': $!";

	# Chmod it to use the default umask
	chmod 0666 & ~umask(), $self->stored_file
	    or $log->warning("Unable to chmod '%s'", $self->file);

	$log->info("File '%s' saved", $self->stored_file);
	return 'ok';
    }

    1;
}

package RIFEC::Handler {
    use Carp qw(confess);
    use Data::Dumper;
    use Encode qw();
    use HTTP::Message;
    use HTTP::Status qw(:constants);
    use Params::Validate;
    use Socket qw();
    use XML::Simple qw(:strict);

    sub new {
        my $class = shift;

        my $self = {};
        bless($self, $class);

        return $self;
    }

    sub session {
        validate_pos(@_, 1, { optional => 1, isa => 'RIFEC::Session' });
        my ($self, $set) = @_;

        $self->{'session'} = $set if @_ > 1;
        return $self->{'session'};
    }

    # We jump through some hoops to make XML::Simple output the SOAP
    # response the way we want it instead of having to template-write
    # the XML ourselves.  Most of those hoops are hidden here:
    sub _wrap_response {
        validate_pos(@_, 1, 1, 1);
        my ($self, $blockname, $values) = @_;

	$log->trace($blockname . ": " . Dumper($values));

	# { a => b, c => d } --> { a => [ b ], c => [ d ] }
	foreach my $key (keys %$values) {
	    if (!ref($values->{$key})) {
		$values->{$key} = [ $values->{$key} ];
	    }
	}
	# Add XML namespace attribute:
	$values->{'xmlns'} = "http://localhost/api/soap/eyefilm";

	my %wrap = (
	    'SOAP-ENV:Envelope'  => {
		'xmlns:SOAP-ENV' => 'http://schemas.xmlsoap.org/soap/envelope/',
		'SOAP-ENV:Body'  => {
		    $blockname => [ $values ]
		},
	    });

	return \%wrap;
    }

    # Do basic sanity checking of the parameters coming in.
    # XML::Simple will fail on XML syntax, this sub does some reality
    # checking on the contents.
    sub _extract_params {
        validate_pos(@_, 1, { type => Params::Validate::HASHREF }, 1);
        my ($self, $body, $bodyname) = @_;

        # The filename regexp lives in the ::File class since it is
        # used in other places as well
        my $md5sum       = qr|\A [a-z0-9]{32} \z|xi;
        my $macaddress   = qr|\A [a-z0-9]{12} \z|xi;
        # We are not too concerned with the length of the number: We
        # do syntax checking here, the handling code can take care of
        # the semantics.
        my $number       = qr|\A \d+          \z|xi;
        # Again, no too worried about the length of the string, as
        # long as it's syntactically correct:
        my $simplestring = qr|\A [a-z]+       \z|xi;

        my %paramspec_of = (
            "ns1:StartSession" => {
                'macaddress'            => { regex => $macaddress },
                'transfermodetimestamp' => { regex => $number     },
                'cnonce'                => { regex => $md5sum     },
                'transfermode'          => { regex => $number     },
            },
            "ns1:GetPhotoStatus" => {
                'filesize'      => { regex => $number                        },
                'flags'         => { regex => $number                        },
                'filename'      => { regex => $RIFEC::File::filename_regexp  },
                'macaddress'    => { regex => $macaddress                    },
                'credential'    => { regex => $md5sum                        },
                'filesignature' => { regex => $md5sum                        },
            },
            "ns1:UploadPhoto" => {
                'filesize'      => { regex => $number                        },
                'flags'         => { regex => $number                        },
                'filename'      => { regex => $RIFEC::File::filename_regexp  },
                'macaddress'    => { regex => $macaddress                    },
                'fileid'        => { regex => $number                        },
                'encryption'    => { regex => $simplestring                  },
                'filesignature' => { regex => $md5sum                        },
            },
            "ns1:MarkLastPhotoInRoll" => {
                'macaddress'    => { regex => $macaddress },
                'mergedelta'    => { regex => $number     },
            },
        );

        # Params::Validate has checked that $body is a HashRef
        # already, so we can go straight on the keys inside it:
        confess "No element '$bodyname' in body"
            unless exists $body->{$bodyname} && defined $body->{$bodyname};
        confess "Element '$bodyname' is not a hash ref"
            unless ref($body->{$bodyname}) eq 'HASH';

        Params::Validate::validation_options('stack_skip' => 2);
        my %p = validate( @{[ $body->{$bodyname} ]}, $paramspec_of{$bodyname} );
        Params::Validate::validation_options('stack_skip' => 1);

        return \%p;
    }

    sub startsession {
        validate_pos(@_, 1, { type => Params::Validate::HASHREF });
        my ($self, $soapbody) = @_;

	my $params = $self->_extract_params($soapbody, "ns1:StartSession");

	$log->info("StartSession from '%s' (%s)",
		   $config->cardname($params->{'macaddress'}),
		   $params->{'macaddress'});
	$log->trace("StartSession: " . Dumper($params));

	my $s = RIFEC::Session->new(
	    'card'                  => $params->{'macaddress'},
	    'card_nonce'            => $params->{'cnonce'},
	    'transfermode'          => $params->{'transfermode'},
	    'transfermodetimestamp' => $params->{'transfermodetimestamp'});

	$self->session($s);

	return $self->_wrap_response(
	    'StartSessionResponse',
	    {
		'credential'            => $s->server_credential(),
		'snonce'                => $s->server_nonce(),
		'transfermode'          => $s->transfermode(),
		'transfermodetimestamp' => $s->transfermodetimestamp(),
		'upsyncallowed'         => 'false',
	    });
    }

    sub getphotostatus {
        validate_pos(@_, 1, { type => Params::Validate::HASHREF });
        my ($self, $soapbody) = @_;

	my $params = $self->_extract_params($soapbody, "ns1:GetPhotoStatus");

	$log->info("GetPhotoStatus for '%s' from '%s' (%s)",
		   $params->{'filename'},
		   $config->cardname($self->session->card),
		   $self->session->card);
	$log->trace("GetPhotoStatus: " . Dumper($params));

	my $s = $self->session();

	# Verify a valid session & credential: This is the credential
	# check before uploading photos.
	if (lc $params->{'macaddress'} ne lc $s->card())
	{
	    confess sprintf("MAC from card != session MAC (%s != %s)",
                            lc $params->{'macaddress'},
                            lc $s->card());
	}
	if (lc $params->{'credential'} ne lc $s->card_credential())
	{
	    confess sprintf("Card credential invalid (%s != %s)",
                            lc $params->{'credential'},
                            lc $s->card_credential());
	}

	$s->authenticated(1);

	return $self->_wrap_response(
	    'GetPhotoStatusResponse',
	    {
		'fileid' => $config->counter,
		'offset' => '0',
	    });
    }

    sub init_file_object {
        validate_pos(@_, 1, 1);
        my ($self, $body) = @_;

        my $soapbody;
        my $eval_result = eval {
            $soapbody = XML::Simple::XMLin($body,
                                           ForceArray => 0,
                                           KeyAttr    => []);
        };
        if (!defined $eval_result) {
            confess "XML::Simple::XMLin died: $@";
        }

        confess "Unable to find SOAP Body in Upload envelope"
            unless (ref($soapbody) eq 'HASH' &&
                    exists $soapbody->{'SOAP-ENV:Body'});

	my $pi = $self->_extract_params($soapbody->{'SOAP-ENV:Body'},
                                        "ns1:UploadPhoto");
	$log->trace("Upload SoapEnvelope: " . Dumper($pi));

	return RIFEC::File->new(
	    'tarfilename'   => $pi->{'filename'},
	    'size'          => $pi->{'filesize'},
	    'encryption'    => $pi->{'encryption'},
	    'filesignature' => $pi->{'filesignature'},
	    'session'       => $self->session());
    }

    sub read_socket {
        my ($self, $conn, $len) = @_;
        confess "Wrong connection type object"
            unless $conn->isa('HTTP::Daemon::ClientConn');

        my $content = $conn->read_buffer('');
        my $read_count = length($content);
        my $blocksize = 1024;

        my $fdset = '';
        vec($fdset,fileno($conn),1) = 1;

        while ($read_count < $len) {
            $blocksize = $len - $read_count
                if $blocksize > ($len - $read_count);

            my $into;

            my $n = select($fdset, undef, undef, $config->sockettimeout);
            confess "select() timed out waiting for socket"
                if $n == 0;
            confess "select() returned error: $!"
                if $n < 0;

            my $s = sysread($conn, $into, $blocksize);
            confess "Read failed!"
                unless defined $s;
            confess "Reached EOF!"
                if $s == 0;

            $content    .= $into;
            $read_count += length $into;
        }

        if ($read_count > $len) {
            my $tail = substr $content, $len;
            $content = substr $content, 0, $len;
            $conn->read_buffer($tail);
        }
        return $content;
    }

    sub stow_away {
        my ($self, $buffer, $keep, $to) = @_;

        my $stow;
        my $giveback;

        if ($keep == 0) {
            $stow = $buffer;
            $giveback = '';
        }
        else {
            $stow = substr $buffer, 0, -$keep;
            $giveback = substr $buffer, -$keep;
        }

        if (ref($to) eq 'SCALAR') {
            $$to .= $stow;
        }
        elsif (ref($to) eq 'File::Temp') {
            print $to $stow;
        }
        else {
            confess "Need a better place to stow!";
        }
        return $giveback;
    }

    sub read_part {
        validate_pos(@_, 1,
                     { isa => 'HTTP::Daemon::ClientConn' },
                     1, 1, 0);
        my ($self, $conn, $boundary, $maxlen, $to_file) = @_;

        my $default_bs = 1024;
        my $overlap = length($boundary) + 8; # \r\n*2 + --*2 = 8
        my $out_var;
        my $to = $to_file || \$out_var;

        my $header = '';
        my $buf    = '';
        my $read_len = 0;

      READ:
        while ($read_len < $maxlen) {
            my $left = $maxlen - $read_len;
            my $readblock = $left < $default_bs ? $left : $default_bs;

            my $add = $self->read_socket($conn, $readblock);

            $buf      .= $add;
            $read_len += length($add);

            # Are we done?
            if ($buf =~ /\A (.*?) (\r?\n)? -- \Q$boundary\E (--)? \r?\n (.*) \z/msxi) {
                my ($keep, $tail) = ($1, $4);

                # The tail needs to go back to the front of the read buffer:
                $conn->read_buffer($tail . $conn->read_buffer(''));
                $read_len -= length($tail);

                # If the body is empty or small enough to match on the
                # first round, we need to chop off the header:
                if (!$header &&
                    $keep =~ s/\A (.*?) \r?\n \r?\n//msxi) {
                    $header = $1;
                }

                $self->stow_away($keep, 0, $to);
                last READ;
            }
            elsif (!$header &&
                   $buf =~ s/\A (.*?) \r?\n \r?\n//msxi)
            {
                $header = $1;
            }
            # Don't start stowing away stuff until we have found the
            # header separator
            if ($header)
            {
                $buf = $self->stow_away($buf, $overlap, $to);
            }
        }
        return $header, $out_var, $maxlen-$read_len;
    }

    sub upload {
        validate_pos(@_, 1,
                     { isa => 'HTTP::Daemon::ClientConn' },
                     { isa => 'HTTP::Request' });
        my ($self, $conn, $request) = @_;

	$log->info("Upload from '%s' (%s)",
		   $config->cardname($self->session->card),
		   $self->session->card);

	confess "Session un-authenticated, upload is a no-go"
	    unless $self->session->authenticated;

        my $left = $request->header('Content-Length');
        my $boundary;
        if ($request->header('Content-Type') =~
            m|\A multipart/form-data; \s* boundary=([^\s;,]+) [,;]? \s* \z|xi) {
            # This is not very general or robust MIME parsing - but
            # then again, we are not a general SOAP server.
            $boundary = $1;
        }
        else {
            confess sprintf("Unrecognized Content-Type header '%s'",
                            $request->header('Content-Type'));
        }

        my ($phead, $pbody);
        # Swallow everything up to and including the starting
        # delimiter:
        (undef, undef, $left) = $self->read_part($conn, $boundary, $left);

        # Process the SOAPENVELOPE part:
        $log->trace("Upload: Processing SOAPENVELOPE part");
        ($phead, $pbody, $left) = $self->read_part($conn, $boundary, $left);

        confess "Unable to verify start of SOAPENVELOPE part"
            unless $phead =~ / name="SOAPENVELOPE"/;

        my $file = $self->init_file_object($pbody);

        # Process the FILENAME part, ie. the file itself:
        $log->trace("Upload: Processing FILENAME part");
        my $recv_fh = $file->receiver_filehandle($file->tarfilename);

        ($phead, undef, $left) = $self->read_part($conn, $boundary, $left, $recv_fh);
	$recv_fh->close()
            or confess "Unable to close receiver FH: $!";

        confess "Unable to verify start of FILENAME part"
            unless $phead =~ / name="FILENAME"/;

        confess "Unable to extract filename from part header"
            unless my ($fn) = $phead =~ /filename="(.*?)"/x;

        confess "File name differs from RIFEC::File state"
            unless $fn eq $file->tarfilename();

        # Last comes the INTEGRITYDIGEST part:
        $log->trace("Upload: Processing INTEGRITYDIGEST part");
        ($phead, $pbody, $left) = $self->read_part($conn, $boundary, $left);

        confess "Unable to verify start of INTEGRITYDIGEST part"
            unless $phead =~ / name="INTEGRITYDIGEST"/;

        $file->integritydigest($pbody);

        # Should we crash if $file->check() fails?  I am thinking not:
        # We log a warning and tell the card whether the operation
        # succeeded or not anyway, so I can't see any extra value
        # added by confess().
	my $ok = $file->check() && $file->extract();

	return $self->_wrap_response(
	    'UploadPhotoResponse',
	    {
		'success' => $ok ? 'true' : 'false',
	    });
    }

    sub marklastphotoinroll {
        validate_pos(@_, 1, { type => Params::Validate::HASHREF });
        my ($self, $soapbody) = @_;

	my $params = $self->_extract_params($soapbody, "ns1:MarkLastPhotoInRoll");

	$log->info("MarkLastPhotoInRoll from '%s' (%s)",
		   $config->cardname($self->session->card),
		   $self->session->card);
	$log->trace("MarkLastPhotoInRoll: " . Dumper($params));

	# As far as I can figure out, there is not much for us to do
	# here.
	return $self->_wrap_response('MarkLastPhotoInRollResponse', {});
    }

    sub _make_http_reply {
        validate_pos(@_, (1) x 4);
        my ($self, $status, $message, $body) = @_;

	# Enforce CRLF:
	$body =~ s/ ([^\r]) \n /$1\r\n/gx;
	my $raw = Encode::encode_utf8($body);

	my $header = HTTP::Headers->new();

	$header->content_type         ('text/xml');
	$header->content_type_charset ('UTF-8');
	$header->content_length       (length($raw));
	$header->server               ('rifec.pl');
	$header->date                 (time);
	$header->header               ('pragma' => 'no-cache');

	return HTTP::Response->new($status, $message, $header, $raw);
    }

    sub dispatch {
        validate_pos(@_, 1,
                     { isa => 'HTTP::Request' },
                     { isa => 'HTTP::Daemon::ClientConn' });
        my ($self, $request, $conn) = @_;

	my $reply;
	my %handlerof = ('"urn:StartSession"'        => \&startsession,
			 '"urn:GetPhotoStatus"'      => \&getphotostatus,
			 '"urn:MarkLastPhotoInRoll"' => \&marklastphotoinroll);

	my $eval_result = eval {
	    my $answer;

	    if ($request->method eq 'POST' &&
		$request->uri->path eq "/api/soap/eyefilm/v1")
            {
		my $action = $request->header('SOAPAction');
                my $length = $request->header('Content-Length');

                my $content = $self->read_socket($conn, $length);

                my $body;
                my $xml_eval_result = eval {
                    $body = XML::Simple::XMLin($content,
                                               ForceArray => 0,
                                               KeyAttr    => []);
                };
                if (!defined $xml_eval_result) {
                    confess "XML::Simple::XMLin died: $@";
                }

		if (ref($handlerof{$action}) eq 'CODE') {
		    $answer = $handlerof{$action}->($self, $body->{'SOAP-ENV:Body'});
		}
		else {
		    confess "Found no handler for [$action]";
		}
	    }
	    elsif ($request->method eq 'POST' &&
		   $request->uri->path eq "/api/soap/eyefilm/v1/upload")
	    {
		$answer = $self->upload($conn, $request);
	    }
	    else {
		confess sprintf("Unknown method/path combo: '%s' '%s'",
                                $request->method,
                                $request->uri->path);
	    }

	    $reply = $self->_make_http_reply(HTTP_OK, "OK",
					     XML::Simple::XMLout($answer,
								 KeepRoot => 1,
								 XMLDecl  => 1,
                                                                 KeyAttr  => []));
	};
        if (!defined $eval_result) {
	    $log->warning("Died in request handling: " . $@);
	    $reply = $self->_make_http_reply(HTTP_INTERNAL_SERVER_ERROR,
					     "Internal server error",
					     "<title>My handler died :(</title>");
	}
	return $reply;
    }

    1;
}

#
# End of class declarations
#

use Carp qw(confess);
use Data::Dumper;
use Getopt::Long;
use HTTP::Daemon;
use HTTP::Status;
use Pod::Usage;
use Proc::Daemon;

# A plain sub containing the listen/fork loop:
sub run_listener {
    local $SIG{CHLD} = 'IGNORE';

    my $d = HTTP::Daemon->new(LocalPort => $config->port(),
                              ReuseAddr => 1)
        || confess "Unable to instantiate HTTP Daemon: $!";
    $log->info("Listening on %s", $d->url());

    while (my $conn = $d->accept())
    {
        $log->debug("Connect from %s:%d", $conn->peerhost(), $conn->peerport());
        $config->bump_counter();

        my $pid = fork();
        if ($pid == 0) { # Child
            $conn->timeout($config->sockettimeout());
            my $handler = RIFEC::Handler->new();

            while (my $req = $conn->get_request(1))
            {
                $log->debug("%s:%d -> %s %s",
                            $conn->peerhost(), $conn->peerport(),
                            $req->method, $req->uri->path);
                $log->trace("Request headers: " .
                            Dumper([ split /\r?\n/, $req->headers_as_string() ]));

                # All sanity checking is done in the handler
                my $http_reply = $handler->dispatch($req, $conn);

                $conn->send_response($http_reply);
            }
            $log->debug("Closed connection!");

            $conn->close();
            undef($conn);
            undef($handler);
            exit 0;
        }
        else { # Parent
            $log->debug("Child %d forked, going back to accept()", $pid);
        }
    }
}

# We don't support a lot of command line options, we defer to the
# config file for configuration.  However, we need a --help, and a
# possibility to set custom config files:

my $cf_file;
my $daemonize;

GetOptions('help|h|?'    => sub { pod2usage(0) },
	   'config|c=s'  => \$cf_file,
	   'daemonize|d' => \$daemonize)
    or pod2usage(2);

$config    = RIFEC::Config->new($cf_file);
$log       = RIFEC::Log->new();

$config->say_hello();

if ($daemonize) {
    if (!$config->logfile()) {
	$log->warning("Daemon mode enabled, but no logfile specified: " .
                      "Logs and error messages will disappear, " .
                      "consider adding LogFile=/path/to/file to your config");
    }
    Proc::Daemon::Init();
    $log->open(); # since Proc::Daemon::Init closes all open fh's
}

run_listener();

__END__

=head1 NAME

rifec.pl - receive and store files from Eye-Fi cards

=head1 SYNOPSIS

rifec.pl [--help|-h|-?] [--config configfile] [--daemonize|-d]

 Options:
  --help|-h|-?         Print this help message
  --config|-c CFGFILE  Use CFGFILE instead of default config location
  --daemonize|-d       Run as a daemon in the background

For modifying the behaviour of the program in other ways, it is
necessary to use the config file.  See the file C<rifec.config>, which
contains a commented example configuration.

=cut
