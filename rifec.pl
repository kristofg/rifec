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

use strict;
use warnings;

use MooseX::Declare;

# One global var for the log object, and one for the config object.
my $log;
my $config;

# Config parsing and handling (accessed through the $config object)
class RIFEC::Config {
    use Config::IniFiles;
    use Cwd qw();
    use Data::Dumper;
    use Carp qw(confess);
    use POSIX qw();

    has 'file' => (is       => 'rw');

    has '_cfg' => (isa      => 'Config::IniFiles',
		   is       => 'ro',
		   lazy     => 1,
		   builder  => '_read_inifile');

    has '_card' => (isa     => 'HashRef',
		    is      => 'ro',
		    lazy    => 1,
		    builder => '_build_cardlist');

    # Yes, this is slightly redundant: We could have used the _card hash
    has '_known_cards' => (isa     => 'ArrayRef',
			   is      => 'rw',
			   default => sub { [] });

    # Share this across all uploads in a session, so we don't hand out
    # exactly the same file id all the time
    has '_counter' => (isa => 'Int', is => 'rw', default => 0);

    method _read_inifile() {
	# Fall back to default if undef or empty:
	$self->file("rifec.config") unless $self->file;

	# Absolute-ize, so that error messages output a full path:
	$self->file( Cwd::abs_path($self->file) );

	confess sprintf("Config file '%s' does not exist", $self->file)
	    unless -e $self->file;
	confess sprintf("Config file '%s' is not readable", $self->file)
	    unless -r $self->file;
	confess sprintf("Config file '%s' is empty", $self->file)
	    unless -s $self->file;

	my $c = Config::IniFiles->new(-file       => $self->file,
				      -nocase     => 1,
                                      -allowempty => 0,
				      -default    => 'main');
	confess "Unable to read config file: ", Dumper(@Config::IniFiles::errors)
	    unless defined($c) && $c;

	return $c;
    }

    method _normalize_mac(Str $in) {
	my $ret = $in;
	$ret =~ s/[-: ]//gx;

        confess "'$in' doesn't look like a MAC address"
            unless $ret =~ m|\A [a-z0-9]{12} \z|xi;

	return lc $ret;
    }

    method _verify_card_settings(HashRef $settings) {
        # Verify that the settings we must have to save pictures are
        # set:
        foreach my $key ('uploadkey', 'folder') {
            confess sprintf("Missing or blank '%s' for card '%s'",
                            $key, $settings->{'name'})
                unless $settings->{$key};
        }

        # Verify that the subfolder settings - if set - make sense and
        # can be used. (It's a no-op if SubFolder is blank, but that's
        # no reason to put garbage into it)
        my $ts = lc $settings->{'subfoldertimesource'};
        if ($ts ne "exif" && $ts ne "local") {
            confess "Unrecognized SubFolderTimeSource '$ts'";
        }
    }

    method _build_cardlist() {
	my $c = $self->_cfg()
	    || confess "Configuration not initialized yet!";
	my %card;
	$self->_known_cards( () );

	foreach my $s ($c->Sections()) {
	    next if $s eq 'main';

	    if ($s =~ /\A card \s+ (.*?) \z/xi) {
		my $cardname = $1;
		my $mac = $self->_normalize_mac( $self->_get($s, 'MacAddress') );

		confess sprintf("Card '$mac' defined multiple times: '%s', '%s'",
                                $card{$mac}->{'name'}, $cardname)
		    if exists $card{$mac};

		$card{$mac} = {
                    'name'                => $cardname,
                    'uploadkey'           => $self->_get($s, 'UploadKey'),
                    'folder'              => $self->_get($s, 'Folder'),
                    'subfolder'           => $self->_get($s, 'SubFolder', 1),
                    'subfoldertimesource' => $self->_get($s, 'SubFolderTimeSource', 1),
                };
                # Default values for blanks:
                $card{$mac}->{'subfoldertimesource'} ||= "exif";
                $self->_verify_card_settings($card{$mac});

		push(@{ $self->_known_cards }, $mac);
            }
	    else {
		confess sprintf("I don't know what to do with section '%s'", $s);
	    }
	}
	return \%card;
    }

    method _get(Str $section, Str $param, Bool $optional? = 0) {
	my $v = $self->_cfg()->val($section, $param);

        # if not optional, it can still be blank, but it must be
        # present, ie. defined:
	confess sprintf("Can't find config value '%s' in section '%s'",
                        $param, $section)
	    unless $optional || defined $v;

	return $v;
    }

    method _cardsetting(Str $card, Str $setting) {
	my $mac = $self->_normalize_mac($card);
        $self->doiknow($mac);
	return $self->_card->{$mac}->{$setting};
    }

    method say_hello() {
	$log->info("Config file '%s', %d card(s) configured:",
                   Cwd::abs_path($self->file),
                   scalar keys %{ $self->_card });

        foreach my $card (keys %{ $self->_card }) {
            $log->info("    Card '%s' (%s), writing to '%s' + '%s'",
                       $self->cardname($card),
                       $card,
                       $self->folder($card),
                       $self->subfolder($card) || '');
        }
    }

    method doiknow(Str $card) {
	my $mac = $self->_normalize_mac($card);

	confess sprintf("Sorry, I don't know the card with MAC '%s'", lc $mac)
	    unless grep { $mac eq $_ } @{ $self->_known_cards };

	return 1;
    }

    # Actual config accessors:
    method loglevel() {
	return $self->_get('main', 'LogLevel');
    }

    method logfile() {
	return $self->_get('main', 'LogFile');
    }

    method port() {
	return $self->_get('main', 'Port');
    }

    method sockettimeout() {
	return $self->_get('main', 'SocketTimeout');
    }

    # Per-card settings:
    method uploadkey(Str $card) {
        return $self->_cardsetting($card, 'uploadkey');
    }

    method folder(Str $card) {
        return $self->_cardsetting($card, 'folder');
    }

    method subfolder(Str $card) {
        return $self->_cardsetting($card, 'subfolder');
    }

    method subfoldertimesource(Str $card) {
        return $self->_cardsetting($card, 'subfoldertimesource');
    }

    method cardname(Str $card) {
        return $self->_cardsetting($card, 'name');
    }

    # I have no idea how important this ID really is
    method counter() {
	return $self->_counter();
    }

    method bump_counter() {
	return $self->_counter( $self->_counter() + 1 );
    }
}

# Logging, accessed through the $log object:
class RIFEC::Log {
    use Data::Dumper;
    use HTTP::Date qw();
    use IO::File;
    use Carp qw(confess);

    has '_fh' => (is      => 'rw',
		  default => sub { \*STDOUT });

    # Short for LogLevel.  I get tired of typing.
    has '_ll' => (isa => 'HashRef[Str]',
		  is  => 'ro',
		  default => sub { {
		      'off'     => 100,
		      'warning' => 4,
		      'info'    => 3,
		      'debug'   => 2,
		      'trace'   => 1,
		      }});

    method BUILD(HashRef $args) {
	$self->open();
    }

    method DEMOLISH() {
	$self->_fh->close()
	    or confess "Unable to close logfile while exiting: $!";
    }

    method open() {
	if (my $lf = $config->logfile()) {
	    my $fh = IO::File->new($lf, O_WRONLY|O_APPEND)
		or confess "Unable to open logfile '$lf' for writing: $!";
	    $self->_fh($fh);
	}
    }

    method _get_preamble(Str $ll) {
	return sprintf("%s %7u %7s ",
		       HTTP::Date::time2isoz(),
		       $$,
		       uc $ll);
    }

    method _print_if(Str $loglevel, Str @str) {
	# Sanity check.
	foreach my $ll ($loglevel, $config->loglevel) {
	    confess sprintf("Invalid loglevel '%s'", $ll)
		unless $self->_ll->{lc $ll};
	}

	if ($self->_ll->{lc $loglevel} >= $self->_ll->{lc $config->loglevel})
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

    method trace(Str @str) {
	$self->_print_if('trace', @str);
    }

    method debug(Str @str) {
	$self->_print_if('debug', @str);
    }

    method info(Str @str) {
	$self->_print_if('info', @str);
    }

    method warning(Str @str) {
	$self->_print_if('warning', @str);
    }
}


class RIFEC::Session {
    use Digest::MD5 qw(md5_hex);
    use IO::File;
    use Data::Dumper;
    use Carp qw(confess);

    # new() params - things we must know right away:
    has 'card'                  => (isa => 'Str', is => 'rw', required => 1);
    has 'transfermode'          => (isa => 'Str', is => 'rw', required => 1);
    has 'transfermodetimestamp' => (isa => 'Str', is => 'rw', required => 1);
    has 'card_nonce'            => (isa => 'Str', is => 'rw', required => 1);

    # things we learn or figure out after construction:
    has 'server_nonce'  => (isa => 'Str', is => 'ro', builder => '_s_nonce');
    has 'authenticated' => (isa => 'Bool', is => 'rw', default => 0);

    # We used to use the Crypt::Random library for this, but it is not
    # available through the package manager of all Linux
    # distributions, and was just a wrapper around reading the data
    # from /dev/urandom anyway.
    method getrandom(Int $bytes) {
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

    method _s_nonce() {
	my $octets = $self->getrandom(16);
	return unpack("H*", $octets);
    }

    # This one mainly consists of sanity checks of the card and
    # session params
    method BUILD(HashRef $args) {
	# Only accept cards that have a config section
	$config->doiknow($args->{'card'});

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
	my $known_txmodes = 2 | 32 | 512;

	if ($args->{'transfermode'} & ~$known_txmodes) {
	    confess sprintf("Unsupported transfermode '%s' from card '%s' (%s)",
                            $args->{'transfermode'},
                            $config->cardname($args->{'card'}),
                            $args->{'card'});
	}
    }

    method server_credential() {
	return md5_hex(pack("H*", $self->card()),
		       pack("H*", $self->card_nonce()),
		       pack("H*", $config->uploadkey($self->card())));
    }

    method card_credential() {
	return md5_hex(pack("H*", $self->card()),
		       pack("H*", $config->uploadkey($self->card())),
		       pack("H*", $self->server_nonce()));
    }
}

class RIFEC::File {
    use Archive::Tar;
    use File::Spec;
    use File::Temp qw();
    use Digest::MD5 qw();
    use POSIX qw();
    use Data::Dumper;
    use Carp qw(confess);

    has 'session'       => (isa => 'RIFEC::Session', is => 'ro', required => 1);

    has 'tarfilename'   => (isa => 'Str', is => 'ro', required => 1);
    has 'size'          => (isa => 'Int', is => 'ro', required => 1);
    has 'encryption'    => (isa => 'Str', is => 'ro', required => 0);
    has 'filesignature' => (isa => 'Str', is => 'ro', required => 1);

    has 'integritydigest'   => (isa => 'Str', is => 'rw', required => 0);
    has 'calculated_digest' => (isa => 'Str', is => 'rw', required => 0);

    has '_tarfile' => (isa => 'Str', is => 'rw');
    has '_file'    => (isa => 'Str', is => 'rw');

    # If your camera produces files containing other characters than I
    # have thought of here, you may have to change this regexp.  (All
    # places checking the file name should use this, so it should be
    # sufficient to update this one place).
    our $filename_regexp = qr|\A [ a-z0-9._-]* \z|xi;

    # See http://forums.eye.fi/viewtopic.php?f=4&t=270#p3874
    #
    # This is an old-fashioned sub, not a method, because of execution
    # speed.  When calling this for every 512 bytes in a 20MB file,
    # the overhead is noticeable.
    sub _calculate_tcp_checksum {
	my ($self, $block) = @_;
	my $val = 0;

	map { $val += $_ } unpack "S*", $block;

        while (my $rest = $val >> 16) {
	    $val = ($val & 0xFFFF) + $rest;
	}
        return pack("S", ~$val);
    }

    method _calculate_integritydigest(Str $block) {
	$log->debug("Calculating integrity digest...");

	# Make sure it is 512-block aligned (it should be)
	confess "Content block not 512-byte aligned!"
	    if length($block) % 512;

	my $md5 = Digest::MD5->new();

	my $i = 0;
	while ($i < length($block)) {
	    $md5->add(
		$self->_calculate_tcp_checksum( substr($block, $i, 512) ));
	    $i += 512;
	}
	$md5->add(pack("H*", $config->uploadkey($self->session()->card())));

	my $md5sum = $md5->hexdigest();

	$log->debug("...done: %s", uc $md5sum);
	return lc $md5sum;
    }

    method store_content(Str $content) {
	my $folder = $config->folder($self->session->card);

	confess sprintf("Top-level destination directory '%s' not found", $folder)
	    unless -e $folder;
	confess sprintf("Top-level destination directory '%s' not a directory", $folder)
	    unless -d $folder;
	confess sprintf("Top-level destination directory '%s' not writeable", $folder)
	    unless -w $folder;

	# Calculate our integritydigest while the file contents are in
	# transfer.  We can't check them yet, though, since the
	# content is received before the integritydigest from the
	# card.
	$self->calculated_digest( $self->_calculate_integritydigest($content) );

	my $tfh = File::Temp->new(
	    TEMPLATE => sprintf(".rifec-transit-%d-XXXXXXXX", $$),
	    DIR      => $folder,
	    UNLINK   => 0);
	my $tfn = $tfh->filename;

	print $tfh $content;
	$tfh->close() or confess "Unable to close '$tfn': $!";
	$self->_tarfile($tfn); # Remember where we put it
	$log->debug("Saved file '%s' ('%s')", $tfn, $self->tarfilename());
    }

    method check() {
	# First that the size matches what the card said:
	my $stat_size = (stat($self->_tarfile))[7];
	if ($stat_size == $self->size)
	{
	    $log->debug("File '%s' is %d bytes long, as expected",
			$self->_tarfile,
			$self->size);
	}
	else {
	    $log->warning("File '%s' is %d bytes long, should have been %d",
                          $self->_tarfile,
                          $stat_size,
                          $self->size);
	    return;
	}

	# Then check the integrity digest field:
	if (lc($self->calculated_digest()) eq lc($self->integritydigest()))
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

    method _subfolder(Str $tmpimage) {
        my $card = $self->session->card;

        my $topfolder   = $config->folder($card);
        my $subfolder;
        my $subtemplate = $config->subfolder($card);

        if (!$subtemplate || $subtemplate =~ /\A \s* \z/mxi) {
            $log->debug("Empty/blank subfolder for card '%s', save to top folder '%s'",
                        $config->cardname($card),
                        $topfolder);
            return $topfolder;
        }

        if ($config->subfoldertimesource($card) eq 'exif') {
            confess "Sorry, EXIF time source is not implemented yet";
            #my $dt = Image::ExifTool::ImageInfo($tmpimage, 'DateTimeOriginal');
            # ...
        }
        # Default/fallback: Use computer clock at time of transfer:
        else {
            $subfolder = POSIX::strftime($subtemplate, localtime());
        }

        my $destination = File::Spec->catfile($topfolder, $subfolder);
        $log->debug("Destination subdirectory: '%s' -> '%s', full path: '%s'",
                    $subtemplate,
                    $subfolder,
                    $destination);

        # Next, create it:
        if (! -e $destination) {
            mkdir $destination
                or confess "Unable to mkdir '$destination': $!";
        }
        # Sanity check:
	confess sprintf("Destination directory '%s' not a directory", $destination)
	    unless -d $destination;
	confess sprintf("Destination directory '%s' not writeable", $destination)
	    unless -w $destination;

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
    method _link_file(Str $tempfile, Str $destination_name) {
	my $tries = 0;
	my $max   = 100;
	my $dst   = $destination_name;
	my $done  = undef;

	while (!$done && $tries < $max) {
	    if (link $tempfile, $dst) {
		$done = $dst;
		$log->debug("'%s' created OK", $dst);
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

    method _extract_tarfile() {
	my $tar = Archive::Tar->new($self->_tarfile());

	my @files = $tar->get_files();
	confess sprintf("I don't know how to handle tarballs with >1 files! (%s)",
                        join(", ", map { $_->name } @files))
	    if scalar(@files) > 1;

	my $f = shift @files;
	confess sprintf("Illegal name of file inside tarball: '%s'", $f->name)
	    unless $f->name =~ $RIFEC::File::filename_regexp;

        # The temp store file goes in the top-level dir, not the
        # per-date-dir:
	my $tfh = File::Temp->new(
	    TEMPLATE => sprintf(".rifec-store-%d-XXXXXXXX", $$),
	    DIR      => $config->folder( $self->session()->card() ),
	    UNLINK   => 0);
	my $tfn = $tfh->filename;

	$log->debug("Writing image '%s' to tempfile '%s'", $f->name, $tfn);

	print $tfh $f->get_content();
	$tfh->flush() or confess "Unable to flush '$tfn': $!";
	$tfh->sync()  or confess "Unable to sync '$tfn': $!";
	$tfh->close() or confess "Unable to close '$tfn': $!";

	# Return the filename of the file in the tarball plus the
	# tempfile this file is currently stored in:
	return ($f->name, $tfn);
    }

    method extract() {
	# Extract the tar file and save the contents to a temp file:
	my ($filename, $tempcontent) = $self->_extract_tarfile();

        # Create the sub folder if necessary:
        my $dest_folder = $self->_subfolder($tempcontent);

	# Do the hard linking from the final file name to the temp
	my $full_filename = File::Spec->catfile($dest_folder, $filename);

	my $outfile = $self->_link_file($tempcontent, $full_filename);

	$log->warning("Destination file '%s' saved as '%s' to avoid collision",
                      $full_filename, $outfile)
	    unless $outfile eq $full_filename;

	$self->_file($outfile); # Remember where we put it

	$log->debug("Removing tar file '%s'", $self->_tarfile());
        unlink $self->_tarfile
            or confess sprintf("Unable to unlink tarfile '%s': $!",
                               $self->_tarfile);

	$log->debug("Removing temp file '%s'", $tempcontent);
        unlink $tempcontent
            or confess "Unable to unlink tempfile '$tempcontent': $!";

	# Chmod it to use the default umask
	chmod 0666 & ~umask(), $self->_file
	    or $log->warning("Unable to chmod '%s'", $self->file);

	$log->info("File '%s' saved", $self->_file());
	return 'ok';
    }
}

class RIFEC::Handler {
    use Data::Dumper;
    use XML::Simple qw(:strict);
    use Encode qw();
    use HTTP::Message;
    use HTTP::Status qw(:constants);
    use Params::Validate qw(validate);
    use Carp qw(confess);

    has 'session' => (isa => 'RIFEC::Session', is => 'rw', required => 0);

    # We jump through some hoops to make XML::Simple output the SOAP
    # response the way we want it instead of having to template-write
    # the XML ourselves.  Most of those hoops are hidden here:
    method _wrap_response(Str $blockname, HashRef $values) {
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
    method _extract_params(HashRef $body, Str $bodyname) {
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

        # MooseX::Declare has checked that $body is a HashRef already,
        # so we can go straight on the keys inside it:
        confess "No element '$bodyname' in body"
            unless exists $body->{$bodyname} && defined $body->{$bodyname};
        confess "Element '$bodyname' is not a hash ref"
            unless ref($body->{$bodyname}) eq 'HASH';

        Params::Validate::validation_options('stack_skip' => 2);
        validate( @{[ $body->{$bodyname} ]}, $paramspec_of{$bodyname} );
    }

    method startsession(HashRef $soapbody) {
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

    method getphotostatus(HashRef $soapbody) {
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
		'fileid' => $config->counter(),
		'offset' => '0',
	    });
    }

    method _soapenvelope($part where { $_->isa('HTTP::Message') }) {
	my $content = $part->content();

        my $soapbody;
        my $eval_result = eval {
            $soapbody = XML::Simple::XMLin($content,
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

    method upload($request where { $_->isa('HTTP::Request') }) {
	$log->info("Upload from '%s' (%s)",
		   $config->cardname($self->session->card),
		   $self->session->card);

	confess "Session un-authenticated, upload is a no-go"
	    unless $self->session()->authenticated();

	my $file;

	my @expected_parts = ('SOAPENVELOPE', 'FILENAME', 'INTEGRITYDIGEST');
	foreach my $part ($request->parts())
	{
	    my ($partname) = $part->headers_as_string() =~ /name="(.*?)"/x;
	    confess "Unable to extract name from part header" unless $partname;

	    # We want those three parts in that particular order
	    my $expect_partname = shift @expected_parts;
	    confess sprintf("Expected part '%s', got '%s'", $expect_partname, $partname)
		unless $partname eq $expect_partname;

	    if ($partname eq 'SOAPENVELOPE')
	    {
		# The RIFEC::File object is initialized from the values
		# in the SOAP envelope:
		$file = $self->_soapenvelope($part);
	    }
	    elsif ($partname eq 'FILENAME')
	    {
		# Sanity checking:
		my ($fn) = $part->headers_as_string() =~ /filename="(.*?)"/x;
		confess "Unable to extract filename from part header" unless $fn;
		confess "File name differs from RIFEC::File state"
		    unless $fn eq $file->tarfilename();
		$file->store_content($part->content());
	    }
	    elsif ($partname eq 'INTEGRITYDIGEST')
	    {
		$file->integritydigest($part->content());
	    }
	}

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

    method marklastphotoinroll(HashRef $soapbody) {
	my $params = $self->_extract_params($soapbody, "ns1:MarkLastPhotoInRoll");

	$log->info("MarkLastPhotoInRoll from '%s' (%s)",
		   $config->cardname($self->session->card),
		   $self->session->card);
	$log->trace("MarkLastPhotoInRoll: " . Dumper($params));

	# As far as I can figure out, there is not much for us to do
	# here.
	return $self->_wrap_response('MarkLastPhotoInRollResponse', {});
    }

    method _make_http_reply(Num $status, Str $message, Str $body) {
	# Enforce CRLF:
	$body =~ s/ ([^\r]) \n /$1\r\n/gx;
	my $raw = Encode::encode_utf8($body);

	my $header = HTTP::Headers->new();

	$header->content_type         ('text/xml');
	$header->content_type_charset ('UTF-8');
	$header->content_length       (length($raw));
	$header->server               ('rifec.pl v0.7');
	$header->date                 (time);
	$header->header               ('pragma' => 'no-cache');

	return HTTP::Response->new($status, $message, $header, $raw);
    }

    method dispatch($request where { $_->isa('HTTP::Request') }) {
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
                my $body;
                my $xml_eval_result = eval {
                    $body = XML::Simple::XMLin($request->content(),
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
		$answer = $self->upload($request);
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
}

class RIFEC::Server {
    use HTTP::Daemon;
    use HTTP::Status;
    use Carp qw(confess);

    method run() {
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
		while (my $req = $conn->get_request())
		{
		    $log->debug("%s:%d -> %s %s",
				$conn->peerhost(), $conn->peerport(),
				$req->method, $req->uri->path);
                    #$log->trace("Request headers: " . $req->headers_as_string());
		    # All sanity checking is done in the handler
		    my $http_reply = $handler->dispatch($req);
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
}

# We don't support a lot of command line options, we defer to the
# config file for configuration.  However, we need a --help, and a
# possibility to set custom config files:

use Getopt::Long;
use Pod::Usage;
use Proc::Daemon;

my $cf_file;
my $daemonize;

GetOptions('help|h|?'    => sub { pod2usage(0) },
	   'config|c=s'  => \$cf_file,
	   'daemonize|d' => \$daemonize)
    or pod2usage(2);

$config    = RIFEC::Config->new('file' => $cf_file);
$log       = RIFEC::Log->new();
my $server = RIFEC::Server->new();

$config->say_hello();
# Daemonize after all the setup, since we want to be able to sanity
# check parameters etc. while still in the foreground
if ($daemonize) {
    if ($log->_fh->fileno == fileno(STDOUT)) {
	$log->warning("Daemon mode enabled while logging to STDOUT: " .
                      "Logs and error messages will disappear. " .
                      "Consider logging to a file instead.");
    }
    Proc::Daemon::Init();
    $log->open(); # since Proc::Daemon::Init closes all open fh's
}

$server->run();

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
