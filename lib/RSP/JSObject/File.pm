package RSP::JSObject::File;

use strict;
use warnings;

use Carp qw( croak );
use MIME::Types;
my $mimetypes = MIME::Types->new;

use base 'RSP::JSObject';

sub new {
  my $class = shift;
  my $fn    = shift;
  my $jsface = shift; ## javascript facing name
  if (!-e $fn) {
    croak "$!: $jsface";
  }
  my $self  = { file => $fn, original => $jsface };
  bless $self, $class;
}

sub jsclass {
  return "File";
}

sub properties {
  return {
    'contents' => {
      'getter' => 'RSP::JSObject::File::as_string',
    },
    'filename' => {
      'getter' => 'RSP::JSObject::File::filename',
    },
    'mimetype' => {
      'getter' => 'RSP::JSObject::File::mimetype',
    },
    'size' => {
      'getter' => 'RSP::JSObject::File::size',
    },
    'length' => {
      'getter' => 'RSP::JSObject::File::size',
    },
    'mtime' =>{
      'getter' => 'RSP::JSObject::File::mtime',
    },
    'exists' => {
      'getter' => 'RSP::JSObject::File::exists',
    }
  };
}

sub methods {
  return {
    'toString' => sub {
      my $self = shift;
      return $self->filename;
    }
  };
}

sub as_function {
  my $self = shift;
  return sub {
    return $self->as_string;
  };
}

sub as_string {
  my $self = shift;
  my $fh   = IO::File->new( $self->{ file } );
  if (!$fh) {
      RSP::Error->throw("could not open $self->{file}: $!");
  }
  my $data = do {
    local $/;
    $fh->getline();
  };
  $fh->close;
  return $data;
}

sub mimetype {
  my $self = shift;
  $self->{original} =~ /\.(\w+)$/;
  my $ext = $1;
  return $mimetypes->mimeTypeOf( $ext )."";
}

## returns the javascript-facing filename
sub filename {
  my $self = shift;
  return $self->{original}
}

sub fullpath {
  my $self = shift;
  return $self->{file};
}

sub size {
  my $self = shift;
  return -s $self->{file};
}

sub exists {
  my $self = shift;
  return -e $self->{file};
}

sub mtime {
  my $self = shift;
  my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
      $atime,$mtime,$ctime,$blksize,$blocks)
      = stat($self->{file});  
  return $mtime;
}

1;
