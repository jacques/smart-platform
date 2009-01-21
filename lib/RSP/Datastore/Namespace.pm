package RSP::Datastore::Namespace;

use strict;
use warnings;

use RSP;
use RSP::Transaction;

use DBI;
use JSON::XS;
use SQL::Abstract;
use Set::Object;
use Carp qw( confess );
use Scalar::Util::Numeric qw( isnum isint isfloat );
use Digest::MD5 qw( md5_hex );
use base 'Class::Accessor::Chained';

__PACKAGE__->mk_accessors(qw(namespace conn tables sa cache));

sub new {
  my $class = shift;
  my $self  = { tables => {}, sa => SQL::Abstract->new };
  bless $self, $class;
}

sub create {
  my $class = shift;
  my $ns    = shift;
  my $self  = $class->new;
  $self->namespace( md5_hex($ns) );
  $self->conn( DBI->connect_cached("dbi:mysql:host=localhost","root","autoload") );
  $self->conn->do("create database " . $self->namespace);  
  $self->conn->do("use " . $self->namespace);

  $self->cache( RSP::Transaction->cache( $ns ) );
  return $self;
}

sub connect {
  my $class = shift;
  my $ns    = shift;
  my $self  = $class->new;
  my $db    = md5_hex($ns);
  $self->namespace( $db );
  $self->conn( DBI->connect_cached("dbi:mysql:host=localhost;database=$db","root","autoload") );

  $self->cache( RSP::Transaction->cache( $ns ) );

  return $self;
}

sub has_type_table {
  my $self = shift;
  my $type = shift;
  if (!$type) {
    confess "no type";
  }
  if (!keys %{ $self->tables }) {
    my $sth  = $self->conn->prepare_cached("SELECT TABLE_NAME FROM information_schema.TABLES WHERE TABLE_SCHEMA=?");
    $sth->execute( $self->namespace );
    while( my $row = $sth->fetchrow_arrayref ) {      
      my $typename = $row->[0];
      $typename =~ s/\_prop.+$//; 
      $self->tables->{ $typename } = 1;
    }
  }
  return $self->tables->{$type}
}

sub create_type_table {
  my $self = shift;
  my $type = shift;
  if (!$type) {
    confess "no type";
  }
  $self->conn->begin_work;
  eval {
    $self->conn->do("CREATE TABLE ${type}_prop_i ( id CHAR(50), propname CHAR(25), propval LONG ) TYPE=InnoDB");
    $self->conn->do("CREATE TABLE ${type}_prop_f ( id CHAR(50), propname CHAR(25), propval FLOAT ) TYPE=InnoDB");
    $self->conn->do("CREATE TABLE ${type}_prop_s ( id CHAR(50), propname CHAR(25), propval VARCHAR(256) ) TYPE=InnoDB");
    $self->conn->do("CREATE TABLE ${type}_prop_o ( id CHAR(50), propname CHAR(25), propval TEXT ) TYPE=InnoDB");
  };
  if ($@) {
    $self->conn->rollback;
    confess $@;
  }
  $self->conn->commit;
}

sub tables_for_type {
  my $self = shift;
  my $type = shift;
  if (!$type) {
    confess "no type";
  }
  my @suffixes = qw( f s o i );
  my @tables = ();
  foreach my $suffix (@suffixes) {
    push @tables, sprintf("%s_prop_%s", $type, $suffix );
  }
  return @tables;
}

sub remove {
  my $self = shift;
  my $type = shift;
  my $id   = shift;
  if (!$type) {
    confess "no type";
  }
  if (!$id) {
    confess "no id";
  }
  $self->conn->begin_work;
  eval {
    $self->_remove_in_transaction( $type, $id );
  };
  if ($@) {
    $self->conn->rollback;
    confess $@;
  }
  $self->conn->commit;
  return 1;
}

sub _remove_in_transaction {
  my $self = shift;
  my $type = shift;
  my $id   = shift;
  my $where  = { id => $id };
  foreach my $table ($self->tables_for_type( $type )) {
    my ($stmt, @bind) = $self->sa->delete($table, $where);
    my $sth = $self->conn->prepare_cached( $stmt );
    $sth->execute( @bind );
    $sth->finish;
  }
  $self->cache->remove( "${type}:${id}" );
}

sub read {
  my $self = shift;
  my $type = shift;
  my $id   = shift;
  if (!$type) {
    confess "no type";
  }
  if (!$id) {
    confess "no id";
  }
  if (!$self->has_type_table( $type )) {
    confess "no such type";
  } else {
    return $self->read_one_object( $type, $id );
  }  
}

sub read_one_object {
  my $self = shift;
  my $type = shift;
  my $id   = shift;
  my $cached = eval { JSON::XS::decode_json( $self->cache->get( "${type}:${id}" ) ); };
  if ( $cached ) {
    return $cached;
  }
  my $fields = ['propname', 'propval'];
  my $where  = { id => $id };
  my $obj;
  foreach my $table ($self->tables_for_type( $type )) {
    my ($stmt, @bind) = $self->sa->select($table, $fields, $where);
    my $sth = $self->conn->prepare_cached( $stmt );
    $sth->execute( @bind );
    while( my $row = $sth->fetchrow_hashref() ) {
      if (!$obj) {
	$obj = { id => $id };
      }
      $obj->{$row->{ propname }} = $row->{propval};
    }
    $sth->finish;
  }
  if (!$obj) {
    confess "no such object $type:$id";
  }
  return $obj;
}

sub write {
  my $self = shift;
  my $type = shift;
  my $obj  = shift;
  if (!$type) {
    confess "no type";
  }
  if (!$obj) {
    confess "no object";
  }
  if (!$self->has_type_table( $type )) {
    $self->create_type_table( $type );
    $self->write_one_object( $type, $obj );
  } else {
    $self->write_one_object( $type, $obj );
  }
}

sub write_one_object {
  my $self = shift;
  my $type = shift;
  my $obj  = shift;
  my $id   = $obj->{id};
  if (!$id) {
    confess "object has no id";
  }
  $self->conn->begin_work;
  eval {
    my $fields = [ 'id', 'propame', 'propval' ];
    $self->_remove_in_transaction( $type, $id );
    foreach my $key (keys %$obj) {
      next if $key eq 'id';
      my $val = $obj->{$key};
      my $svals = { id => $id,  propname => $key, propval => $val };
      my $table = $self->table_for( $type, value => $val );      
      if ( $self->valtype( $val ) eq 'ref' ) {
	$svals->{propval} = JSON::XS::encode_json( $val );
      }
      my ($stmt, @bind) = $self->sa->insert($table, $svals);
      my $sth = $self->conn->prepare_cached($stmt);      
      $sth->execute(@bind);
    }
    $self->cache->set( "${type}:${id}", JSON::XS::encode_json( $obj ) );
  };
  if ($@) {
    $self->conn->rollback;
    confess $@;
  }
  $self->conn->commit;
  return 1;
}

sub delete {
  my $class = shift;
  my $ns    = shift;
  my $self  = $class->connect( $ns );
  $self->conn->do("DROP DATABASE " . $self->namespace);
}

sub query {
  my $self  = shift;
  my $type  = shift;
  my $query = shift;
  my $opts  = shift;
  if (!$type) {
    confess "no type";
  }
  if (!$query) {
    confess "no query";
  }
  my @objects;

  my @sets;
  foreach my $key (keys %$query) {
    my $set = $self->query_one_set( $type, $key, $query->{$key} );    
    push @sets, $set unless $set->is_null;
  }
  if (!@sets) {
    return [];
  } else {
    my $set = shift @sets;
    while( my $nset = shift @sets ) {
      $set = $set->intersection( $nset );
    }
    foreach my $id (@$set) {
      push @objects, $self->read( $type, $id );
    }
  }

  return \@objects;
}

sub table_for {
  my $self  = shift;
  my $type  = shift;
  my $args  = { @_ };
  if (!$args->{value}) { 
    confess "no value";
  }
  my $dt = $self->valtype( $args->{value} );
  my $lookup = {
		'int' => 'i',
		'float' => 'f',
		'ref' => 'o',
		'string' => 's'
	       };
  return "${type}_prop_$lookup->{$dt}";
}

sub valtype {
  my $self = shift;
  my $val  = shift;
  if ( ref( $val ) ) {
    return 'ref';
  } elsif (isnum($val)) {
    if ( isint( $val ) ) {
      return 'int';
    } else {
      return 'float';
    }
  } else {
    return 'string';
  }
}

sub query_one_set {
  my $self = shift;
  my $type = shift;
  my $key  = shift;
  my $val  = shift;
  my $table;
  if (!ref($val)) {
    my $set  = Set::Object->new;
    $table = $self->table_for( $type, value => $val );
    my ($stmt, @bind) = $self->sa->select($table, ['id'], { propname => $key, propval => $val });
    my $sth = $self->conn->prepare_cached( $stmt );
    $sth->execute(@bind);
    while( my $row = $sth->fetchrow_arrayref() ) {
      $set->insert($row->[0]);
    }
    $sth->finish;
#    print "SET FROM QUERY $stmt (@bind) IS ", join(", ", @$set), "\n";
    return $set;
  }
}

1;
