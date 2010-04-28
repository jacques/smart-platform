package RSP::Transaction::Plack;

use Moose;

extends 'RSP::Transaction';

use Plack::Response;

use Encode;
use utf8;

has hostname => (is => 'rw');

use Plack::Request;

sub app {
    return sub {
        my ($env) = @_;
        my $trans = RSP::Transaction::Plack->new();
        my $resp = $trans->process_transaction(Plack::Request->new($env));
        return $resp->finalize;
    };
}

sub encode_body {
  my $self = shift;
  my $body = shift;

  ##
  ## if we have a simple body string, use that, otherwise
  ##  we need to be a bit more clever
  ##
  if (!ref($body)) {
    $self->response->body( encode_utf8($body) );
  } else {
    if ( ref($body) eq 'JavaScript::Function' ) {
      ## it's a javascript function, call it and use the
      ## returned data
      my $content = $self->context->call( $body );
      if ($@) { die $@ };
      $self->response->body( encode_utf8($content) );
    } elsif ( ref($body) && ($body->isa('RSP::JSObject') || $body->does('RSP::Role::JSObject')) ) {
      if ( $body->isa('RSP::JSObject::File') ) {
          open(my $fh, $body->fullpath) or die "Could not open file: $!";
          $self->response->body($fh);
      } else {
	##
	## it's an object that exists in both JS and Perl, convert it
	##  to it's stringified form, with a hint for the content-type.
	##
        my $content = $body->as_string( type => $self->response->headers->content_type );
	$self->response->body(encode_utf8($content));
      }
    } elsif  ( ref($body) && $body->isa('JavaScript::Generator') ) {
        
        # XXX - Chunked encoding not yet supported
        die "Chunked encoding not yet supported";
     
=for comment
        my $resp = $self->response;
      $resp->headers->header('Transfer-Encoding', 'chunked');
      $resp->headers->trailer('X-Trailing');
      my $chunked = Mojo::Filter::Chunked->new;
      my $bytecount = bytes::length( $resp->build() ) + bytes::length( $self->request->build );
      $resp->body_cb(sub {
		       my $content  = shift;
		       my $result = $body->next();
		       if (!$result) {
			 my $header = Mojo::Headers->new;
			 $header->header('X-Trailing', 'true');
			 $self->end( 1 );  ## cleanup the transaction here because we couldn't do it earlier

			 $bytecount += bytes::length( $header->build );
			 my $bwreport = RSP::Consumption::Bandwidth->new();
			 $bwreport->count( $bytecount );
			 $bwreport->host( $self->hostname );
			 $bwreport->uri( $self->url );

			 $self->consumption_log( $bwreport );

			 return $chunked->build( $header );
		       } else {
			 $bytecount += bytes::length( $result );
			 return $chunked->build( $result );
		       }
		     });
=cut

    } else {
      ##
      ## we don't know what to do with it.
      ##
      die "don't know what to do with " . ref($body) . " object";
    }
  }

}

sub encode_array_response {
    my $self = shift;
    my $response = shift;
    my @resp = @$response;
    my ($code, $codestr, $headers, $body);
    if (@resp == 4) {
        ($code, $codestr, $headers, $body) = @resp;
    } elsif (@resp == 3) {
        ($code, $headers, $body) = @resp;
    }
    $self->response->code( $code );

    my @headers = @$headers;
    while( my $key = shift @headers ) {
        my $value = shift @headers;
        ## why do we need to special case this?
        $self->response->headers->push_header( $key, $value );
    }

  $self->encode_body( $body );
}

##
## turns the response from the code into the Mojo::Message object
## that the web server needs.
##
sub encode_response {
    my $self = shift;
    my $response = shift;

    if ( ref( $response ) && ref( $response ) eq 'ARRAY' ) {
        ## we're encoding a list...
        $self->encode_array_response( $response );
    } else {
        ## we're encoding a single thing...
        $self->response->headers->content_type( 'text/html' );
        $self->encode_body( $response );
    }

    $self->response->headers->remove_header('X-Powered-By');
    if ($self->response->headers->server && $self->response->headers->server =~ /Perl/) {
        $self->response->headers->server("Joyent Smart Platform (Plack)/$RSP::VERSION");
    }

    if ( $self->response->headers->header('Transfer-Encoding') &&
        $self->response->headers->header('Transfer-Encoding') eq 'chunked' ) {
        $self->response->headers->remove_header('Content-Length');
    } else {
        if ( !$self->response->headers->content_length) {
            use bytes;
            $self->response->headers->content_length(
	        length($self->response->body)
            );
        }
    }

}

##
## terminates the transaction
##
sub end {
    my $self = shift;
    my $post_callback = shift;

    if ($post_callback || !($self->response->headers->header('Transfer-Encoding') && $self->response->headers->header('Transfer-Encoding') eq "chunked")) {
        $self->report_consumption;
    }

    $self->SUPER::end();

    $self->cleanup_js_environment;
}

sub process_transaction {
    my ($self, $env) = @_;
    my $resp = Plack::Response->new();

    $self->hostname( $env->uri->host );
    $self->response($resp);
    $self->request($env);
    $self->bootstrap;
    $self->run;
    $self->end;

    return $self->response;
}

##
## return the HTTP request object translated into something that
##  JavaScript can process
##
sub build_entrypoint_arguments {
  my $self = shift;

  my $cookies;
  if ( $self->request->cookies ) {
    for my $cookie_name ( keys %{ $self->request->cookies } ) {
      my $name  = $cookie_name;
      my $value = $self->request->cookies->{ $name };
        $cookies->{$name} = "$value";
    }
  }

  my %body  = %{$self->request->body_parameters};
  foreach my $key (keys %body) {
    if ( !ref($body{$key})) {
      $body{$key} = Encode::decode("utf8", $body{$key});
    } else {
      my $body_param_array = $body{$key};
      $body{$key} = [];
      foreach my $p (@$body_param_array) {
	push @{ $body{ $key } }, Encode::decode("utf8", $p);
      }
    }
  }

  my %query = %{$self->request->query_parameters};

  my $request = {};
  my $uploads = {};
  eval {
    $request->{type}    = 'HTTP';
    $request->{uri}     = $self->request->request_uri;
    $request->{method}  = $self->request->method;
    $request->{query}   = \%query,
    $request->{body}    = \%body,
    $request->{cookies} = $cookies;

    ## if we've got a multipart request, don't bother with
    ## the content.

    if ( $self->request->uploads ) {
      ## map the uploads to RSP file objects
        for my $name (keys %{ $self->request->uploads }){
            my $val = $self->request->uploads->{$name};
            $uploads->{$name} = RSP::JSObject::File->new($val->path, $val->basename);
        }
    } else {
      $request->{content} = Encode::decode( 'utf8', $self->request->body );
    }

    $request->{headers} = {
			   map {
			     my $val = scalar( $self->request->headers->header( $_ ) );
			     ( $_ => $val )
			   } $self->request->headers->header_field_names
			  };

    $request->{queryString} = $self->request->uri->query;
  };

  $request->{uploads} = $uploads;

  return $request;
}

##
## this is mojo specific
##
sub bw_consumed {
    #my $self = shift;
  #my $ib = $self->inbound_bw_consumed;
  #my $ob = $self->outbound_bw_consumed;
  #return $ib + $ob;
}

sub outbound_bw_consumed {
  my $self = shift;
  bytes::length( $self->response->build() );
}

sub inbound_bw_consumed {
  my $self = shift;
  bytes::length( $self->request->build() );
}

1;
