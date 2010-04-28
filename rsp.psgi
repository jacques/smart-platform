#!/usr/bin/env perl
### Plack app

use RSP;
use RSP::Transaction::Plack;
my $rsp = RSP->new;
my $app = RSP::Transaction::Plack->app();

