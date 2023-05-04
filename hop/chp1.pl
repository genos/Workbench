#!/usr/bin/env perl

use Modern::Perl "2013";
use autodie;

sub section {
    my $sec = shift;
    my $line = '-' x 17;
    print "\n$line\nSection $sec\n$line\n";
}

=pod
binary expansion of an integer
=cut

section(1.1);

sub binary {
    my ($n) = @_;
    return $n if $n == 0 || $n == 1;
    my $k = int( $n >> 1 );
    my $b = $n % 2;
    my $E = binary($k);
    return $E . $b;
}
say binary(37);


section(1.2);

=pod
n!
=cut

sub factorial {
    my ($n) = @_;
    return $n <= 0 ? 1 : $n * factorial( $n - 1 );
}
say factorial(17);


section(1.3);

=pod
Towers of Hanoi
hanoi(N, start, end, extra)
=cut

sub hanoi {
    my ( $n, $start, $end, $extra ) = @_;
    if ( $n == 1 ) {
        say "Move disk #1 from $start to $end.";
    } else {
        hanoi( $n - 1, $start, $extra, $end );
        say "Move disk #$n from $start to $end";
        hanoi( $n - 1, $extra, $end, $start );
    }
}

hanoi( 3, 'A', 'C', 'B' );

=pod
New Hanoi, with extra argument to be called each time
=cut

sub hanoi2 {
    my ( $n, $start, $end, $extra, $move_disk ) = @_;
    if ( $n == 1 ) {
        $move_disk->( 1, $start, $end );
    } else {
        hanoi( $n - 1, $start, $extra, $end, $move_disk );
        $move_disk->( $n, $start, $end );
        hanoi( $n - 1, $extra, $end, $start, $move_disk );
    }
}

=pod
Callback for hanoi2 to print out moves
=cut

sub print_instruction {
    my ( $disk, $start, $end ) = @_;
    say "Move disk #$disk from $start to $end";
}

hanoi2( 3, 'A', 'C', 'B', \&print_instruction );

=pod
Callback for hanoi2 to check for illegal moves
=cut

my @position = ( '', ('A') x 3 );

sub check_move {
    my ( $disk, $start, $end ) = @_;
    if ( $disk < 1 || $disk > $#position ) {
        die "Bad disk number $disk. Should be in 1..$#position.\n";
    }
    unless ( $position[$disk] eq $start ) {
        die(      "Tried to move disk $disk from $start,"
                . " but it's on $position[$disk].\n" );
    }
    for my $i ( 1 .. $disk - 1 ) {
        if ( $position[$i] eq $start ) {
            die(      "Can't move disk $disk from $start"
                    . " because $i is on top of it.\n" );
        } elsif ( $position[$i] eq $end ) {
            die(      "Can't move disk $disk to $end "
                    . " because $i is already there.\n" );

        }
    }
    say "Moving disk $disk from $start to $end.";
    $position[$disk] = $end;
}

#hanoi2( 3, 'A', 'C', 'B', \&check_move );


section(1.4);
